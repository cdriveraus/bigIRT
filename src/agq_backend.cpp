// Adaptive Gauss-Hermite quadrature for the person integrals.
//
// WHY THIS EXISTS. The Laplace approximation to each person's marginal is
// biased for the 3PL and 4PL, and the bias does not average out across items:
// measured on simulated data (dev/optim/laplace_vs_agq.R), moving from the
// Laplace optimum to the optimum of the essentially exact marginal shifted
// every discrimination the same way, by about half a standard error for the
// 3PL and nearly one for the 4PL, with the asymptote means moving by one and a
// half to two. The shift is fixed per person while the standard errors shrink
// with N, which is the signature of the under-coverage the coverage study
// found. Laplace can also invent optima: on a 2,000-item 3PL fit it held one
// steep, high-guessing item at a point where the exact marginal's gradient in
// that item's difficulty was -3.6 per unit, and the fit ended 12 nats short.
//
// WHAT IT COMPUTES. For person i with free abilities theta (d of the K
// dimensions), centred at the Laplace mode m with precision P = L L',
//
//   log p(y_i) = -1/2 log|P| + d/2 log(2 pi)
//                + log sum_q w_q exp( lp_i(m + L^-T z_q) + |z_q|^2 / 2 )
//
// over the product Gauss-Hermite rule (z_q, w_q) for a standard normal
// weight. lp is the same log posterior kernel the Laplace person step
// maximises -- likelihood plus the prior's quadratic form, without the
// prior's normalising constant -- so with a single node at z = 0 this is the
// Laplace value exactly, which the tests use.
//
// THE GRADIENT is the Fisher identity: for any parameter psi,
//
//   d/dpsi log p(y_i) = sum_q pi_iq d/dpsi lp_i(theta_iq),
//
// with pi_iq the normalised node weights. That is the exact gradient of the
// quadrature objective when the nodes are held fixed, and of the true
// integral whatever the nodes; when the nodes are re-centred at each
// evaluation it differs from the derivative of the quadrature objective only
// by how the quadrature error moves with the centring. No third derivatives
// and no mode adjoint are needed, and every block -- item parameters, their
// covariate coefficients, person covariates, the ability mean and the latent
// covariance -- is a posterior expectation.

#include <RcppEigen.h>
#include <RcppParallel.h>
#include <vector>
#include <memory>
#include <cmath>
#include <algorithm>
#include "bigirt_obsindex.h"
using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

inline double agq_inv_logit(const double x) {
  if (x >= 0.0) { const double z = std::exp(-x); return 1.0 / (1.0 + z); }
  const double z = std::exp(x); return z / (1.0 + z);
}
inline double agq_softplus(const double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}

// A read-only view of an R array, or an owned copy when R handed over a
// different storage type (a view of a coerced temporary would dangle). Views
// are taken on the main thread at the .Call boundary; the array outlives the
// call. See BigIRTOwnedVector in laplace_backend.cpp for why not copies.
template <typename T>
struct AgqVec {
  std::shared_ptr< std::vector<T> > v;
  const T* p = nullptr;
  std::size_t len = 0;
  int nr = 0, nc = 0;
  AgqVec() : v(std::make_shared< std::vector<T> >()) {}
  inline const T& operator[](std::size_t i) const { return p[i]; }
  inline const T& operator()(int i, int j) const {
    return p[static_cast<std::size_t>(i) + static_cast<std::size_t>(nr) * static_cast<std::size_t>(j)];
  }
  inline std::size_t count() const { return len; }
  inline int size() const { return static_cast<int>(len); }
};
inline AgqVec<double> AgqVec_d(SEXP x) {
  AgqVec<double> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == REALSXP) { if (o.len) o.p = REAL(x); }
  else { Rcpp::NumericVector r(x); o.v = std::make_shared< std::vector<double> >(r.begin(), r.end());
    o.p = o.v->data(); }
  return o;
}
inline AgqVec<int> AgqVec_i(SEXP x) {
  AgqVec<int> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == INTSXP) { if (o.len) o.p = INTEGER(x); }
  else if (TYPEOF(x) == LGLSXP) { if (o.len) o.p = LOGICAL(x); }
  else { Rcpp::IntegerVector r(x); o.v = std::make_shared< std::vector<int> >(r.begin(), r.end());
    o.p = o.v->data(); }
  return o;
}

// Log-likelihood of one response and its derivatives with respect to the
// linear predictor and the two asymptotes; the probability is clamped exactly
// as the Laplace kernels clamp it, so the two objectives agree at a node.
struct AgqRow { double ll, g_eta, g_c, g_d; };
inline AgqRow agq_row(const double y, const double eta, const double c, const double d) {
  const double g = agq_inv_logit(eta);
  const double u = d - c;
  double p = c + u * g;
  if (p < 1e-12) p = 1e-12;
  if (p > 1.0 - 1e-12) p = 1.0 - 1e-12;
  const double r = std::max(p * (1.0 - p), 1e-12);
  const double coeff = (y - p) / r;
  AgqRow out;
  out.ll = y * std::log(p) + (1.0 - y) * std::log(1.0 - p);
  out.g_eta = coeff * u * g * (1.0 - g);
  out.g_c = coeff * (1.0 - g);
  out.g_d = coeff * g;
  return out;
}

// A rule for d free dimensions: M nodes z_q (M x d, column major) for a
// standard normal weight, and their log weights. The product Gauss-Hermite
// rule is one such rule; so is a set of quasi-Monte Carlo points with equal
// weights, for which the formula above is importance sampling through the
// Laplace Gaussian. Weights must be positive.
struct AgqRule { int M = 1, d = 0; std::vector<double> Z, lw; };

struct AgqWorker : public RcppParallel::Worker {
  const BigIRTObsIndex& obs_by_subj;
  AgqVec<int> score;
  AgqVec<double> theta_mode, precision, person_pred;
  AgqVec<int> fixed_ability;
  AgqVec<double> fixed_ability_value;
  AgqVec<int> A_ref, A_beta_row;
  AgqVec<double> A_fixed_value, A_pred;
  AgqVec<int> B_ref, B_beta_row, C_ref, C_beta_row, D_ref, D_beta_row;
  AgqVec<double> B_fixed_value, B_pred, C_fixed_value, C_pred, D_fixed_value, D_pred;
  AgqVec<double> Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta;
  AgqVec<double> prior_mean, prior_precision;
  AgqVec<int> free_mask;
  const std::vector<AgqRule>& rules;   // indexed by the number of free dimensions
  const int K, P;
  const bool want_person;
  // outputs owned by R, written per subject (disjoint)
  double* eap;        // N x K
  double* post_cov;   // K x K x N
  double* value_by_subj;
  // reduced
  double value;
  int failure;
  std::vector<double> gA, gB, gC, gD, gAb, gBb, gCb, gDb, gAbil, gMean, S;

  AgqWorker(const BigIRTObsIndex& obs_by_subj,
            const std::vector<AgqRule>& rules,
            int K, int P, bool want_person)
    : obs_by_subj(obs_by_subj), rules(rules), K(K), P(P),
      want_person(want_person), eap(nullptr), post_cov(nullptr), value_by_subj(nullptr),
      value(0.0), failure(0) {}

  AgqWorker(const AgqWorker& o, RcppParallel::Split)
    : obs_by_subj(o.obs_by_subj), score(o.score), theta_mode(o.theta_mode), precision(o.precision),
      person_pred(o.person_pred), fixed_ability(o.fixed_ability), fixed_ability_value(o.fixed_ability_value),
      A_ref(o.A_ref), A_beta_row(o.A_beta_row), A_fixed_value(o.A_fixed_value), A_pred(o.A_pred),
      B_ref(o.B_ref), B_beta_row(o.B_beta_row), C_ref(o.C_ref), C_beta_row(o.C_beta_row),
      D_ref(o.D_ref), D_beta_row(o.D_beta_row), B_fixed_value(o.B_fixed_value), B_pred(o.B_pred),
      C_fixed_value(o.C_fixed_value), C_pred(o.C_pred), D_fixed_value(o.D_fixed_value), D_pred(o.D_pred),
      Abilitybeta(o.Abilitybeta), invspApars(o.invspApars), invspAbeta(o.invspAbeta), Bpars(o.Bpars),
      Bbeta(o.Bbeta), logitCpars(o.logitCpars), logitCbeta(o.logitCbeta), logitDpars(o.logitDpars),
      logitDbeta(o.logitDbeta), prior_mean(o.prior_mean), prior_precision(o.prior_precision),
      free_mask(o.free_mask), rules(o.rules), K(o.K), P(o.P),
      want_person(o.want_person), eap(o.eap), post_cov(o.post_cov), value_by_subj(o.value_by_subj),
      value(0.0), failure(0) { zero_like(o); }

  void zero_like(const AgqWorker& o) {
    gA.assign(o.gA.size(), 0.0); gB.assign(o.gB.size(), 0.0);
    gC.assign(o.gC.size(), 0.0); gD.assign(o.gD.size(), 0.0);
    gAb.assign(o.gAb.size(), 0.0); gBb.assign(o.gBb.size(), 0.0);
    gCb.assign(o.gCb.size(), 0.0); gDb.assign(o.gDb.size(), 0.0);
    gAbil.assign(o.gAbil.size(), 0.0); gMean.assign(o.gMean.size(), 0.0);
    S.assign(o.S.size(), 0.0);
  }

  inline double beta_term(int obs, const AgqVec<double>& pred, const AgqVec<double>& beta, int brow) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return 0.0;
    double out = 0.0;
    for (int j = 0; j < pred.nc; ++j) out += pred(obs, j) * beta(brow - 1, j);
    return out;
  }
  inline void beta_grad(std::vector<double>& g, const AgqVec<double>& pred, const AgqVec<double>& beta,
                        int obs, int brow, double v) {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return;
    for (int j = 0; j < pred.nc; ++j)
      g[static_cast<std::size_t>(brow - 1 + beta.nr * j)] += v * pred(obs, j);
  }

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t s = begin; s < end; ++s) {
      const int subj = static_cast<int>(s);
      const BigIRTObsSpan obs = obs_by_subj[subj];
      const int n = static_cast<int>(obs.size());
      std::vector<int> act;
      for (int k = 0; k < K; ++k) if (free_mask(subj, k) != 0) act.push_back(k);
      const int d = static_cast<int>(act.size());

      VectorXd m(K), mu(K);
      MatrixXd P0(K, K), Pp(K, K);
      for (int k = 0; k < K; ++k) { m(k) = theta_mode(subj, k); mu(k) = prior_mean(subj, k); }
      for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) {
        const std::size_t ix = static_cast<std::size_t>(r + K * c) + static_cast<std::size_t>(K) * K * subj;
        P0(r, c) = prior_precision[ix]; Pp(r, c) = precision[ix];
      }
      // Row constants: loadings, intercept, asymptotes, the ability offset
      // from person covariates, and the chain-rule factors of the links.
      std::vector<double> brow(n), crow(n), drow(n), cmul(n), dmul(n);
      MatrixXd arow(n, K), amul(n, K), off(n, K);
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        const int bb = B_ref[o] > 0 ? (Bbeta.nr == 1 ? 1 : B_ref[o]) : 0;
        brow[oi] = B_ref[o] > 0 ? Bpars[B_ref[o] - 1] + beta_term(o, B_pred, Bbeta, bb) : B_fixed_value[o];
        crow[oi] = C_fixed_value[o]; cmul[oi] = 0.0;
        if (C_ref[o] > 0) {
          const int cb = logitCbeta.nr == 1 ? 1 : C_ref[o];
          const double sg = agq_inv_logit(logitCpars[C_ref[o] - 1] + beta_term(o, C_pred, logitCbeta, cb));
          crow[oi] = 0.5 * sg; cmul[oi] = 0.5 * sg * (1.0 - sg);
        }
        drow[oi] = D_fixed_value[o]; dmul[oi] = 0.0;
        if (D_ref[o] > 0) {
          const int db = logitDbeta.nr == 1 ? 1 : D_ref[o];
          const double sg = agq_inv_logit(logitDpars[D_ref[o] - 1] + beta_term(o, D_pred, logitDbeta, db));
          drow[oi] = 0.5 * sg + 0.5; dmul[oi] = 0.5 * sg * (1.0 - sg);
        }
        for (int k = 0; k < K; ++k) {
          double a = A_fixed_value(o, k), am = 0.0;
          if (A_ref(o, k) > 0) {
            const int ab = invspAbeta.nr == 1 ? 1 : A_ref(o, k);
            const double raw = invspApars[A_ref(o, k) - 1] + beta_term(o, A_pred, invspAbeta, ab);
            a = agq_softplus(raw); am = agq_inv_logit(raw);
          }
          arow(oi, k) = a; amul(oi, k) = am;
          double of = 0.0;
          if (fixed_ability(o, k) == 0) for (int j = 0; j < P; ++j) of += person_pred(o, j) * Abilitybeta(k, j);
          off(oi, k) = of;
        }
      }

      // Cholesky of the active block of the Laplace precision. Node offsets
      // are delta = L^-T z, so that delta has covariance P^-1.
      MatrixXd Lt;
      double half_logdet = 0.0;
      if (d > 0) {
        MatrixXd Pa(d, d);
        for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) Pa(r, c) = Pp(act[r], act[c]);
        Eigen::LLT<MatrixXd> llt(Pa);
        if (llt.info() != Eigen::Success) { failure = 1; continue; }
        Lt = llt.matrixU();
        for (int r = 0; r < d; ++r) half_logdet += std::log(Lt(r, r));
      }

      const AgqRule& rule = rules[static_cast<std::size_t>(d)];
      const long nq = rule.M;
      // Pass 1: the log integrand at every node, keeping each row's score
      // terms so the gradient pass need not evaluate the rows again.
      std::vector<double> lw(static_cast<std::size_t>(nq));
      std::vector<VectorXd> th(static_cast<std::size_t>(nq), m);
      std::vector<double> buf_eta(static_cast<std::size_t>(nq) * n), buf_c(buf_eta.size()), buf_d(buf_eta.size());
      for (long q = 0; q < nq; ++q) {
        double lwq = rule.lw[static_cast<std::size_t>(q)];
        VectorXd z(d);
        for (int k = 0; k < d; ++k) { z(k) = rule.Z[static_cast<std::size_t>(q + nq * k)];
          lwq += 0.5 * z(k) * z(k); }
        if (d > 0) {
          const VectorXd delta = Lt.triangularView<Eigen::Upper>().solve(z);
          for (int k = 0; k < d; ++k) th[q](act[k]) += delta(k);
        }
        const VectorXd dv = th[q] - mu;
        double lp = -0.5 * dv.dot(P0 * dv);
        const std::size_t base = static_cast<std::size_t>(q) * n;
        for (int oi = 0; oi < n; ++oi) {
          const int o = obs[oi];
          double eta = -brow[oi];
          for (int k = 0; k < K; ++k) {
            const double ab = fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : th[q](k) + off(oi, k);
            eta += arow(oi, k) * ab;
          }
          const AgqRow rt = agq_row(static_cast<double>(score[o]), eta, crow[oi], drow[oi]);
          lp += rt.ll;
          buf_eta[base + oi] = rt.g_eta; buf_c[base + oi] = rt.g_c; buf_d[base + oi] = rt.g_d;
        }
        lw[q] = lwq + lp;
      }
      double mx = -INFINITY;
      for (long q = 0; q < nq; ++q) mx = std::max(mx, lw[q]);
      double se = 0.0;
      for (long q = 0; q < nq; ++q) se += std::exp(lw[q] - mx);
      const double lse = mx + std::log(se);
      const double vi = lse - half_logdet + 0.5 * d * std::log(2.0 * M_PI);
      value += vi;
      if (value_by_subj) value_by_subj[subj] = vi;

      // Pass 2: posterior expectations. Each row's weighted sums over the
      // nodes are formed first and scattered into the gradient once per row.
      VectorXd Eth = VectorXd::Zero(K);
      MatrixXd Ett = MatrixXd::Zero(K, K);
      std::vector<double> pw(static_cast<std::size_t>(nq));
      for (long q = 0; q < nq; ++q) {
        pw[q] = std::exp(lw[q] - lse);
        const VectorXd dv = th[q] - mu;
        Eth += pw[q] * th[q];
        Ett += pw[q] * dv * dv.transpose();
      }
      {
        const VectorXd gm = P0 * (Eth - mu);
        for (int k = 0; k < K; ++k) gMean[k] += gm(k);
      }
      VectorXd s_ab(K);
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        double s_eta = 0.0, s_c = 0.0, s_d = 0.0;
        s_ab.setZero();
        for (long q = 0; q < nq; ++q) {
          const std::size_t ix = static_cast<std::size_t>(q) * n + oi;
          const double ge = pw[q] * buf_eta[ix];
          s_eta += ge; s_c += pw[q] * buf_c[ix]; s_d += pw[q] * buf_d[ix];
          for (int k = 0; k < K; ++k)
            s_ab(k) += ge * (fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : th[q](k) + off(oi, k));
        }
        for (int k = 0; k < K; ++k) {
          if (A_ref(o, k) > 0) {
            const double graw = s_ab(k) * amul(oi, k);
            gA[static_cast<std::size_t>(A_ref(o, k) - 1)] += graw;
            beta_grad(gAb, A_pred, invspAbeta, o, invspAbeta.nr == 1 ? 1 : A_ref(o, k), graw);
          }
          if (fixed_ability(o, k) == 0 && P > 0)
            for (int j = 0; j < P; ++j)
              gAbil[static_cast<std::size_t>(k + K * j)] += s_eta * arow(oi, k) * person_pred(o, j);
        }
        if (B_ref[o] > 0) {
          gB[static_cast<std::size_t>(B_ref[o] - 1)] -= s_eta;
          beta_grad(gBb, B_pred, Bbeta, o, Bbeta.nr == 1 ? 1 : B_ref[o], -s_eta);
        }
        if (C_ref[o] > 0) {
          const double gc = s_c * cmul[oi];
          gC[static_cast<std::size_t>(C_ref[o] - 1)] += gc;
          beta_grad(gCb, C_pred, logitCbeta, o, logitCbeta.nr == 1 ? 1 : C_ref[o], gc);
        }
        if (D_ref[o] > 0) {
          const double gd = s_d * dmul[oi];
          gD[static_cast<std::size_t>(D_ref[o] - 1)] += gd;
          beta_grad(gDb, D_pred, logitDbeta, o, logitDbeta.nr == 1 ? 1 : D_ref[o], gd);
        }
      }
      for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) S[static_cast<std::size_t>(r + K * c)] += Ett(r, c);
      if (want_person) {
        for (int k = 0; k < K; ++k) eap[static_cast<std::size_t>(subj) + static_cast<std::size_t>(obs_by_subj.size()) * k] = Eth(k);
        const VectorXd dm = Eth - mu;
        const MatrixXd C = Ett - dm * dm.transpose();
        for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c)
          post_cov[static_cast<std::size_t>(r + K * c) + static_cast<std::size_t>(K) * K * subj] = C(r, c);
      }
    }
  }

  void join(const AgqWorker& o) {
    value += o.value;
    if (o.failure) failure = o.failure;
    auto add = [](std::vector<double>& a, const std::vector<double>& b) {
      for (std::size_t i = 0; i < a.size(); ++i) a[i] += b[i]; };
    add(gA, o.gA); add(gB, o.gB); add(gC, o.gC); add(gD, o.gD);
    add(gAb, o.gAb); add(gBb, o.gBb); add(gCb, o.gCb); add(gDb, o.gDb);
    add(gAbil, o.gAbil); add(gMean, o.gMean); add(S, o.S);
  }
};

} // namespace

// Arguments follow the Laplace block kernels, plus the centring (mode and
// posterior precision from the Laplace person step), the one-dimensional
// Gauss-Hermite rule for a standard normal weight (nodes, log weights), and
// whether to return per-person posterior means and covariances.
extern "C" SEXP _bigIRT_agq_fg_cpp_impl(
    SEXP idSEXP, SEXP scoreSEXP, SEXP theta_modeSEXP, SEXP precisionSEXP,
    SEXP person_predSEXP, SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_beta_rowSEXP, SEXP A_predSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_beta_rowSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_beta_rowSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_beta_rowSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP, SEXP prior_precisionSEXP, SEXP free_maskSEXP,
    SEXP nodesSEXP, SEXP log_weightsSEXP, SEXP want_personSEXP, SEXP grain_sizeSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mode(theta_modeSEXP);
  Rcpp::NumericMatrix person_pred(person_predSEXP);
  Rcpp::NumericMatrix Abilitybeta(AbilitybetaSEXP);
  const int Nobs = id.size(), N = theta_mode.nrow(), K = theta_mode.ncol();
  const int P = person_pred.ncol() > 0 && Abilitybeta.ncol() == person_pred.ncol() ? person_pred.ncol() : 0;
  const bool want_person = Rcpp::as<bool>(want_personSEXP);
  const std::size_t grain = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));
  BigIRTObsIndex obs_by_subj;
  if (!obs_by_subj.build(id, Nobs, N)) Rcpp::stop("id must be coded from 1 to Nsubs.");
  // Rules for 0..K free dimensions: element d of the two lists is the node
  // matrix (M x d) and log weights for persons with d free dimensions.
  Rcpp::List zl(nodesSEXP), wl(log_weightsSEXP);
  if (zl.size() != K || wl.size() != K) Rcpp::stop("AGQ: need one rule for each of 1..K free dimensions.");
  std::vector<AgqRule> rules(static_cast<std::size_t>(K) + 1);
  rules[0].M = 1; rules[0].d = 0; rules[0].lw.assign(1, 0.0);
  for (int d = 1; d <= K; ++d) {
    Rcpp::NumericMatrix Z = zl[d - 1];
    Rcpp::NumericVector lwv = wl[d - 1];
    if (Z.ncol() != d || Z.nrow() != lwv.size() || Z.nrow() < 1)
      Rcpp::stop("AGQ: rule %d has the wrong shape.", d);
    rules[d].M = Z.nrow(); rules[d].d = d;
    rules[d].Z.assign(Z.begin(), Z.end()); rules[d].lw.assign(lwv.begin(), lwv.end());
  }
  AgqWorker w(obs_by_subj, rules, K, P, want_person);
  auto cp_d = [](SEXP x) { return AgqVec_d(x); };
  auto cp_i = [](SEXP x) { return AgqVec_i(x); };
  w.score = cp_i(scoreSEXP); w.theta_mode = cp_d(theta_modeSEXP); w.precision = cp_d(precisionSEXP);
  w.person_pred = cp_d(person_predSEXP); w.fixed_ability = cp_i(fixed_abilitySEXP);
  w.fixed_ability_value = cp_d(fixed_ability_valueSEXP);
  w.A_ref = cp_i(A_refSEXP); w.A_fixed_value = cp_d(A_fixed_valueSEXP);
  w.A_beta_row = cp_i(A_beta_rowSEXP); w.A_pred = cp_d(A_predSEXP);
  w.B_ref = cp_i(B_refSEXP); w.B_fixed_value = cp_d(B_fixed_valueSEXP);
  w.B_beta_row = cp_i(B_beta_rowSEXP); w.B_pred = cp_d(B_predSEXP);
  w.C_ref = cp_i(C_refSEXP); w.C_fixed_value = cp_d(C_fixed_valueSEXP);
  w.C_beta_row = cp_i(C_beta_rowSEXP); w.C_pred = cp_d(C_predSEXP);
  w.D_ref = cp_i(D_refSEXP); w.D_fixed_value = cp_d(D_fixed_valueSEXP);
  w.D_beta_row = cp_i(D_beta_rowSEXP); w.D_pred = cp_d(D_predSEXP);
  w.Abilitybeta = cp_d(AbilitybetaSEXP);
  w.invspApars = cp_d(invspAparsSEXP); w.invspAbeta = cp_d(invspAbetaSEXP);
  w.Bpars = cp_d(BparsSEXP); w.Bbeta = cp_d(BbetaSEXP);
  w.logitCpars = cp_d(logitCparsSEXP); w.logitCbeta = cp_d(logitCbetaSEXP);
  w.logitDpars = cp_d(logitDparsSEXP); w.logitDbeta = cp_d(logitDbetaSEXP);
  w.prior_mean = cp_d(prior_meanSEXP); w.prior_precision = cp_d(prior_precisionSEXP);
  w.free_mask = cp_i(free_maskSEXP);
  // A matrix with zero rows in R still reports its column count; the kernels
  // treat "no predictors" as zero columns.
  if (w.A_pred.nr == 0) w.A_pred.nc = 0;
  if (w.B_pred.nr == 0) w.B_pred.nc = 0;
  if (w.C_pred.nr == 0) w.C_pred.nc = 0;
  if (w.D_pred.nr == 0) w.D_pred.nc = 0;
  w.gA.assign(w.invspApars.count(), 0.0); w.gB.assign(w.Bpars.count(), 0.0);
  w.gC.assign(w.logitCpars.count(), 0.0); w.gD.assign(w.logitDpars.count(), 0.0);
  w.gAb.assign(w.invspAbeta.count(), 0.0); w.gBb.assign(w.Bbeta.count(), 0.0);
  w.gCb.assign(w.logitCbeta.count(), 0.0); w.gDb.assign(w.logitDbeta.count(), 0.0);
  w.gAbil.assign(static_cast<std::size_t>(K) * P, 0.0); w.gMean.assign(K, 0.0);
  w.S.assign(static_cast<std::size_t>(K) * K, 0.0);
  Rcpp::NumericMatrix eap(want_person ? N : 0, K);
  Rcpp::NumericVector post_cov(want_person ? K * K * N : 0);
  Rcpp::NumericVector vbs(N);
  if (want_person) { w.eap = eap.begin(); w.post_cov = post_cov.begin();
    post_cov.attr("dim") = Rcpp::IntegerVector::create(K, K, N); }
  w.value_by_subj = vbs.begin();
  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(N), w, grain);
  if (w.failure) Rcpp::stop("AGQ: a person's Laplace precision could not be factored.");
  auto asv = [](const std::vector<double>& v) { return Rcpp::NumericVector(v.begin(), v.end()); };
  Rcpp::NumericMatrix gAbil(K, P), S(K, K);
  std::copy(w.gAbil.begin(), w.gAbil.end(), gAbil.begin());
  std::copy(w.S.begin(), w.S.end(), S.begin());
  return Rcpp::List::create(
    Rcpp::Named("value") = w.value, Rcpp::Named("value_by_subj") = vbs,
    Rcpp::Named("grad_A") = asv(w.gA), Rcpp::Named("grad_B") = asv(w.gB),
    Rcpp::Named("grad_C") = asv(w.gC), Rcpp::Named("grad_D") = asv(w.gD),
    Rcpp::Named("grad_A_beta") = asv(w.gAb), Rcpp::Named("grad_B_beta") = asv(w.gBb),
    Rcpp::Named("grad_C_beta") = asv(w.gCb), Rcpp::Named("grad_D_beta") = asv(w.gDb),
    Rcpp::Named("grad_ability_beta") = gAbil, Rcpp::Named("grad_mean") = asv(w.gMean),
    Rcpp::Named("S") = S, Rcpp::Named("eap") = eap, Rcpp::Named("post_cov") = post_cov);
  END_RCPP
}
