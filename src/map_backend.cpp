// Joint MAP: the mode of the joint posterior over item parameters and person
// abilities, by profiling the persons out.
//
// WHAT IT COMPUTES. For item-side parameters psi (item parameters, their
// covariate coefficients, the ability mean and the person-covariate
// coefficients),
//
//   f(psi) = sum_i max_theta_i lp_i(theta_i; psi) + log p(psi),
//   lp_i   = sum_j log p(y_ij | theta_i, psi) - 1/2 (theta_i - mu)' P0 (theta_i - mu),
//
// which is the same person kernel the Laplace path maximises, without the
// prior's normalising constant. The maximiser of f is the joint mode: at it,
// every theta_i is its person's mode and psi is stationary. This replaces a
// Stan model that optimised every person and item coordinate jointly by
// L-BFGS, which had to find a few hundred thousand person coordinates with a
// quasi-Newton memory of a hundred and a single step size shared with the
// items.
//
// THE GRADIENT needs no adjoint. theta_i* maximises lp_i, so d lp_i / d theta
// is zero there in every free dimension and the envelope theorem leaves only
// the partial derivative in psi at the mode. That is the Laplace objective's
// gradient without the log-determinant terms -- which is also why the Laplace
// optimiser, its Fisher metric (the Schur complement of the joint information
// over the persons, which is this objective's curvature) and its certificate
// apply here unchanged. The gradient is exact only at the mode, so the person
// tolerance has to be tight; at the default 1e-6 the error is far below the
// optimiser's stopping rule.
//
// ONE PASS PER PERSON. Each person's rows are read once: their loadings,
// intercepts and asymptotes are formed, the mode is solved by Newton, and the
// gradient, the reported posterior precision and, on request, the item blocks
// of the Hessian are accumulated at it. The Laplace path takes three passes
// for the same (person step, row abilities, item kernel), forming the row
// parameters twice.
//
// THE NEWTON STEP is the Laplace person step's: Newton on the observed
// curvature where it is positive definite, with a short backtracking search,
// and Fisher scoring otherwise. Fisher information is always positive
// definite, which is what guarantees an uphill step from anywhere; the
// observed curvature converges quadratically near the mode. The mode solved
// for is the same either way.
//
// THE REPORTED PRECISION is the observed curvature at the mode, prior
// included, with the data's part clipped to positive semidefinite as on the
// Laplace path (src/laplace_backend.cpp, bigirt_psd_clip), so the two
// estimators report person posterior SDs the same way. It is conditional on
// the item parameters.
//
// THE ITEM BLOCKS (`block_items`, for the convergence certificate's fast
// route, R/certify.R) are the exact per-item blocks of this objective's
// Hessian:
//
//   d2f/dpsi_j2 = sum_r h_r + sum_i X_ij' H_i^-1 X_ij,
//
// with h_r the Hessian of row r's log-likelihood in item j's parameter slots,
// X_ij = sum over person i's rows of item j of d2 l / dtheta dpsi, and H_i the
// person's observed precision over the free dimensions. The second term is
// the curvature the person absorbs as their mode moves with the item. Each
// block keeps the coupling between an item and its own persons exactly and
// drops the coupling between two items through the persons who answered both,
// as the quadrature blocks do. Slots per item are B, the loading on each of
// the K dimensions, C and D (K + 3), in the order src/agq_backend.cpp uses,
// followed by the item's covariate coefficients: each row's h_r and X_ij
// terms are carried into them by the row's own covariate values before an
// item's rows are summed (src/bigirt_item_blocks.h).

#include <RcppEigen.h>
#include <RcppParallel.h>
#include <vector>
#include <memory>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <chrono>
#include "bigirt_obsindex.h"
#include "bigirt_item_blocks.h"
using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

inline double map_inv_logit(const double x) {
  if (x >= 0.0) { const double z = std::exp(-x); return 1.0 / (1.0 + z); }
  const double z = std::exp(x); return z / (1.0 + z);
}
inline double map_softplus(const double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}

// A read-only view of an R array taken on the main thread at the .Call
// boundary, or an owned copy when R handed over another storage type (a view
// of a coerced temporary would dangle). See BigIRTOwnedVector in
// laplace_backend.cpp for why views rather than copies.
template <typename T>
struct MapVec {
  std::shared_ptr< std::vector<T> > own;
  const T* p = nullptr;
  std::size_t len = 0;
  int nr = 0, nc = 0;
  inline const T& operator[](std::size_t i) const { return p[i]; }
  inline const T& operator()(int i, int j) const {
    return p[static_cast<std::size_t>(i) + static_cast<std::size_t>(nr) * static_cast<std::size_t>(j)];
  }
};
inline MapVec<double> map_d(SEXP x) {
  MapVec<double> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == REALSXP) { if (o.len) o.p = REAL(x); }
  else {
    Rcpp::NumericVector r(x);
    o.own = std::make_shared< std::vector<double> >(r.begin(), r.end());
    o.p = o.own->data();
  }
  return o;
}
inline MapVec<int> map_i(SEXP x) {
  MapVec<int> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == INTSXP) { if (o.len) o.p = INTEGER(x); }
  else if (TYPEOF(x) == LGLSXP) { if (o.len) o.p = LOGICAL(x); }
  else {
    Rcpp::IntegerVector r(x);
    o.own = std::make_shared< std::vector<int> >(r.begin(), r.end());
    o.p = o.own->data();
  }
  return o;
}

// One response's log-likelihood and its first two derivatives in the linear
// predictor, at the clamped probability the Laplace kernels use, plus the
// Fisher information. `wl` is the observed curvature -d2 l/d eta2 (the same
// expression as bigirt_row_terms' dgrad_eta_deta, negated).
struct MapRow { double ll, ge, gc, gd, w, wl, g, p, coeff; };
inline MapRow map_row(const double y, const double eta, const double c, const double d) {
  MapRow o;
  o.g = map_inv_logit(eta);
  const double q = o.g * (1.0 - o.g);
  const double u = d - c;
  double p = c + u * o.g;
  if (p < 1e-12) p = 1e-12;
  if (p > 1.0 - 1e-12) p = 1.0 - 1e-12;
  o.p = p;
  const double s = u * q;
  const double r = std::max(p * (1.0 - p), 1e-12);
  const double e = y - p;
  o.coeff = e / r;
  o.ll = y * std::log(p) + (1.0 - y) * std::log(1.0 - p);
  o.ge = o.coeff * s;
  o.gc = o.coeff * (1.0 - o.g);
  o.gd = o.coeff * o.g;
  o.w = s * s / r;
  const double dq = q * (1.0 - 2.0 * o.g);
  const double ds = u * dq;
  const double dr = s * (1.0 - 2.0 * p);
  const double dcoeff = (-s * r - e * dr) / (r * r);
  o.wl = -(dcoeff * s + o.coeff * ds);
  return o;
}

struct MapWorker : public RcppParallel::Worker {
  const BigIRTObsIndex& obs_by_subj;
  MapVec<int> score;
  MapVec<double> theta_init, person_pred;
  MapVec<int> fixed_ability;
  MapVec<double> fixed_ability_value;
  MapVec<int> A_ref;
  MapVec<double> A_fixed_value, A_pred;
  MapVec<int> A_signed;
  MapVec<int> B_ref, C_ref, D_ref;
  MapVec<double> B_fixed_value, B_pred, C_fixed_value, C_pred, D_fixed_value, D_pred;
  MapVec<double> Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta,
    logitDpars, logitDbeta;
  MapVec<double> prior_mean, prior_precision;
  MapVec<int> free_mask;
  MapVec<int> block_items;
  const int K, P;
  const double jitter, tol;
  const int max_attempts, max_iter;
  const bool keep_covariance;
  int n_items = 0, S_slots = 0;
  BigirtBlockLayout bl;
  // Outputs owned by R, written per subject (disjoint).
  double* theta_mode = nullptr;   // N x K
  double* precision = nullptr;    // K x K x N
  double* precision_chol = nullptr;
  double* covariance = nullptr;
  double* logdet = nullptr;
  double* lp_by_subj = nullptr;
  int* niter = nullptr;
  int* converged = nullptr;
  int* status = nullptr;
  double* re_eta = nullptr;       // optional per-row effective values
  double* re_c = nullptr;
  double* re_d = nullptr;
  double* re_load = nullptr;      // Nobs x K
  int nobs = 0;
  // Reduced.
  double value = 0.0;
  std::vector<double> gA, gB, gC, gD, gAb, gBb, gCb, gDb, gAbil, gMean, blk;

  MapWorker(const BigIRTObsIndex& ix, int K, int P, double jitter, double tol,
            int max_attempts, int max_iter, bool keep_covariance)
    : obs_by_subj(ix), K(K), P(P), jitter(jitter), tol(tol), max_attempts(max_attempts),
      max_iter(max_iter), keep_covariance(keep_covariance) {}

  MapWorker(const MapWorker& o, RcppParallel::Split)
    : obs_by_subj(o.obs_by_subj), score(o.score), theta_init(o.theta_init), person_pred(o.person_pred),
      fixed_ability(o.fixed_ability), fixed_ability_value(o.fixed_ability_value),
      A_ref(o.A_ref), A_fixed_value(o.A_fixed_value), A_pred(o.A_pred), A_signed(o.A_signed),
      B_ref(o.B_ref), C_ref(o.C_ref), D_ref(o.D_ref),
      B_fixed_value(o.B_fixed_value), B_pred(o.B_pred), C_fixed_value(o.C_fixed_value), C_pred(o.C_pred),
      D_fixed_value(o.D_fixed_value), D_pred(o.D_pred),
      Abilitybeta(o.Abilitybeta), invspApars(o.invspApars), invspAbeta(o.invspAbeta), Bpars(o.Bpars),
      Bbeta(o.Bbeta), logitCpars(o.logitCpars), logitCbeta(o.logitCbeta), logitDpars(o.logitDpars),
      logitDbeta(o.logitDbeta), prior_mean(o.prior_mean), prior_precision(o.prior_precision),
      free_mask(o.free_mask), block_items(o.block_items), K(o.K), P(o.P), jitter(o.jitter), tol(o.tol),
      max_attempts(o.max_attempts), max_iter(o.max_iter), keep_covariance(o.keep_covariance),
      n_items(o.n_items), S_slots(o.S_slots), bl(o.bl),
      theta_mode(o.theta_mode), precision(o.precision), precision_chol(o.precision_chol),
      covariance(o.covariance), logdet(o.logdet), lp_by_subj(o.lp_by_subj), niter(o.niter),
      converged(o.converged), status(o.status), re_eta(o.re_eta), re_c(o.re_c), re_d(o.re_d),
      re_load(o.re_load), nobs(o.nobs) {
    gA.assign(o.gA.size(), 0.0); gB.assign(o.gB.size(), 0.0);
    gC.assign(o.gC.size(), 0.0); gD.assign(o.gD.size(), 0.0);
    gAb.assign(o.gAb.size(), 0.0); gBb.assign(o.gBb.size(), 0.0);
    gCb.assign(o.gCb.size(), 0.0); gDb.assign(o.gDb.size(), 0.0);
    gAbil.assign(o.gAbil.size(), 0.0); gMean.assign(o.gMean.size(), 0.0);
    blk.assign(o.blk.size(), 0.0);
  }

  // A covariate coefficient row: one shared row, or the row of the item's
  // own free parameter (itemSpecificBetas). 0 for a fixed parameter.
  static inline int beta_row(const int ref, const MapVec<double>& beta) {
    return ref > 0 ? (beta.nr == 1 ? 1 : ref) : 0;
  }
  inline double beta_term(int o, const MapVec<double>& pred, const MapVec<double>& beta, int brow) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return 0.0;
    double out = 0.0;
    for (int j = 0; j < pred.nc; ++j) out += pred(o, j) * beta(brow - 1, j);
    return out;
  }
  inline void beta_grad(std::vector<double>& g, const MapVec<double>& pred, const MapVec<double>& beta,
                        int o, int brow, double v) {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0 || v == 0.0) return;
    for (int j = 0; j < pred.nc; ++j)
      g[static_cast<std::size_t>(brow - 1 + beta.nr * j)] += v * pred(o, j);
  }

  void operator()(std::size_t begin, std::size_t end) {
    // Per-split scratch, grown to the largest person seen.
    std::vector<double> eta0, b_, c_, d_, cm_, dm_, a_, am_, e_, ab0_, mult_;
    for (std::size_t s = begin; s < end; ++s) {
      const int subj = static_cast<int>(s);
      const BigIRTObsSpan obs = obs_by_subj[s];
      const int n = static_cast<int>(obs.size());
      const std::size_t nK = static_cast<std::size_t>(n) * K;
      eta0.resize(n); b_.resize(n); c_.resize(n); d_.resize(n); cm_.resize(n); dm_.resize(n);
      a_.resize(nK); am_.resize(nK); e_.resize(nK); ab0_.resize(nK); mult_.resize(nK);

      VectorXd mu(K), theta(K);
      MatrixXd P0(K, K);
      std::vector<int> act;
      for (int k = 0; k < K; ++k) {
        mu(k) = prior_mean(subj, k);
        theta(k) = theta_init(subj, k);
        if (free_mask(subj, k) != 0) act.push_back(k);
      }
      for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c)
        P0(r, c) = prior_precision[static_cast<std::size_t>(r + K * c) + static_cast<std::size_t>(K) * K * subj];
      const int d = static_cast<int>(act.size());

      // Row constants. A row's ability on dimension k is ab0 + mult * theta_k:
      // the fixed value (mult 0) where the ability is fixed, otherwise the
      // person-covariate offset plus the base ability. e = loading * mult is
      // what the base ability sees, and eta0 is everything else in the linear
      // predictor.
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        b_[oi] = B_ref[o] > 0 ? Bpars[B_ref[o] - 1] + beta_term(o, B_pred, Bbeta, beta_row(B_ref[o], Bbeta))
                              : B_fixed_value[o];
        c_[oi] = C_fixed_value[o]; cm_[oi] = 0.0;
        if (C_ref[o] > 0) {
          const double sg = map_inv_logit(logitCpars[C_ref[o] - 1] +
            beta_term(o, C_pred, logitCbeta, beta_row(C_ref[o], logitCbeta)));
          c_[oi] = 0.5 * sg; cm_[oi] = 0.5 * sg * (1.0 - sg);
        }
        d_[oi] = D_fixed_value[o]; dm_[oi] = 0.0;
        if (D_ref[o] > 0) {
          const double sg = map_inv_logit(logitDpars[D_ref[o] - 1] +
            beta_term(o, D_pred, logitDbeta, beta_row(D_ref[o], logitDbeta)));
          d_[oi] = 0.5 * sg + 0.5; dm_[oi] = 0.5 * sg * (1.0 - sg);
        }
        double e0 = -b_[oi];
        for (int k = 0; k < K; ++k) {
          const std::size_t ik = static_cast<std::size_t>(oi) * K + k;
          double a = A_fixed_value(o, k), am = 0.0;
          if (A_ref(o, k) > 0) {
            const int q = A_ref(o, k);
            const double raw = invspApars[q - 1] + beta_term(o, A_pred, invspAbeta, beta_row(q, invspAbeta));
            // The loading link: softplus, or the identity for a signed
            // loading (see the note at the top of laplace_backend.cpp).
            if (A_signed[q - 1] != 0) { a = raw; am = 1.0; }
            else { a = map_softplus(raw); am = map_inv_logit(raw); }
          }
          a_[ik] = a; am_[ik] = am;
          if (fixed_ability(o, k) != 0) {
            ab0_[ik] = fixed_ability_value(o, k); mult_[ik] = 0.0;
          } else {
            double of = 0.0;
            for (int j = 0; j < P; ++j) of += person_pred(o, j) * Abilitybeta(k, j);
            ab0_[ik] = of; mult_[ik] = 1.0;
          }
          e_[ik] = a * mult_[ik];
          e0 += a * ab0_[ik];
        }
        eta0[oi] = e0;
      }

      // The person's log posterior kernel, its gradient in theta, and both
      // curvatures (Fisher for the safe step, observed for the fast one).
      auto evaluate = [&](const VectorXd& th, double& lp, VectorXd& grad, MatrixXd& Hf, MatrixXd& Ho) {
        const VectorXd dev = th - mu;
        const VectorXd Pd = P0 * dev;
        lp = -0.5 * dev.dot(Pd);
        grad = -Pd;
        Hf = P0; Ho = P0;
        for (int oi = 0; oi < n; ++oi) {
          const double* e = &e_[static_cast<std::size_t>(oi) * K];
          double eta = eta0[oi];
          for (int k = 0; k < K; ++k) eta += e[k] * th(k);
          const MapRow rt = map_row(static_cast<double>(score[obs[oi]]), eta, c_[oi], d_[oi]);
          lp += rt.ll;
          for (int r = 0; r < K; ++r) {
            if (e[r] == 0.0) continue;
            grad(r) += rt.ge * e[r];
            for (int c = 0; c < K; ++c) {
              if (e[c] == 0.0) continue;
              Hf(r, c) += rt.w * e[r] * e[c];
              Ho(r, c) += rt.wl * e[r] * e[c];
            }
          }
        }
      };

      double lp = 0.0;
      VectorXd grad(K);
      MatrixXd Hf(K, K), Ho(K, K);
      evaluate(theta, lp, grad, Hf, Ho);
      bool conv = (d == 0);
      int st = conv ? 0 : 1, used = 0;
      VectorXd gsub(d), th_try(K), g_try(K);
      MatrixXd Hf_try(K, K), Ho_try(K, K);
      for (int it = 0; it < max_iter && !conv; ++it) {
        used = it + 1;
        double gn = 0.0;
        for (int a = 0; a < d; ++a) { gsub(a) = grad(act[a]); gn += gsub(a) * gsub(a); }
        if (std::sqrt(gn) < tol) { conv = true; break; }
        bool moved = false;
        // Newton on the observed curvature where it is positive definite.
        {
          MatrixXd Hs(d, d);
          for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) Hs(r, c) = Ho(act[r], act[c]);
          Eigen::LLT<MatrixXd> llt(Hs);
          if (llt.info() == Eigen::Success) {
            const VectorXd step = llt.solve(gsub);
            double damp = 1.0;
            for (int bt = 0; bt < 4 && !moved; ++bt) {
              th_try = theta;
              for (int a = 0; a < d; ++a) th_try(act[a]) += damp * step(a);
              double lp_try;
              evaluate(th_try, lp_try, g_try, Hf_try, Ho_try);
              if (std::isfinite(lp_try) && lp_try >= lp - 1e-10) {
                theta = th_try; lp = lp_try; grad = g_try; Hf = Hf_try; Ho = Ho_try; moved = true;
              } else damp *= 0.5;
            }
          }
        }
        if (moved) continue;
        // Fisher scoring, with a ridge if rounding has broken definiteness.
        MatrixXd Hs(d, d);
        for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) Hs(r, c) = Hf(act[r], act[c]);
        Eigen::LLT<MatrixXd> llt;
        double ridge = jitter;
        bool ok = false;
        for (int attempt = 0; attempt < max_attempts && !ok; ++attempt) {
          MatrixXd Ht = Hs;
          Ht.diagonal().array() += ridge;
          llt.compute(Ht);
          ok = llt.info() == Eigen::Success;
          ridge *= 10.0;
        }
        if (!ok) { st = 4; break; }
        const VectorXd step = llt.solve(gsub);
        double damp = 1.0;
        for (int bt = 0; bt < 12 && !moved; ++bt) {
          th_try = theta;
          for (int a = 0; a < d; ++a) th_try(act[a]) += damp * step(a);
          double lp_try;
          evaluate(th_try, lp_try, g_try, Hf_try, Ho_try);
          if (std::isfinite(lp_try) && lp_try >= lp - 1e-10) {
            theta = th_try; lp = lp_try; grad = g_try; Hf = Hf_try; Ho = Ho_try; moved = true;
          } else damp *= 0.5;
        }
        if (!moved) { st = 2; break; }
      }
      if (!conv && st == 1 && d > 0) {
        double gn = 0.0;
        for (int a = 0; a < d; ++a) gn += grad(act[a]) * grad(act[a]);
        if (std::sqrt(gn) < tol) conv = true;
      }
      if (conv) st = 0;

      // Outputs for the person: the mode, and the reported precision -- the
      // observed curvature with the data's part clipped to positive
      // semidefinite, fixed dimensions pinned -- as the Laplace path reports it.
      for (int k = 0; k < K; ++k) theta_mode[subj + static_cast<std::size_t>(obs_by_subj.size()) * k] = theta(k);
      lp_by_subj[subj] = lp;
      niter[subj] = used;
      converged[subj] = conv ? 1 : 0;
      status[subj] = st;
      {
        // Clipped over the free abilities only, as the Laplace kernels do
        // (bigirt_guard_free_curvature in laplace_backend.cpp): a fixed
        // ability's curvature has no say in the free block.
        MatrixXd like = Ho - P0;
        for (int k = 0; k < K; ++k) if (free_mask(subj, k) == 0) { like.row(k).setZero(); like.col(k).setZero(); }
        if (d > 0) {
          MatrixXd sub(d, d);
          for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) sub(r, c) = like(act[r], act[c]);
          Eigen::LDLT<MatrixXd> ldl(sub);
          if (ldl.info() != Eigen::Success || ldl.vectorD().minCoeff() < 0.0) {
            Eigen::SelfAdjointEigenSolver<MatrixXd> es(sub);
            const VectorXd lam = es.eigenvalues().cwiseMax(0.0);
            sub = es.eigenvectors() * lam.asDiagonal() * es.eigenvectors().transpose();
            for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) like(act[r], act[c]) = sub(r, c);
          }
        }
        MatrixXd prec = P0 + like;
        for (int k = 0; k < K; ++k) if (free_mask(subj, k) == 0) {
          prec.row(k).setZero(); prec.col(k).setZero(); prec(k, k) = 1.0 / jitter;
        }
        Eigen::LLT<MatrixXd> llt;
        double ridge = jitter;
        bool ok = false;
        for (int attempt = 0; attempt < max_attempts && !ok; ++attempt) {
          MatrixXd Pt = prec;
          Pt.diagonal().array() += ridge;
          llt.compute(Pt);
          if (llt.info() == Eigen::Success) { prec = Pt; ok = true; }
          ridge *= 10.0;
        }
        if (!ok) { prec = MatrixXd::Identity(K, K) / jitter; llt.compute(prec); status[subj] = 5; }
        const MatrixXd L = llt.matrixL();
        double ld = 0.0;
        for (int k = 0; k < K; ++k) ld += 2.0 * std::log(std::max(L(k, k), 1e-300));
        logdet[subj] = ld;
        const std::size_t base = static_cast<std::size_t>(K) * K * subj;
        for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) {
          precision[base + r + K * c] = prec(r, c);
          precision_chol[base + r + K * c] = L(r, c);
        }
        if (keep_covariance) {
          const MatrixXd Sg = llt.solve(MatrixXd::Identity(K, K));
          for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) covariance[base + r + K * c] = Sg(r, c);
        }
      }

      // The objective and its gradient at the mode: partial derivatives only
      // (envelope theorem; see the header).
      value += lp;
      {
        const VectorXd Pd = P0 * (theta - mu);
        for (int k = 0; k < K; ++k) gMean[k] += Pd(k);
      }
      const bool blocks = n_items > 0;
      const int SS = S_slots, EE = bl.E;
      // For the item blocks: the person's observed precision over the free
      // dimensions, and each row's cross derivatives d2 l / dtheta dpsi in
      // the item's coordinates (slots, then covariate coefficients).
      Eigen::LDLT<MatrixXd> Hldl;
      bool use_corr = false;
      std::vector<double> X;          // n x d x EE, row-major by row
      if (blocks && d > 0) {
        MatrixXd Hs(d, d);
        for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c) Hs(r, c) = Ho(act[r], act[c]);
        Hldl.compute(Hs);
        use_corr = Hldl.info() == Eigen::Success && Hldl.isPositive() && Hldl.vectorD().minCoeff() > 0.0;
        if (use_corr) X.assign(static_cast<std::size_t>(n) * d * EE, 0.0);
      }
      std::vector<double> sv(SS), hv(static_cast<std::size_t>(SS) * SS);
      std::vector<double> xv(static_cast<std::size_t>(EE - SS) + 1), Xs(static_cast<std::size_t>(d) * SS + 1);
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        const double* e = &e_[static_cast<std::size_t>(oi) * K];
        const double* a = &a_[static_cast<std::size_t>(oi) * K];
        const double* am = &am_[static_cast<std::size_t>(oi) * K];
        const double* ab0 = &ab0_[static_cast<std::size_t>(oi) * K];
        const double* mu_ = &mult_[static_cast<std::size_t>(oi) * K];
        double eta = eta0[oi];
        for (int k = 0; k < K; ++k) eta += e[k] * theta(k);
        const double y = static_cast<double>(score[o]);
        const MapRow rt = map_row(y, eta, c_[oi], d_[oi]);
        if (re_eta) {
          re_eta[o] = eta; re_c[o] = c_[oi]; re_d[o] = d_[oi];
          for (int k = 0; k < K; ++k) re_load[o + static_cast<std::size_t>(nobs) * k] = a[k];
        }
        // Difficulty.
        if (B_ref[o] > 0) {
          gB[static_cast<std::size_t>(B_ref[o] - 1)] -= rt.ge;
          beta_grad(gBb, B_pred, Bbeta, o, beta_row(B_ref[o], Bbeta), -rt.ge);
        }
        // Loadings: d eta / d raw_k = ability_k * d loading / d raw.
        for (int k = 0; k < K; ++k) {
          if (A_ref(o, k) <= 0) continue;
          const double ab = ab0[k] + mu_[k] * theta(k);
          const double g_raw = rt.ge * ab * am[k];
          gA[static_cast<std::size_t>(A_ref(o, k) - 1)] += g_raw;
          beta_grad(gAb, A_pred, invspAbeta, o, beta_row(A_ref(o, k), invspAbeta), g_raw);
        }
        if (C_ref[o] > 0) {
          const double g = rt.gc * cm_[oi];
          gC[static_cast<std::size_t>(C_ref[o] - 1)] += g;
          beta_grad(gCb, C_pred, logitCbeta, o, beta_row(C_ref[o], logitCbeta), g);
        }
        if (D_ref[o] > 0) {
          const double g = rt.gd * dm_[oi];
          gD[static_cast<std::size_t>(D_ref[o] - 1)] += g;
          beta_grad(gDb, D_pred, logitDbeta, o, beta_row(D_ref[o], logitDbeta), g);
        }
        // Person covariates move the row's ability on the free dimensions.
        if (P > 0) for (int k = 0; k < K; ++k) {
          if (e[k] == 0.0) continue;
          const double gk = rt.ge * e[k];
          for (int j = 0; j < P; ++j) gAbil[static_cast<std::size_t>(k) + static_cast<std::size_t>(K) * j] += gk * person_pred(o, j);
        }

        if (!blocks) continue;
        const int it = block_items[o];
        if (it <= 0 || it > n_items) continue;
        // Score and Hessian of the row in its item's slots (B, the loading on
        // each dimension, C, D), as src/agq_backend.cpp forms them.
        const double g = rt.g, p = rt.p, coeff = rt.coeff;
        const double u = d_[oi] - c_[oi];
        const double lpp = -(y / (p * p) + (1.0 - y) / ((1.0 - p) * (1.0 - p)));
        const double sp = g * (1.0 - g), spp = sp * (1.0 - 2.0 * g);
        const double pe = u * sp, pc = 1.0 - g, pd = g, pee = u * spp, pec = -sp, ped = sp;
        const double le = coeff * pe, lc = coeff * pc, ld = coeff * pd;
        const double lee = lpp * pe * pe + coeff * pee, lec = lpp * pe * pc + coeff * pec;
        const double led = lpp * pe * pd + coeff * ped;
        const double lcc = lpp * pc * pc, lcd = lpp * pc * pd, ldd = lpp * pd * pd;
        const double eb = B_ref[o] > 0 ? -1.0 : 0.0;
        double ea[64], eaa[64];
        for (int k = 0; k < K; ++k) {
          ea[k] = 0.0; eaa[k] = 0.0;
          if (A_ref(o, k) > 0) {
            const double ab = ab0[k] + mu_[k] * theta(k);
            ea[k] = ab * am[k];
            eaa[k] = A_signed[A_ref(o, k) - 1] != 0 ? 0.0 : ab * am[k] * (1.0 - am[k]);
          }
        }
        const double cx = cm_[oi], cxx = cm_[oi] * (1.0 - 4.0 * c_[oi]);
        const double dz = dm_[oi], dzz = dm_[oi] * (3.0 - 4.0 * d_[oi]);
        const int iC = K + 1, iD = K + 2;
        std::fill(hv.begin(), hv.end(), 0.0);
        auto put = [&](int r, int c, double v) { hv[r + SS * c] = v; hv[c + SS * r] = v; };
        put(0, 0, lee * eb * eb);
        for (int k = 0; k < K; ++k) {
          put(0, 1 + k, lee * eb * ea[k]);
          for (int l = k; l < K; ++l)
            put(1 + k, 1 + l, lee * ea[k] * ea[l] + (k == l ? le * eaa[k] : 0.0));
          put(1 + k, iC, lec * ea[k] * cx);
          put(1 + k, iD, led * ea[k] * dz);
        }
        put(0, iC, lec * eb * cx); put(iC, iC, lcc * cx * cx + lc * cxx);
        put(0, iD, led * eb * dz); put(iD, iD, ldd * dz * dz + ld * dzz);
        put(iC, iD, lcd * cx * dz);
        bl.covariates(o, K, A_ref, B_ref, C_ref, D_ref, A_pred, B_pred, C_pred, D_pred, xv.data());
        bl.add(&blk[static_cast<std::size_t>(it - 1) * EE * EE], hv.data(), xv.data());
        if (use_corr) {
          // d2 l / dtheta_m dpsi_s = e_m (l_ee eta_s + l_ec c_x [s = C] +
          // l_ed d_z [s = D]) + l_e mult_m amul_m [s = loading m].
          sv[0] = lee * eb;
          for (int k = 0; k < K; ++k) sv[1 + k] = lee * ea[k];
          sv[iC] = lec * cx; sv[iD] = led * dz;
          for (int aa = 0; aa < d; ++aa) {
            const int m = act[aa];
            for (int ss = 0; ss < SS; ++ss) Xs[aa * SS + ss] = e[m] * sv[ss];
            if (A_ref(o, m) > 0) Xs[aa * SS + 1 + m] += le * mu_[m] * am[m];
          }
          bl.extend_rows(Xs.data(), d, xv.data(), &X[static_cast<std::size_t>(oi) * d * EE]);
        }
      }
      // The person's share: X' H^-1 X for each item they answered, the rows
      // of one item summed first (a person may answer an item more than once).
      if (use_corr && n > 0) {
        std::vector<int> ord(n);
        std::iota(ord.begin(), ord.end(), 0);
        std::sort(ord.begin(), ord.end(), [&](int x, int z) { return block_items[obs[x]] < block_items[obs[z]]; });
        MatrixXd Xi(d, EE);
        int pos = 0;
        while (pos < n) {
          const int item = block_items[obs[ord[pos]]];
          Xi.setZero();
          int q = pos;
          for (; q < n && block_items[obs[ord[q]]] == item; ++q) {
            const double* Xr = &X[static_cast<std::size_t>(ord[q]) * d * EE];
            for (int aa = 0; aa < d; ++aa) for (int ss = 0; ss < EE; ++ss) Xi(aa, ss) += Xr[aa * EE + ss];
          }
          if (item > 0 && item <= n_items) {
            const MatrixXd corr = Xi.transpose() * Hldl.solve(Xi);
            double* B = &blk[static_cast<std::size_t>(item - 1) * EE * EE];
            for (int r = 0; r < EE; ++r) for (int c = 0; c < EE; ++c) B[r + EE * c] += corr(r, c);
          }
          pos = q;
        }
      }
    }
  }

  void join(const MapWorker& o) {
    value += o.value;
    auto add = [](std::vector<double>& a, const std::vector<double>& b) {
      for (std::size_t i = 0; i < a.size(); ++i) a[i] += b[i];
    };
    add(gA, o.gA); add(gB, o.gB); add(gC, o.gC); add(gD, o.gD);
    add(gAb, o.gAb); add(gBb, o.gBb); add(gCb, o.gCb); add(gDb, o.gDb);
    add(gAbil, o.gAbil); add(gMean, o.gMean); add(blk, o.blk);
  }
};

inline Rcpp::NumericMatrix map_matrix(const std::vector<double>& v, int nr, int nc) {
  Rcpp::NumericMatrix out(nr, nc);
  std::copy(v.begin(), v.end(), out.begin());
  return out;
}

} // namespace

// Arguments follow the Laplace person and item kernels (R/mapBackend.R holds
// the wrapper), without their unused beta-row maps.
extern "C" SEXP _bigIRT_map_fg_cpp_impl(
    SEXP idSEXP, SEXP scoreSEXP, SEXP theta_initSEXP, SEXP person_predSEXP,
    SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_predSEXP, SEXP A_signedSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP, SEXP prior_precisionSEXP, SEXP free_maskSEXP,
    SEXP jitterSEXP, SEXP max_attemptsSEXP, SEXP max_iterSEXP, SEXP tolSEXP,
    SEXP keep_covarianceSEXP, SEXP grain_sizeSEXP, SEXP want_row_effectiveSEXP,
    SEXP block_itemsSEXP) {
  BEGIN_RCPP
  using Clock = std::chrono::steady_clock;
  const auto t0 = Clock::now();
  Rcpp::IntegerVector id(idSEXP);
  const int Nobs = id.size();
  const int Nsubs = Rf_nrows(theta_initSEXP);
  const int K = Rf_ncols(theta_initSEXP);
  if (K > 61) Rcpp::stop("MAP: at most 61 dimensions are supported.");
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const double tol = Rcpp::as<double>(tolSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const int max_iter = Rcpp::as<int>(max_iterSEXP);
  const bool keep_covariance = Rcpp::as<bool>(keep_covarianceSEXP);
  const bool want_re = Rcpp::as<bool>(want_row_effectiveSEXP);
  const std::size_t grain = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));

  BigIRTObsIndex ix;
  if (!ix.build(id, Nobs, Nsubs)) Rcpp::stop("MAP: id must be coded from 1 to Nsubs.");

  const int P = Rf_ncols(person_predSEXP);
  MapWorker w(ix, K, P, jitter, tol, max_attempts, max_iter, keep_covariance);
  w.score = map_i(scoreSEXP);
  w.theta_init = map_d(theta_initSEXP);
  w.person_pred = map_d(person_predSEXP);
  w.fixed_ability = map_i(fixed_abilitySEXP);
  w.fixed_ability_value = map_d(fixed_ability_valueSEXP);
  w.A_ref = map_i(A_refSEXP); w.A_fixed_value = map_d(A_fixed_valueSEXP); w.A_pred = map_d(A_predSEXP);
  w.A_signed = map_i(A_signedSEXP);
  w.B_ref = map_i(B_refSEXP); w.B_fixed_value = map_d(B_fixed_valueSEXP); w.B_pred = map_d(B_predSEXP);
  w.C_ref = map_i(C_refSEXP); w.C_fixed_value = map_d(C_fixed_valueSEXP); w.C_pred = map_d(C_predSEXP);
  w.D_ref = map_i(D_refSEXP); w.D_fixed_value = map_d(D_fixed_valueSEXP); w.D_pred = map_d(D_predSEXP);
  w.Abilitybeta = map_d(AbilitybetaSEXP);
  w.invspApars = map_d(invspAparsSEXP); w.invspAbeta = map_d(invspAbetaSEXP);
  w.Bpars = map_d(BparsSEXP); w.Bbeta = map_d(BbetaSEXP);
  w.logitCpars = map_d(logitCparsSEXP); w.logitCbeta = map_d(logitCbetaSEXP);
  w.logitDpars = map_d(logitDparsSEXP); w.logitDbeta = map_d(logitDbetaSEXP);
  w.prior_mean = map_d(prior_meanSEXP); w.prior_precision = map_d(prior_precisionSEXP);
  w.free_mask = map_i(free_maskSEXP);
  w.block_items = map_i(block_itemsSEXP);

  // Shape checks, on the main thread where an error can be raised.
  auto rows_ok = [&](const MapVec<int>& v, int nc) { return v.nr == Nobs && v.nc == nc; };
  auto drows_ok = [&](const MapVec<double>& v, int nc) { return v.nr == Nobs && v.nc == nc; };
  if (static_cast<int>(w.score.len) != Nobs || !rows_ok(w.fixed_ability, K) || !drows_ok(w.fixed_ability_value, K) ||
      !rows_ok(w.A_ref, K) || !drows_ok(w.A_fixed_value, K) ||
      static_cast<int>(w.B_ref.len) != Nobs || static_cast<int>(w.C_ref.len) != Nobs ||
      static_cast<int>(w.D_ref.len) != Nobs || w.person_pred.nr != Nobs ||
      w.prior_mean.nr != Nsubs || w.prior_mean.nc != K || w.free_mask.nr != Nsubs || w.free_mask.nc != K ||
      static_cast<int>(w.prior_precision.len) != K * K * Nsubs)
    Rcpp::stop("MAP: unexpected dimensions in the kernel inputs.");
  if (static_cast<int>(w.A_signed.len) != static_cast<int>(w.invspApars.len))
    Rcpp::stop("MAP: A_signed must hold one entry per free loading.");
  if (P > 0 && (w.Abilitybeta.nr != K || w.Abilitybeta.nc != P))
    Rcpp::stop("MAP: Abilitybeta must be K x (number of person predictors).");
  // Every index the workers follow is checked here, so no worker can read
  // out of bounds.
  auto check_ref = [&](const MapVec<int>& ref, std::size_t npar, const char* what) {
    for (std::size_t i = 0; i < ref.len; ++i)
      if (ref[i] < 0 || static_cast<std::size_t>(ref[i]) > npar) Rcpp::stop("MAP: %s index out of range.", what);
  };
  check_ref(w.A_ref, w.invspApars.len, "loading");
  check_ref(w.B_ref, w.Bpars.len, "difficulty");
  check_ref(w.C_ref, w.logitCpars.len, "lower asymptote");
  check_ref(w.D_ref, w.logitDpars.len, "upper asymptote");
  auto check_beta = [&](const MapVec<int>& ref, const MapVec<double>& beta, const MapVec<double>& pred, const char* what) {
    if (pred.nc == 0) return;
    if (pred.nr != Nobs || beta.nc != pred.nc) Rcpp::stop("MAP: %s covariates do not match their coefficients.", what);
    for (std::size_t i = 0; i < ref.len; ++i)
      if (ref[i] > 0 && beta.nr != 1 && ref[i] > beta.nr) Rcpp::stop("MAP: %s coefficient row out of range.", what);
  };
  check_beta(w.A_ref, w.invspAbeta, w.A_pred, "loading");
  check_beta(w.B_ref, w.Bbeta, w.B_pred, "difficulty");
  check_beta(w.C_ref, w.logitCbeta, w.C_pred, "lower-asymptote");
  check_beta(w.D_ref, w.logitDbeta, w.D_pred, "upper-asymptote");

  w.gA.assign(w.invspApars.len, 0.0); w.gB.assign(w.Bpars.len, 0.0);
  w.gC.assign(w.logitCpars.len, 0.0); w.gD.assign(w.logitDpars.len, 0.0);
  w.gAb.assign(w.invspAbeta.len, 0.0); w.gBb.assign(w.Bbeta.len, 0.0);
  w.gCb.assign(w.logitCbeta.len, 0.0); w.gDb.assign(w.logitDbeta.len, 0.0);
  w.gAbil.assign(static_cast<std::size_t>(K) * P, 0.0); w.gMean.assign(K, 0.0);
  if (w.block_items.len > 0) {
    if (static_cast<int>(w.block_items.len) != Nobs) Rcpp::stop("MAP: block_items needs one entry per row.");
    int mx = 0;
    for (std::size_t i = 0; i < w.block_items.len; ++i) mx = std::max(mx, w.block_items[i]);
    w.n_items = mx; w.S_slots = K + 3;
    w.bl.init(K, w.B_pred.nc, w.A_pred.nc, w.C_pred.nc, w.D_pred.nc);
    w.blk.assign(static_cast<std::size_t>(w.bl.E) * w.bl.E * mx, 0.0);
  }

  Rcpp::NumericMatrix theta_mode(Nsubs, K);
  Rcpp::NumericVector precision(static_cast<R_xlen_t>(K) * K * Nsubs);
  precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision_chol(static_cast<R_xlen_t>(K) * K * Nsubs);
  precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector covariance(keep_covariance ? static_cast<R_xlen_t>(K) * K * Nsubs : 0);
  if (keep_covariance) covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector logdet(Nsubs), lp(Nsubs);
  Rcpp::IntegerVector niter(Nsubs), status(Nsubs);
  Rcpp::LogicalVector converged(Nsubs);
  w.theta_mode = theta_mode.begin(); w.precision = precision.begin();
  w.precision_chol = precision_chol.begin();
  w.covariance = keep_covariance ? covariance.begin() : nullptr;
  w.logdet = logdet.begin(); w.lp_by_subj = lp.begin();
  w.niter = niter.begin(); w.converged = LOGICAL(converged); w.status = status.begin();
  Rcpp::NumericVector re_eta(want_re ? Nobs : 0), re_c(want_re ? Nobs : 0), re_d(want_re ? Nobs : 0);
  Rcpp::NumericMatrix re_load(want_re ? Nobs : 0, want_re ? K : 0);
  if (want_re) {
    w.re_eta = re_eta.begin(); w.re_c = re_c.begin(); w.re_d = re_d.begin();
    w.re_load = re_load.begin(); w.nobs = Nobs;
  }

  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(Nsubs), w, grain);
  const auto t1 = Clock::now();

  Rcpp::RObject blocks = R_NilValue;
  if (w.n_items > 0) {
    Rcpp::NumericVector b(w.blk.begin(), w.blk.end());
    b.attr("dim") = Rcpp::IntegerVector::create(w.bl.E, w.bl.E, w.n_items);
    blocks = b;
  }
  Rcpp::RObject re = R_NilValue;
  if (want_re) re = Rcpp::List::create(Rcpp::Named("eta_row") = re_eta, Rcpp::Named("c_row") = re_c,
    Rcpp::Named("d_row") = re_d, Rcpp::Named("loadings") = re_load);

  Rcpp::List posterior = Rcpp::List::create(
    Rcpp::Named("theta_mode") = theta_mode, Rcpp::Named("precision") = precision,
    Rcpp::Named("precision_chol") = precision_chol, Rcpp::Named("logdet_precision") = logdet,
    Rcpp::Named("objective") = lp, Rcpp::Named("niter") = niter,
    Rcpp::Named("converged") = converged, Rcpp::Named("status_code") = status);
  if (keep_covariance) posterior["covariance"] = covariance;

  const double sec = std::chrono::duration<double>(t1 - t0).count();
  return Rcpp::List::create(
    Rcpp::Named("value") = w.value,
    Rcpp::Named("posterior") = posterior,
    Rcpp::Named("item_fg") = Rcpp::List::create(
      Rcpp::Named("grad_A") = Rcpp::NumericVector(w.gA.begin(), w.gA.end()),
      Rcpp::Named("grad_B") = Rcpp::NumericVector(w.gB.begin(), w.gB.end()),
      Rcpp::Named("grad_C") = Rcpp::NumericVector(w.gC.begin(), w.gC.end()),
      Rcpp::Named("grad_D") = Rcpp::NumericVector(w.gD.begin(), w.gD.end()),
      Rcpp::Named("grad_A_beta") = map_matrix(w.gAb, w.invspAbeta.nr, w.invspAbeta.nc),
      Rcpp::Named("grad_B_beta") = map_matrix(w.gBb, w.Bbeta.nr, w.Bbeta.nc),
      Rcpp::Named("grad_C_beta") = map_matrix(w.gCb, w.logitCbeta.nr, w.logitCbeta.nc),
      Rcpp::Named("grad_D_beta") = map_matrix(w.gDb, w.logitDbeta.nr, w.logitDbeta.nc),
      Rcpp::Named("grad_ability_beta") = map_matrix(w.gAbil, K, P),
      Rcpp::Named("grad_mean") = Rcpp::NumericVector(w.gMean.begin(), w.gMean.end()),
      Rcpp::Named("item_blocks") = blocks,
      Rcpp::Named("row_effective") = re),
    Rcpp::Named("timings") = Rcpp::List::create(
      Rcpp::Named("kernelSec") = sec, Rcpp::Named("personKernelSec") = sec,
      Rcpp::Named("itemKernelSec") = 0.0, Rcpp::Named("totalSec") = sec));
  END_RCPP
}
