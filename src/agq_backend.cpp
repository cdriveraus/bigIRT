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
//
// THE ITEM BLOCKS of the Hessian, on request (`block_items`), by the Louis
// identity: with the nodes held fixed, for rows r and t of one person,
//
//   d2/dpsi_r dpsi_t log p(y_i) = sum_q pi_iq (h_rq [r = t] + s_rq s_tq')
//                                 - (sum_q pi_iq s_rq)(sum_q pi_iq s_tq)',
//
// s_rq and h_rq the score and Hessian of row r's log-likelihood at node q in
// its item's own parameters. Only r = t is kept: each item's block, with the
// coupling through its own persons' posteriors exact and the coupling between
// two items through the persons who answered both dropped. One pass, at the
// cost of one evaluation, where differencing the gradient costs two
// evaluations per parameter. A grouped rule contributes group by group, with
// the group coefficients. Per item, parameter slots are B, the loading on
// each of the K dimensions, C and D; slots a row does not free are zero. They
// are followed by the item's covariate coefficients, each row's slot block
// carried into them by its own covariate values (src/bigirt_item_blocks.h).
//
// PER-RESPONSE OUTPUT, on request (`row_out`), for score-based tests
// (R/scores.R): each row's score sum_q pi_iq s_rq in the slots asked for, its
// score in the linear predictor, and its r = t Louis block, negated. These
// are the terms the gradient and the item blocks above already sum, written
// out row by row instead, so the per-row scores of an item sum exactly to the
// kernel's gradient in it. Summed over a person's rows on one item, they are
// that person's marginal score in the item's parameters -- rows on other
// items do not involve them -- which is what makes them usable as independent
// contributions across persons.

#include <RcppEigen.h>
#include <RcppParallel.h>
#include <vector>
#include <memory>
#include <cmath>
#include <algorithm>
#include "bigirt_obsindex.h"
#include "bigirt_item_blocks.h"
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

// Second derivative of one row's log-likelihood in its linear predictor,
// for P = c + (d - c) logistic(eta): with P' and P'' its first two
// derivatives in eta, d/deta [(y - P) P' / (P(1 - P))]. In the 2PL this is
// -P(1 - P), without y. Used by the group-variance scores below.
inline double agq_row_h(const double y, const double eta, const double c, const double d) {
  const double g = agq_inv_logit(eta);
  const double u = d - c;
  double p = c + u * g;
  if (p < 1e-12) p = 1e-12;
  if (p > 1.0 - 1e-12) p = 1.0 - 1e-12;
  const double r = std::max(p * (1.0 - p), 1e-12);
  const double p1 = u * g * (1.0 - g);
  const double p2 = p1 * (1.0 - 2.0 * g);
  return -p1 * p1 / r - (y - p) * (1.0 - 2.0 * p) * p1 * p1 / (r * r) + (y - p) * p2 / r;
}

// A rule for d free dimensions: M nodes z_q (M x d, column major) for a
// standard normal weight, and their log weights. The product Gauss-Hermite
// rule is one such rule; so is a set of quasi-Monte Carlo points with equal
// weights, for which the formula above is importance sampling through the
// Laplace Gaussian. Weights must be positive.
//
// A rule may also be a signed combination of several such rules: each group
// of nodes is a rule in its own right, and the estimate is sum_g coef_g log(sum_{q in g} w_q f(z_q)) with the coefficients
// summing to one. That is the pairwise (anchored-ANOVA) expansion,
// log I ~ sum_{k<l} log I_kl - (d - 2) sum_k log I_k + (d-1)(d-2)/2 log I_0,
// each term a Gauss-Hermite rule on a coordinate plane, line or the origin.
// The planes share their axis nodes and the origin, so nodes are stored once
// and a group lists its members (node, log weight in that group): at d = 9
// with 5 nodes, 613 distinct nodes instead of 946. The Fisher identity still
// gives the gradient, group by group, so pass 2 is unchanged but for signed
// node weights sum_g coef_g pi_gq. Ungrouped (no members), lw is per node and
// the rule is one group of coefficient one.
struct AgqRule {
  int M = 1, d = 0, G = 1;
  std::vector<double> Z, lw, coef{1.0};
  std::vector<int> mnode, mgrp;   // memberships: node, group (0-based)
  std::vector<double> mlw;        // and the node's log weight in that group
};

struct AgqWorker : public RcppParallel::Worker {
  const BigIRTObsIndex& obs_by_subj;
  AgqVec<int> score;
  AgqVec<double> theta_mode, precision, person_pred;
  AgqVec<int> fixed_ability;
  AgqVec<double> fixed_ability_value;
  AgqVec<int> A_ref, A_beta_row;
  AgqVec<double> A_fixed_value, A_pred;
  // 1 where a free loading takes the identity link rather than softplus
  // (fitIRT's signedLoadings; see the note on the link in laplace_backend.cpp).
  AgqVec<int> A_signed;
  AgqVec<int> B_ref, B_beta_row, C_ref, C_beta_row, D_ref, D_beta_row;
  AgqVec<double> B_fixed_value, B_pred, C_fixed_value, C_pred, D_fixed_value, D_pred;
  AgqVec<double> Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta;
  AgqVec<double> prior_mean, prior_precision;
  AgqVec<int> free_mask;
  const std::vector<AgqRule>& rules;   // indexed by the number of free dimensions
  const int K, P;
  const bool want_person;
  // Item blocks of the Hessian (see the header): item of each row (1-based,
  // 0 skips the row), S = K + 3 slots an item, and E columns with its
  // covariate coefficients (src/bigirt_item_blocks.h).
  AgqVec<int> block_items;
  int n_items = 0, S_slots = 0;
  BigirtBlockLayout bl;
  std::vector<double> blk;
  // Per-response output (scoreContributions(), R/scores.R), on request: for
  // each row, its score in the slots listed in `slot_out`, its score in the
  // linear predictor and its leave-one-out predictive probability
  // (row_score, Nobs x (n + 2)), and the Louis
  // information of those slots (row_info, Nobs x n(n + 1)/2, the upper
  // triangle packed by column). Every row belongs to one person and is
  // written by the worker that has that person, so the writes are disjoint.
  double* row_score = nullptr;
  double* row_info = nullptr;
  int Nobs_rows = 0;
  std::vector<int> slot_out;
  // Held-out rows (row_hold, 1 for held out; empty for none): left out of the
  // posterior, the objective and the gradient, but still scored, with the
  // nodes reweighted by the row's own likelihood: each held-out row is scored
  // as if it were the person's only response beyond the rest. That is
  // purification's anchoring (R/scores.R) in one pass.
  AgqVec<int> row_hold;
  // Group-variance scores (modificationTests(), R/modify.R), on request: rows
  // labelled 1..vc_n by `vc_group` (0 for none) form candidate sets that may
  // share a person effect u ~ N(0, tau^2) in their linear predictors. Each
  // person's score in tau^2 at zero is 1/2 E[(sum_set g)^2 + sum_set h] over
  // the posterior, g and h the rows' first and second derivatives in eta
  // (Lin, 1997); written to vc_out (N x vc_n). Held-out rows are skipped.
  AgqVec<int> vc_group;
  int vc_n = 0;
  double* vc_out = nullptr;
  // The same scores for every direction at once, without forming the matrix:
  // M = sum over persons of E[g g' + diag(h)] over the posterior, items by
  // items, is the score for adding residual covariance among items, and a new
  // dimension with loadings v has score v'Mv / 2. Given row weights (the
  // weight v of each row's item, Nobs x mv_m), each row's contribution to
  // (Mv) for its item: E[g_r (sum_s v_s g_s)] + v_r E[h_r], over the person's
  // rows s (mv_out, Nobs x mv_m); summed by item in R (R/modify.R).
  AgqVec<double> mv_v;
  double* mv_out = nullptr;
  // Item-shape scores (modificationTests(), R/modify.R), on request: for each
  // row, E[g t^p | y] for p = 1, 2, 3 over the posterior (held-out rows with
  // their reweighted nodes), g the row's score in its linear predictor and t
  // the row's ability along its item's loadings, a'(theta + offset) / |a|.
  // A term gamma_p t^p added to the linear predictor has exactly this score
  // at gamma_p = 0 (the Fisher identity), so curvature of the item's response
  // function beyond the model's can be tested (Nobs x 3).
  double* shape_out = nullptr;
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
      A_signed(o.A_signed),
      B_ref(o.B_ref), B_beta_row(o.B_beta_row), C_ref(o.C_ref), C_beta_row(o.C_beta_row),
      D_ref(o.D_ref), D_beta_row(o.D_beta_row), B_fixed_value(o.B_fixed_value), B_pred(o.B_pred),
      C_fixed_value(o.C_fixed_value), C_pred(o.C_pred), D_fixed_value(o.D_fixed_value), D_pred(o.D_pred),
      Abilitybeta(o.Abilitybeta), invspApars(o.invspApars), invspAbeta(o.invspAbeta), Bpars(o.Bpars),
      Bbeta(o.Bbeta), logitCpars(o.logitCpars), logitCbeta(o.logitCbeta), logitDpars(o.logitDpars),
      logitDbeta(o.logitDbeta), prior_mean(o.prior_mean), prior_precision(o.prior_precision),
      free_mask(o.free_mask), rules(o.rules), K(o.K), P(o.P),
      want_person(o.want_person), block_items(o.block_items), n_items(o.n_items), S_slots(o.S_slots),
      bl(o.bl), row_score(o.row_score), row_info(o.row_info), Nobs_rows(o.Nobs_rows),
      slot_out(o.slot_out), row_hold(o.row_hold), vc_group(o.vc_group), vc_n(o.vc_n), vc_out(o.vc_out),
      mv_v(o.mv_v), mv_out(o.mv_out), shape_out(o.shape_out),
      eap(o.eap), post_cov(o.post_cov), value_by_subj(o.value_by_subj),
      value(0.0), failure(0) { zero_like(o); }

  void zero_like(const AgqWorker& o) {
    gA.assign(o.gA.size(), 0.0); gB.assign(o.gB.size(), 0.0);
    gC.assign(o.gC.size(), 0.0); gD.assign(o.gD.size(), 0.0);
    gAb.assign(o.gAb.size(), 0.0); gBb.assign(o.gBb.size(), 0.0);
    gCb.assign(o.gCb.size(), 0.0); gDb.assign(o.gDb.size(), 0.0);
    gAbil.assign(o.gAbil.size(), 0.0); gMean.assign(o.gMean.size(), 0.0);
    S.assign(o.S.size(), 0.0);
    blk.assign(o.blk.size(), 0.0);
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
            if (A_signed[A_ref(o, k) - 1] != 0) { a = raw; am = 1.0; }
            else { a = agq_softplus(raw); am = agq_inv_logit(raw); }
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
      const bool want_blocks = n_items > 0;
      const bool want_curv = want_blocks || row_info != nullptr;
      std::vector<double> buf_etav(want_curv ? buf_eta.size() : 0);
      // With per-row output, each node's probability of a correct response,
      // for the row's leave-one-out predictive probability (see pass 2).
      std::vector<double> buf_p(row_score ? buf_eta.size() : 0);
      const bool has_hold = row_score && row_hold.count() > 0;
      // With group-variance output, each node's second derivative in eta.
      const bool want_vc = vc_n > 0 && vc_out != nullptr;
      const bool want_mv = mv_out != nullptr && mv_v.nc > 0;
      std::vector<double> buf_h(want_vc || want_mv ? buf_eta.size() : 0);
      const bool grouped = rule.G > 1 || !rule.mnode.empty();
      for (long q = 0; q < nq; ++q) {
        // A grouped rule's weights belong to its memberships, not its nodes.
        double lwq = grouped ? 0.0 : rule.lw[static_cast<std::size_t>(q)];
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
          if (want_curv) buf_etav[base + oi] = eta;
          if (row_score) {
            double pc = crow[oi] + (drow[oi] - crow[oi]) * agq_inv_logit(eta);
            buf_p[base + oi] = std::min(std::max(pc, 1e-12), 1.0 - 1e-12);
          }
          if (!(has_hold && row_hold[o] != 0)) lp += rt.ll;
          buf_eta[base + oi] = rt.g_eta; buf_c[base + oi] = rt.g_c; buf_d[base + oi] = rt.g_d;
          if (want_vc || want_mv) buf_h[base + oi] = agq_row_h(static_cast<double>(score[o]), eta, crow[oi], drow[oi]);
        }
        lw[q] = lwq + lp;
      }
      // Each group's log-sum-exp over its memberships; ungrouped, one group
      // whose members are the nodes. lw is now the node's log integrand.
      const int G = rule.G;
      const long nm = grouped ? static_cast<long>(rule.mnode.size()) : nq;
      auto mem_node = [&](long j) -> long { return grouped ? rule.mnode[j] : j; };
      auto mem_val = [&](long j) -> double { return grouped ? rule.mlw[j] + lw[rule.mnode[j]] : lw[j]; };
      auto mem_grp = [&](long j) -> int { return grouped ? rule.mgrp[j] : 0; };
      std::vector<double> gmx(static_cast<std::size_t>(G), -INFINITY), gse(static_cast<std::size_t>(G), 0.0);
      for (long j = 0; j < nm; ++j) { const int g = mem_grp(j); gmx[g] = std::max(gmx[g], mem_val(j)); }
      for (long j = 0; j < nm; ++j) { const int g = mem_grp(j); gse[g] += std::exp(mem_val(j) - gmx[g]); }
      double comb = 0.0;
      for (int g = 0; g < G; ++g) { gse[g] = gmx[g] + std::log(gse[g]); comb += rule.coef[g] * gse[g]; }
      const double vi = comb - half_logdet + 0.5 * d * std::log(2.0 * M_PI);
      value += vi;
      if (value_by_subj) value_by_subj[subj] = vi;

      // Pass 2: posterior expectations (signed, for a grouped rule: a node's
      // weight sums over the groups it belongs to). Each row's weighted sums
      // over the nodes are formed first and scattered into the gradient once
      // per row.
      VectorXd Eth = VectorXd::Zero(K);
      MatrixXd Ett = MatrixXd::Zero(K, K);
      std::vector<double> pw(static_cast<std::size_t>(nq), 0.0);
      for (long j = 0; j < nm; ++j) { const int g = mem_grp(j);
        pw[mem_node(j)] += rule.coef[g] * std::exp(mem_val(j) - gse[g]); }
      for (long q = 0; q < nq; ++q) {
        const VectorXd dv = th[q] - mu;
        Eth += pw[q] * th[q];
        Ett += pw[q] * dv * dv.transpose();
      }
      {
        const VectorXd gm = P0 * (Eth - mu);
        for (int k = 0; k < K; ++k) gMean[k] += gm(k);
      }
      if (want_vc) {
        std::vector<double> Gs(static_cast<std::size_t>(nq) * vc_n, 0.0), Hs(Gs.size(), 0.0);
        std::vector<char> seen(static_cast<std::size_t>(vc_n), 0);
        for (int oi = 0; oi < n; ++oi) {
          const int o = obs[oi];
          const int k = vc_group[o];
          if (k <= 0 || k > vc_n || (has_hold && row_hold[o] != 0)) continue;
          seen[static_cast<std::size_t>(k - 1)] = 1;
          for (long q = 0; q < nq; ++q) {
            const std::size_t ix = static_cast<std::size_t>(q) * n + oi;
            Gs[static_cast<std::size_t>(q) * vc_n + (k - 1)] += buf_eta[ix];
            Hs[static_cast<std::size_t>(q) * vc_n + (k - 1)] += buf_h[ix];
          }
        }
        const std::size_t Ns = obs_by_subj.size();
        for (int k = 0; k < vc_n; ++k) {
          double t = 0.0;
          if (seen[static_cast<std::size_t>(k)]) for (long q = 0; q < nq; ++q) {
            const double gq = Gs[static_cast<std::size_t>(q) * vc_n + k];
            t += pw[q] * (gq * gq + Hs[static_cast<std::size_t>(q) * vc_n + k]);
          }
          vc_out[static_cast<std::size_t>(subj) + Ns * k] = 0.5 * t;
        }
      }
      if (want_mv) {
        const int m = mv_v.nc;
        const std::size_t No = static_cast<std::size_t>(mv_v.nr);
        std::vector<double> Sv(static_cast<std::size_t>(nq) * m, 0.0);
        for (int oi = 0; oi < n; ++oi) {
          const int o = obs[oi];
          if (has_hold && row_hold[o] != 0) continue;
          for (int c = 0; c < m; ++c) {
            const double vo = mv_v(o, c);
            if (vo == 0.0) continue;
            for (long q = 0; q < nq; ++q)
              Sv[static_cast<std::size_t>(q) * m + c] += vo * buf_eta[static_cast<std::size_t>(q) * n + oi];
          }
        }
        for (int oi = 0; oi < n; ++oi) {
          const int o = obs[oi];
          const bool skip = has_hold && row_hold[o] != 0;
          for (int c = 0; c < m; ++c) {
            double t = 0.0;
            if (!skip) {
              const double vo = mv_v(o, c);
              for (long q = 0; q < nq; ++q) {
                const std::size_t ix = static_cast<std::size_t>(q) * n + oi;
                t += pw[q] * (buf_eta[ix] * Sv[static_cast<std::size_t>(q) * m + c] + vo * buf_h[ix]);
              }
            }
            mv_out[static_cast<std::size_t>(o) + No * c] = t;
          }
        }
      }
      VectorXd s_ab(K);
      // A held-out row's node weights: the posterior without it, times its
      // own likelihood, renormalised (group by group for a grouped rule).
      std::vector<double> hold_w(has_hold ? static_cast<std::size_t>(nq) * n : 0, 0.0);
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        const bool hold = has_hold && row_hold[o] != 0;
        const double* w = pw.data();
        if (hold) {
          const double y = static_cast<double>(score[o]);
          double* rw = &hold_w[static_cast<std::size_t>(oi) * nq];
          auto lik = [&](long q) { const double p = buf_p[static_cast<std::size_t>(q) * n + oi]; return y > 0.5 ? p : 1.0 - p; };
          if (!grouped) {
            double den = 0.0;
            for (long q = 0; q < nq; ++q) { rw[q] = pw[q] * lik(q); den += rw[q]; }
            for (long q = 0; q < nq; ++q) rw[q] /= den;
          } else {
            std::vector<double> den(static_cast<std::size_t>(G), 0.0);
            for (long j = 0; j < nm; ++j) den[mem_grp(j)] += std::exp(mem_val(j) - gse[mem_grp(j)]) * lik(mem_node(j));
            for (long j = 0; j < nm; ++j) { const int g = mem_grp(j);
              if (den[g] > 0.0) rw[mem_node(j)] += rule.coef[g] * std::exp(mem_val(j) - gse[g]) * lik(mem_node(j)) / den[g]; }
          }
          w = rw;
        }
        double s_eta = 0.0, s_c = 0.0, s_d = 0.0;
        s_ab.setZero();
        for (long q = 0; q < nq; ++q) {
          const std::size_t ix = static_cast<std::size_t>(q) * n + oi;
          const double ge = w[q] * buf_eta[ix];
          s_eta += ge; s_c += w[q] * buf_c[ix]; s_d += w[q] * buf_d[ix];
          for (int k = 0; k < K; ++k)
            s_ab(k) += ge * (fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : th[q](k) + off(oi, k));
        }
        if (shape_out) {
          double an = 0.0;
          for (int k = 0; k < K; ++k) an += arow(oi, k) * arow(oi, k);
          an = an > 0.0 ? std::sqrt(an) : 1.0;
          double t1 = 0.0, t2 = 0.0, t3 = 0.0;
          for (long q = 0; q < nq; ++q) {
            const double ge = w[q] * buf_eta[static_cast<std::size_t>(q) * n + oi];
            double t = 0.0;
            for (int k = 0; k < K; ++k)
              t += arow(oi, k) * (fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : th[q](k) + off(oi, k));
            t /= an;
            t1 += ge * t; t2 += ge * t * t; t3 += ge * t * t * t;
          }
          const std::size_t No = static_cast<std::size_t>(Nobs_rows);
          shape_out[static_cast<std::size_t>(o)] = t1;
          shape_out[static_cast<std::size_t>(o) + No] = t2;
          shape_out[static_cast<std::size_t>(o) + 2 * No] = t3;
        }
        if (!hold) for (int k = 0; k < K; ++k) {
          if (A_ref(o, k) > 0) {
            const double graw = s_ab(k) * amul(oi, k);
            gA[static_cast<std::size_t>(A_ref(o, k) - 1)] += graw;
            beta_grad(gAb, A_pred, invspAbeta, o, invspAbeta.nr == 1 ? 1 : A_ref(o, k), graw);
          }
          if (fixed_ability(o, k) == 0 && P > 0)
            for (int j = 0; j < P; ++j)
              gAbil[static_cast<std::size_t>(k + K * j)] += s_eta * arow(oi, k) * person_pred(o, j);
        }
        if (!hold && B_ref[o] > 0) {
          gB[static_cast<std::size_t>(B_ref[o] - 1)] -= s_eta;
          beta_grad(gBb, B_pred, Bbeta, o, Bbeta.nr == 1 ? 1 : B_ref[o], -s_eta);
        }
        if (!hold && C_ref[o] > 0) {
          const double gc = s_c * cmul[oi];
          gC[static_cast<std::size_t>(C_ref[o] - 1)] += gc;
          beta_grad(gCb, C_pred, logitCbeta, o, logitCbeta.nr == 1 ? 1 : C_ref[o], gc);
        }
        if (!hold && D_ref[o] > 0) {
          const double gd = s_d * dmul[oi];
          gD[static_cast<std::size_t>(D_ref[o] - 1)] += gd;
          beta_grad(gDb, D_pred, logitDbeta, o, logitDbeta.nr == 1 ? 1 : D_ref[o], gd);
        }
        // The row's own score, slot by slot: exactly the terms just scattered
        // into the gradient, so per-row scores sum to it by construction. A
        // slot the row does not free scores zero, as in row_slots().
        if (row_score) {
          const std::size_t no = static_cast<std::size_t>(Nobs_rows);
          const int ns = static_cast<int>(slot_out.size());
          for (int c = 0; c < ns; ++c) {
            const int sl = slot_out[static_cast<std::size_t>(c)];
            double v = 0.0;
            if (sl == 0) v = B_ref[o] > 0 ? -s_eta : 0.0;
            else if (sl <= K) v = A_ref(o, sl - 1) > 0 ? s_ab(sl - 1) * amul(oi, sl - 1) : 0.0;
            else if (sl == K + 1) v = C_ref[o] > 0 ? s_c * cmul[oi] : 0.0;
            else v = D_ref[o] > 0 ? s_d * dmul[oi] : 0.0;
            row_score[static_cast<std::size_t>(o) + no * c] = v;
          }
          row_score[static_cast<std::size_t>(o) + no * ns] = s_eta;
          // The row's leave-one-out predictive probability of a correct
          // response, P~ = E[P(theta) | the person's other responses]: the
          // posterior weights with this row's likelihood divided back out.
          // The row's score is exactly that of the Bernoulli p(y_r | y_-r)
          // with probability P~, which gives each row an expected
          // information that is positive semidefinite (R/scores.R). A
          // grouped rule takes the conditional expectation group by group.
          const double y = static_cast<double>(score[o]);
          double pt = 0.0;
          if (hold) {
            for (long q = 0; q < nq; ++q) pt += pw[q] * buf_p[static_cast<std::size_t>(q) * n + oi];
          } else if (!grouped) {
            double num = 0.0, den = 0.0;
            for (long q = 0; q < nq; ++q) {
              const double pq = buf_p[static_cast<std::size_t>(q) * n + oi];
              const double w = pw[q] / (y > 0.5 ? pq : 1.0 - pq);
              num += w * pq; den += w;
            }
            pt = num / den;
          } else {
            std::vector<double> num(static_cast<std::size_t>(G), 0.0), den(static_cast<std::size_t>(G), 0.0);
            for (long j = 0; j < nm; ++j) {
              const int g = mem_grp(j);
              const double pq = buf_p[static_cast<std::size_t>(mem_node(j)) * n + oi];
              const double w = std::exp(mem_val(j) - gse[g]) / (y > 0.5 ? pq : 1.0 - pq);
              num[g] += w * pq; den[g] += w;
            }
            for (int g = 0; g < G; ++g) if (den[g] > 0.0) pt += rule.coef[g] * num[g] / den[g];
          }
          row_score[static_cast<std::size_t>(o) + no * (ns + 1)] = std::min(std::max(pt, 1e-12), 1.0 - 1e-12);
        }
      }
      for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) S[static_cast<std::size_t>(r + K * c)] += Ett(r, c);
      if (want_curv) item_blocks_impl(obs, n, nq, th, off, amul, crow, cmul, drow, dmul,
                                        buf_etav, pw, grouped, nm, gse, rule,
                                        mem_node, mem_val, mem_grp, has_hold ? &hold_w : nullptr);
      if (want_person) {
        for (int k = 0; k < K; ++k) eap[static_cast<std::size_t>(subj) + static_cast<std::size_t>(obs_by_subj.size()) * k] = Eth(k);
        const VectorXd dm = Eth - mu;
        const MatrixXd C = Ett - dm * dm.transpose();
        for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c)
          post_cov[static_cast<std::size_t>(r + K * c) + static_cast<std::size_t>(K) * K * subj] = C(r, c);
      }
    }
  }

  // Score and Hessian of one row's log-likelihood at one node, in its item's
  // slots (B, loading on each dimension, C, D). eta is the row's linear
  // predictor at the node; ab the row's abilities there.
  inline void row_slots(const int o, const int oi, const double eta, const VectorXd& thq,
                        const MatrixXd& off, const std::vector<double>& crow,
                        const std::vector<double>& cmul, const std::vector<double>& drow,
                        const std::vector<double>& dmul, const MatrixXd& amul,
                        std::vector<double>& sv, std::vector<double>* hv) const {
    const int SS = S_slots;
    const double y = static_cast<double>(score[o]);
    const double g = agq_inv_logit(eta);
    const double c = crow[oi], dd = drow[oi], u = dd - c;
    double p = c + u * g;
    if (p < 1e-12) p = 1e-12;
    if (p > 1.0 - 1e-12) p = 1.0 - 1e-12;
    const double coeff = (y - p) / std::max(p * (1.0 - p), 1e-12);
    const double lpp = -(y / (p * p) + (1.0 - y) / ((1.0 - p) * (1.0 - p)));
    const double sp = g * (1.0 - g), spp = sp * (1.0 - 2.0 * g);
    // p's derivatives in the linear predictor and the two asymptotes.
    const double pe = u * sp, pc = 1.0 - g, pd = g, pee = u * spp, pec = -sp, ped = sp;
    const double le = coeff * pe, lc = coeff * pc, ld = coeff * pd;
    const double lee = lpp * pe * pe + coeff * pee, lec = lpp * pe * pc + coeff * pec;
    const double led = lpp * pe * pd + coeff * ped;
    const double lcc = lpp * pc * pc, lcd = lpp * pc * pd, ldd = lpp * pd * pd;
    // The links' chain-rule factors: eta = sum_k softplus(raw_k) ab_k - b,
    // c = logistic(x)/2, d = logistic(z)/2 + 1/2. A slot the row does not
    // free has factor zero, so its score and Hessian rows are zero. A signed
    // loading is raw_k itself: slope one, second derivative zero.
    const double eb = B_ref[o] > 0 ? -1.0 : 0.0;
    double ea[64], eaa[64];
    for (int k = 0; k < K; ++k) {
      ea[k] = 0.0; eaa[k] = 0.0;
      if (A_ref(o, k) > 0) {
        const double ab = fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : thq(k) + off(oi, k);
        ea[k] = ab * amul(oi, k);
        eaa[k] = A_signed[A_ref(o, k) - 1] != 0 ? 0.0 : ab * amul(oi, k) * (1.0 - amul(oi, k));
      }
    }
    const double cx = cmul[oi], cxx = cmul[oi] * (1.0 - 4.0 * c);
    const double dz = dmul[oi], dzz = dmul[oi] * (3.0 - 4.0 * dd);
    const int iC = K + 1, iD = K + 2;
    sv[0] = le * eb;
    for (int k = 0; k < K; ++k) sv[1 + k] = le * ea[k];
    sv[iC] = lc * cx; sv[iD] = ld * dz;
    if (!hv) return;
    std::vector<double>& h = *hv;
    auto put = [&](int a, int b, double v) { h[a + SS * b] = v; h[b + SS * a] = v; };
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
  }

  // Each row's contribution to its item's block (the Louis identity; see the
  // header): the node-weighted sum of h + s s', less the outer product of the
  // weighted score, group by group for a grouped rule; then carried into the
  // item's covariate coefficients by the row's covariate values. With
  // `row_info` set, each row's block in the `slot_out` slots is also written
  // out, negated (information rather than Hessian), whether or not item
  // blocks were asked for.
  template <class MemNode, class MemVal, class MemGrp>
  void item_blocks_impl(const BigIRTObsSpan& obs, int n, long nq, const std::vector<VectorXd>& th,
                        const MatrixXd& off, const MatrixXd& amul, const std::vector<double>& crow,
                        const std::vector<double>& cmul, const std::vector<double>& drow,
                        const std::vector<double>& dmul, const std::vector<double>& buf_etav,
                        const std::vector<double>& pw, bool grouped, long nm,
                        const std::vector<double>& gse, const AgqRule& rule,
                        MemNode mem_node, MemVal mem_val, MemGrp mem_grp,
                        const std::vector<double>* hold_w = nullptr) {
    const int SS = S_slots;
    const bool want_blocks = n_items > 0;
    std::vector<double> sv(SS), hv(static_cast<std::size_t>(SS) * SS), M(hv.size()), m(SS);
    std::vector<double> mg(grouped ? static_cast<std::size_t>(rule.G) * SS : 0);
    std::vector<double> xv(want_blocks ? static_cast<std::size_t>(bl.E - SS) + 1 : 0);
    const std::size_t EE = static_cast<std::size_t>(bl.E) * bl.E;
    const int ns = static_cast<int>(slot_out.size());
    const std::size_t no = static_cast<std::size_t>(Nobs_rows);
    for (int oi = 0; oi < n; ++oi) {
      const int o = obs[oi];
      // A held-out row is not in the objective, so it adds nothing to the
      // item blocks; its information takes its own node weights, the Louis
      // identity for the posterior that includes it.
      const bool hold = hold_w != nullptr && row_hold[o] != 0;
      const int it = want_blocks ? block_items[o] : 0;
      const bool to_block = !hold && want_blocks && it > 0 && it <= n_items;
      if (!to_block && !row_info) continue;
      std::fill(M.begin(), M.end(), 0.0); std::fill(m.begin(), m.end(), 0.0);
      if (hold) {
        const double* rw = &(*hold_w)[static_cast<std::size_t>(oi) * nq];
        for (long q = 0; q < nq; ++q) {
          if (rw[q] == 0.0) continue;
          row_slots(o, oi, buf_etav[static_cast<std::size_t>(q) * n + oi], th[q], off, crow, cmul,
                    drow, dmul, amul, sv, &hv);
          for (int a = 0; a < SS; ++a) {
            m[a] += rw[q] * sv[a];
            for (int b = 0; b < SS; ++b) M[a + SS * b] += rw[q] * (hv[a + SS * b] + sv[a] * sv[b]);
          }
        }
        for (int a = 0; a < SS; ++a) for (int b = 0; b < SS; ++b) M[a + SS * b] -= m[a] * m[b];
        if (row_info) {
          int c = 0;
          for (int bb = 0; bb < ns; ++bb) for (int aa = 0; aa <= bb; ++aa, ++c)
            row_info[static_cast<std::size_t>(o) + no * c] =
              -M[slot_out[static_cast<std::size_t>(aa)] + SS * slot_out[static_cast<std::size_t>(bb)]];
        }
        continue;
      }
      for (long q = 0; q < nq; ++q) {
        if (pw[q] == 0.0) continue;
        row_slots(o, oi, buf_etav[static_cast<std::size_t>(q) * n + oi], th[q], off, crow, cmul,
                  drow, dmul, amul, sv, &hv);
        for (int a = 0; a < SS; ++a) {
          if (!grouped) m[a] += pw[q] * sv[a];
          for (int b = 0; b < SS; ++b) M[a + SS * b] += pw[q] * (hv[a + SS * b] + sv[a] * sv[b]);
        }
      }
      if (!grouped) {
        for (int a = 0; a < SS; ++a) for (int b = 0; b < SS; ++b) M[a + SS * b] -= m[a] * m[b];
      } else {
        std::fill(mg.begin(), mg.end(), 0.0);
        for (long j = 0; j < nm; ++j) {
          const int g = mem_grp(j);
          const double wj = std::exp(mem_val(j) - gse[g]);
          const long q = mem_node(j);
          row_slots(o, oi, buf_etav[static_cast<std::size_t>(q) * n + oi], th[q], off, crow, cmul,
                    drow, dmul, amul, sv, nullptr);
          for (int a = 0; a < SS; ++a) mg[static_cast<std::size_t>(g) * SS + a] += wj * sv[a];
        }
        for (int g = 0; g < rule.G; ++g) {
          const double* mgg = &mg[static_cast<std::size_t>(g) * SS];
          for (int a = 0; a < SS; ++a) for (int b = 0; b < SS; ++b)
            M[a + SS * b] -= rule.coef[g] * mgg[a] * mgg[b];
        }
      }
      if (row_info) {
        int c = 0;
        for (int bb = 0; bb < ns; ++bb) for (int aa = 0; aa <= bb; ++aa, ++c)
          row_info[static_cast<std::size_t>(o) + no * c] =
            -M[slot_out[static_cast<std::size_t>(aa)] + SS * slot_out[static_cast<std::size_t>(bb)]];
      }
      if (!to_block) continue;
      bl.covariates(o, K, A_ref, B_ref, C_ref, D_ref, A_pred, B_pred, C_pred, D_pred, xv.data());
      bl.add(&blk[static_cast<std::size_t>(it - 1) * EE], M.data(), xv.data());
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
    add(blk, o.blk);
  }
};

} // namespace

// The item blocks as an E x E x items array, or NULL when none were asked for.
static SEXP blocks_out(const AgqWorker& w) {
  if (w.n_items == 0) return R_NilValue;
  Rcpp::NumericVector out(w.blk.begin(), w.blk.end());
  out.attr("dim") = Rcpp::IntegerVector::create(w.bl.E, w.bl.E, w.n_items);
  return out;
}

// Arguments follow the Laplace block kernels, plus the centring (mode and
// posterior precision from the Laplace person step), the one-dimensional
// Gauss-Hermite rule for a standard normal weight (nodes, log weights), and
// whether to return per-person posterior means and covariances.
extern "C" SEXP _bigIRT_agq_fg_cpp_impl(
    SEXP idSEXP, SEXP scoreSEXP, SEXP theta_modeSEXP, SEXP precisionSEXP,
    SEXP person_predSEXP, SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_beta_rowSEXP, SEXP A_predSEXP, SEXP A_signedSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_beta_rowSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_beta_rowSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_beta_rowSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP, SEXP prior_precisionSEXP, SEXP free_maskSEXP,
    SEXP nodesSEXP, SEXP log_weightsSEXP, SEXP groupsSEXP, SEXP coefsSEXP,
    SEXP want_personSEXP, SEXP grain_sizeSEXP, SEXP block_itemsSEXP,
    SEXP row_outSEXP, SEXP slot_outSEXP, SEXP row_holdSEXP, SEXP vc_groupSEXP, SEXP vc_nSEXP,
    SEXP mv_vSEXP, SEXP shape_outSEXP) {
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
  const bool any_groups = !Rf_isNull(groupsSEXP);
  for (int d = 1; d <= K; ++d) {
    Rcpp::NumericMatrix Z = zl[d - 1];
    Rcpp::NumericVector lwv = wl[d - 1];
    const bool gd = any_groups && !Rf_isNull(Rcpp::List(groupsSEXP)[d - 1]);
    if (Z.ncol() != d || Z.nrow() < 1 || (!gd && Z.nrow() != lwv.size()))
      Rcpp::stop("AGQ: rule %d has the wrong shape.", d);
    rules[d].M = Z.nrow(); rules[d].d = d;
    rules[d].Z.assign(Z.begin(), Z.end()); rules[d].lw.assign(lwv.begin(), lwv.end());
  }
  // Optional groups, per d (NULL for an ordinary rule): an integer matrix of
  // memberships (node, group), 1-based, whose log weights are then that d's
  // log_weights, one per membership; and the groups' coefficients.
  if (!Rf_isNull(groupsSEXP)) {
    Rcpp::List gl(groupsSEXP), cl(coefsSEXP);
    if (gl.size() != K || cl.size() != K) Rcpp::stop("AGQ: need groups and coefficients for each of 1..K.");
    for (int d = 1; d <= K; ++d) {
      if (Rf_isNull(gl[d - 1])) continue;
      Rcpp::IntegerMatrix gm = gl[d - 1];
      Rcpp::NumericVector cv = cl[d - 1];
      AgqRule& r = rules[d];
      if (gm.ncol() != 2 || gm.nrow() != static_cast<int>(r.lw.size()))
        Rcpp::stop("AGQ: rule %d needs one (node, group) row per log weight.", d);
      r.G = cv.size();
      r.coef.assign(cv.begin(), cv.end());
      r.mlw = r.lw;
      r.mnode.resize(gm.nrow()); r.mgrp.resize(gm.nrow());
      for (int j = 0; j < gm.nrow(); ++j) {
        if (gm(j, 0) < 1 || gm(j, 0) > r.M || gm(j, 1) < 1 || gm(j, 1) > r.G)
          Rcpp::stop("AGQ: rule %d has a membership out of range.", d);
        r.mnode[j] = gm(j, 0) - 1; r.mgrp[j] = gm(j, 1) - 1;
      }
    }
  }
  AgqWorker w(obs_by_subj, rules, K, P, want_person);
  auto cp_d = [](SEXP x) { return AgqVec_d(x); };
  auto cp_i = [](SEXP x) { return AgqVec_i(x); };
  w.score = cp_i(scoreSEXP); w.theta_mode = cp_d(theta_modeSEXP); w.precision = cp_d(precisionSEXP);
  w.person_pred = cp_d(person_predSEXP); w.fixed_ability = cp_i(fixed_abilitySEXP);
  w.fixed_ability_value = cp_d(fixed_ability_valueSEXP);
  w.A_ref = cp_i(A_refSEXP); w.A_fixed_value = cp_d(A_fixed_valueSEXP);
  w.A_beta_row = cp_i(A_beta_rowSEXP); w.A_pred = cp_d(A_predSEXP);
  w.A_signed = cp_i(A_signedSEXP);
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
  if (w.A_signed.count() != w.invspApars.count())
    Rcpp::stop("AGQ: A_signed must hold one entry per free loading.");
  // A matrix with zero rows in R still reports its column count; the kernels
  // treat "no predictors" as zero columns.
  if (w.A_pred.nr == 0) w.A_pred.nc = 0;
  if (w.B_pred.nr == 0) w.B_pred.nc = 0;
  if (w.C_pred.nr == 0) w.C_pred.nc = 0;
  if (w.D_pred.nr == 0) w.D_pred.nc = 0;
  // Item blocks: the item of each row, 1-based; empty asks for none.
  w.block_items = cp_i(block_itemsSEXP);
  if (w.block_items.count() > 0) {
    if (static_cast<int>(w.block_items.count()) != Nobs) Rcpp::stop("AGQ: block_items needs one entry per row.");
    if (K > 61) Rcpp::stop("AGQ: item blocks support at most 61 dimensions.");
    int mx = 0;
    for (std::size_t i = 0; i < w.block_items.count(); ++i) mx = std::max(mx, w.block_items[i]);
    w.n_items = mx;
    w.bl.init(K, w.B_pred.nc, w.A_pred.nc, w.C_pred.nc, w.D_pred.nc);
    w.blk.assign(static_cast<std::size_t>(w.bl.E) * w.bl.E * mx, 0.0);
  }
  w.S_slots = K + 3;
  // Per-response output: 0 none, 1 scores, 2 scores and information, in the
  // slots listed (0-based: B, the loading on each dimension, C, D).
  const int row_out = Rcpp::as<int>(row_outSEXP);
  Rcpp::IntegerVector slot_out(slot_outSEXP);
  if (row_out > 0) {
    if (K > 61) Rcpp::stop("AGQ: per-response output supports at most 61 dimensions.");
    for (int c = 0; c < slot_out.size(); ++c)
      if (slot_out[c] < 0 || slot_out[c] >= K + 3) Rcpp::stop("AGQ: slot_out entries must lie in 0..K+2.");
    w.slot_out.assign(slot_out.begin(), slot_out.end());
    w.Nobs_rows = Nobs;
  }
  const int ns_out = static_cast<int>(w.slot_out.size());
  Rcpp::NumericMatrix row_scores(row_out >= 1 ? Nobs : 0, row_out >= 1 ? ns_out + 2 : 0);
  Rcpp::NumericMatrix row_info(row_out >= 2 ? Nobs : 0, row_out >= 2 ? ns_out * (ns_out + 1) / 2 : 0);
  if (row_out >= 1) w.row_score = row_scores.begin();
  w.row_hold = AgqVec_i(row_holdSEXP);
  if (w.row_hold.count() > 0 && (row_out < 1 || static_cast<int>(w.row_hold.count()) != Nobs))
    Rcpp::stop("AGQ: row_hold needs per-response output and one entry per row.");
  if (row_out >= 2 && ns_out > 0) w.row_info = row_info.begin();
  // Group-variance scores: a set label per row (0 none, 1..vc_n), N x vc_n out.
  w.vc_group = AgqVec_i(vc_groupSEXP);
  w.vc_n = Rcpp::as<int>(vc_nSEXP);
  if (w.vc_n > 0 && static_cast<int>(w.vc_group.count()) != Nobs)
    Rcpp::stop("AGQ: vc_group needs one entry per row.");
  Rcpp::NumericMatrix vc_scores(w.vc_n > 0 ? N : 0, std::max(w.vc_n, 0));
  if (w.vc_n > 0) w.vc_out = vc_scores.begin();
  // Residual-covariance products: row weights Nobs x m (0 x 0 for none).
  w.mv_v = AgqVec_d(mv_vSEXP);
  if (w.mv_v.nr == 0) w.mv_v.nc = 0;
  if (w.mv_v.nc > 0 && w.mv_v.nr != Nobs) Rcpp::stop("AGQ: mv_v needs one row per response.");
  Rcpp::NumericMatrix mv_res(w.mv_v.nc > 0 ? Nobs : 0, w.mv_v.nc);
  if (w.mv_v.nc > 0) w.mv_out = mv_res.begin();
  // Item-shape scores: Nobs x 3 (0 x 0 for none). They are written per row,
  // so they need the row count the per-response output sets.
  const bool want_shape = Rcpp::as<int>(shape_outSEXP) > 0;
  Rcpp::NumericMatrix shape_res(want_shape ? Nobs : 0, want_shape ? 3 : 0);
  if (want_shape) { w.shape_out = shape_res.begin(); w.Nobs_rows = Nobs; }
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
    Rcpp::Named("S") = S, Rcpp::Named("eap") = eap, Rcpp::Named("post_cov") = post_cov,
    Rcpp::Named("item_blocks") = blocks_out(w),
    Rcpp::Named("row_scores") = row_scores, Rcpp::Named("row_info") = row_info,
    Rcpp::Named("vc_scores") = vc_scores, Rcpp::Named("mv") = mv_res, Rcpp::Named("shape") = shape_res);
  END_RCPP
}
