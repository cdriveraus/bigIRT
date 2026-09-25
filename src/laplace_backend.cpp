#include <RcppEigen.h>
#include <RcppParallel.h>
#include <chrono>
#include <atomic>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>
#include "bigirt_obsindex.h"
using Eigen::MatrixXd;
using Eigen::VectorXd;

// Worker inputs: read-only views of the R arrays, no copy.
//
// These used to be owned copies, made on every .Call. The copies were there
// because RcppParallel's RVector/RMatrix views had been the source of
// process-level exits in predictor-bearing fits on Windows, and they were
// correct -- but they were serial, O(Nobs) per array per call, and on a 1.5M
// response fit at eight threads the person kernel spent more time in its
// serial part (copies and index) than in its parallel one.
//
// A view here is a raw pointer taken at the .Call boundary on the main thread
// (which also materialises any ALTREP vector there, so no worker thread ever
// calls into R), plus its dimensions. It holds no R object. The arrays it
// points at are .Call arguments, or members of the persistent prepared-item
// object, so they outlive every worker. An empty array is a null pointer with
// zero extent, and every accessor site already guards on the extent.
template <typename T>
struct BigIRTOwnedVector {
  const T* ptr = nullptr;
  int n = 0;
  BigIRTOwnedVector() {}
  template <typename V> explicit BigIRTOwnedVector(const V& x) : n(static_cast<int>(x.size())) {
    if (n > 0) ptr = &(*x.begin());
  }
  inline int length() const { return n; }
  inline int size() const { return n; }
  inline const T& operator[](const int i) const { return ptr[i]; }
};

template <typename T>
struct BigIRTOwnedMatrix {
  int nr = 0, nc = 0;
  const T* ptr = nullptr;
  BigIRTOwnedMatrix() {}
  template <typename M> explicit BigIRTOwnedMatrix(const M& x) : nr(x.nrow()), nc(x.ncol()) {
    if (nr > 0 && nc > 0) ptr = &(*x.begin());
  }
  inline int nrow() const { return nr; }
  inline int ncol() const { return nc; }
  inline const T& operator()(const int i, const int j) const {
    return ptr[static_cast<std::size_t>(i) + static_cast<std::size_t>(nr) * static_cast<std::size_t>(j)];
  }
};

extern "C" SEXP _bigIRT_laplace_person_step_block_cpp_impl(
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP _bigIRT_laplace_item_block_objective_cpp_impl(
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP);

inline double bigirt_softplus_scalar(const double x) {
  if (x > 0.0) return x + std::log1p(std::exp(-x));
  return std::log1p(std::exp(x));
}

inline double bigirt_stable_inv_logit(const double x) {
  if (x >= 0.0) {
    const double z = std::exp(-x);
    return 1.0 / (1.0 + z);
  } else {
    const double z = std::exp(x);
    return z / (1.0 + z);
  }
}

inline double bigirt_clamp_prob(const double p) {
  if (p < 1e-12) return 1e-12;
  if (p > 1.0 - 1e-12) return 1.0 - 1e-12;
  return p;
}

// TWO DIFFERENT CURVATURES LIVE IN THIS FILE. Do not collapse them.
//
//   w  = s^2/r          Fisher (expected) information. Always positive, so
//                       prior + sum(w a a') is always positive definite.
//                       This is what the NEWTON STEP uses, and that is the
//                       only reason the mode solve is guaranteed to take an
//                       uphill step from anywhere. Keep it.
//
//   wl = -d2(ll)/deta2  the observed Hessian. This is what the LAPLACE
//                       APPROXIMATION uses: log|H|, the reported person
//                       covariance, the latent-correlation M-step.
//
// They are the same number for the 2PL, because the logit link is canonical
// there and the second derivative of the log-likelihood contains no y. For
// the 3PL and 4PL they differ, and the difference matters twice over.
// Statistically, Laplace approximates one integrand, so it needs that
// integrand's curvature rather than an average over responses the person did
// not give. Mechanically, the mode adjoint applies the implicit function
// theorem and needs the inverse of the actual Hessian: feeding it the Fisher
// surrogate made the item gradient miss the true total derivative by 1e-3 to
// 4e-2 for the 3PL and 4PL, at one, two and three dimensions, while the 2PL
// stayed exact and hid it. See tests/testthat/test-laplace-direct-gradients.R.
//
// Fisher curvature was selectable until September 2026 and is no longer:
// there was no regime in which it was the right choice for the Laplace part,
// and carrying it meant carrying backend state that could disagree with the
// fit that produced it.

struct BigIRTRowTerms {
  double eta;
  double g;
  double q;
  double u;
  double p;
  double s;
  double r;
  double coeff;
  double ll;
  // Fisher information, for the Newton step only (see the note above).
  double w;
  // The observed curvature and its three derivatives: what the Laplace
  // approximation and its gradient use.
  double wl;
  double dwl_deta;
  double dwl_dc;
  double dwl_dd;
  double grad_eta;
  double grad_c;
  double grad_d;
  double dgrad_eta_deta;
  double dgrad_eta_dc;
  double dgrad_eta_dd;
};

inline BigIRTRowTerms bigirt_row_terms(const double y, const double eta,
  const double c, const double d) {
  BigIRTRowTerms out;
  out.eta = eta;
  out.g = bigirt_stable_inv_logit(eta);
  out.q = out.g * (1.0 - out.g);
  out.u = d - c;
  out.p = bigirt_clamp_prob(c + out.u * out.g);
  out.s = out.u * out.q;
  out.r = std::max(out.p * (1.0 - out.p), 1e-12);
  out.coeff = (y - out.p) / out.r;
  out.ll = y * std::log(out.p) + (1.0 - y) * std::log(1.0 - out.p);
  out.w = (out.s * out.s) / out.r;

  const double dq_deta = out.q * (1.0 - 2.0 * out.g);
  const double ds_deta = out.u * dq_deta;
  const double dr_deta = out.s * (1.0 - 2.0 * out.p);

  const double dp_dc = 1.0 - out.g;
  const double ds_dc = -out.q;
  const double dr_dc = dp_dc * (1.0 - 2.0 * out.p);

  const double dp_dd = out.g;
  const double ds_dd = out.q;
  const double dr_dd = dp_dd * (1.0 - 2.0 * out.p);

  out.grad_eta = out.coeff * out.s;
  out.grad_c = out.coeff * (1.0 - out.g);
  out.grad_d = out.coeff * out.g;
  const double dcoeff_deta = (-out.s * out.r - (y - out.p) * dr_deta) / (out.r * out.r);
  const double dcoeff_dc = (-(1.0 - out.g) * out.r - (y - out.p) * dr_dc) / (out.r * out.r);
  const double dcoeff_dd = (-out.g * out.r - (y - out.p) * dr_dd) / (out.r * out.r);
  out.dgrad_eta_deta = dcoeff_deta * out.s + out.coeff * ds_deta;
  out.dgrad_eta_dc = dcoeff_dc * out.s + out.coeff * ds_dc;
  out.dgrad_eta_dd = dcoeff_dd * out.s + out.coeff * ds_dd;

  {
    // Observed information. dgrad_eta_deta is already d2(ll)/deta2, so the
    // curvature itself is free; only its three derivatives cost anything,
    // and they are one order further in the same recursion. Verified against
    // complex-step differentiation to 4e-15 relative.
    const double e = y - out.p;
    const double r2 = out.r * out.r;
    const double d2q_deta = dq_deta * (1.0 - 2.0 * out.g) - 2.0 * out.q * out.q;
    const double d2s_deta = out.u * d2q_deta;
    const double d2r_deta = ds_deta * (1.0 - 2.0 * out.p) - 2.0 * out.s * out.s;
    const double Nn = dcoeff_deta * r2;          // numerator of dcoeff_deta
    const double dNn = -ds_deta * out.r - e * d2r_deta;   // the s*dr terms cancel
    const double d2coeff_deta = (dNn * out.r - 2.0 * Nn * dr_deta) / (r2 * out.r);

    out.wl = -out.dgrad_eta_deta;
    out.dwl_deta = -(d2coeff_deta * out.s + 2.0 * dcoeff_deta * ds_deta +
                     out.coeff * d2s_deta);

    // u = d - c, so du/dc = -1 and du/dd = +1; dp/dc = 1-g, dp/dd = g. The
    // sigmoid blocks g, q and dq_deta do not depend on c or d.
    const double drv_c = ds_dc * (1.0 - 2.0 * out.p) - 2.0 * out.s * dp_dc;
    const double Nv_c = -ds_dc * out.r - out.s * dr_dc + dp_dc * dr_deta - e * drv_c;
    const double d2coeff_dc = (Nv_c * r2 - Nn * 2.0 * out.r * dr_dc) / (r2 * r2);
    out.dwl_dc = -(d2coeff_dc * out.s + dcoeff_deta * ds_dc +
                   dcoeff_dc * ds_deta + out.coeff * (-dq_deta));

    const double drv_d = ds_dd * (1.0 - 2.0 * out.p) - 2.0 * out.s * dp_dd;
    const double Nv_d = -ds_dd * out.r - out.s * dr_dd + dp_dd * dr_deta - e * drv_d;
    const double d2coeff_dd = (Nv_d * r2 - Nn * 2.0 * out.r * dr_dd) / (r2 * r2);
    out.dwl_dd = -(d2coeff_dd * out.s + dcoeff_deta * ds_dd +
                   dcoeff_dd * ds_deta + out.coeff * dq_deta);
  }
  return out;
}

// Raw row terms, for the tests. Nothing in the fitting path calls this.
extern "C" SEXP _bigIRT_laplace_row_terms_debug_cpp_impl(
    SEXP ySEXP, SEXP etaSEXP, SEXP cSEXP, SEXP dSEXP) {
  BEGIN_RCPP
  const BigIRTRowTerms rt = bigirt_row_terms(
    Rcpp::as<double>(ySEXP), Rcpp::as<double>(etaSEXP),
    Rcpp::as<double>(cSEXP), Rcpp::as<double>(dSEXP));
  return Rcpp::List::create(
    Rcpp::Named("ll") = rt.ll, Rcpp::Named("p") = rt.p,
    Rcpp::Named("grad_eta") = rt.grad_eta, Rcpp::Named("grad_c") = rt.grad_c,
    Rcpp::Named("grad_d") = rt.grad_d, Rcpp::Named("w") = rt.w,
    Rcpp::Named("wl") = rt.wl,
    Rcpp::Named("dwl_deta") = rt.dwl_deta, Rcpp::Named("dwl_dc") = rt.dwl_dc,
    Rcpp::Named("dwl_dd") = rt.dwl_dd,
    Rcpp::Named("dgrad_eta_deta") = rt.dgrad_eta_deta);
  END_RCPP
}

// Is the data's own contribution to the curvature positive semidefinite?
//
// At a true mode the FULL Hessian must be, but the likelihood part alone need
// not be. On a flat shoulder the observed curvature from the data can be net
// negative, leaving a Gaussian propped up only by the prior and far wider
// than the posterior it is approximating. Measured on a simulated 4PL fit,
// one person in 800 came back with a posterior SD of 2.72 where quadrature
// said 0.81 (Fisher said 0.50). Fisher information cannot do this, being
// positive term by term, so those persons fall back to it.
//
// For K = 1 this reduces to
// "the summed observed information from the responses is not negative".
inline bool bigirt_like_curvature_ok(const MatrixXd& like) {
  if (like.rows() == 0) return true;
  Eigen::LDLT<MatrixXd> ldl(like);
  if (ldl.info() != Eigen::Success) return false;
  return ldl.vectorD().minCoeff() >= 0.0;
}

// Nearest positive semidefinite matrix, by clipping negative eigenvalues.
//
// This replaces an earlier design that switched the whole person over to the
// Fisher curvature instead. That switch was discontinuous: at the boundary
// the data curvature passes through zero, so log|H| stepped from log|prior|
// to log|prior + W_fisher|. Measured by scanning one item difficulty across a
// crossing, that is a jump of about 0.8 nats in a person's contribution --
// 1765 times the ordinary step between adjacent points -- and the gradient
// carries no term for it, which is precisely what a line search cannot cope
// with. Clipping is continuous in W, so the objective is continuous, and
// where clipping is active log|H| is locally flat in the item parameters, so
// the matching gradient contribution is zero.
//
// Only reached when the LDLT test above fails, so the eigen solve costs
// nothing on the overwhelming majority of persons.
// Clipping counters. When every eigenvalue is clipped the curvature becomes
// the prior alone, log|H| is exactly constant in the item parameters, and
// zeroing its gradient contribution is exact -- that is always the case for
// K = 1. When only SOME are clipped, which needs K > 1, zeroing the whole
// contribution is an approximation: the value stays continuous but the
// gradient does not match it in the unclipped directions.
//
// That case was never observed -- zero occurrences in 4,800 simulated
// persons at K = 1, 2 and 3, with the worst data-curvature eigenvalue still
// at +0.43 -- so it is counted and reported rather than silently
// approximated. If it ever fires on real data, fitIRT says so.
static std::atomic<long> bigirt_laplace_clip_full{0};
static std::atomic<long> bigirt_laplace_clip_partial{0};

extern "C" SEXP _bigIRT_laplace_clip_counts_cpp_impl(SEXP resetSEXP) {
  BEGIN_RCPP
  const long f = bigirt_laplace_clip_full.load();
  const long p = bigirt_laplace_clip_partial.load();
  if (Rcpp::as<bool>(resetSEXP)) {
    bigirt_laplace_clip_full.store(0);
    bigirt_laplace_clip_partial.store(0);
  }
  return Rcpp::IntegerVector::create(
    Rcpp::Named("full") = static_cast<int>(f),
    Rcpp::Named("partial") = static_cast<int>(p));
  END_RCPP
}

inline MatrixXd bigirt_psd_clip(const MatrixXd& W) {
  Eigen::SelfAdjointEigenSolver<MatrixXd> es(W);
  if (es.info() != Eigen::Success) {
    bigirt_laplace_clip_full.fetch_add(1);
    return MatrixXd::Zero(W.rows(), W.cols());
  }
  VectorXd lam = es.eigenvalues();
  int nclipped = 0;
  for (int i = 0; i < lam.size(); ++i) if (lam(i) < 0.0) { lam(i) = 0.0; ++nclipped; }
  if (nclipped == lam.size()) bigirt_laplace_clip_full.fetch_add(1);
  else if (nclipped > 0) bigirt_laplace_clip_partial.fetch_add(1);
  return es.eigenvectors() * lam.asDiagonal() * es.eigenvectors().transpose();
}

inline MatrixXd bigirt_matrix_from_array(const Rcpp::NumericVector& arr, const int K, const int idx) {
  MatrixXd out = MatrixXd::Zero(K, K);
  for (int r = 0; r < K; ++r) {
    for (int c = 0; c < K; ++c) {
      out(r, c) = arr[r + K * c + K * K * idx];
    }
  }
  return out;
}

inline MatrixXd bigirt_matrix_from_array_parallel(const RcppParallel::RVector<double>& arr, const int K, const int idx) {
  MatrixXd out = MatrixXd::Zero(K, K);
  for (int r = 0; r < K; ++r) {
    for (int c = 0; c < K; ++c) {
      out(r, c) = arr[r + K * c + K * K * idx];
    }
  }
  return out;
}

inline MatrixXd bigirt_corr_from_params_normalized_chol(const Rcpp::NumericVector& pars, const int K) {
  MatrixXd L = MatrixXd::Identity(K, K);
  int idx = 0;
  for (int i = 1; i < K; ++i) {
    for (int j = 0; j < i; ++j) {
      L(i, j) = pars[idx++];
    }
  }
  MatrixXd S = L * L.transpose();
  VectorXd d = S.diagonal().array().max(1e-12).sqrt();
  MatrixXd R = S;
  for (int i = 0; i < K; ++i) {
    for (int j = 0; j < K; ++j) {
      R(i, j) /= (d(i) * d(j));
    }
  }
  return R;
}

struct BigIRTLaplaceRowAbilityWorker : public RcppParallel::Worker {
  RcppParallel::RVector<int> id;
  RcppParallel::RMatrix<double> theta_mode;
  RcppParallel::RMatrix<double> person_pred;
  RcppParallel::RMatrix<int> fixed_ability;
  RcppParallel::RMatrix<double> fixed_ability_value;
  RcppParallel::RMatrix<double> Abilitybeta;
  RcppParallel::RMatrix<double> row_ability;
  const int K;

  BigIRTLaplaceRowAbilityWorker(
    Rcpp::IntegerVector id,
    Rcpp::NumericMatrix theta_mode,
    Rcpp::NumericMatrix person_pred,
    Rcpp::IntegerMatrix fixed_ability,
    Rcpp::NumericMatrix fixed_ability_value,
    Rcpp::NumericMatrix Abilitybeta,
    Rcpp::NumericMatrix row_ability,
    const int K)
    : id(id), theta_mode(theta_mode), person_pred(person_pred),
      fixed_ability(fixed_ability), fixed_ability_value(fixed_ability_value),
      Abilitybeta(Abilitybeta), row_ability(row_ability), K(K) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t obs_idx = begin; obs_idx < end; ++obs_idx) {
      const int obs = static_cast<int>(obs_idx);
      const int subj = id[obs] - 1;
      for (int k = 0; k < K; ++k) {
        double val = theta_mode(subj, k);
        for (int j = 0; j < person_pred.ncol(); ++j) {
          val += person_pred(obs, j) * Abilitybeta(k, j);
        }
        if (fixed_ability(obs, k) != 0) {
          val = fixed_ability_value(obs, k);
        }
        row_ability(obs, k) = val;
      }
    }
  }
};

inline MatrixXd bigirt_corr_from_params_stan_corsqrt(const Rcpp::NumericVector& pars, const int K) {
  MatrixXd L = MatrixXd::Zero(K, K);
  L(0, 0) = 1.0;
  int idx = 0;
  for (int i = 1; i < K; ++i) {
    VectorXd raw(i);
    for (int j = 0; j < i; ++j) raw(j) = std::tanh(pars[idx++]);
    double prod_term = 1.0;
    for (int j = 0; j < i; ++j) {
      if (j > 0) prod_term *= std::sqrt(std::max(1.0 - raw(j - 1) * raw(j - 1), 1e-12));
      L(i, j) = raw(j) * prod_term;
    }
    double diag_term = 1.0;
    for (int j = 0; j < i; ++j) diag_term *= std::sqrt(std::max(1.0 - raw(j) * raw(j), 1e-12));
    L(i, i) = diag_term;
  }
  return L * L.transpose();
}

inline MatrixXd bigirt_corr_from_params(const Rcpp::NumericVector& pars, const int K, const int paramization) {
  if (paramization == 1) return bigirt_corr_from_params_stan_corsqrt(pars, K);
  return bigirt_corr_from_params_normalized_chol(pars, K);
}

inline MatrixXd bigirt_prior_precision_from_corr(const Rcpp::NumericVector& pars,
  const Rcpp::NumericVector& ability_sd, const int K, const double jitter, const int paramization = 0) {
  MatrixXd R = bigirt_corr_from_params(pars, K, paramization);
  MatrixXd D = MatrixXd::Zero(K, K);
  for (int k = 0; k < K; ++k) D(k, k) = std::max(ability_sd[k], jitter);
  MatrixXd Sigma = D * R * D;
  Sigma.diagonal().array() += jitter;
  Eigen::LLT<MatrixXd> llt(Sigma);
  if (llt.info() != Eigen::Success) {
    Sigma.diagonal().array() += 1e-6;
    llt.compute(Sigma);
    if (llt.info() != Eigen::Success) {
      Rcpp::stop("Failed to build prior precision from AbilityCorr parameters.");
    }
  }
  return llt.solve(MatrixXd::Identity(K, K));
}

// Native M-step for a Gaussian residual-ability prior.  theta_residual is the
// posterior mode of u_i in theta_i = X_i beta + u_i.  Regressing u on X gives
// the coefficient correction that restores E[u | X] = 0 without using an R
// fitting path.
// Per-person score and log-determinant-slope accumulations over responses.
//
// The R implementation of this was a third of every objective evaluation on
// fits with person covariates, because the adjoint gradient path needs it and
// it walks Nobs-length vectors several times over.  The arithmetic is entirely
// elementwise plus one scatter by person, so it ports directly.
extern "C" SEXP _bigIRT_laplace_person_row_terms_cpp_impl(
    SEXP idsSEXP, SEXP ySEXP, SEXP etaSEXP, SEXP cSEXP, SEXP dSEXP,
    SEXP loadingsSEXP, SEXP sigmaSEXP, SEXP NSEXP, SEXP KSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector ids(idsSEXP);
  Rcpp::NumericVector y(ySEXP), eta(etaSEXP), crow(cSEXP), drow(dSEXP);
  Rcpp::NumericMatrix lo(loadingsSEXP);
  Rcpp::NumericVector sigma(sigmaSEXP);
  const int N = Rcpp::as<int>(NSEXP);
  const int K = Rcpp::as<int>(KSEXP);
  const R_xlen_t n = ids.size();
  if(y.size() != n || eta.size() != n || crow.size() != n || drow.size() != n ||
     lo.nrow() != n || lo.ncol() != K)
    Rcpp::stop("Unexpected dimensions in Laplace person row terms.");
  if(sigma.size() != (R_xlen_t)K * K * N)
    Rcpp::stop("Posterior covariance array has unexpected size.");

  Rcpp::NumericMatrix score(N, K), slope(N, K);
  const double lo_p = 1e-12, hi_p = 1.0 - 1e-12;
  for(R_xlen_t i = 0; i < n; ++i) {
    const int subj = ids[i] - 1;                 // R is 1-based
    if(subj < 0 || subj >= N) Rcpp::stop("Person index out of range.");
    const double g = bigirt_stable_inv_logit(eta[i]);
    const double q = g * (1.0 - g);
    const double u = drow[i] - crow[i];
    double p = crow[i] + u * g;
    if(p < lo_p) p = lo_p; else if(p > hi_p) p = hi_p;
    const double s = u * q;
    double r = p * (1.0 - p);
    if(r < lo_p) r = lo_p;
    const double grad_eta = ((y[i] - p) / r) * s;

    // Taken from the shared row terms: the observed curvature depends on
    // the response, where the Fisher one this loop used to compute did not.
    const double dw_deta = bigirt_row_terms(y[i], eta[i], crow[i], drow[i]).dwl_deta;

    // aSa = a' Sigma a for this response, from the K^2 blocks of this person.
    double aSa = 0.0;
    const R_xlen_t base = (R_xlen_t)K * K * subj;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      if(lk == 0.0) continue;
      for(int l = 0; l < K; ++l)
        aSa += lk * lo(i, l) * sigma[base + k + (R_xlen_t)K * l];
    }
    const double slope_i = dw_deta * aSa;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      score(subj, k) += grad_eta * lk;
      slope(subj, k) += slope_i * lk;
    }
  }
  return Rcpp::List::create(Rcpp::Named("score") = score,
                            Rcpp::Named("slope") = slope);
  END_RCPP
}

// a' Sigma a per response, for the item-information blocks. The R version made
// K^2 passes over the response set, one per covariance block.
extern "C" SEXP _bigIRT_row_aSa_cpp_impl(
    SEXP idsSEXP, SEXP loadingsSEXP, SEXP sigmaSEXP, SEXP NSEXP, SEXP KSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector ids(idsSEXP);
  Rcpp::NumericMatrix lo(loadingsSEXP);
  Rcpp::NumericVector sigma(sigmaSEXP);
  const int N = Rcpp::as<int>(NSEXP), K = Rcpp::as<int>(KSEXP);
  const R_xlen_t n = ids.size();
  if(lo.nrow() != n || lo.ncol() != K) Rcpp::stop("Unexpected dimensions in aSa.");
  if(sigma.size() != (R_xlen_t)K * K * N) Rcpp::stop("Posterior covariance array has unexpected size.");
  Rcpp::NumericVector out(n);
  for(R_xlen_t i = 0; i < n; ++i) {
    const int subj = ids[i] - 1;
    if(subj < 0 || subj >= N) Rcpp::stop("Person index out of range.");
    const R_xlen_t base = (R_xlen_t)K * K * subj;
    double acc = 0.0;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      if(lk == 0.0) continue;
      for(int l = 0; l < K; ++l) acc += lk * lo(i, l) * sigma[base + k + (R_xlen_t)K * l];
    }
    out[i] = acc;
  }
  return out;
  END_RCPP
}

// Item information blocks: S[b1,b2,item] = sum_rows V_b1 V_b2 wcorr, gathered
// by item. The R version ran one rowsum over the whole response set for each
// of the P(P+1)/2 distinct pairs; this makes a single pass.
extern "C" SEXP _bigIRT_item_info_accum_cpp_impl(
    SEXP itemSEXP, SEXP VSEXP, SEXP wcorrSEXP, SEXP niSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector item(itemSEXP);
  Rcpp::NumericMatrix V(VSEXP);
  Rcpp::NumericVector w(wcorrSEXP);
  const int ni = Rcpp::as<int>(niSEXP);
  const R_xlen_t n = item.size();
  const int P = V.ncol();
  if(V.nrow() != n || w.size() != n) Rcpp::stop("Unexpected dimensions in item info accumulation.");
  Rcpp::NumericVector out((R_xlen_t)P * P * ni);
  std::vector<double> v(P);
  for(R_xlen_t i = 0; i < n; ++i) {
    const int it = item[i] - 1;
    if(it < 0 || it >= ni) Rcpp::stop("Item index out of range.");
    const double wi = w[i];
    if(wi == 0.0) continue;
    for(int b = 0; b < P; ++b) v[b] = V(i, b);
    const R_xlen_t base = (R_xlen_t)P * P * it;
    for(int b1 = 0; b1 < P; ++b1) {
      const double v1 = v[b1] * wi;
      if(v1 == 0.0) continue;
      for(int b2 = b1; b2 < P; ++b2) {
        const double add = v1 * v[b2];
        out[base + b1 + (R_xlen_t)P * b2] += add;
        if(b2 > b1) out[base + b2 + (R_xlen_t)P * b1] += add;
      }
    }
  }
  out.attr("dim") = Rcpp::IntegerVector::create(P, P, ni);
  return out;
  END_RCPP
}

// Person row terms and the within-person ability-beta gradient, grouped by
// person and run in parallel.
//
// The beta gradient's third term needs c_i = sum_j slope_w_ij lo_ij before it
// can weight any of that person's responses, which is why this was two sweeps
// over the whole response set. Grouped by person the dependency is local: both
// passes run over one person's rows, which is a hundred or so values that stay
// in cache, so nothing has to be stored between them and no accumulator scales
// with N * K^2 * P.
//
// It also parallelises cleanly. Persons are disjoint, so score and slope are
// written to slots no other thread touches; only the K x P gradient needs a
// reduction, and it is tiny.
struct BigIRTLaplacePersonBetaWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<int> order, starts;
  const RcppParallel::RVector<double> y, eta, crow, drow, sigma;
  const RcppParallel::RMatrix<double> lo, x;
  const RcppParallel::RMatrix<int> fixed;
  const RcppParallel::RVector<int> vary;
  RcppParallel::RMatrix<double> score, slope;
  const int K, P;
  std::vector<double> out;

  BigIRTLaplacePersonBetaWorker(const Rcpp::IntegerVector& order_, const Rcpp::IntegerVector& starts_,
      const Rcpp::NumericVector& y_, const Rcpp::NumericVector& eta_,
      const Rcpp::NumericVector& c_, const Rcpp::NumericVector& d_,
      const Rcpp::NumericVector& sigma_, const Rcpp::NumericMatrix& lo_,
      const Rcpp::NumericMatrix& x_, const Rcpp::IntegerMatrix& fixed_,
      const Rcpp::IntegerVector& vary_, Rcpp::NumericMatrix score_,
      Rcpp::NumericMatrix slope_, int K_, int P_)
    : order(order_), starts(starts_), y(y_), eta(eta_), crow(c_), drow(d_),
      sigma(sigma_), lo(lo_), x(x_), fixed(fixed_), vary(vary_),
      score(score_), slope(slope_), K(K_), P(P_), out((size_t)K_*P_, 0.0) {}

  BigIRTLaplacePersonBetaWorker(const BigIRTLaplacePersonBetaWorker& o, RcppParallel::Split)
    : order(o.order), starts(o.starts), y(o.y), eta(o.eta), crow(o.crow), drow(o.drow),
      sigma(o.sigma), lo(o.lo), x(o.x), fixed(o.fixed), vary(o.vary),
      score(o.score), slope(o.slope), K(o.K), P(o.P), out((size_t)o.K*o.P, 0.0) {}

  void join(const BigIRTLaplacePersonBetaWorker& o) {
    for(size_t i = 0; i < out.size(); ++i) out[i] += o.out[i];
  }

  void operator()(std::size_t begin, std::size_t end) {
    const double lo_p = 1e-12, hi_p = 1.0 - 1e-12;
    std::vector<double> c_i(K), e_i(K);
    for(std::size_t subj = begin; subj < end; ++subj) {
      const int from = starts[subj], to = starts[subj + 1];
      if(from == to) continue;
      const R_xlen_t base = (R_xlen_t)K * K * subj;
      for(int k = 0; k < K; ++k) c_i[k] = 0.0;

      // pass A: person totals
      for(int t = from; t < to; ++t) {
        const R_xlen_t i = order[t];
        const double g = bigirt_stable_inv_logit(eta[i]);
        const double q = g * (1.0 - g);
        const double u = drow[i] - crow[i];
        double pp = crow[i] + u * g;
        if(pp < lo_p) pp = lo_p; else if(pp > hi_p) pp = hi_p;
        const double sc = u * q;
        double r = pp * (1.0 - pp);
        if(r < lo_p) r = lo_p;
        const double grad_eta = ((y[i] - pp) / r) * sc;
        const double dw_deta = bigirt_row_terms(y[i], eta[i], crow[i], drow[i]).dwl_deta;
        double aSa = 0.0;
        for(int k = 0; k < K; ++k) {
          const double lk = lo(i, k);
          if(lk == 0.0) continue;
          for(int l = 0; l < K; ++l) aSa += lk * lo(i, l) * sigma[base + k + (R_xlen_t)K * l];
        }
        const double slope_i = dw_deta * aSa;
        for(int k = 0; k < K; ++k) {
          const double lk = lo(i, k);
          score(subj, k) += grad_eta * lk;
          slope(subj, k) += slope_i * lk;
          c_i[k] += slope_i * lk;
        }
      }
      if(P == 0) continue;

      // e_i = Sigma_i c_i, so q_j below is a dot product rather than a K^2 form
      for(int n = 0; n < K; ++n) {
        double acc = 0.0;
        for(int m = 0; m < K; ++m) acc += sigma[base + n + (R_xlen_t)K * m] * c_i[m];
        e_i[n] = acc;
      }

      // pass B: same rows, still warm
      for(int t = from; t < to; ++t) {
        const R_xlen_t i = order[t];
        const double g = bigirt_stable_inv_logit(eta[i]);
        const double q = g * (1.0 - g);
        const double u = drow[i] - crow[i];
        double pp = crow[i] + u * g;
        if(pp < lo_p) pp = lo_p; else if(pp > hi_p) pp = hi_p;
        const double sc = u * q;
        double r = pp * (1.0 - pp);
        if(r < lo_p) r = lo_p;
        const double grad_eta = ((y[i] - pp) / r) * sc;
        const BigIRTRowTerms rt_i = bigirt_row_terms(y[i], eta[i], crow[i], drow[i]);
        const double dw_deta = rt_i.dwl_deta;
        const double hess_w = rt_i.wl;
        double aSa = 0.0, q_row = 0.0;
        for(int k = 0; k < K; ++k) {
          const double lk = lo(i, k);
          q_row += e_i[k] * lk;
          if(lk == 0.0) continue;
          for(int l = 0; l < K; ++l) aSa += lk * lo(i, l) * sigma[base + k + (R_xlen_t)K * l];
        }
        const double core = grad_eta - 0.5 * dw_deta * aSa + 0.5 * hess_w * q_row;
        for(int a = 0; a < P; ++a) {
          const double xa = x(i, vary[a]);
          if(xa == 0.0) continue;
          for(int k = 0; k < K; ++k) {
            if(fixed(i, k) != 0) continue;
            out[(size_t)k * P + a] += core * lo(i, k) * xa;
          }
        }
      }
    }
  }
};

extern "C" SEXP _bigIRT_laplace_person_beta_grouped_cpp_impl(
    SEXP orderSEXP, SEXP startsSEXP, SEXP ySEXP, SEXP etaSEXP, SEXP cSEXP,
    SEXP dSEXP, SEXP loadingsSEXP, SEXP sigmaSEXP, SEXP xSEXP, SEXP varyIdxSEXP,
    SEXP fixedSEXP, SEXP NSEXP, SEXP KSEXP, SEXP grainSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector order(orderSEXP), starts(startsSEXP), vary(varyIdxSEXP);
  Rcpp::NumericVector y(ySEXP), eta(etaSEXP), crow(cSEXP), drow(dSEXP), sigma(sigmaSEXP);
  Rcpp::NumericMatrix lo(loadingsSEXP), x(xSEXP);
  Rcpp::IntegerMatrix fixed(fixedSEXP);
  const int N = Rcpp::as<int>(NSEXP), K = Rcpp::as<int>(KSEXP);
  const std::size_t grain = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grainSEXP)));
  const R_xlen_t n = y.size();
  const int P = vary.size();
  if(eta.size() != n || crow.size() != n || drow.size() != n || lo.nrow() != n ||
     lo.ncol() != K || x.nrow() != n || fixed.nrow() != n || fixed.ncol() != K ||
     order.size() != n || starts.size() != N + 1)
    Rcpp::stop("Unexpected dimensions in Laplace grouped person/beta pass.");
  if(sigma.size() != (R_xlen_t)K * K * N)
    Rcpp::stop("Posterior covariance array has unexpected size.");
  for(int a = 0; a < P; ++a)
    if(vary[a] < 0 || vary[a] >= x.ncol()) Rcpp::stop("Predictor index out of range.");

  Rcpp::NumericMatrix score(N, K), slope(N, K);
  BigIRTLaplacePersonBetaWorker w(order, starts, y, eta, crow, drow, sigma, lo, x,
                                  fixed, vary, score, slope, K, P);
  RcppParallel::parallelReduce(static_cast<std::size_t>(0),
                               static_cast<std::size_t>(N), w, grain);
  Rcpp::NumericMatrix out(K, P);
  for(int k = 0; k < K; ++k) for(int a = 0; a < P; ++a) out(k, a) = w.out[(size_t)k * P + a];
  return Rcpp::List::create(Rcpp::Named("score") = score,
                            Rcpp::Named("slope") = slope,
                            Rcpp::Named("beta") = out);
  END_RCPP
}

// Person row terms and the within-person ability-beta gradient in one pass.
//
// The two used to be separate sweeps over the responses because the beta
// gradient's third term needs c_i = sum_j slope_w_ij lo_ij, a per-person total
// that is only complete once the first sweep has finished. It does not need it
// per response, though. Writing that term out,
//
//   T3[k,p] = 1/2 sum_i sum_{m,n} Sigma_i[m,n] c_i[m] D_i[n,k,p]
//   D_i[n,k,p] = sum_j hess_w_ij lo_ijn lo_ijk x_ijp
//
// separates into two per-person accumulators, and D_i accumulates in the same
// sweep that builds c_i. The combination afterwards runs over persons rather
// than responses. So one pass over the response set replaces two.
//
// D costs N*K*K*P doubles. That is small for the dimensions these models are
// used at (4 MB for 67k persons, two scales, two varying covariates) but grows
// with K^2, so past a budget the function falls back to the original second
// sweep, which needs no storage.
extern "C" SEXP _bigIRT_laplace_person_beta_fused_cpp_impl(
    SEXP idsSEXP, SEXP ySEXP, SEXP etaSEXP, SEXP cSEXP, SEXP dSEXP,
    SEXP loadingsSEXP, SEXP sigmaSEXP, SEXP xSEXP, SEXP varyIdxSEXP,
    SEXP fixedSEXP, SEXP NSEXP, SEXP KSEXP, SEXP maxDoublesSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector ids(idsSEXP);
  Rcpp::NumericVector y(ySEXP), eta(etaSEXP), crow(cSEXP), drow(dSEXP);
  Rcpp::NumericMatrix lo(loadingsSEXP);
  Rcpp::NumericVector sigma(sigmaSEXP);
  Rcpp::NumericMatrix x(xSEXP);
  Rcpp::IntegerVector vary(varyIdxSEXP);
  Rcpp::IntegerMatrix fixed(fixedSEXP);
  const int N = Rcpp::as<int>(NSEXP);
  const int K = Rcpp::as<int>(KSEXP);
  const double max_doubles = Rcpp::as<double>(maxDoublesSEXP);
  const R_xlen_t n = ids.size();
  const int P = vary.size();
  if(y.size() != n || eta.size() != n || crow.size() != n || drow.size() != n ||
     lo.nrow() != n || lo.ncol() != K || x.nrow() != n ||
     fixed.nrow() != n || fixed.ncol() != K)
    Rcpp::stop("Unexpected dimensions in Laplace fused person/beta pass.");
  if(sigma.size() != (R_xlen_t)K * K * N)
    Rcpp::stop("Posterior covariance array has unexpected size.");
  for(int a = 0; a < P; ++a)
    if(vary[a] < 0 || vary[a] >= x.ncol()) Rcpp::stop("Predictor index out of range.");

  Rcpp::NumericMatrix score(N, K), slope(N, K), out(K, P);
  const double lo_p = 1e-12, hi_p = 1.0 - 1e-12;

  const double want = (double)N * K * K * P;
  const bool store_D = (P > 0) && (want <= max_doubles);
  std::vector<double> D;
  if(store_D) D.assign((size_t)want, 0.0);

  // -- one sweep: person totals, the two direct beta terms, and D ------------
  for(R_xlen_t i = 0; i < n; ++i) {
    const int subj = ids[i] - 1;
    if(subj < 0 || subj >= N) Rcpp::stop("Person index out of range.");
    const double g = bigirt_stable_inv_logit(eta[i]);
    const double q = g * (1.0 - g);
    const double u = drow[i] - crow[i];
    double pp = crow[i] + u * g;
    if(pp < lo_p) pp = lo_p; else if(pp > hi_p) pp = hi_p;
    const double sc = u * q;
    double r = pp * (1.0 - pp);
    if(r < lo_p) r = lo_p;
    const double grad_eta = ((y[i] - pp) / r) * sc;

    const BigIRTRowTerms rt_i = bigirt_row_terms(y[i], eta[i], crow[i], drow[i]);
    const double dw_deta = rt_i.dwl_deta;
    const double hess_w = rt_i.wl;

    const R_xlen_t base = (R_xlen_t)K * K * subj;
    double aSa = 0.0;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      if(lk == 0.0) continue;
      for(int l = 0; l < K; ++l) aSa += lk * lo(i, l) * sigma[base + k + (R_xlen_t)K * l];
    }
    const double slope_i = dw_deta * aSa;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      score(subj, k) += grad_eta * lk;
      slope(subj, k) += slope_i * lk;
    }
    if(P == 0 || !store_D) continue;

    const double core12 = grad_eta - 0.5 * slope_i;
    for(int a = 0; a < P; ++a) {
      const double xa = x(i, vary[a]);
      if(xa == 0.0) continue;
      for(int k = 0; k < K; ++k) {
        if(fixed(i, k) != 0) continue;
        const double lk = lo(i, k);
        if(lk == 0.0) continue;
        out(k, a) += core12 * lk * xa;
        const double hx = hess_w * lk * xa;
        if(hx == 0.0) continue;
        for(int nn = 0; nn < K; ++nn) {
          const double ln = lo(i, nn);
          if(ln == 0.0) continue;
          D[(((size_t)subj * K + nn) * K + k) * P + a] += hx * ln;
        }
      }
    }
  }

  // -- combine over persons, not responses ----------------------------------
  if(store_D && P > 0) {
    for(int subj = 0; subj < N; ++subj) {
      const R_xlen_t base = (R_xlen_t)K * K * subj;
      double e[16];
      const bool small = (K <= 16);
      std::vector<double> ebig;
      double* ep = e;
      if(!small) { ebig.assign(K, 0.0); ep = ebig.data(); }
      for(int nn = 0; nn < K; ++nn) {
        double acc = 0.0;
        for(int m = 0; m < K; ++m) acc += sigma[base + nn + (R_xlen_t)K * m] * slope(subj, m);
        ep[nn] = acc;
      }
      for(int a = 0; a < P; ++a)
        for(int k = 0; k < K; ++k) {
          double acc = 0.0;
          for(int nn = 0; nn < K; ++nn)
            acc += ep[nn] * D[(((size_t)subj * K + nn) * K + k) * P + a];
          out(k, a) += 0.5 * acc;
        }
    }
  }

  return Rcpp::List::create(Rcpp::Named("score") = score,
                            Rcpp::Named("slope") = slope,
                            Rcpp::Named("beta") = out,
                            Rcpp::Named("fused") = store_D);
  END_RCPP
}

// Ability-beta gradient for person predictors that vary within a person.
//
// The R path derived the same per-response quantities the row-terms kernel
// above already computes, then assembled the gradient in R. Both are avoided
// here: this repeats the arithmetic (about twenty flops, cheaper than storing
// and re-reading three vectors the length of the response set) and accumulates
// the K x P result directly.
//
// The gradient is
//
//   out[k,p] = sum_j ( ge_j - 1/2 sw_j + 1/2 hw_j q_j ) lo_jk x_jp
//
// with q_j = c_i' Sigma_i lo_j and c_i the per-person slope total supplied in
// cg. See the derivation note in bigIRT_laplace_ability_beta_contribution.
extern "C" SEXP _bigIRT_laplace_ability_beta_rows_cpp_impl(
    SEXP idsSEXP, SEXP ySEXP, SEXP etaSEXP, SEXP cSEXP, SEXP dSEXP,
    SEXP loadingsSEXP, SEXP sigmaSEXP, SEXP cgSEXP, SEXP xSEXP,
    SEXP varyIdxSEXP, SEXP fixedSEXP, SEXP NSEXP, SEXP KSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector ids(idsSEXP);
  Rcpp::NumericVector y(ySEXP), eta(etaSEXP), crow(cSEXP), drow(dSEXP);
  Rcpp::NumericMatrix lo(loadingsSEXP);
  Rcpp::NumericVector sigma(sigmaSEXP);
  Rcpp::NumericMatrix cg(cgSEXP);
  Rcpp::NumericMatrix x(xSEXP);
  Rcpp::IntegerVector vary(varyIdxSEXP);        // 0-based columns of x
  Rcpp::IntegerMatrix fixed(fixedSEXP);
  const int N = Rcpp::as<int>(NSEXP);
  const int K = Rcpp::as<int>(KSEXP);
  const R_xlen_t n = ids.size();
  const int P = vary.size();
  if(y.size() != n || eta.size() != n || crow.size() != n || drow.size() != n ||
     lo.nrow() != n || lo.ncol() != K || x.nrow() != n ||
     fixed.nrow() != n || fixed.ncol() != K || cg.nrow() != N || cg.ncol() != K)
    Rcpp::stop("Unexpected dimensions in Laplace ability-beta row gradient.");
  if(sigma.size() != (R_xlen_t)K * K * N)
    Rcpp::stop("Posterior covariance array has unexpected size.");
  for(int a = 0; a < P; ++a)
    if(vary[a] < 0 || vary[a] >= x.ncol()) Rcpp::stop("Predictor index out of range.");

  Rcpp::NumericMatrix out(K, P);
  const double lo_p = 1e-12, hi_p = 1.0 - 1e-12;
  std::vector<double> w(K);
  for(R_xlen_t i = 0; i < n; ++i) {
    const int subj = ids[i] - 1;
    if(subj < 0 || subj >= N) Rcpp::stop("Person index out of range.");
    const double g = bigirt_stable_inv_logit(eta[i]);
    const double q = g * (1.0 - g);
    const double u = drow[i] - crow[i];
    double pp = crow[i] + u * g;
    if(pp < lo_p) pp = lo_p; else if(pp > hi_p) pp = hi_p;
    const double sc = u * q;
    double r = pp * (1.0 - pp);
    if(r < lo_p) r = lo_p;
    const double grad_eta = ((y[i] - pp) / r) * sc;

    const BigIRTRowTerms rt_i = bigirt_row_terms(y[i], eta[i], crow[i], drow[i]);
    const double dw_deta = rt_i.dwl_deta;
    const double hess_w = rt_i.wl;

    // aSa = a' Sigma a, and q_row = c_i' Sigma_i a, over the same K^2 blocks.
    const R_xlen_t base = (R_xlen_t)K * K * subj;
    double aSa = 0.0, q_row = 0.0;
    for(int k = 0; k < K; ++k) {
      const double lk = lo(i, k);
      const double ck = cg(subj, k);
      if(lk == 0.0 && ck == 0.0) continue;
      for(int l = 0; l < K; ++l) {
        const double skl = sigma[base + k + (R_xlen_t)K * l];
        const double ll = lo(i, l);
        if(lk != 0.0) aSa += lk * ll * skl;
        if(ck != 0.0) q_row += ck * skl * ll;
      }
    }
    const double slope_w = dw_deta * aSa;
    const double core = grad_eta - 0.5 * slope_w + 0.5 * hess_w * q_row;

    bool any = false;
    for(int k = 0; k < K; ++k) {
      w[k] = (fixed(i, k) != 0) ? 0.0 : core * lo(i, k);
      if(w[k] != 0.0) any = true;
    }
    if(!any) continue;
    for(int a = 0; a < P; ++a) {
      const double xa = x(i, vary[a]);
      if(xa == 0.0) continue;
      for(int k = 0; k < K; ++k) out(k, a) += w[k] * xa;
    }
  }
  return out;
  END_RCPP
}

struct BigIRTLaplaceItemObjectiveWorker : public RcppParallel::Worker {
  const std::vector< std::vector<int> >& obs_by_subj;
  RcppParallel::RVector<int> score;
  RcppParallel::RMatrix<double> row_ability;
  RcppParallel::RVector<double> b;
  RcppParallel::RVector<double> c;
  RcppParallel::RVector<double> d;
  RcppParallel::RMatrix<double> loadings;
  RcppParallel::RVector<double> prior_precision;
  const int K;
  const double jitter;
  const int max_attempts;
  int failure_code;
  RcppParallel::RMatrix<double> grad_loadings;
  RcppParallel::RVector<double> grad_b;
  RcppParallel::RVector<double> grad_c;
  RcppParallel::RVector<double> grad_d;
  RcppParallel::RVector<double> eta;
  RcppParallel::RVector<double> p_row;
  RcppParallel::RVector<double> w_row;
  RcppParallel::RVector<double> logdet_precision;
  RcppParallel::RVector<double> objective_by_subj;
  double objective;

  BigIRTLaplaceItemObjectiveWorker(
      const std::vector< std::vector<int> >& obs_by_subj,
      Rcpp::IntegerVector score,
      Rcpp::NumericMatrix row_ability,
      Rcpp::NumericVector b,
      Rcpp::NumericVector c,
      Rcpp::NumericVector d,
      Rcpp::NumericMatrix loadings,
      Rcpp::NumericVector prior_precision,
      const int K,
      const double jitter,
      const int max_attempts,
      Rcpp::NumericMatrix grad_loadings,
      Rcpp::NumericVector grad_b,
      Rcpp::NumericVector grad_c,
      Rcpp::NumericVector grad_d,
      Rcpp::NumericVector eta,
      Rcpp::NumericVector p_row,
      Rcpp::NumericVector w_row,
      Rcpp::NumericVector logdet_precision,
      Rcpp::NumericVector objective_by_subj)
    : obs_by_subj(obs_by_subj), score(score), row_ability(row_ability),
      b(b), c(c), d(d), loadings(loadings), prior_precision(prior_precision),
      K(K), jitter(jitter), max_attempts(max_attempts),
      grad_loadings(grad_loadings), grad_b(grad_b), grad_c(grad_c), grad_d(grad_d),
      eta(eta), p_row(p_row), w_row(w_row), logdet_precision(logdet_precision),
      objective_by_subj(objective_by_subj), objective(0.0) {}

  BigIRTLaplaceItemObjectiveWorker(const BigIRTLaplaceItemObjectiveWorker& other, RcppParallel::Split)
    : obs_by_subj(other.obs_by_subj), score(other.score), row_ability(other.row_ability),
      b(other.b), c(other.c), d(other.d), loadings(other.loadings), prior_precision(other.prior_precision),
      K(other.K), jitter(other.jitter), max_attempts(other.max_attempts),
      grad_loadings(other.grad_loadings), grad_b(other.grad_b), grad_c(other.grad_c), grad_d(other.grad_d),
      eta(other.eta), p_row(other.p_row), w_row(other.w_row), logdet_precision(other.logdet_precision),
      objective_by_subj(other.objective_by_subj), objective(0.0) {}

  void operator()(std::size_t begin, std::size_t end) {
    const MatrixXd eye = MatrixXd::Identity(K, K);
    for (std::size_t subj = begin; subj < end; ++subj) {
      MatrixXd Q = MatrixXd::Zero(K, K);
      for (int r = 0; r < K; ++r) {
        for (int cidx = 0; cidx < K; ++cidx) {
          Q(r, cidx) = prior_precision[r + K * cidx + K * K * static_cast<int>(subj)];
        }
      }
      const MatrixXd Q_prior = Q;

      const std::vector<int>& obs_idx = obs_by_subj[subj];
      std::vector<BigIRTRowTerms> row_terms(obs_idx.size());
      double loglik = 0.0;
      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        double eta_obs = -b[obs];
        for (int k = 0; k < K; ++k) eta_obs += loadings(obs, k) * row_ability(obs, k);
        BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta_obs, c[obs], d[obs]);
        row_terms[oi] = rt;
        eta[obs] = eta_obs;
        p_row[obs] = rt.p;
        w_row[obs] = rt.wl;
        loglik += rt.ll;
        for (int r = 0; r < K; ++r) {
          const double ar = loadings(obs, r);
          if (ar == 0.0) continue;
          for (int cidx = 0; cidx < K; ++cidx) {
            const double ac = loadings(obs, cidx);
            if (ac == 0.0) continue;
            Q(r, cidx) += rt.wl * ar * ac;
          }
        }
      }

      // Same guard as the person kernels, reached from identical inputs so it
      // reaches the identical decision: if the data's own curvature is not
      // positive semidefinite this person falls back to Fisher. `guarded`
      // also switches the gradient terms below, so the value and the gradient
      // continue to describe one function.
      bool guarded = false;
      {
        const MatrixXd like = Q - Q_prior;
        if (!bigirt_like_curvature_ok(like)) {
          guarded = true;
          Q = Q_prior + bigirt_psd_clip(like);
          for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) w_row[obs_idx[oi]] = 0.0;
        }
      }

      Eigen::LLT<MatrixXd> llt;
      double chol_jitter = jitter;
      bool success = false;
      for (int attempt = 0; attempt < max_attempts; ++attempt) {
        MatrixXd try_prec = Q;
        try_prec.diagonal().array() += chol_jitter;
        llt.compute(try_prec);
        if (llt.info() == Eigen::Success) {
          Q = try_prec;
          success = true;
          break;
        }
        chol_jitter *= 10.0;
      }
      if (!success) Rcpp::stop("Cholesky failed while evaluating Laplace objective.");

      MatrixXd Sigma = llt.solve(eye);
      double logdet = 0.0;
      MatrixXd L = llt.matrixL();
      for (int k = 0; k < K; ++k) logdet += 2.0 * std::log(std::max(L(k, k), 1e-12));
      logdet_precision[static_cast<int>(subj)] = logdet;
      const double subj_obj = loglik + 0.5 * static_cast<double>(K) * std::log(2.0 * M_PI) - 0.5 * logdet;
      objective_by_subj[static_cast<int>(subj)] = subj_obj;
      objective += subj_obj;

      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        VectorXd a(K);
        VectorXd z(K);
        for (int k = 0; k < K; ++k) {
          a(k) = loadings(obs, k);
          z(k) = row_ability(obs, k);
        }
        const VectorXd Sa = Sigma * a;
        const double aSa = a.dot(Sa);
        const BigIRTRowTerms& rt = row_terms[oi];
        // Where the curvature was clipped, log|H| is locally flat in the
        // item parameters, so its gradient contribution -- including the
        // mode adjoint, which is itself a derivative of log|H| -- is zero.
        const double gw       = guarded ? 0.0 : rt.wl;
        const double gdw_deta = guarded ? 0.0 : rt.dwl_deta;
        const double gdw_dc   = guarded ? 0.0 : rt.dwl_dc;
        const double gdw_dd   = guarded ? 0.0 : rt.dwl_dd;
        for (int k = 0; k < K; ++k) {
          grad_loadings(obs, k) =
            rt.grad_eta * z(k) - 0.5 * (gdw_deta * z(k) * aSa + 2.0 * gw * Sa(k));
        }
        grad_b[obs] = -rt.grad_eta + 0.5 * gdw_deta * aSa;
        grad_c[obs] = rt.grad_c - 0.5 * gdw_dc * aSa;
        grad_d[obs] = rt.grad_d - 0.5 * gdw_dd * aSa;
      }
    }
  }

  void join(const BigIRTLaplaceItemObjectiveWorker& rhs) {
    objective += rhs.objective;
  }
};

extern "C" SEXP _bigIRT_laplace_item_objective_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP row_abilitySEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP loadingsSEXP,
    SEXP prior_precisionSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP grain_sizeSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix row_ability(row_abilitySEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::NumericMatrix loadings(loadingsSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const std::size_t grain_size = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));

  const int Nobs = id.size();
  const int K = row_ability.ncol();
  const int Nsubs = Rcpp::as<Rcpp::IntegerVector>(prior_precision.attr("dim"))[2];
  if (row_ability.nrow() != Nobs || loadings.nrow() != Nobs || loadings.ncol() != K) {
    Rcpp::stop("row_ability and loadings must have dimensions Nobs x K.");
  }
  std::vector< std::vector<int> > obs_by_subj(Nsubs);
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    obs_by_subj[subj].push_back(obs);
  }

  Rcpp::NumericMatrix grad_loadings(Nobs, K);
  Rcpp::NumericVector grad_b(Nobs);
  Rcpp::NumericVector grad_c(Nobs);
  Rcpp::NumericVector grad_d(Nobs);
  Rcpp::NumericVector eta(Nobs);
  Rcpp::NumericVector p_row(Nobs);
  Rcpp::NumericVector w_row(Nobs);
  Rcpp::NumericVector logdet_precision(Nsubs);
  Rcpp::NumericVector objective_by_subj(Nsubs);
  BigIRTLaplaceItemObjectiveWorker worker(
    obs_by_subj, score, row_ability, b, c, d, loadings, prior_precision, K, jitter, max_attempts,
    grad_loadings, grad_b, grad_c, grad_d, eta, p_row, w_row, logdet_precision, objective_by_subj
  );
  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(Nsubs), worker, grain_size);

  return Rcpp::List::create(
    Rcpp::Named("objective") = worker.objective,
    Rcpp::Named("grad_loadings") = grad_loadings,
    Rcpp::Named("grad_b") = grad_b,
    Rcpp::Named("grad_c") = grad_c,
    Rcpp::Named("grad_d") = grad_d,
    Rcpp::Named("eta") = eta,
    Rcpp::Named("p_row") = p_row,
    Rcpp::Named("w_row") = w_row,
    Rcpp::Named("logdet_precision") = logdet_precision,
    Rcpp::Named("objective_by_subj") = objective_by_subj
  );
}

// This is deliberately a plain C++ solver, not an RcppParallel Worker.  It
// owns its inputs and is invoked serially until a separate no-throw reduction
// wrapper is introduced.
// Reduction over people for the frozen-mode item objective and gradient.
// The worker owns copies of everything it reads, signals failure through
// failure_code rather than by throwing, and already carries the splitting
// constructor and join a reduction needs, so it is safe to run in parallel.
// This is the dominant cost of a fit: L-BFGS evaluates it many times per
// item block, and while it ran serially the cores argument had almost no
// effect on total runtime.
// Order subjects so that consecutive subjects touch overlapping item
// parameters. The item-block gradient is a sum over subjects, so any
// permutation leaves the result unchanged to within floating-point
// reassociation -- which the reduction already incurs whenever the thread
// count changes. Only locality depends on the order, so nothing has to be
// undone afterwards.
//
// Sparse assessment data is the case this matters for. With items assigned by
// test form, a chunk of arbitrarily ordered subjects touches nearly the whole
// bank, so every chunk pays for every item. Grouping subjects by the items
// they actually answered shrinks that footprint. The signature is a subject's
// sorted set of item references, compared lexicographically, which puts
// subjects sitting identical forms adjacent and similar forms nearby. It
// assumes nothing about the design.
static std::vector<int> bigIRT_subject_locality_order(
    const BigIRTObsIndex& obs_by_subj,
    const Rcpp::IntegerVector& B_ref) {
  const std::size_t n = obs_by_subj.size();
  std::vector<int> order(n);
  for (std::size_t i = 0; i < n; ++i) order[i] = static_cast<int>(i);
  if (n < 2 || B_ref.size() == 0) return order;

  const int nref = static_cast<int>(B_ref.size());
  std::vector< std::vector<int> > sig(n);
  for (std::size_t i = 0; i < n; ++i) {
    const BigIRTObsSpan obs = obs_by_subj[i];
    std::vector<int> k;
    k.reserve(obs.size());
    for (std::size_t j = 0; j < obs.size(); ++j) {
      const int r = obs[j];
      if (r >= 0 && r < nref) k.push_back(B_ref[r]);
    }
    std::sort(k.begin(), k.end());
    k.erase(std::unique(k.begin(), k.end()), k.end());
    sig[i].swap(k);
  }
  std::sort(order.begin(), order.end(),
    [&sig](const int a, const int b) { return sig[a] < sig[b]; });
  return order;
}

struct BigIRTLaplaceItemBlockWorker : public RcppParallel::Worker {
  const BigIRTObsIndex& obs_by_subj;
  BigIRTOwnedVector<int> score;
  BigIRTOwnedMatrix<double> row_ability;
  BigIRTOwnedMatrix<int> A_ref;
  BigIRTOwnedMatrix<double> A_fixed_value;
  BigIRTOwnedMatrix<int> A_beta_row;
  BigIRTOwnedMatrix<double> A_pred;
  BigIRTOwnedVector<int> B_ref;
  BigIRTOwnedVector<double> B_fixed_value;
  BigIRTOwnedVector<int> B_beta_row;
  BigIRTOwnedMatrix<double> B_pred;
  BigIRTOwnedVector<int> C_ref;
  BigIRTOwnedVector<double> C_fixed_value;
  BigIRTOwnedVector<int> C_beta_row;
  BigIRTOwnedMatrix<double> C_pred;
  BigIRTOwnedVector<int> D_ref;
  BigIRTOwnedVector<double> D_fixed_value;
  BigIRTOwnedVector<int> D_beta_row;
  BigIRTOwnedMatrix<double> D_pred;
  BigIRTOwnedVector<double> invspApars;
  BigIRTOwnedMatrix<double> invspAbeta;
  BigIRTOwnedVector<double> Bpars;
  BigIRTOwnedMatrix<double> Bbeta;
  BigIRTOwnedVector<double> logitCpars;
  BigIRTOwnedMatrix<double> logitCbeta;
  BigIRTOwnedVector<double> logitDpars;
  BigIRTOwnedMatrix<double> logitDbeta;
  BigIRTOwnedVector<double> prior_precision;
  const int K;
  const int nA;
  const int nB;
  const int nC;
  const int nD;
  const int nA_beta_row;
  const int nB_beta_row;
  const int nC_beta_row;
  const int nD_beta_row;
  const int pA;
  const int pB;
  const int pC;
  const int pD;
  const double jitter;
  const int max_attempts;
  int failure_code;
  double objective;
  std::vector<double> grad_A;
  std::vector<double> grad_B;
  std::vector<double> grad_C;
  std::vector<double> grad_D;
  std::vector<double> grad_A_beta;
  std::vector<double> grad_B_beta;
  std::vector<double> grad_C_beta;
  std::vector<double> grad_D_beta;
  // Borrowed, never owned: shared across every split rather than copied.
  const std::vector<int>* subject_order = nullptr;
  // Multiplier on the mode-adjoint correction. 1 reproduces the shipped
  // behaviour, 0 removes the correction entirely, leaving the frozen-mode
  // gradient that matches the objective the item optimiser actually evaluates.
  double adjoint_scale = 1.0;
  // Weight on the Laplace log-determinant, applied to the objective and to both
  // of its gradient contributions, so that value and gradient always describe
  // the same function. 1 is the Laplace objective; 0 drops the Occam term and
  // leaves the joint posterior; values between damp it. This is the honest
  // place to put shrinkage, as against the accidental damping that arises from
  // an under-weighted adjoint alone.
  double logdet_scale = 1.0;

  // Row-effective outputs. This worker already derives the effective
  // discrimination, difficulty, asymptotes and linear predictor for every
  // response; without these it discards them and R rebuilds the same four
  // quantities for the adjoint gradient path, which was a quarter of every
  // objective evaluation. Each response belongs to exactly one subject, so the
  // writes are disjoint and need no synchronisation. Borrowed, never owned.
  double* out_eta = nullptr;
  double* out_c = nullptr;
  double* out_d = nullptr;
  double* out_loadings = nullptr;   // Nobs x K, column major
  int out_nobs = 0;

  BigIRTLaplaceItemBlockWorker(
      const BigIRTObsIndex& obs_by_subj,
      Rcpp::IntegerVector score,
      Rcpp::NumericMatrix row_ability,
      Rcpp::IntegerMatrix A_ref,
      Rcpp::NumericMatrix A_fixed_value,
      Rcpp::IntegerMatrix A_beta_row,
      Rcpp::NumericMatrix A_pred,
      Rcpp::IntegerVector B_ref,
      Rcpp::NumericVector B_fixed_value,
      Rcpp::IntegerVector B_beta_row,
      Rcpp::NumericMatrix B_pred,
      Rcpp::IntegerVector C_ref,
      Rcpp::NumericVector C_fixed_value,
      Rcpp::IntegerVector C_beta_row,
      Rcpp::NumericMatrix C_pred,
      Rcpp::IntegerVector D_ref,
      Rcpp::NumericVector D_fixed_value,
      Rcpp::IntegerVector D_beta_row,
      Rcpp::NumericMatrix D_pred,
      Rcpp::NumericVector invspApars,
      Rcpp::NumericMatrix invspAbeta,
      Rcpp::NumericVector Bpars,
      Rcpp::NumericMatrix Bbeta,
      Rcpp::NumericVector logitCpars,
      Rcpp::NumericMatrix logitCbeta,
      Rcpp::NumericVector logitDpars,
      Rcpp::NumericMatrix logitDbeta,
      Rcpp::NumericVector prior_precision,
      const int K,
      const double jitter,
      const int max_attempts)
    : obs_by_subj(obs_by_subj), score(score), row_ability(row_ability),
      A_ref(A_ref), A_fixed_value(A_fixed_value), A_beta_row(A_beta_row), A_pred(A_pred),
      B_ref(B_ref), B_fixed_value(B_fixed_value), B_beta_row(B_beta_row), B_pred(B_pred),
      C_ref(C_ref), C_fixed_value(C_fixed_value), C_beta_row(C_beta_row), C_pred(C_pred),
      D_ref(D_ref), D_fixed_value(D_fixed_value), D_beta_row(D_beta_row), D_pred(D_pred),
      invspApars(invspApars), invspAbeta(invspAbeta), Bpars(Bpars), Bbeta(Bbeta),
      logitCpars(logitCpars), logitCbeta(logitCbeta), logitDpars(logitDpars), logitDbeta(logitDbeta),
      prior_precision(prior_precision), K(K), nA(invspApars.length()), nB(Bpars.length()),
      nC(logitCpars.length()), nD(logitDpars.length()), nA_beta_row(invspAbeta.nrow()),
      nB_beta_row(Bbeta.nrow()), nC_beta_row(logitCbeta.nrow()), nD_beta_row(logitDbeta.nrow()),
      pA(A_pred.ncol()), pB(B_pred.ncol()), pC(C_pred.ncol()), pD(D_pred.ncol()),
      jitter(jitter), max_attempts(max_attempts), failure_code(0), objective(0.0),
      grad_A(static_cast<std::size_t>(std::max(0, nA)), 0.0),
      grad_B(static_cast<std::size_t>(std::max(0, nB)), 0.0),
      grad_C(static_cast<std::size_t>(std::max(0, nC)), 0.0),
      grad_D(static_cast<std::size_t>(std::max(0, nD)), 0.0),
      grad_A_beta(static_cast<std::size_t>(std::max(0, nA_beta_row * pA)), 0.0),
      grad_B_beta(static_cast<std::size_t>(std::max(0, nB_beta_row * pB)), 0.0),
      grad_C_beta(static_cast<std::size_t>(std::max(0, nC_beta_row * pC)), 0.0),
      grad_D_beta(static_cast<std::size_t>(std::max(0, nD_beta_row * pD)), 0.0) {}

  BigIRTLaplaceItemBlockWorker(const BigIRTLaplaceItemBlockWorker& other, RcppParallel::Split)
    : obs_by_subj(other.obs_by_subj), score(other.score), row_ability(other.row_ability),
      A_ref(other.A_ref), A_fixed_value(other.A_fixed_value), A_beta_row(other.A_beta_row), A_pred(other.A_pred),
      B_ref(other.B_ref), B_fixed_value(other.B_fixed_value), B_beta_row(other.B_beta_row), B_pred(other.B_pred),
      C_ref(other.C_ref), C_fixed_value(other.C_fixed_value), C_beta_row(other.C_beta_row), C_pred(other.C_pred),
      D_ref(other.D_ref), D_fixed_value(other.D_fixed_value), D_beta_row(other.D_beta_row), D_pred(other.D_pred),
      invspApars(other.invspApars), invspAbeta(other.invspAbeta), Bpars(other.Bpars), Bbeta(other.Bbeta),
      logitCpars(other.logitCpars), logitCbeta(other.logitCbeta), logitDpars(other.logitDpars), logitDbeta(other.logitDbeta),
      prior_precision(other.prior_precision), K(other.K), nA(other.nA), nB(other.nB),
      nC(other.nC), nD(other.nD), nA_beta_row(other.nA_beta_row), nB_beta_row(other.nB_beta_row),
      nC_beta_row(other.nC_beta_row), nD_beta_row(other.nD_beta_row), pA(other.pA), pB(other.pB),
      pC(other.pC), pD(other.pD), jitter(other.jitter), max_attempts(other.max_attempts), failure_code(0),
      objective(0.0), grad_A(static_cast<std::size_t>(std::max(0, other.nA)), 0.0),
      grad_B(static_cast<std::size_t>(std::max(0, other.nB)), 0.0),
      grad_C(static_cast<std::size_t>(std::max(0, other.nC)), 0.0),
      grad_D(static_cast<std::size_t>(std::max(0, other.nD)), 0.0),
      grad_A_beta(static_cast<std::size_t>(std::max(0, other.nA_beta_row * other.pA)), 0.0),
      grad_B_beta(static_cast<std::size_t>(std::max(0, other.nB_beta_row * other.pB)), 0.0),
      grad_C_beta(static_cast<std::size_t>(std::max(0, other.nC_beta_row * other.pC)), 0.0),
      grad_D_beta(static_cast<std::size_t>(std::max(0, other.nD_beta_row * other.pD)), 0.0),
      subject_order(other.subject_order), adjoint_scale(other.adjoint_scale),
      logdet_scale(other.logdet_scale), out_eta(other.out_eta), out_c(other.out_c),
      out_d(other.out_d), out_loadings(other.out_loadings), out_nobs(other.out_nobs) {}

  inline double softplus(const double x) const {
    if (x > 0.0) return x + std::log1p(std::exp(-x));
    return std::log1p(std::exp(x));
  }

  inline double add_beta_term(const int row_idx, const BigIRTOwnedMatrix<double>& pred,
    const BigIRTOwnedMatrix<double>& beta, const int beta_row) const {
    if (beta_row <= 0 || pred.ncol() == 0 || beta.nrow() == 0) return 0.0;
    const int brow = beta_row - 1;
    double out = 0.0;
    for (int j = 0; j < pred.ncol(); ++j) out += pred(row_idx, j) * beta(brow, j);
    return out;
  }

  inline void add_beta_grad(std::vector<double>& grad_beta, const int nrow_beta, const int p,
    const BigIRTOwnedMatrix<double>& pred, const int row_idx, const int beta_row, const double value) {
    if (beta_row <= 0 || p == 0 || nrow_beta <= 0) return;
    const int brow = beta_row - 1;
    for (int j = 0; j < p; ++j) {
      grad_beta[static_cast<std::size_t>(brow + nrow_beta * j)] += value * pred(row_idx, j);
    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    const MatrixXd eye = MatrixXd::Identity(K, K);
    for (std::size_t s = begin; s < end; ++s) {
      const std::size_t subj = subject_order
        ? static_cast<std::size_t>((*subject_order)[s]) : s;
      MatrixXd Q = MatrixXd::Zero(K, K);
      for (int r = 0; r < K; ++r) {
        for (int cidx = 0; cidx < K; ++cidx) {
          Q(r, cidx) = prior_precision[r + K * cidx + K * K * static_cast<int>(subj)];
        }
      }
      const MatrixXd Q_prior = Q;

      const BigIRTObsSpan obs_idx = obs_by_subj[subj];
      std::vector<BigIRTRowTerms> row_terms(obs_idx.size());
      std::vector<VectorXd> row_a(obs_idx.size(), VectorXd::Zero(K));
      std::vector<VectorXd> row_a_mult(obs_idx.size(), VectorXd::Zero(K));
      std::vector<double> row_c_mult(obs_idx.size(), 0.0);
      std::vector<double> row_d_mult(obs_idx.size(), 0.0);
      double loglik = 0.0;

      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        const int b_beta_idx = B_ref[obs] > 0 ? (nB_beta_row == 1 ? 1 : B_ref[obs]) : 0;
        const int c_beta_idx = C_ref[obs] > 0 ? (nC_beta_row == 1 ? 1 : C_ref[obs]) : 0;
        const int d_beta_idx = D_ref[obs] > 0 ? (nD_beta_row == 1 ? 1 : D_ref[obs]) : 0;
        double b_row = B_fixed_value[obs];
        if (B_ref[obs] > 0) {
          b_row = Bpars[B_ref[obs] - 1] + add_beta_term(obs, B_pred, Bbeta, b_beta_idx);
        }

        double c_row = C_fixed_value[obs];
        if (C_ref[obs] > 0) {
          const double raw_c = logitCpars[C_ref[obs] - 1] + add_beta_term(obs, C_pred, logitCbeta, c_beta_idx);
          const double sig_c = bigirt_stable_inv_logit(raw_c);
          c_row = 0.5 * sig_c;
          row_c_mult[oi] = 0.5 * sig_c * (1.0 - sig_c);
        }

        double d_row = D_fixed_value[obs];
        if (D_ref[obs] > 0) {
          const double raw_d = logitDpars[D_ref[obs] - 1] + add_beta_term(obs, D_pred, logitDbeta, d_beta_idx);
          const double sig_d = bigirt_stable_inv_logit(raw_d);
          d_row = 0.5 * sig_d + 0.5;
          row_d_mult[oi] = 0.5 * sig_d * (1.0 - sig_d);
        }

        double eta_obs = -b_row;
        for (int k = 0; k < K; ++k) {
          double a_val = A_fixed_value(obs, k);
          if (A_ref(obs, k) > 0) {
            const int a_beta_idx = nA_beta_row == 1 ? 1 : A_ref(obs, k);
            const double raw_a = invspApars[A_ref(obs, k) - 1] + add_beta_term(obs, A_pred, invspAbeta, a_beta_idx);
            const double sig_a = bigirt_stable_inv_logit(raw_a);
            a_val = softplus(raw_a);
            row_a_mult[oi](k) = sig_a;
          }
          row_a[oi](k) = a_val;
          eta_obs += a_val * row_ability(obs, k);
        }

        if (out_eta != nullptr) {
          out_eta[obs] = eta_obs;
          out_c[obs] = c_row;
          out_d[obs] = d_row;
          for (int k = 0; k < K; ++k)
            out_loadings[static_cast<std::size_t>(obs) +
                         static_cast<std::size_t>(out_nobs) * static_cast<std::size_t>(k)] =
              row_a[oi](k);
        }

        BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta_obs, c_row, d_row);
        row_terms[oi] = rt;
        loglik += rt.ll;
        for (int r = 0; r < K; ++r) {
          const double ar = row_a[oi](r);
          if (ar == 0.0) continue;
          for (int cidx = 0; cidx < K; ++cidx) {
            const double ac = row_a[oi](cidx);
            if (ac == 0.0) continue;
            Q(r, cidx) += rt.wl * ar * ac;
          }
        }
      }

      // Same guard as the person kernels, reached from identical inputs so it
      // reaches the identical decision: if the data's own curvature is not
      // positive semidefinite this person falls back to Fisher. `guarded`
      // also switches the adjoint and gradient terms below, so the value and
      // the gradient continue to describe one function.
      bool guarded = false;
      {
        const MatrixXd like = Q - Q_prior;
        if (!bigirt_like_curvature_ok(like)) {
          guarded = true;
          Q = Q_prior + bigirt_psd_clip(like);
        }
      }

      Eigen::LLT<MatrixXd> llt;
      double chol_jitter = jitter;
      bool success = false;
      for (int attempt = 0; attempt < max_attempts; ++attempt) {
        MatrixXd try_prec = Q;
        try_prec.diagonal().array() += chol_jitter;
        llt.compute(try_prec);
        if (llt.info() == Eigen::Success) {
          Q = try_prec;
          success = true;
          break;
        }
        chol_jitter *= 10.0;
      }
      if (!success) { failure_code = 1; return; }

      MatrixXd Sigma = llt.solve(eye);
      MatrixXd L = llt.matrixL();
      double logdet = 0.0;
      for (int k = 0; k < K; ++k) logdet += 2.0 * std::log(std::max(L(k, k), 1e-12));
      objective += loglik + 0.5 * static_cast<double>(K) * std::log(2.0 * M_PI)
        - logdet_scale * 0.5 * logdet;

      // The direct optimizer differentiates a Laplace objective whose modes
      // depend on item parameters.  The original frozen-mode gradient omitted
      // this log-determinant contribution.  An adjoint solve gives the full
      // correction with one KxK solve per person rather than one per item
      // parameter: -1/2 u' d(score_theta)/d(psi), where
      // u = Sigma * d(log|H|)/d(theta).
      VectorXd trace_by_theta = VectorXd::Zero(K);
      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const VectorXd& a = row_a[oi];
        const VectorXd Sa = Sigma * a;
        const double aSa = a.dot(Sa);
        for (int k = 0; k < K; ++k) trace_by_theta(k) +=
          (guarded ? 0.0 : row_terms[oi].dwl_deta) * a(k) * aSa;
      }
      // Scaling here carries logdet_scale into every adjoint term at once,
      // since they all reach the gradient through mode_adjoint or ua.
      const VectorXd mode_adjoint = (adjoint_scale * logdet_scale) * (Sigma * trace_by_theta);

      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        const int b_beta_idx = B_ref[obs] > 0 ? (nB_beta_row == 1 ? 1 : B_ref[obs]) : 0;
        const int c_beta_idx = C_ref[obs] > 0 ? (nC_beta_row == 1 ? 1 : C_ref[obs]) : 0;
        const int d_beta_idx = D_ref[obs] > 0 ? (nD_beta_row == 1 ? 1 : D_ref[obs]) : 0;
        const VectorXd& a = row_a[oi];
        VectorXd z(K);
        for (int k = 0; k < K; ++k) z(k) = row_ability(obs, k);
        const VectorXd Sa = Sigma * a;
        const double aSa = a.dot(Sa);
        const BigIRTRowTerms& rt = row_terms[oi];
        // Where the curvature was clipped, log|H| is locally flat in the
        // item parameters, so its gradient contribution -- including the
        // mode adjoint, which is itself a derivative of log|H| -- is zero.
        const double gw       = guarded ? 0.0 : rt.wl;
        const double gdw_deta = guarded ? 0.0 : rt.dwl_deta;
        const double gdw_dc   = guarded ? 0.0 : rt.dwl_dc;
        const double gdw_dd   = guarded ? 0.0 : rt.dwl_dd;
        const double ua = mode_adjoint.dot(a);

        for (int k = 0; k < K; ++k) {
          const double g_load = rt.grad_eta * z(k) -
            logdet_scale * 0.5 * (gdw_deta * z(k) * aSa + 2.0 * gw * Sa(k)) -
            0.5 * (mode_adjoint(k) * rt.grad_eta + ua * rt.dgrad_eta_deta * z(k));
          const double g_raw = g_load * row_a_mult[oi](k);
          if (A_ref(obs, k) > 0) grad_A[static_cast<std::size_t>(A_ref(obs, k) - 1)] += g_raw;
          const int a_beta_idx = A_ref(obs, k) > 0 ? (nA_beta_row == 1 ? 1 : A_ref(obs, k)) : 0;
          add_beta_grad(grad_A_beta, nA_beta_row, pA, A_pred, obs, a_beta_idx, g_raw);
        }

        const double g_b = -rt.grad_eta + logdet_scale * 0.5 * gdw_deta * aSa +
          0.5 * ua * rt.dgrad_eta_deta;
        if (B_ref[obs] > 0) grad_B[static_cast<std::size_t>(B_ref[obs] - 1)] += g_b;
        add_beta_grad(grad_B_beta, nB_beta_row, pB, B_pred, obs, b_beta_idx, g_b);

        const double g_c_raw = (rt.grad_c - logdet_scale * 0.5 * gdw_dc * aSa -
          0.5 * ua * rt.dgrad_eta_dc) * row_c_mult[oi];
        if (C_ref[obs] > 0) grad_C[static_cast<std::size_t>(C_ref[obs] - 1)] += g_c_raw;
        add_beta_grad(grad_C_beta, nC_beta_row, pC, C_pred, obs, c_beta_idx, g_c_raw);

        const double g_d_raw = (rt.grad_d - logdet_scale * 0.5 * gdw_dd * aSa -
          0.5 * ua * rt.dgrad_eta_dd) * row_d_mult[oi];
        if (D_ref[obs] > 0) grad_D[static_cast<std::size_t>(D_ref[obs] - 1)] += g_d_raw;
        add_beta_grad(grad_D_beta, nD_beta_row, pD, D_pred, obs, d_beta_idx, g_d_raw);
      }
    }
  }

  void join(const BigIRTLaplaceItemBlockWorker& rhs) {
    if (rhs.failure_code != 0) failure_code = rhs.failure_code;
    objective += rhs.objective;
    for (std::size_t i = 0; i < grad_A.size(); ++i) grad_A[i] += rhs.grad_A[i];
    for (std::size_t i = 0; i < grad_B.size(); ++i) grad_B[i] += rhs.grad_B[i];
    for (std::size_t i = 0; i < grad_C.size(); ++i) grad_C[i] += rhs.grad_C[i];
    for (std::size_t i = 0; i < grad_D.size(); ++i) grad_D[i] += rhs.grad_D[i];
    for (std::size_t i = 0; i < grad_A_beta.size(); ++i) grad_A_beta[i] += rhs.grad_A_beta[i];
    for (std::size_t i = 0; i < grad_B_beta.size(); ++i) grad_B_beta[i] += rhs.grad_B_beta[i];
    for (std::size_t i = 0; i < grad_C_beta.size(); ++i) grad_C_beta[i] += rhs.grad_C_beta[i];
    for (std::size_t i = 0; i < grad_D_beta.size(); ++i) grad_D_beta[i] += rhs.grad_D_beta[i];
  }
};

// Persistent data for repeated item objective evaluations.
//
// The item line search evaluates the objective about eleven times per outer
// iteration, and every one of those calls previously re-marshalled all the
// per-response arrays from R: id, score, row_ability, and the ref/fixed/pred
// triples for A, B, C and D. Those are data, not parameters -- they do not
// change while the item block is being optimised -- so the marshalling was
// repeated for nothing, and on a fit with millions of responses it dominated
// the R-level share of the profile.
//
// Holding them in one object behind an external pointer means they are
// converted once per item step. Each evaluation then passes only the item
// parameter vectors, which are small. The Rcpp vector types preserve their
// underlying SEXPs for as long as this object lives, so the data stay valid
// across calls without further copying.
struct BigIRTItemPrepared {
  Rcpp::IntegerVector id, score;
  Rcpp::NumericMatrix row_ability;
  Rcpp::IntegerMatrix A_ref, A_beta_row;
  Rcpp::NumericMatrix A_fixed_value, A_pred;
  Rcpp::IntegerVector B_ref, B_beta_row, C_ref, C_beta_row, D_ref, D_beta_row;
  Rcpp::NumericVector B_fixed_value, C_fixed_value, D_fixed_value;
  Rcpp::NumericMatrix B_pred, C_pred, D_pred;
  Rcpp::NumericVector prior_precision;
  int K = 0, Nsubs = 0, max_attempts = 0;
  double jitter = 0.0;
  BigIRTObsIndex obs_by_subj;
  std::vector<int> subject_order;
};

extern "C" SEXP _bigIRT_laplace_item_eval_cpp_impl(
    SEXP ptrSEXP,
    SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP adjoint_scaleSEXP, SEXP logdet_scaleSEXP, SEXP grain_sizeSEXP) {
  BEGIN_RCPP
  Rcpp::XPtr<BigIRTItemPrepared> ptr(ptrSEXP);
  BigIRTItemPrepared& d = *ptr;

  Rcpp::NumericVector invspApars(invspAparsSEXP);
  Rcpp::NumericMatrix invspAbeta(invspAbetaSEXP);
  Rcpp::NumericVector Bpars(BparsSEXP);
  Rcpp::NumericMatrix Bbeta(BbetaSEXP);
  Rcpp::NumericVector logitCpars(logitCparsSEXP);
  Rcpp::NumericMatrix logitCbeta(logitCbetaSEXP);
  Rcpp::NumericVector logitDpars(logitDparsSEXP);
  Rcpp::NumericMatrix logitDbeta(logitDbetaSEXP);

  BigIRTLaplaceItemBlockWorker worker(
    d.obs_by_subj, d.score, d.row_ability, d.A_ref, d.A_fixed_value, d.A_beta_row, d.A_pred,
    d.B_ref, d.B_fixed_value, d.B_beta_row, d.B_pred,
    d.C_ref, d.C_fixed_value, d.C_beta_row, d.C_pred,
    d.D_ref, d.D_fixed_value, d.D_beta_row, d.D_pred,
    invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
    d.prior_precision, d.K, d.jitter, d.max_attempts
  );
  worker.subject_order = &d.subject_order;
  worker.adjoint_scale = Rcpp::as<double>(adjoint_scaleSEXP);
  worker.logdet_scale = Rcpp::as<double>(logdet_scaleSEXP);

  const std::size_t grain_size = static_cast<std::size_t>(Rcpp::as<int>(grain_sizeSEXP));
  RcppParallel::parallelReduce(static_cast<std::size_t>(0),
    static_cast<std::size_t>(d.Nsubs), worker, grain_size);
  if (worker.failure_code != 0)
    Rcpp::stop("Laplace block objective failed to factor a person precision matrix.");

  return Rcpp::List::create(
    Rcpp::Named("objective") = worker.objective,
    Rcpp::Named("grad_A") = Rcpp::NumericVector(worker.grad_A.begin(), worker.grad_A.end()),
    Rcpp::Named("grad_B") = Rcpp::NumericVector(worker.grad_B.begin(), worker.grad_B.end()),
    Rcpp::Named("grad_C") = Rcpp::NumericVector(worker.grad_C.begin(), worker.grad_C.end()),
    Rcpp::Named("grad_D") = Rcpp::NumericVector(worker.grad_D.begin(), worker.grad_D.end()),
    Rcpp::Named("grad_A_beta") = Rcpp::NumericVector(worker.grad_A_beta.begin(), worker.grad_A_beta.end()),
    Rcpp::Named("grad_B_beta") = Rcpp::NumericVector(worker.grad_B_beta.begin(), worker.grad_B_beta.end()),
    Rcpp::Named("grad_C_beta") = Rcpp::NumericVector(worker.grad_C_beta.begin(), worker.grad_C_beta.end()),
    Rcpp::Named("grad_D_beta") = Rcpp::NumericVector(worker.grad_D_beta.begin(), worker.grad_D_beta.end()));
  END_RCPP
}

extern "C" SEXP _bigIRT_laplace_item_block_objective_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP row_abilitySEXP,
    SEXP A_refSEXP,
    SEXP A_fixed_valueSEXP,
    SEXP A_beta_rowSEXP,
    SEXP A_predSEXP,
    SEXP B_refSEXP,
    SEXP B_fixed_valueSEXP,
    SEXP B_beta_rowSEXP,
    SEXP B_predSEXP,
    SEXP C_refSEXP,
    SEXP C_fixed_valueSEXP,
    SEXP C_beta_rowSEXP,
    SEXP C_predSEXP,
    SEXP D_refSEXP,
    SEXP D_fixed_valueSEXP,
    SEXP D_beta_rowSEXP,
    SEXP D_predSEXP,
    SEXP invspAparsSEXP,
    SEXP invspAbetaSEXP,
    SEXP BparsSEXP,
    SEXP BbetaSEXP,
    SEXP logitCparsSEXP,
    SEXP logitCbetaSEXP,
    SEXP logitDparsSEXP,
    SEXP logitDbetaSEXP,
    SEXP prior_precisionSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP adjoint_scaleSEXP,
    SEXP logdet_scaleSEXP,
    SEXP grain_sizeSEXP,
    SEXP want_row_effectiveSEXP) {
  BEGIN_RCPP

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix row_ability(row_abilitySEXP);
  Rcpp::IntegerMatrix A_ref(A_refSEXP);
  Rcpp::NumericMatrix A_fixed_value(A_fixed_valueSEXP);
  Rcpp::IntegerMatrix A_beta_row(A_beta_rowSEXP);
  Rcpp::NumericMatrix A_pred(A_predSEXP);
  Rcpp::IntegerVector B_ref(B_refSEXP);
  Rcpp::NumericVector B_fixed_value(B_fixed_valueSEXP);
  Rcpp::IntegerVector B_beta_row(B_beta_rowSEXP);
  Rcpp::NumericMatrix B_pred(B_predSEXP);
  Rcpp::IntegerVector C_ref(C_refSEXP);
  Rcpp::NumericVector C_fixed_value(C_fixed_valueSEXP);
  Rcpp::IntegerVector C_beta_row(C_beta_rowSEXP);
  Rcpp::NumericMatrix C_pred(C_predSEXP);
  Rcpp::IntegerVector D_ref(D_refSEXP);
  Rcpp::NumericVector D_fixed_value(D_fixed_valueSEXP);
  Rcpp::IntegerVector D_beta_row(D_beta_rowSEXP);
  Rcpp::NumericMatrix D_pred(D_predSEXP);
  Rcpp::NumericVector invspApars(invspAparsSEXP);
  Rcpp::NumericMatrix invspAbeta(invspAbetaSEXP);
  Rcpp::NumericVector Bpars(BparsSEXP);
  Rcpp::NumericMatrix Bbeta(BbetaSEXP);
  Rcpp::NumericVector logitCpars(logitCparsSEXP);
  Rcpp::NumericMatrix logitCbeta(logitCbetaSEXP);
  Rcpp::NumericVector logitDpars(logitDparsSEXP);
  Rcpp::NumericMatrix logitDbeta(logitDbetaSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const std::size_t grain_size = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));

  const int Nobs = id.size();
  const int K = row_ability.ncol();
  const int Nsubs = Rcpp::as<Rcpp::IntegerVector>(prior_precision.attr("dim"))[2];
  if (row_ability.nrow() != Nobs || A_ref.nrow() != Nobs || A_ref.ncol() != K ||
      A_fixed_value.nrow() != Nobs || A_fixed_value.ncol() != K ||
      A_beta_row.nrow() != Nobs || A_beta_row.ncol() != K) {
    Rcpp::stop("A context matrices must have dimensions Nobs x K.");
  }
  if (B_ref.size() != Nobs || C_ref.size() != Nobs || D_ref.size() != Nobs) {
    Rcpp::stop("B/C/D reference vectors must have length Nobs.");
  }

  BigIRTObsIndex obs_by_subj;
  if (!obs_by_subj.build(id, Nobs, Nsubs)) Rcpp::stop("id must be coded from 1 to Nsubs.");

  BigIRTLaplaceItemBlockWorker worker(
    obs_by_subj, score, row_ability, A_ref, A_fixed_value, A_beta_row, A_pred,
    B_ref, B_fixed_value, B_beta_row, B_pred,
    C_ref, C_fixed_value, C_beta_row, C_pred,
    D_ref, D_fixed_value, D_beta_row, D_pred,
    invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
    prior_precision, K, jitter, max_attempts
  );

  // Walk subjects in locality order rather than input order. This changes only
  // which subjects land in the same chunk, not the sum they contribute to.
  // Off by default; see the note in the prepare path.
  std::vector<int> subject_order;
  if (std::getenv("BIGIRT_LOCALITY_ORDER") != nullptr) {
    subject_order = bigIRT_subject_locality_order(obs_by_subj, B_ref);
  } else {
    subject_order.resize(obs_by_subj.size());
    for (std::size_t z = 0; z < subject_order.size(); ++z)
      subject_order[z] = static_cast<int>(z);
  }
  worker.subject_order = &subject_order;
  worker.adjoint_scale = Rcpp::as<double>(adjoint_scaleSEXP);
  worker.logdet_scale = Rcpp::as<double>(logdet_scaleSEXP);

  // Only the adjoint gradient path wants the per-response effective values,
  // and it is not always present, so they are not materialised unless asked
  // for; four Nobs-sized buffers are not free at eight million responses.
  const bool want_re = Rcpp::as<bool>(want_row_effectiveSEXP);
  Rcpp::NumericVector re_eta(want_re ? Nobs : 0);
  Rcpp::NumericVector re_c(want_re ? Nobs : 0);
  Rcpp::NumericVector re_d(want_re ? Nobs : 0);
  Rcpp::NumericMatrix re_loadings(want_re ? Nobs : 0, want_re ? K : 0);
  if (want_re) {
    worker.out_eta = re_eta.begin();
    worker.out_c = re_c.begin();
    worker.out_d = re_d.begin();
    worker.out_loadings = re_loadings.begin();
    worker.out_nobs = Nobs;
  }

  // The kernel is free of R access and reports failure through failure_code,
  // so the reduction is safe; the failure check stays on the calling thread.
  RcppParallel::parallelReduce(static_cast<std::size_t>(0),
    static_cast<std::size_t>(Nsubs), worker, grain_size);
  if (worker.failure_code != 0)
    Rcpp::stop("Laplace block objective failed to factor a person precision matrix.");

  Rcpp::NumericVector grad_A(worker.grad_A.begin(), worker.grad_A.end());
  Rcpp::NumericVector grad_B(worker.grad_B.begin(), worker.grad_B.end());
  Rcpp::NumericVector grad_C(worker.grad_C.begin(), worker.grad_C.end());
  Rcpp::NumericVector grad_D(worker.grad_D.begin(), worker.grad_D.end());
  Rcpp::NumericMatrix grad_A_beta(invspAbeta.nrow(), invspAbeta.ncol());
  Rcpp::NumericMatrix grad_B_beta(Bbeta.nrow(), Bbeta.ncol());
  Rcpp::NumericMatrix grad_C_beta(logitCbeta.nrow(), logitCbeta.ncol());
  Rcpp::NumericMatrix grad_D_beta(logitDbeta.nrow(), logitDbeta.ncol());
  std::copy(worker.grad_A_beta.begin(), worker.grad_A_beta.end(), grad_A_beta.begin());
  std::copy(worker.grad_B_beta.begin(), worker.grad_B_beta.end(), grad_B_beta.begin());
  std::copy(worker.grad_C_beta.begin(), worker.grad_C_beta.end(), grad_C_beta.begin());
  std::copy(worker.grad_D_beta.begin(), worker.grad_D_beta.end(), grad_D_beta.begin());

  Rcpp::RObject re_out = R_NilValue;
  if (want_re) re_out = Rcpp::List::create(
    Rcpp::Named("eta_row") = re_eta,
    Rcpp::Named("c_row") = re_c,
    Rcpp::Named("d_row") = re_d,
    Rcpp::Named("loadings") = re_loadings);

  return Rcpp::List::create(
    Rcpp::Named("objective") = worker.objective,
    Rcpp::Named("grad_A") = grad_A,
    Rcpp::Named("grad_B") = grad_B,
    Rcpp::Named("grad_C") = grad_C,
    Rcpp::Named("grad_D") = grad_D,
    Rcpp::Named("grad_A_beta") = grad_A_beta,
    Rcpp::Named("grad_B_beta") = grad_B_beta,
    Rcpp::Named("grad_C_beta") = grad_C_beta,
    Rcpp::Named("grad_D_beta") = grad_D_beta,
    Rcpp::Named("row_effective") = re_out
  );
  END_RCPP
}

extern "C" SEXP _bigIRT_laplace_person_step_block_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP theta_initSEXP,
    SEXP person_predSEXP,
    SEXP fixed_abilitySEXP,
    SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP,
    SEXP A_fixed_valueSEXP,
    SEXP A_beta_rowSEXP,
    SEXP A_predSEXP,
    SEXP B_refSEXP,
    SEXP B_fixed_valueSEXP,
    SEXP B_beta_rowSEXP,
    SEXP B_predSEXP,
    SEXP C_refSEXP,
    SEXP C_fixed_valueSEXP,
    SEXP C_beta_rowSEXP,
    SEXP C_predSEXP,
    SEXP D_refSEXP,
    SEXP D_fixed_valueSEXP,
    SEXP D_beta_rowSEXP,
    SEXP D_predSEXP,
    SEXP AbilitybetaSEXP,
    SEXP invspAparsSEXP,
    SEXP invspAbetaSEXP,
    SEXP BparsSEXP,
    SEXP BbetaSEXP,
    SEXP logitCparsSEXP,
    SEXP logitCbetaSEXP,
    SEXP logitDparsSEXP,
    SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP,
    SEXP prior_precisionSEXP,
    SEXP free_maskSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP max_iterSEXP,
    SEXP tolSEXP,
    SEXP keep_covarianceSEXP,
    SEXP grain_sizeSEXP) {
  BEGIN_RCPP

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix theta_init(theta_initSEXP);
  Rcpp::NumericMatrix person_pred(person_predSEXP);
  Rcpp::IntegerMatrix fixed_ability(fixed_abilitySEXP);
  Rcpp::NumericMatrix fixed_ability_value(fixed_ability_valueSEXP);
  Rcpp::IntegerMatrix A_ref(A_refSEXP);
  Rcpp::NumericMatrix A_fixed_value(A_fixed_valueSEXP);
  Rcpp::IntegerMatrix A_beta_row(A_beta_rowSEXP);
  Rcpp::NumericMatrix A_pred(A_predSEXP);
  Rcpp::IntegerVector B_ref(B_refSEXP);
  Rcpp::NumericVector B_fixed_value(B_fixed_valueSEXP);
  Rcpp::IntegerVector B_beta_row(B_beta_rowSEXP);
  Rcpp::NumericMatrix B_pred(B_predSEXP);
  Rcpp::IntegerVector C_ref(C_refSEXP);
  Rcpp::NumericVector C_fixed_value(C_fixed_valueSEXP);
  Rcpp::IntegerVector C_beta_row(C_beta_rowSEXP);
  Rcpp::NumericMatrix C_pred(C_predSEXP);
  Rcpp::IntegerVector D_ref(D_refSEXP);
  Rcpp::NumericVector D_fixed_value(D_fixed_valueSEXP);
  Rcpp::IntegerVector D_beta_row(D_beta_rowSEXP);
  Rcpp::NumericMatrix D_pred(D_predSEXP);
  Rcpp::NumericMatrix Abilitybeta(AbilitybetaSEXP);
  Rcpp::NumericVector invspApars(invspAparsSEXP);
  Rcpp::NumericMatrix invspAbeta(invspAbetaSEXP);
  Rcpp::NumericVector Bpars(BparsSEXP);
  Rcpp::NumericMatrix Bbeta(BbetaSEXP);
  Rcpp::NumericVector logitCpars(logitCparsSEXP);
  Rcpp::NumericMatrix logitCbeta(logitCbetaSEXP);
  Rcpp::NumericVector logitDpars(logitDparsSEXP);
  Rcpp::NumericMatrix logitDbeta(logitDbetaSEXP);
  Rcpp::NumericMatrix prior_mean(prior_meanSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  Rcpp::IntegerMatrix free_mask(free_maskSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const int max_iter = Rcpp::as<int>(max_iterSEXP);
  const double tol = Rcpp::as<double>(tolSEXP);
  const bool keep_covariance = Rcpp::as<bool>(keep_covarianceSEXP);
  const std::size_t grain_size = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));

  const int Nobs = id.size();
  const int Nsubs = theta_init.nrow();
  const int K = theta_init.ncol();
  if (person_pred.nrow() != Nobs || fixed_ability.nrow() != Nobs || fixed_ability.ncol() != K ||
      fixed_ability_value.nrow() != Nobs || fixed_ability_value.ncol() != K ||
      A_ref.nrow() != Nobs || A_ref.ncol() != K || A_fixed_value.nrow() != Nobs || A_fixed_value.ncol() != K ||
      A_beta_row.nrow() != Nobs || A_beta_row.ncol() != K || prior_mean.nrow() != Nsubs || prior_mean.ncol() != K) {
    Rcpp::stop("Unexpected dimensions in Laplace person-step block inputs.");
  }

  BigIRTObsIndex obs_by_subj;
  if (!obs_by_subj.build(id, Nobs, Nsubs)) Rcpp::stop("id must be coded from 1 to Nsubs.");

  Rcpp::NumericMatrix theta_mode(Nsubs, K);
  Rcpp::NumericVector precision(K * K * Nsubs);
  precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision_chol(K * K * Nsubs);
  precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector logdet_precision(Nsubs);
  Rcpp::IntegerVector niter(Nsubs);
  Rcpp::LogicalVector converged(Nsubs);
  Rcpp::IntegerVector status_code(Nsubs);
  Rcpp::NumericVector objective(Nsubs);
  Rcpp::NumericVector covariance;
  if (keep_covariance) {
    covariance = Rcpp::NumericVector(K * K * Nsubs);
    covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }

  struct BigIRTLaplacePersonBlockWorker : public RcppParallel::Worker {
    const BigIRTObsIndex& obs_by_subj;
    BigIRTOwnedVector<int> score;
    BigIRTOwnedMatrix<double> theta_init;
    BigIRTOwnedMatrix<double> person_pred;
    BigIRTOwnedMatrix<int> fixed_ability;
    BigIRTOwnedMatrix<double> fixed_ability_value;
    BigIRTOwnedMatrix<int> A_ref;
    BigIRTOwnedMatrix<double> A_fixed_value;
    BigIRTOwnedMatrix<int> A_beta_row;
    BigIRTOwnedMatrix<double> A_pred;
    BigIRTOwnedVector<int> B_ref;
    BigIRTOwnedVector<double> B_fixed_value;
    BigIRTOwnedVector<int> B_beta_row;
    BigIRTOwnedMatrix<double> B_pred;
    BigIRTOwnedVector<int> C_ref;
    BigIRTOwnedVector<double> C_fixed_value;
    BigIRTOwnedVector<int> C_beta_row;
    BigIRTOwnedMatrix<double> C_pred;
    BigIRTOwnedVector<int> D_ref;
    BigIRTOwnedVector<double> D_fixed_value;
    BigIRTOwnedVector<int> D_beta_row;
    BigIRTOwnedMatrix<double> D_pred;
    BigIRTOwnedMatrix<double> Abilitybeta;
    BigIRTOwnedVector<double> invspApars;
    BigIRTOwnedMatrix<double> invspAbeta;
    BigIRTOwnedVector<double> Bpars;
    BigIRTOwnedMatrix<double> Bbeta;
    BigIRTOwnedVector<double> logitCpars;
    BigIRTOwnedMatrix<double> logitCbeta;
    BigIRTOwnedVector<double> logitDpars;
    BigIRTOwnedMatrix<double> logitDbeta;
    BigIRTOwnedMatrix<double> prior_mean;
    BigIRTOwnedVector<double> prior_precision;
    BigIRTOwnedMatrix<int> free_mask;
    const int K;
    const double jitter;
    const int max_attempts;
    const int max_iter;
    const double tol;
    const bool keep_covariance;
    RcppParallel::RMatrix<double> theta_mode;
    RcppParallel::RVector<double> precision;
    RcppParallel::RVector<double> precision_chol;
    RcppParallel::RVector<double> logdet_precision;
    RcppParallel::RVector<int> niter;
    RcppParallel::RVector<int> converged;
    RcppParallel::RVector<int> status_code;
    RcppParallel::RVector<double> objective;
    RcppParallel::RVector<double> covariance;

    BigIRTLaplacePersonBlockWorker(
      const BigIRTObsIndex& obs_by_subj,
      Rcpp::IntegerVector score,
      Rcpp::NumericMatrix theta_init,
      Rcpp::NumericMatrix person_pred,
      Rcpp::IntegerMatrix fixed_ability,
      Rcpp::NumericMatrix fixed_ability_value,
      Rcpp::IntegerMatrix A_ref,
      Rcpp::NumericMatrix A_fixed_value,
      Rcpp::IntegerMatrix A_beta_row,
      Rcpp::NumericMatrix A_pred,
      Rcpp::IntegerVector B_ref,
      Rcpp::NumericVector B_fixed_value,
      Rcpp::IntegerVector B_beta_row,
      Rcpp::NumericMatrix B_pred,
      Rcpp::IntegerVector C_ref,
      Rcpp::NumericVector C_fixed_value,
      Rcpp::IntegerVector C_beta_row,
      Rcpp::NumericMatrix C_pred,
      Rcpp::IntegerVector D_ref,
      Rcpp::NumericVector D_fixed_value,
      Rcpp::IntegerVector D_beta_row,
      Rcpp::NumericMatrix D_pred,
      Rcpp::NumericMatrix Abilitybeta,
      Rcpp::NumericVector invspApars,
      Rcpp::NumericMatrix invspAbeta,
      Rcpp::NumericVector Bpars,
      Rcpp::NumericMatrix Bbeta,
      Rcpp::NumericVector logitCpars,
      Rcpp::NumericMatrix logitCbeta,
      Rcpp::NumericVector logitDpars,
      Rcpp::NumericMatrix logitDbeta,
      Rcpp::NumericMatrix prior_mean,
      Rcpp::NumericVector prior_precision,
      Rcpp::IntegerMatrix free_mask,
      const int K,
      const double jitter,
      const int max_attempts,
      const int max_iter,
      const double tol,
      const bool keep_covariance,
      Rcpp::NumericMatrix theta_mode,
      Rcpp::NumericVector precision,
      Rcpp::NumericVector precision_chol,
      Rcpp::NumericVector logdet_precision,
      Rcpp::IntegerVector niter,
      Rcpp::LogicalVector converged,
      Rcpp::IntegerVector status_code,
      Rcpp::NumericVector objective,
      Rcpp::NumericVector covariance)
      : obs_by_subj(obs_by_subj), score(score), theta_init(theta_init), person_pred(person_pred),
        fixed_ability(fixed_ability), fixed_ability_value(fixed_ability_value),
        A_ref(A_ref), A_fixed_value(A_fixed_value), A_beta_row(A_beta_row), A_pred(A_pred),
        B_ref(B_ref), B_fixed_value(B_fixed_value), B_beta_row(B_beta_row), B_pred(B_pred),
        C_ref(C_ref), C_fixed_value(C_fixed_value), C_beta_row(C_beta_row), C_pred(C_pred),
        D_ref(D_ref), D_fixed_value(D_fixed_value), D_beta_row(D_beta_row), D_pred(D_pred),
        Abilitybeta(Abilitybeta), invspApars(invspApars), invspAbeta(invspAbeta),
        Bpars(Bpars), Bbeta(Bbeta), logitCpars(logitCpars), logitCbeta(logitCbeta),
        logitDpars(logitDpars), logitDbeta(logitDbeta), prior_mean(prior_mean),
        prior_precision(prior_precision), free_mask(free_mask), K(K), jitter(jitter),
        max_attempts(max_attempts), max_iter(max_iter), tol(tol), keep_covariance(keep_covariance),
        theta_mode(theta_mode), precision(precision), precision_chol(precision_chol),
        logdet_precision(logdet_precision), niter(niter), converged(converged),
        status_code(status_code),
        objective(objective), covariance(covariance) {}

    inline double softplus(const double x) const {
      if (x > 0.0) return x + std::log1p(std::exp(-x));
      return std::log1p(std::exp(x));
    }

    inline double add_beta_term(const int row_idx, const BigIRTOwnedMatrix<double>& pred,
      const BigIRTOwnedMatrix<double>& beta, const int beta_row) const {
      if (beta_row <= 0 || pred.ncol() == 0 || beta.nrow() == 0) return 0.0;
      const int brow = beta_row - 1;
      double out = 0.0;
      for (int j = 0; j < pred.ncol(); ++j) out += pred(row_idx, j) * beta(brow, j);
      return out;
    }

    void operator()(std::size_t begin, std::size_t end) {
      const MatrixXd eye = MatrixXd::Identity(K, K);
      for (std::size_t subj_idx = begin; subj_idx < end; ++subj_idx) {
        const int subj = static_cast<int>(subj_idx);
        VectorXd theta(K);
        VectorXd mu(K);
        MatrixXd prior_prec(K, K);
        for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c)
          prior_prec(r, c) = prior_precision[r + K * c + K * K * subj];
        std::vector<int> active;
        active.reserve(K);
        for (int k = 0; k < K; ++k) {
          theta(k) = theta_init(subj, k);
          mu(k) = prior_mean(subj, k);
          if (free_mask(subj, k) != 0) active.push_back(k);
        }

        const BigIRTObsSpan obs_idx = obs_by_subj[subj];
        std::vector<VectorXd> ability_offset(obs_idx.size(), VectorXd::Zero(K));
        std::vector<VectorXd> loadings(obs_idx.size(), VectorXd::Zero(K));
        std::vector<double> b_row(obs_idx.size(), 0.0);
        std::vector<double> c_row(obs_idx.size(), 0.0);
        std::vector<double> d_row(obs_idx.size(), 1.0);

        for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
          const int obs = obs_idx[oi];
          for (int k = 0; k < K; ++k) {
            double off = 0.0;
            for (int j = 0; j < person_pred.ncol(); ++j) off += person_pred(obs, j) * Abilitybeta(k, j);
            if (fixed_ability(obs, k) != 0) {
              off = 0.0;
            }
            ability_offset[oi](k) = off;

            double a_val = A_fixed_value(obs, k);
            if (A_ref(obs, k) > 0) {
              // The beta-row index is not per response. With a single shared
              // effect the vector holds one element, so indexing it by the
              // response number reads far past its end; with item-specific
              // effects the row is chosen by the item. The item kernel already
              // guards this way -- this worker did not, which is why it
              // segfaulted whenever item covariates were present.
              const int a_beta_idx = (invspAbeta.nrow() == 1) ? 1 : A_ref(obs, k);
              const double raw_a = invspApars[A_ref(obs, k) - 1] + add_beta_term(obs, A_pred, invspAbeta, a_beta_idx);
              a_val = softplus(raw_a);
            }
            loadings[oi](k) = a_val;
          }

          b_row[oi] = B_fixed_value[obs];
          if (B_ref[obs] > 0) {
            const int b_beta_idx = (Bbeta.nrow() == 1) ? 1 : B_ref[obs];
            b_row[oi] = Bpars[B_ref[obs] - 1] + add_beta_term(obs, B_pred, Bbeta, b_beta_idx);
          }

          c_row[oi] = C_fixed_value[obs];
          if (C_ref[obs] > 0) {
            const int c_beta_idx = (logitCbeta.nrow() == 1) ? 1 : C_ref[obs];
            const double raw_c = logitCpars[C_ref[obs] - 1] + add_beta_term(obs, C_pred, logitCbeta, c_beta_idx);
            c_row[oi] = 0.5 * bigirt_stable_inv_logit(raw_c);
          }

          d_row[oi] = D_fixed_value[obs];
          if (D_ref[obs] > 0) {
            const int d_beta_idx = (logitDbeta.nrow() == 1) ? 1 : D_ref[obs];
            const double raw_d = logitDpars[D_ref[obs] - 1] + add_beta_term(obs, D_pred, logitDbeta, d_beta_idx);
            d_row[oi] = 0.5 * bigirt_stable_inv_logit(raw_d) + 0.5;
          }
        }

        // `prec_out` is Fisher (laplace_curv false) or the observed curvature
        // (true, the final Hessian). `obs_out`, when given, receives the
        // observed curvature as well, from the same pass: the Newton loop
        // prefers it wherever it is positive definite (see below).
        auto logpost_and_grad = [&](const VectorXd& theta_eval, VectorXd& grad_out,
          MatrixXd& prec_out, double& lp_out, const bool laplace_curv,
          MatrixXd* obs_out = nullptr) {
          grad_out = -prior_prec * (theta_eval - mu);
          prec_out = prior_prec;
          if (obs_out) *obs_out = prior_prec;
          lp_out = -0.5 * (theta_eval - mu).dot(prior_prec * (theta_eval - mu));
          for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
            const int obs = obs_idx[oi];
            VectorXd ability = theta_eval + ability_offset[oi];
            for (int k = 0; k < K; ++k) if (fixed_ability(obs, k) != 0) ability(k) = fixed_ability_value(obs, k);
            double eta = -b_row[oi];
            for (int k = 0; k < K; ++k) eta += loadings[oi](k) * ability(k);
            BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta, c_row[oi], d_row[oi]);
            lp_out += rt.ll;
            const double w_use = laplace_curv ? rt.wl : rt.w;
            for (int k = 0; k < K; ++k) grad_out(k) += rt.grad_eta * loadings[oi](k);
            for (int r = 0; r < K; ++r) {
              const double ar = loadings[oi](r);
              if (ar == 0.0) continue;
              for (int cc = 0; cc < K; ++cc) {
                const double ac = loadings[oi](cc);
                if (ac == 0.0) continue;
                prec_out(r, cc) += w_use * ar * ac;
                if (obs_out) (*obs_out)(r, cc) += rt.wl * ar * ac;
              }
            }
          }
        };

        VectorXd grad(K);
        MatrixXd prec(K, K);
        MatrixXd prec_obs(K, K);
        double lp = NA_REAL;
        logpost_and_grad(theta, grad, prec, lp, false, &prec_obs);

        bool subj_converged = active.empty();
        // Status codes match the serial kernel: 0 resolved, 1 not converged,
        // 2 no acceptable step, 4 factorisation failed. Without them the R
        // wrapper derived converged from a field this kernel never returned,
        // replacing it with an empty vector; the convergence fraction became
        // NaN and the outer loop then ran to its iteration limit.
        int subj_status = subj_converged ? 0 : 1;
        int used_iter = 0;
        for (int it = 0; it < max_iter && !subj_converged; ++it) {
          used_iter = it + 1;
          double gnorm = 0.0;
          for (size_t ai = 0; ai < active.size(); ++ai) gnorm += grad(active[ai]) * grad(active[ai]);
          gnorm = std::sqrt(gnorm);
          if (gnorm < tol) {
            subj_converged = true;
            break;
          }

          MatrixXd sub_prec(active.size(), active.size());
          MatrixXd sub_obs(active.size(), active.size());
          VectorXd sub_grad(active.size());
          for (size_t r = 0; r < active.size(); ++r) {
            sub_grad(r) = grad(active[r]);
            for (size_t cc = 0; cc < active.size(); ++cc) {
              sub_prec(r, cc) = prec(active[r], active[cc]);
              sub_obs(r, cc) = prec_obs(active[r], active[cc]);
            }
          }

          // Newton on the observed curvature where it is positive definite.
          // Fisher scoring alone converges only linearly for the 3PL and 4PL,
          // where the two differ (5.7 iterations per person on average at
          // tolerance 1e-6); near the mode the observed Hessian is the actual
          // second derivative and converges quadratically. Away from it, where
          // the observed curvature can be indefinite, or if its step finds no
          // ascent, the Fisher step below is taken instead -- the mode solved
          // for is the same either way, only the path to it changes.
          {
            Eigen::LLT<MatrixXd> llt_obs(sub_obs);
            if (llt_obs.info() == Eigen::Success) {
              const VectorXd step_obs = llt_obs.solve(sub_grad);
              double damping_obs = 1.0;
              VectorXd theta_o = theta, grad_o(K);
              MatrixXd prec_o(K, K), obs_o(K, K);
              double lp_o = lp;
              bool ok_obs = false;
              for (int bt = 0; bt < 4 && !ok_obs; ++bt) {
                theta_o = theta;
                for (size_t ai = 0; ai < active.size(); ++ai) theta_o(active[ai]) = theta(active[ai]) + damping_obs * step_obs(ai);
                logpost_and_grad(theta_o, grad_o, prec_o, lp_o, false, &obs_o);
                if (R_finite(lp_o) && lp_o >= lp - 1e-10) ok_obs = true; else damping_obs *= 0.5;
              }
              if (ok_obs) {
                theta = theta_o; grad = grad_o; prec = prec_o; prec_obs = obs_o; lp = lp_o;
                continue;
              }
            }
          }

          Eigen::LLT<MatrixXd> llt;
          double chol_jitter = jitter;
          bool success = false;
          for (int attempt = 0; attempt < max_attempts; ++attempt) {
            MatrixXd try_prec = sub_prec;
            try_prec.diagonal().array() += chol_jitter;
            llt.compute(try_prec);
            if (llt.info() == Eigen::Success) {
              sub_prec = try_prec;
              success = true;
              break;
            }
            chol_jitter *= 10.0;
          }
          // Rcpp::stop here would longjmp out of a worker thread, which is
          // undefined behaviour. Record it instead, as the serial kernel does.
          if (!success) { subj_status = 4; break; }

          VectorXd step = llt.solve(sub_grad);
          double damping = 1.0;
          bool accepted = false;
          VectorXd theta_try = theta;
          VectorXd grad_try(K);
          MatrixXd prec_try(K, K);
          MatrixXd prec_obs_try(K, K);
          double lp_try = lp;
          for (int bt = 0; bt < 12; ++bt) {
            theta_try = theta;
            for (size_t ai = 0; ai < active.size(); ++ai) theta_try(active[ai]) = theta(active[ai]) + damping * step(ai);
            logpost_and_grad(theta_try, grad_try, prec_try, lp_try, false, &prec_obs_try);
            if (R_finite(lp_try) && lp_try >= lp - 1e-10) {
              accepted = true;
              break;
            }
            damping *= 0.5;
          }
          if (!accepted) { subj_status = 2; break; }
          theta = theta_try;
          grad = grad_try;
          prec = prec_try;
          prec_obs = prec_obs_try;
          lp = lp_try;
        }

        logpost_and_grad(theta, grad, prec, lp, true);
        {
          const MatrixXd like = prec - prior_prec;
          if (!bigirt_like_curvature_ok(like))
            prec = prior_prec + bigirt_psd_clip(like);
        }
        objective[subj] = lp;
        if (subj_converged && subj_status == 1) subj_status = 0;
        niter[subj] = used_iter;
        status_code[subj] = subj_status;
        converged[subj] = subj_converged ? 1 : 0;

        for (int k = 0; k < K; ++k) theta_mode(subj, k) = theta(k);

        for (int k = 0; k < K; ++k) {
          if (free_mask(subj, k) == 0) {
            for (int kk = 0; kk < K; ++kk) {
              prec(k, kk) = 0.0;
              prec(kk, k) = 0.0;
            }
            prec(k, k) = 1.0 / jitter;
          }
        }

        Eigen::LLT<MatrixXd> llt_full;
        double chol_jitter = jitter;
        bool success = false;
        for (int attempt = 0; attempt < max_attempts; ++attempt) {
          MatrixXd try_prec = prec;
          try_prec.diagonal().array() += chol_jitter;
          llt_full.compute(try_prec);
          if (llt_full.info() == Eigen::Success) {
            prec = try_prec;
            success = true;
            break;
          }
          chol_jitter *= 10.0;
        }
        if (!success) {
          // As in the serial kernel: retry with the Fisher curvature before
          // giving up, so an indefinite observed Hessian on one person cannot
          // abort the fit.
          logpost_and_grad(theta, grad, prec, lp, false);
          for (int k = 0; k < K; ++k) {
            if (free_mask(subj, k) == 0) {
              for (int kk = 0; kk < K; ++kk) {
                prec(k, kk) = 0.0;
                prec(kk, k) = 0.0;
              }
              prec(k, k) = 1.0 / jitter;
            }
          }
          chol_jitter = jitter;
          for (int attempt = 0; attempt < max_attempts; ++attempt) {
            MatrixXd try_prec = prec;
            try_prec.diagonal().array() += chol_jitter;
            llt_full.compute(try_prec);
            if (llt_full.info() == Eigen::Success) {
              prec = try_prec;
              success = true;
              break;
            }
            chol_jitter *= 10.0;
          }
        }
        if (!success) Rcpp::stop("Cholesky failed while finalizing person precision.");

        MatrixXd L = llt_full.matrixL();
        double logdet = 0.0;
        for (int k = 0; k < K; ++k) logdet += 2.0 * std::log(std::max(L(k, k), 1e-12));
        logdet_precision[subj] = logdet;

        for (int r = 0; r < K; ++r) {
          for (int cc = 0; cc < K; ++cc) {
            precision[r + K * cc + K * K * subj] = prec(r, cc);
            precision_chol[r + K * cc + K * K * subj] = L(r, cc);
          }
        }

        if (keep_covariance) {
          MatrixXd Sigma = llt_full.solve(eye);
          for (int r = 0; r < K; ++r) {
            for (int cc = 0; cc < K; ++cc) covariance[r + K * cc + K * K * subj] = Sigma(r, cc);
          }
        }
      }
    }
  };

  BigIRTLaplacePersonBlockWorker worker(
    obs_by_subj, score, theta_init, person_pred, fixed_ability, fixed_ability_value,
    A_ref, A_fixed_value, A_beta_row, A_pred,
    B_ref, B_fixed_value, B_beta_row, B_pred,
    C_ref, C_fixed_value, C_beta_row, C_pred,
    D_ref, D_fixed_value, D_beta_row, D_pred,
    Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
    prior_mean, prior_precision, free_mask, K, jitter, max_attempts, max_iter, tol,
    keep_covariance, theta_mode, precision, precision_chol, logdet_precision, niter, converged, status_code, objective, covariance
  );
  // Parallel whatever the covariates. This used to run serially whenever
  // item predictors were present: the worker read its inputs through
  // RcppParallel's RVector/RMatrix views, which exited the process on Windows
  // in predictor-bearing fits. The inputs are now owned copies (as in the item
  // kernel), and the outputs are written per subject, disjointly.
  RcppParallel::parallelFor(static_cast<std::size_t>(0), static_cast<std::size_t>(Nsubs), worker, grain_size);

  if (keep_covariance) {
    return Rcpp::List::create(
      Rcpp::Named("theta_mode") = theta_mode,
      Rcpp::Named("precision") = precision,
      Rcpp::Named("precision_chol") = precision_chol,
      Rcpp::Named("logdet_precision") = logdet_precision,
      Rcpp::Named("covariance") = covariance,
      Rcpp::Named("objective") = objective,
      Rcpp::Named("niter") = niter,
      Rcpp::Named("converged") = converged,
      Rcpp::Named("status_code") = status_code
    );
  }

  return Rcpp::List::create(
      Rcpp::Named("theta_mode") = theta_mode,
      Rcpp::Named("precision") = precision,
      Rcpp::Named("precision_chol") = precision_chol,
      Rcpp::Named("logdet_precision") = logdet_precision,
      Rcpp::Named("objective") = objective,
      Rcpp::Named("niter") = niter,
      Rcpp::Named("converged") = converged,
      Rcpp::Named("status_code") = status_code
    );
  END_RCPP
}

extern "C" SEXP _bigIRT_laplace_direct_block_fg_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP theta_initSEXP,
    SEXP person_predSEXP,
    SEXP fixed_abilitySEXP,
    SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP,
    SEXP A_fixed_valueSEXP,
    SEXP A_beta_rowSEXP,
    SEXP A_predSEXP,
    SEXP B_refSEXP,
    SEXP B_fixed_valueSEXP,
    SEXP B_beta_rowSEXP,
    SEXP B_predSEXP,
    SEXP C_refSEXP,
    SEXP C_fixed_valueSEXP,
    SEXP C_beta_rowSEXP,
    SEXP C_predSEXP,
    SEXP D_refSEXP,
    SEXP D_fixed_valueSEXP,
    SEXP D_beta_rowSEXP,
    SEXP D_predSEXP,
    SEXP AbilitybetaSEXP,
    SEXP invspAparsSEXP,
    SEXP invspAbetaSEXP,
    SEXP BparsSEXP,
    SEXP BbetaSEXP,
    SEXP logitCparsSEXP,
    SEXP logitCbetaSEXP,
    SEXP logitDparsSEXP,
    SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP,
    SEXP prior_precisionSEXP,
    SEXP free_maskSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP max_iterSEXP,
    SEXP tolSEXP,
    SEXP keep_covarianceSEXP,
    SEXP grain_sizeSEXP,
    SEXP want_row_effectiveSEXP) {
  using Clock = std::chrono::steady_clock;
  const auto t0_total = Clock::now();

  Rcpp::List posterior = Rcpp::as<Rcpp::List>(_bigIRT_laplace_person_step_block_cpp_impl(
    idSEXP, scoreSEXP, theta_initSEXP, person_predSEXP,
    fixed_abilitySEXP, fixed_ability_valueSEXP,
    A_refSEXP, A_fixed_valueSEXP, A_beta_rowSEXP, A_predSEXP,
    B_refSEXP, B_fixed_valueSEXP, B_beta_rowSEXP, B_predSEXP,
    C_refSEXP, C_fixed_valueSEXP, C_beta_rowSEXP, C_predSEXP,
    D_refSEXP, D_fixed_valueSEXP, D_beta_rowSEXP, D_predSEXP,
    AbilitybetaSEXP, invspAparsSEXP, invspAbetaSEXP, BparsSEXP, BbetaSEXP,
    logitCparsSEXP, logitCbetaSEXP, logitDparsSEXP, logitDbetaSEXP,
    prior_meanSEXP, prior_precisionSEXP, free_maskSEXP,
    jitterSEXP, max_attemptsSEXP, max_iterSEXP, tolSEXP,
    keep_covarianceSEXP, grain_sizeSEXP
  ));
  const auto t1_person = Clock::now();

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mode = posterior["theta_mode"];
  Rcpp::NumericMatrix person_pred(person_predSEXP);
  Rcpp::IntegerMatrix fixed_ability(fixed_abilitySEXP);
  Rcpp::NumericMatrix fixed_ability_value(fixed_ability_valueSEXP);
  Rcpp::NumericMatrix Abilitybeta(AbilitybetaSEXP);
  const int Nobs = id.size();
  const int K = theta_mode.ncol();
  const std::size_t grain_size = static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grain_sizeSEXP)));
  Rcpp::NumericMatrix row_ability(Nobs, K);
  BigIRTLaplaceRowAbilityWorker row_worker(
    id, theta_mode, person_pred, fixed_ability, fixed_ability_value,
    Abilitybeta, row_ability, K
  );
  RcppParallel::parallelFor(static_cast<std::size_t>(0), static_cast<std::size_t>(Nobs), row_worker, grain_size);
  const auto t2_row = Clock::now();

  // The direct path wants the mode-adjoint correction at full weight; the
  // scale exists so the blockwise path can be run with it disabled for
  // comparison.
  Rcpp::NumericVector adjoint_scale_holder = Rcpp::NumericVector::create(1.0);
  Rcpp::NumericVector logdet_scale_holder = Rcpp::NumericVector::create(1.0);
  Rcpp::List item_fg = Rcpp::as<Rcpp::List>(_bigIRT_laplace_item_block_objective_cpp_impl(
    idSEXP, scoreSEXP, row_ability,
    A_refSEXP, A_fixed_valueSEXP, A_beta_rowSEXP, A_predSEXP,
    B_refSEXP, B_fixed_valueSEXP, B_beta_rowSEXP, B_predSEXP,
    C_refSEXP, C_fixed_valueSEXP, C_beta_rowSEXP, C_predSEXP,
    D_refSEXP, D_fixed_valueSEXP, D_beta_rowSEXP, D_predSEXP,
    invspAparsSEXP, invspAbetaSEXP, BparsSEXP, BbetaSEXP,
    logitCparsSEXP, logitCbetaSEXP, logitDparsSEXP, logitDbetaSEXP,
    prior_precisionSEXP, jitterSEXP, max_attemptsSEXP,
    adjoint_scale_holder, logdet_scale_holder, grain_sizeSEXP,
    want_row_effectiveSEXP
  ));
  const auto t3_item = Clock::now();

  Rcpp::NumericVector objective_by_subj = posterior["objective"];
  Rcpp::NumericVector logdet_precision = posterior["logdet_precision"];
  double direct_value = 0.0;
  for (int subj = 0; subj < objective_by_subj.size(); ++subj) {
    direct_value += objective_by_subj[subj] +
      0.5 * static_cast<double>(K) * std::log(2.0 * M_PI) -
      0.5 * logdet_precision[subj];
  }
  const auto t4_total = Clock::now();
  const auto duration_sec = [](const Clock::time_point& a, const Clock::time_point& b) -> double {
    return std::chrono::duration<double>(b - a).count();
  };

  return Rcpp::List::create(
    Rcpp::Named("value") = direct_value,
    Rcpp::Named("posterior") = posterior,
    Rcpp::Named("item_fg") = item_fg,
    Rcpp::Named("timings") = Rcpp::List::create(
      Rcpp::Named("personKernelSec") = duration_sec(t0_total, t1_person),
      Rcpp::Named("rowAssemblySec") = duration_sec(t1_person, t2_row),
      Rcpp::Named("itemKernelSec") = duration_sec(t2_row, t3_item),
      Rcpp::Named("kernelSec") = duration_sec(t0_total, t3_item),
      Rcpp::Named("postKernelSec") = duration_sec(t3_item, t4_total),
      Rcpp::Named("totalSec") = duration_sec(t0_total, t4_total)
    )
  );
}

extern "C" SEXP _bigIRT_laplace_corr_grad_cpp_impl(
    SEXP theta_modeSEXP,
    SEXP covarianceSEXP,
    SEXP precisionSEXP,
    SEXP prior_meanSEXP,
    SEXP ability_sdSEXP,
    SEXP corr_parSEXP,
    SEXP logdet_slopeSEXP,
    SEXP corr_paramizationSEXP,
    SEXP jitterSEXP) {

  Rcpp::NumericMatrix theta_mode(theta_modeSEXP);
  Rcpp::NumericVector covariance(covarianceSEXP);
  Rcpp::NumericVector precision(precisionSEXP);
  Rcpp::NumericVector prior_mean(prior_meanSEXP);
  Rcpp::NumericVector ability_sd(ability_sdSEXP);
  Rcpp::NumericVector corr_par(corr_parSEXP);
  Rcpp::NumericMatrix logdet_slope(logdet_slopeSEXP);
  const int corr_paramization = Rcpp::as<int>(corr_paramizationSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);

  const int Nsubs = theta_mode.nrow();
  const int K = theta_mode.ncol();
  if (K <= 1 || corr_par.size() == 0) return Rcpp::NumericVector(0);

  Rcpp::IntegerVector cov_dim = covariance.attr("dim");
  Rcpp::IntegerVector prec_dim = precision.attr("dim");
  if (cov_dim.size() != 3 || cov_dim[0] != K || cov_dim[1] != K || cov_dim[2] != Nsubs) {
    Rcpp::stop("covariance must have dimensions K x K x Nsubs.");
  }
  if (prec_dim.size() != 3 || prec_dim[0] != K || prec_dim[1] != K || prec_dim[2] != Nsubs) {
    Rcpp::stop("precision must have dimensions K x K x Nsubs.");
  }

  MatrixXd Q_current = bigirt_prior_precision_from_corr(corr_par, ability_sd, K, jitter, corr_paramization);
  MatrixXd Qinv = Q_current.inverse();
  MatrixXd GQ = 0.5 * static_cast<double>(Nsubs) * Qinv;
  for (int subj = 0; subj < Nsubs; ++subj) {
    VectorXd x(K);
    for (int k = 0; k < K; ++k) x(k) = theta_mode(subj, k) - prior_mean[k];
    MatrixXd Sigma_j = bigirt_matrix_from_array(covariance, K, subj);
    GQ -= 0.5 * (x * x.transpose());
    GQ -= 0.5 * Sigma_j;
    // Adjoint. The mode moves with the correlation parameters, and the log
    // determinant is not part of the mode condition, so dL/dtheta_i is -g_i/2
    // rather than zero. Differentiating the mode condition gives
    // dtheta_i/drho = -H_i^-1 (dQ/drho) r_i, and the resulting contribution
    // g_i' Sigma_i dQ r_i is linear in dQ, so it enters here as
    // (1/2) r_i g_i' Sigma_i and rides the existing chain rule. Without it the
    // gradient ran about four per cent short against finite differences.
    if (logdet_slope.nrow() == Nsubs && logdet_slope.ncol() == K) {
      VectorXd gvec(K);
      for (int k = 0; k < K; ++k) gvec(k) = logdet_slope(subj, k);
      GQ += 0.5 * (x * (Sigma_j * gvec).transpose());
    }
  }
  // Symmetrise through a temporary. Eigen aliases on A = A.transpose(): the
  // assignment overwrites entries that the transpose still has to read, so the
  // result is not the symmetric part. It was harmless while every contribution
  // to GQ was symmetric already -- Qinv, x x' and Sigma_j all are, and for a
  // symmetric matrix the operation is the identity whatever order it runs in.
  // It only became visible once the asymmetric adjoint term above joined them.
  {
    MatrixXd GQ_sym = 0.5 * (GQ + GQ.transpose());
    GQ = GQ_sym;
  }

  Rcpp::NumericVector grad(corr_par.size());
  if (corr_paramization == 1) {
    MatrixXd L = MatrixXd::Zero(K, K);
    L(0, 0) = 1.0;
    int pidx = 0;
    std::vector<VectorXd> raw_by_row(static_cast<std::size_t>(K));
    for (int i = 1; i < K; ++i) {
      raw_by_row[static_cast<std::size_t>(i)] = VectorXd(i);
      for (int j = 0; j < i; ++j) raw_by_row[static_cast<std::size_t>(i)](j) = std::tanh(corr_par[pidx++]);
      double prod_term = 1.0;
      for (int j = 0; j < i; ++j) {
        if (j > 0) prod_term *= std::sqrt(std::max(1.0 - std::pow(raw_by_row[static_cast<std::size_t>(i)](j - 1), 2.0), 1e-12));
        L(i, j) = raw_by_row[static_cast<std::size_t>(i)](j) * prod_term;
      }
      double diag_term = 1.0;
      for (int j = 0; j < i; ++j) diag_term *= std::sqrt(std::max(1.0 - std::pow(raw_by_row[static_cast<std::size_t>(i)](j), 2.0), 1e-12));
      L(i, i) = diag_term;
    }
    pidx = 0;
    for (int i = 1; i < K; ++i) {
      const VectorXd& raw = raw_by_row[static_cast<std::size_t>(i)];
      for (int t = 0; t < i; ++t) {
        MatrixXd dL = MatrixXd::Zero(K, K);
        const double raw_t = raw(t);
        const double d_raw_d_par = 1.0 - raw_t * raw_t;
        double prefix_t = 1.0;
        for (int m = 0; m < t; ++m) prefix_t *= std::sqrt(std::max(1.0 - raw(m) * raw(m), 1e-12));
        for (int k = t; k < i; ++k) {
          if (k == t) {
            dL(i, k) = prefix_t;
          } else {
            dL(i, k) = L(i, k) * (-raw_t / std::max(1.0 - raw_t * raw_t, 1e-12));
          }
        }
        dL(i, i) = L(i, i) * (-raw_t / std::max(1.0 - raw_t * raw_t, 1e-12));
        dL *= d_raw_d_par;
        MatrixXd dR = dL * L.transpose() + L * dL.transpose();
        MatrixXd dSigma = MatrixXd::Zero(K, K);
        for (int ii = 0; ii < K; ++ii) {
          for (int jj = 0; jj < K; ++jj) dSigma(ii, jj) = ability_sd[ii] * dR(ii, jj) * ability_sd[jj];
        }
        MatrixXd dQ = -Q_current * dSigma * Q_current;
        grad[pidx++] = (GQ.array() * dQ.array()).sum();
      }
    }
  } else {
    MatrixXd L = MatrixXd::Identity(K, K);
    int pidx = 0;
    for (int i = 1; i < K; ++i) {
      for (int j = 0; j < i; ++j) {
        L(i, j) = corr_par[pidx++];
      }
    }
    MatrixXd S = L * L.transpose();
    VectorXd d = S.diagonal().array().max(1e-12).sqrt();
    MatrixXd R = S;
    for (int i = 0; i < K; ++i) {
      for (int j = 0; j < K; ++j) {
        R(i, j) /= (d(i) * d(j));
      }
    }
    pidx = 0;
    for (int a = 1; a < K; ++a) {
      for (int b = 0; b < a; ++b) {
        MatrixXd dS = MatrixXd::Zero(K, K);
        for (int j = 0; j < K; ++j) {
          dS(a, j) += L(j, b);
          dS(j, a) += L(j, b);
        }
        MatrixXd dR = MatrixXd::Zero(K, K);
        const double alpha_a = L(a, b) / std::max(S(a, a), 1e-12);
        for (int i = 0; i < K; ++i) {
          for (int j = 0; j < K; ++j) {
            double term = dS(i, j) / (d(i) * d(j));
            double corr_scale = 0.0;
            if (i == a) corr_scale += alpha_a;
            if (j == a) corr_scale += alpha_a;
            term -= R(i, j) * corr_scale;
            dR(i, j) = term;
          }
        }
        MatrixXd dSigma = MatrixXd::Zero(K, K);
        for (int i = 0; i < K; ++i) {
          for (int j = 0; j < K; ++j) {
            dSigma(i, j) = ability_sd[i] * dR(i, j) * ability_sd[j];
          }
        }
        MatrixXd dQ = -Q_current * dSigma * Q_current;
        grad[pidx++] = (GQ.array() * dQ.array()).sum();
      }
    }
  }
  return grad;
}
