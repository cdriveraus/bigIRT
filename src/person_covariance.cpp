#include <RcppEigen.h>
using Eigen::MatrixXd;

inline double stable_inv_logit(const double x) {
  if (x >= 0.0) {
    const double z = std::exp(-x);
    return 1.0 / (1.0 + z);
  } else {
    const double z = std::exp(x);
    return z / (1.0 + z);
  }
}

extern "C" SEXP _bigIRT_person_covariance_cpp_impl(
    SEXP idSEXP,
    SEXP theta_meanSEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP row_ptrSEXP,
    SEXP load_idxSEXP,
    SEXP load_valSEXP,
    SEXP prior_precisionSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP return_precisionSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mean(theta_meanSEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::IntegerVector row_ptr(row_ptrSEXP);
  Rcpp::IntegerVector load_idx(load_idxSEXP);
  Rcpp::NumericVector load_val(load_valSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const bool return_precision = Rcpp::as<bool>(return_precisionSEXP);

  const int Nobs = id.size();
  const int Nsubs = theta_mean.nrow();
  const int K = theta_mean.ncol();
  Rcpp::IntegerVector prior_dim = prior_precision.attr("dim");
  if (prior_dim.size() != 3 || prior_dim[0] != K || prior_dim[1] != K || prior_dim[2] != Nsubs) {
    Rcpp::stop("prior_precision must have dimensions K x K x Nsubs.");
  }

  std::vector< MatrixXd > precision_store(Nsubs);
  for (int s = 0; s < Nsubs; ++s) {
    precision_store[s] = MatrixXd::Zero(K, K);
    for (int r = 0; r < K; ++r) {
      for (int cidx = 0; cidx < K; ++cidx) {
        precision_store[s](r, cidx) = prior_precision[r + K * cidx + K * K * s];
      }
    }
  }

  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) {
      Rcpp::stop("id must be coded from 1 to Nsubs.");
    }

    const int start = row_ptr[obs] - 1;
    const int end = row_ptr[obs + 1] - 1;

    double eta = -b[obs];
    for (int ptr = start; ptr < end; ++ptr) {
      const int k = load_idx[ptr] - 1;
      eta += load_val[ptr] * theta_mean(subj, k);
    }

    const double g = stable_inv_logit(eta);
    const double p = c[obs] + (d[obs] - c[obs]) * g;
    const double dp = (d[obs] - c[obs]) * g * (1.0 - g);
    const double denom = std::max(p * (1.0 - p), 1e-12);
    const double w = (dp * dp) / denom;

    if (w <= 0.0 || !R_finite(w)) continue;

    for (int ip = start; ip < end; ++ip) {
      const int ki = load_idx[ip] - 1;
      const double avi = load_val[ip];
      for (int jp = start; jp < end; ++jp) {
        const int kj = load_idx[jp] - 1;
        precision_store[subj](ki, kj) += w * avi * load_val[jp];
      }
    }
  }

  Rcpp::NumericVector covariance(K * K * Nsubs);
  covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision;
  Rcpp::NumericVector precision_chol;
  if (return_precision) {
    precision = Rcpp::NumericVector(K * K * Nsubs);
    precision_chol = Rcpp::NumericVector(K * K * Nsubs);
    precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
    precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }
  Rcpp::NumericVector chol_jitter_used(Nsubs);

  const MatrixXd eye = MatrixXd::Identity(K, K);
  for (int s = 0; s < Nsubs; ++s) {
    MatrixXd Q = precision_store[s];
    Eigen::LLT<MatrixXd> llt;
    double chol_jitter = jitter;
    bool success = false;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
      MatrixXd Q_try = Q;
      Q_try.diagonal().array() += chol_jitter;
      llt.compute(Q_try);
      if (llt.info() == Eigen::Success) {
        Q = Q_try;
        success = true;
        break;
      }
      chol_jitter *= 10.0;
    }

    if (!success) {
      Rcpp::stop("Cholesky failed while building person covariance matrices.");
    }

    MatrixXd Sigma = llt.solve(eye);
    MatrixXd L;
    if (return_precision) {
      L = llt.matrixL();
    }
    chol_jitter_used[s] = chol_jitter;

    for (int r = 0; r < K; ++r) {
      for (int cidx = 0; cidx < K; ++cidx) {
        covariance[r + K * cidx + K * K * s] = Sigma(r, cidx);
        if (return_precision) {
          precision[r + K * cidx + K * K * s] = Q(r, cidx);
          precision_chol[r + K * cidx + K * K * s] = L(r, cidx);
        }
      }
    }
  }

  if (return_precision) {
    return Rcpp::List::create(
      Rcpp::Named("covariance") = covariance,
      Rcpp::Named("precision") = precision,
      Rcpp::Named("precision_chol") = precision_chol,
      Rcpp::Named("chol_jitter_used") = chol_jitter_used
    );
  }
  return Rcpp::List::create(
    Rcpp::Named("covariance") = covariance,
    Rcpp::Named("chol_jitter_used") = chol_jitter_used
  );
}
