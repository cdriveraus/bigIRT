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

extern "C" SEXP _bigIRT_person_sigma_points_cpp_impl(
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
    SEXP sigma_scaleSEXP) {

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
  const double sigma_scale = Rcpp::as<double>(sigma_scaleSEXP);

  const int Nobs = id.size();
  const int Nsubs = theta_mean.nrow();
  const int K = theta_mean.ncol();
  const int Nsamp = 2 * K + 1;
  const double lambda = 1.0;
  const double spread = sigma_scale * std::sqrt(K + lambda);
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

  Rcpp::NumericVector ability_samples(Nsubs * K * Nsamp);
  ability_samples.attr("dim") = Rcpp::IntegerVector::create(Nsubs, K, Nsamp);
  Rcpp::NumericVector posterior_sd(Nsubs * K);
  posterior_sd.attr("dim") = Rcpp::IntegerVector::create(Nsubs, K);
  Rcpp::NumericVector cov_mean(K * K);
  cov_mean.attr("dim") = Rcpp::IntegerVector::create(K, K);
  Rcpp::NumericVector chol_jitter_used(Nsubs);
  Rcpp::NumericVector weights(Nsamp);
  weights[0] = lambda / (K + lambda);
  for (int si = 1; si < Nsamp; ++si) {
    weights[si] = 1.0 / (2.0 * (K + lambda));
  }

  const MatrixXd eye = MatrixXd::Identity(K, K);
  MatrixXd cov_sum = MatrixXd::Zero(K, K);
  for (int s = 0; s < Nsubs; ++s) {
    MatrixXd Q = precision_store[s];
    Eigen::LLT<MatrixXd> lltQ;
    double chol_jitter = jitter;
    bool success = false;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
      MatrixXd Q_try = Q;
      Q_try.diagonal().array() += chol_jitter;
      lltQ.compute(Q_try);
      if (lltQ.info() == Eigen::Success) {
        Q = Q_try;
        success = true;
        break;
      }
      chol_jitter *= 10.0;
    }
    if (!success) {
      Rcpp::stop("Cholesky failed while building person covariance matrices.");
    }
    chol_jitter_used[s] = chol_jitter;

    MatrixXd Sigma = lltQ.solve(eye);
    cov_sum += Sigma;
    for (int k = 0; k < K; ++k) {
      posterior_sd[s + Nsubs * k] = std::sqrt(std::max(Sigma(k, k), 0.0));
      ability_samples[s + Nsubs * k + Nsubs * K * 0] = theta_mean(s, k);
    }

    Eigen::LLT<MatrixXd> lltSigma;
    MatrixXd SigmaTry = Sigma;
    SigmaTry = (SigmaTry + SigmaTry.transpose()) * 0.5;
    SigmaTry.diagonal().array() += jitter;
    lltSigma.compute(SigmaTry);
    MatrixXd U;
    if (lltSigma.info() == Eigen::Success) {
      U = lltSigma.matrixU();
    } else {
      U = MatrixXd::Zero(K, K);
      for (int k = 0; k < K; ++k) {
        U(k, k) = std::sqrt(std::max(Sigma(k, k), jitter));
      }
    }

    for (int kcol = 0; kcol < K; ++kcol) {
      for (int r = 0; r < K; ++r) {
        const double offset = spread * U(r, kcol);
        const double mu = theta_mean(s, r);
        ability_samples[s + Nsubs * r + Nsubs * K * (1 + kcol)] = mu + offset;
        ability_samples[s + Nsubs * r + Nsubs * K * (1 + K + kcol)] = mu - offset;
      }
    }
  }

  cov_sum /= static_cast<double>(Nsubs);
  for (int r = 0; r < K; ++r) {
    for (int cidx = 0; cidx < K; ++cidx) {
      cov_mean[r + K * cidx] = cov_sum(r, cidx);
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("ability_samples") = ability_samples,
    Rcpp::Named("weights") = weights,
    Rcpp::Named("posterior_sd") = posterior_sd,
    Rcpp::Named("cov_mean") = cov_mean,
    Rcpp::Named("chol_jitter_used") = chol_jitter_used
  );
}

extern "C" SEXP _bigIRT_person_sigma_points_cpp_dense_impl(
    SEXP idSEXP,
    SEXP theta_meanSEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP loadingsSEXP,
    SEXP prior_precisionSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP sigma_scaleSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mean(theta_meanSEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::NumericMatrix loadings(loadingsSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const double sigma_scale = Rcpp::as<double>(sigma_scaleSEXP);

  const int Nobs = id.size();
  const int Nsubs = theta_mean.nrow();
  const int K = theta_mean.ncol();
  if (loadings.nrow() != Nobs || loadings.ncol() != K) {
    Rcpp::stop("loadings must have dimensions Nobs x K.");
  }
  const int Nsamp = 2 * K + 1;
  const double lambda = 1.0;
  const double spread = sigma_scale * std::sqrt(K + lambda);
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

    double eta = -b[obs];
    for (int k = 0; k < K; ++k) {
      eta += loadings(obs, k) * theta_mean(subj, k);
    }

    const double g = stable_inv_logit(eta);
    const double p = c[obs] + (d[obs] - c[obs]) * g;
    const double dp = (d[obs] - c[obs]) * g * (1.0 - g);
    const double denom = std::max(p * (1.0 - p), 1e-12);
    const double w = (dp * dp) / denom;
    if (w <= 0.0 || !R_finite(w)) continue;

    for (int ki = 0; ki < K; ++ki) {
      const double avi = loadings(obs, ki);
      if (avi == 0.0) continue;
      for (int kj = 0; kj < K; ++kj) {
        const double avj = loadings(obs, kj);
        if (avj == 0.0) continue;
        precision_store[subj](ki, kj) += w * avi * avj;
      }
    }
  }

  Rcpp::NumericVector ability_samples(Nsubs * K * Nsamp);
  ability_samples.attr("dim") = Rcpp::IntegerVector::create(Nsubs, K, Nsamp);
  Rcpp::NumericVector posterior_sd(Nsubs * K);
  posterior_sd.attr("dim") = Rcpp::IntegerVector::create(Nsubs, K);
  Rcpp::NumericVector cov_mean(K * K);
  cov_mean.attr("dim") = Rcpp::IntegerVector::create(K, K);
  Rcpp::NumericVector chol_jitter_used(Nsubs);
  Rcpp::NumericVector weights(Nsamp);
  weights[0] = lambda / (K + lambda);
  for (int si = 1; si < Nsamp; ++si) {
    weights[si] = 1.0 / (2.0 * (K + lambda));
  }

  const MatrixXd eye = MatrixXd::Identity(K, K);
  MatrixXd cov_sum = MatrixXd::Zero(K, K);
  for (int s = 0; s < Nsubs; ++s) {
    MatrixXd Q = precision_store[s];
    Eigen::LLT<MatrixXd> lltQ;
    double chol_jitter = jitter;
    bool success = false;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
      MatrixXd Q_try = Q;
      Q_try.diagonal().array() += chol_jitter;
      lltQ.compute(Q_try);
      if (lltQ.info() == Eigen::Success) {
        Q = Q_try;
        success = true;
        break;
      }
      chol_jitter *= 10.0;
    }
    if (!success) {
      Rcpp::stop("Cholesky failed while building person covariance matrices.");
    }
    chol_jitter_used[s] = chol_jitter;

    MatrixXd Sigma = lltQ.solve(eye);
    cov_sum += Sigma;
    for (int k = 0; k < K; ++k) {
      posterior_sd[s + Nsubs * k] = std::sqrt(std::max(Sigma(k, k), 0.0));
      ability_samples[s + Nsubs * k + Nsubs * K * 0] = theta_mean(s, k);
    }

    Eigen::LLT<MatrixXd> lltSigma;
    MatrixXd SigmaTry = Sigma;
    SigmaTry = (SigmaTry + SigmaTry.transpose()) * 0.5;
    SigmaTry.diagonal().array() += jitter;
    lltSigma.compute(SigmaTry);
    MatrixXd U;
    if (lltSigma.info() == Eigen::Success) {
      U = lltSigma.matrixU();
    } else {
      U = MatrixXd::Zero(K, K);
      for (int k = 0; k < K; ++k) {
        U(k, k) = std::sqrt(std::max(Sigma(k, k), jitter));
      }
    }

    for (int kcol = 0; kcol < K; ++kcol) {
      for (int r = 0; r < K; ++r) {
        const double offset = spread * U(r, kcol);
        const double mu = theta_mean(s, r);
        ability_samples[s + Nsubs * r + Nsubs * K * (1 + kcol)] = mu + offset;
        ability_samples[s + Nsubs * r + Nsubs * K * (1 + K + kcol)] = mu - offset;
      }
    }
  }

  cov_sum /= static_cast<double>(Nsubs);
  for (int r = 0; r < K; ++r) {
    for (int cidx = 0; cidx < K; ++cidx) {
      cov_mean[r + K * cidx] = cov_sum(r, cidx);
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("ability_samples") = ability_samples,
    Rcpp::Named("weights") = weights,
    Rcpp::Named("posterior_sd") = posterior_sd,
    Rcpp::Named("cov_mean") = cov_mean,
    Rcpp::Named("chol_jitter_used") = chol_jitter_used
  );
}
