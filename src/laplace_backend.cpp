#include <RcppEigen.h>
#include <RcppParallel.h>
#include <chrono>
using Eigen::MatrixXd;
using Eigen::VectorXd;

extern "C" SEXP _bigIRT_laplace_person_step_block_cpp_impl(
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP _bigIRT_laplace_item_block_objective_cpp_impl(
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
    SEXP);

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
  double w;
  double dw_deta;
  double dw_dc;
  double dw_dd;
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
  out.dw_deta = (2.0 * out.s * ds_deta * out.r - out.s * out.s * dr_deta) / (out.r * out.r);

  const double dp_dc = 1.0 - out.g;
  const double ds_dc = -out.q;
  const double dr_dc = dp_dc * (1.0 - 2.0 * out.p);
  out.dw_dc = (2.0 * out.s * ds_dc * out.r - out.s * out.s * dr_dc) / (out.r * out.r);

  const double dp_dd = out.g;
  const double ds_dd = out.q;
  const double dr_dd = dp_dd * (1.0 - 2.0 * out.p);
  out.dw_dd = (2.0 * out.s * ds_dd * out.r - out.s * out.s * dr_dd) / (out.r * out.r);

  out.grad_eta = out.coeff * out.s;
  out.grad_c = out.coeff * (1.0 - out.g);
  out.grad_d = out.coeff * out.g;
  const double dcoeff_deta = (-out.s * out.r - (y - out.p) * dr_deta) / (out.r * out.r);
  const double dcoeff_dc = (-(1.0 - out.g) * out.r - (y - out.p) * dr_dc) / (out.r * out.r);
  const double dcoeff_dd = (-out.g * out.r - (y - out.p) * dr_dd) / (out.r * out.r);
  out.dgrad_eta_deta = dcoeff_deta * out.s + out.coeff * ds_deta;
  out.dgrad_eta_dc = dcoeff_dc * out.s + out.coeff * ds_dc;
  out.dgrad_eta_dd = dcoeff_dd * out.s + out.coeff * ds_dd;
  return out;
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

extern "C" SEXP _bigIRT_laplace_person_step_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP theta_initSEXP,
    SEXP ability_offsetSEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP loadingsSEXP,
    SEXP prior_meanSEXP,
    SEXP prior_precisionSEXP,
    SEXP free_maskSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP max_iterSEXP,
    SEXP tolSEXP,
    SEXP keep_covarianceSEXP,
    SEXP grain_sizeSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix theta_init(theta_initSEXP);
  Rcpp::NumericMatrix ability_offset(ability_offsetSEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::NumericMatrix loadings(loadingsSEXP);
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
  if (ability_offset.nrow() != Nobs || ability_offset.ncol() != K) {
    Rcpp::stop("ability_offset must have dimensions Nobs x K.");
  }
  if (loadings.nrow() != Nobs || loadings.ncol() != K) {
    Rcpp::stop("loadings must have dimensions Nobs x K.");
  }
  if (prior_mean.nrow() != Nsubs || prior_mean.ncol() != K) {
    Rcpp::stop("prior_mean must have dimensions Nsubs x K.");
  }

  Rcpp::IntegerVector prior_dim = prior_precision.attr("dim");
  if (prior_dim.size() != 3 || prior_dim[0] != K || prior_dim[1] != K || prior_dim[2] != Nsubs) {
    Rcpp::stop("prior_precision must have dimensions K x K x Nsubs.");
  }

  std::vector< std::vector<int> > obs_by_subj(Nsubs);
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    obs_by_subj[subj].push_back(obs);
  }

  Rcpp::NumericMatrix theta_mode(Nsubs, K);
  Rcpp::NumericVector precision(K * K * Nsubs);
  precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision_chol(K * K * Nsubs);
  precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector logdet_precision(Nsubs);
  Rcpp::IntegerVector niter(Nsubs);
  Rcpp::LogicalVector converged(Nsubs);
  Rcpp::NumericVector objective(Nsubs);
  Rcpp::NumericVector covariance;
  if (keep_covariance) {
    covariance = Rcpp::NumericVector(K * K * Nsubs);
    covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }

  struct BigIRTLaplacePersonWorker : public RcppParallel::Worker {
    const std::vector< std::vector<int> >& obs_by_subj;
    const Rcpp::IntegerVector& score;
    const Rcpp::NumericMatrix& theta_init;
    const Rcpp::NumericMatrix& ability_offset;
    const Rcpp::NumericVector& b;
    const Rcpp::NumericVector& c;
    const Rcpp::NumericVector& d;
    const Rcpp::NumericMatrix& loadings;
    const Rcpp::NumericMatrix& prior_mean;
    const Rcpp::NumericVector& prior_precision;
    const Rcpp::IntegerMatrix& free_mask;
    const double jitter;
    const int max_attempts;
    const int max_iter;
    const double tol;
    const bool keep_covariance;
    const int K;
    Rcpp::NumericMatrix& theta_mode;
    Rcpp::NumericVector& precision;
    Rcpp::NumericVector& precision_chol;
    Rcpp::NumericVector& logdet_precision;
    Rcpp::IntegerVector& niter;
    Rcpp::LogicalVector& converged;
    Rcpp::NumericVector& objective;
    Rcpp::NumericVector& covariance;

    BigIRTLaplacePersonWorker(
      const std::vector< std::vector<int> >& obs_by_subj,
      const Rcpp::IntegerVector& score,
      const Rcpp::NumericMatrix& theta_init,
      const Rcpp::NumericMatrix& ability_offset,
      const Rcpp::NumericVector& b,
      const Rcpp::NumericVector& c,
      const Rcpp::NumericVector& d,
      const Rcpp::NumericMatrix& loadings,
      const Rcpp::NumericMatrix& prior_mean,
      const Rcpp::NumericVector& prior_precision,
      const Rcpp::IntegerMatrix& free_mask,
      const double jitter,
      const int max_attempts,
      const int max_iter,
      const double tol,
      const bool keep_covariance,
      const int K,
      Rcpp::NumericMatrix& theta_mode,
      Rcpp::NumericVector& precision,
      Rcpp::NumericVector& precision_chol,
      Rcpp::NumericVector& logdet_precision,
      Rcpp::IntegerVector& niter,
      Rcpp::LogicalVector& converged,
      Rcpp::NumericVector& objective,
      Rcpp::NumericVector& covariance)
      : obs_by_subj(obs_by_subj), score(score), theta_init(theta_init),
        ability_offset(ability_offset), b(b), c(c), d(d), loadings(loadings),
        prior_mean(prior_mean), prior_precision(prior_precision), free_mask(free_mask),
        jitter(jitter), max_attempts(max_attempts), max_iter(max_iter), tol(tol),
        keep_covariance(keep_covariance), K(K), theta_mode(theta_mode),
        precision(precision), precision_chol(precision_chol),
        logdet_precision(logdet_precision), niter(niter), converged(converged),
        objective(objective), covariance(covariance) {}

    void operator()(std::size_t begin, std::size_t end) {
      const MatrixXd eye = MatrixXd::Identity(K, K);
      for (std::size_t subj_idx = begin; subj_idx < end; ++subj_idx) {
        const int subj = static_cast<int>(subj_idx);
        VectorXd theta(K);
        VectorXd mu(K);
        MatrixXd prior_prec = bigirt_matrix_from_array(prior_precision, K, subj);
        std::vector<int> active;
        active.reserve(K);
        for (int k = 0; k < K; ++k) {
          theta(k) = theta_init(subj, k);
          mu(k) = prior_mean(subj, k);
          if (free_mask(subj, k) != 0) active.push_back(k);
        }

        auto logpost_and_grad = [&](const VectorXd& theta_eval, VectorXd& grad_out,
          MatrixXd& prec_out, double& lp_out) {
          grad_out = -prior_prec * (theta_eval - mu);
          prec_out = prior_prec;
          lp_out = -0.5 * (theta_eval - mu).dot(prior_prec * (theta_eval - mu));
          for (size_t oi = 0; oi < obs_by_subj[subj].size(); ++oi) {
            const int obs = obs_by_subj[subj][oi];
            VectorXd ability = theta_eval;
            for (int k = 0; k < K; ++k) ability(k) += ability_offset(obs, k);
            double eta = -b[obs];
            for (int k = 0; k < K; ++k) eta += loadings(obs, k) * ability(k);
            BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta, c[obs], d[obs]);
            lp_out += rt.ll;
            for (int k = 0; k < K; ++k) {
              grad_out(k) += rt.grad_eta * loadings(obs, k);
            }
            for (int r = 0; r < K; ++r) {
              const double ar = loadings(obs, r);
              if (ar == 0.0) continue;
              for (int cc = 0; cc < K; ++cc) {
                const double ac = loadings(obs, cc);
                if (ac == 0.0) continue;
                prec_out(r, cc) += rt.w * ar * ac;
              }
            }
          }
        };

        VectorXd grad(K);
        MatrixXd prec(K, K);
        double lp = NA_REAL;
        logpost_and_grad(theta, grad, prec, lp);

        bool subj_converged = active.empty();
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
          VectorXd sub_grad(active.size());
          for (size_t r = 0; r < active.size(); ++r) {
            sub_grad(r) = grad(active[r]);
            for (size_t cc = 0; cc < active.size(); ++cc) {
              sub_prec(r, cc) = prec(active[r], active[cc]);
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
          if (!success) {
            Rcpp::stop("Cholesky failed while updating person modes.");
          }

          VectorXd step = llt.solve(sub_grad);
          double damping = 1.0;
          bool accepted = false;
          VectorXd theta_try = theta;
          VectorXd grad_try(K);
          MatrixXd prec_try(K, K);
          double lp_try = lp;
          for (int bt = 0; bt < 12; ++bt) {
            theta_try = theta;
            for (size_t ai = 0; ai < active.size(); ++ai) {
              theta_try(active[ai]) = theta(active[ai]) + damping * step(ai);
            }
            logpost_and_grad(theta_try, grad_try, prec_try, lp_try);
            if (R_finite(lp_try) && lp_try >= lp - 1e-10) {
              accepted = true;
              break;
            }
            damping *= 0.5;
          }
          if (!accepted) break;
          theta = theta_try;
          grad = grad_try;
          prec = prec_try;
          lp = lp_try;
        }

        logpost_and_grad(theta, grad, prec, lp);
        objective[subj] = lp;
        niter[subj] = used_iter;
        converged[subj] = subj_converged;

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
            for (int cc = 0; cc < K; ++cc) {
              covariance[r + K * cc + K * K * subj] = Sigma(r, cc);
            }
          }
        }
      }
    }
  };

  BigIRTLaplacePersonWorker worker(
    obs_by_subj, score, theta_init, ability_offset, b, c, d, loadings,
    prior_mean, prior_precision, free_mask, jitter, max_attempts, max_iter,
    tol, keep_covariance, K, theta_mode, precision, precision_chol,
    logdet_precision, niter, converged, objective, covariance
  );

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
      Rcpp::Named("converged") = converged
    );
  }

  return Rcpp::List::create(
    Rcpp::Named("theta_mode") = theta_mode,
    Rcpp::Named("precision") = precision,
    Rcpp::Named("precision_chol") = precision_chol,
    Rcpp::Named("logdet_precision") = logdet_precision,
    Rcpp::Named("objective") = objective,
    Rcpp::Named("niter") = niter,
    Rcpp::Named("converged") = converged
  );
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
        w_row[obs] = rt.w;
        loglik += rt.ll;
        for (int r = 0; r < K; ++r) {
          const double ar = loadings(obs, r);
          if (ar == 0.0) continue;
          for (int cidx = 0; cidx < K; ++cidx) {
            const double ac = loadings(obs, cidx);
            if (ac == 0.0) continue;
            Q(r, cidx) += rt.w * ar * ac;
          }
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
        for (int k = 0; k < K; ++k) {
          grad_loadings(obs, k) =
            rt.grad_eta * z(k) - 0.5 * (rt.dw_deta * z(k) * aSa + 2.0 * rt.w * Sa(k));
        }
        grad_b[obs] = -rt.grad_eta + 0.5 * rt.dw_deta * aSa;
        grad_c[obs] = rt.grad_c - 0.5 * rt.dw_dc * aSa;
        grad_d[obs] = rt.grad_d - 0.5 * rt.dw_dd * aSa;
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

struct BigIRTLaplaceItemBlockWorker : public RcppParallel::Worker {
  const std::vector< std::vector<int> >& obs_by_subj;
  RcppParallel::RVector<int> score;
  RcppParallel::RMatrix<double> row_ability;
  RcppParallel::RMatrix<int> A_ref;
  RcppParallel::RMatrix<double> A_fixed_value;
  RcppParallel::RMatrix<int> A_beta_row;
  RcppParallel::RMatrix<double> A_pred;
  RcppParallel::RVector<int> B_ref;
  RcppParallel::RVector<double> B_fixed_value;
  RcppParallel::RVector<int> B_beta_row;
  RcppParallel::RMatrix<double> B_pred;
  RcppParallel::RVector<int> C_ref;
  RcppParallel::RVector<double> C_fixed_value;
  RcppParallel::RVector<int> C_beta_row;
  RcppParallel::RMatrix<double> C_pred;
  RcppParallel::RVector<int> D_ref;
  RcppParallel::RVector<double> D_fixed_value;
  RcppParallel::RVector<int> D_beta_row;
  RcppParallel::RMatrix<double> D_pred;
  RcppParallel::RVector<double> invspApars;
  RcppParallel::RMatrix<double> invspAbeta;
  RcppParallel::RVector<double> Bpars;
  RcppParallel::RMatrix<double> Bbeta;
  RcppParallel::RVector<double> logitCpars;
  RcppParallel::RMatrix<double> logitCbeta;
  RcppParallel::RVector<double> logitDpars;
  RcppParallel::RMatrix<double> logitDbeta;
  RcppParallel::RVector<double> prior_precision;
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
  double objective;
  std::vector<double> grad_A;
  std::vector<double> grad_B;
  std::vector<double> grad_C;
  std::vector<double> grad_D;
  std::vector<double> grad_A_beta;
  std::vector<double> grad_B_beta;
  std::vector<double> grad_C_beta;
  std::vector<double> grad_D_beta;

  BigIRTLaplaceItemBlockWorker(
      const std::vector< std::vector<int> >& obs_by_subj,
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
      jitter(jitter), max_attempts(max_attempts), objective(0.0),
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
      pC(other.pC), pD(other.pD), jitter(other.jitter), max_attempts(other.max_attempts),
      objective(0.0), grad_A(static_cast<std::size_t>(std::max(0, other.nA)), 0.0),
      grad_B(static_cast<std::size_t>(std::max(0, other.nB)), 0.0),
      grad_C(static_cast<std::size_t>(std::max(0, other.nC)), 0.0),
      grad_D(static_cast<std::size_t>(std::max(0, other.nD)), 0.0),
      grad_A_beta(static_cast<std::size_t>(std::max(0, other.nA_beta_row * other.pA)), 0.0),
      grad_B_beta(static_cast<std::size_t>(std::max(0, other.nB_beta_row * other.pB)), 0.0),
      grad_C_beta(static_cast<std::size_t>(std::max(0, other.nC_beta_row * other.pC)), 0.0),
      grad_D_beta(static_cast<std::size_t>(std::max(0, other.nD_beta_row * other.pD)), 0.0) {}

  inline double softplus(const double x) const {
    if (x > 0.0) return x + std::log1p(std::exp(-x));
    return std::log1p(std::exp(x));
  }

  inline double add_beta_term(const int row_idx, const RcppParallel::RMatrix<double>& pred,
    const RcppParallel::RMatrix<double>& beta, const int beta_row) const {
    if (beta_row <= 0 || pred.ncol() == 0 || beta.nrow() == 0) return 0.0;
    const int brow = beta_row - 1;
    double out = 0.0;
    for (int j = 0; j < pred.ncol(); ++j) out += pred(row_idx, j) * beta(brow, j);
    return out;
  }

  inline void add_beta_grad(std::vector<double>& grad_beta, const int nrow_beta, const int p,
    const RcppParallel::RMatrix<double>& pred, const int row_idx, const int beta_row, const double value) {
    if (beta_row <= 0 || p == 0 || nrow_beta <= 0) return;
    const int brow = beta_row - 1;
    for (int j = 0; j < p; ++j) {
      grad_beta[static_cast<std::size_t>(brow + nrow_beta * j)] += value * pred(row_idx, j);
    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    const MatrixXd eye = MatrixXd::Identity(K, K);
    for (std::size_t subj = begin; subj < end; ++subj) {
      MatrixXd Q = MatrixXd::Zero(K, K);
      for (int r = 0; r < K; ++r) {
        for (int cidx = 0; cidx < K; ++cidx) {
          Q(r, cidx) = prior_precision[r + K * cidx + K * K * static_cast<int>(subj)];
        }
      }

      const std::vector<int>& obs_idx = obs_by_subj[subj];
      std::vector<BigIRTRowTerms> row_terms(obs_idx.size());
      std::vector<VectorXd> row_a(obs_idx.size(), VectorXd::Zero(K));
      std::vector<VectorXd> row_a_mult(obs_idx.size(), VectorXd::Zero(K));
      std::vector<double> row_c_mult(obs_idx.size(), 0.0);
      std::vector<double> row_d_mult(obs_idx.size(), 0.0);
      double loglik = 0.0;

      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        double b_row = B_fixed_value[obs];
        if (B_ref[obs] > 0) {
          b_row = Bpars[B_ref[obs] - 1] + add_beta_term(obs, B_pred, Bbeta, B_beta_row[obs]);
        }

        double c_row = C_fixed_value[obs];
        if (C_ref[obs] > 0) {
          const double raw_c = logitCpars[C_ref[obs] - 1] + add_beta_term(obs, C_pred, logitCbeta, C_beta_row[obs]);
          const double sig_c = bigirt_stable_inv_logit(raw_c);
          c_row = 0.5 * sig_c;
          row_c_mult[oi] = 0.5 * sig_c * (1.0 - sig_c);
        }

        double d_row = D_fixed_value[obs];
        if (D_ref[obs] > 0) {
          const double raw_d = logitDpars[D_ref[obs] - 1] + add_beta_term(obs, D_pred, logitDbeta, D_beta_row[obs]);
          const double sig_d = bigirt_stable_inv_logit(raw_d);
          d_row = 0.5 * sig_d + 0.5;
          row_d_mult[oi] = 0.5 * sig_d * (1.0 - sig_d);
        }

        double eta_obs = -b_row;
        for (int k = 0; k < K; ++k) {
          double a_val = A_fixed_value(obs, k);
          if (A_ref(obs, k) > 0) {
            const double raw_a = invspApars[A_ref(obs, k) - 1] + add_beta_term(obs, A_pred, invspAbeta, A_beta_row(obs, k));
            const double sig_a = bigirt_stable_inv_logit(raw_a);
            a_val = softplus(raw_a);
            row_a_mult[oi](k) = sig_a;
          }
          row_a[oi](k) = a_val;
          eta_obs += a_val * row_ability(obs, k);
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
            Q(r, cidx) += rt.w * ar * ac;
          }
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
      if (!success) Rcpp::stop("Cholesky failed while evaluating Laplace block objective.");

      MatrixXd Sigma = llt.solve(eye);
      MatrixXd L = llt.matrixL();
      double logdet = 0.0;
      for (int k = 0; k < K; ++k) logdet += 2.0 * std::log(std::max(L(k, k), 1e-12));
      objective += loglik + 0.5 * static_cast<double>(K) * std::log(2.0 * M_PI) - 0.5 * logdet;

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
        for (int k = 0; k < K; ++k) trace_by_theta(k) += row_terms[oi].dw_deta * a(k) * aSa;
      }
      const VectorXd mode_adjoint = Sigma * trace_by_theta;

      for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
        const int obs = obs_idx[oi];
        const VectorXd& a = row_a[oi];
        VectorXd z(K);
        for (int k = 0; k < K; ++k) z(k) = row_ability(obs, k);
        const VectorXd Sa = Sigma * a;
        const double aSa = a.dot(Sa);
        const BigIRTRowTerms& rt = row_terms[oi];
        const double ua = mode_adjoint.dot(a);

        for (int k = 0; k < K; ++k) {
          const double g_load = rt.grad_eta * z(k) -
            0.5 * (rt.dw_deta * z(k) * aSa + 2.0 * rt.w * Sa(k)) -
            0.5 * (mode_adjoint(k) * rt.grad_eta + ua * rt.dgrad_eta_deta * z(k));
          const double g_raw = g_load * row_a_mult[oi](k);
          if (A_ref(obs, k) > 0) grad_A[static_cast<std::size_t>(A_ref(obs, k) - 1)] += g_raw;
          add_beta_grad(grad_A_beta, nA_beta_row, pA, A_pred, obs, A_beta_row(obs, k), g_raw);
        }

        const double g_b = -rt.grad_eta + 0.5 * rt.dw_deta * aSa +
          0.5 * ua * rt.dgrad_eta_deta;
        if (B_ref[obs] > 0) grad_B[static_cast<std::size_t>(B_ref[obs] - 1)] += g_b;
        add_beta_grad(grad_B_beta, nB_beta_row, pB, B_pred, obs, B_beta_row[obs], g_b);

        const double g_c_raw = (rt.grad_c - 0.5 * rt.dw_dc * aSa -
          0.5 * ua * rt.dgrad_eta_dc) * row_c_mult[oi];
        if (C_ref[obs] > 0) grad_C[static_cast<std::size_t>(C_ref[obs] - 1)] += g_c_raw;
        add_beta_grad(grad_C_beta, nC_beta_row, pC, C_pred, obs, C_beta_row[obs], g_c_raw);

        const double g_d_raw = (rt.grad_d - 0.5 * rt.dw_dd * aSa -
          0.5 * ua * rt.dgrad_eta_dd) * row_d_mult[oi];
        if (D_ref[obs] > 0) grad_D[static_cast<std::size_t>(D_ref[obs] - 1)] += g_d_raw;
        add_beta_grad(grad_D_beta, nD_beta_row, pD, D_pred, obs, D_beta_row[obs], g_d_raw);
      }
    }
  }

  void join(const BigIRTLaplaceItemBlockWorker& rhs) {
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
    SEXP grain_sizeSEXP) {

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

  std::vector< std::vector<int> > obs_by_subj(Nsubs);
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    obs_by_subj[subj].push_back(obs);
  }

  BigIRTLaplaceItemBlockWorker worker(
    obs_by_subj, score, row_ability, A_ref, A_fixed_value, A_beta_row, A_pred,
    B_ref, B_fixed_value, B_beta_row, B_pred,
    C_ref, C_fixed_value, C_beta_row, C_pred,
    D_ref, D_fixed_value, D_beta_row, D_pred,
    invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
    prior_precision, K, jitter, max_attempts
  );

  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(Nsubs), worker, grain_size);

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

  return Rcpp::List::create(
    Rcpp::Named("objective") = worker.objective,
    Rcpp::Named("grad_A") = grad_A,
    Rcpp::Named("grad_B") = grad_B,
    Rcpp::Named("grad_C") = grad_C,
    Rcpp::Named("grad_D") = grad_D,
    Rcpp::Named("grad_A_beta") = grad_A_beta,
    Rcpp::Named("grad_B_beta") = grad_B_beta,
    Rcpp::Named("grad_C_beta") = grad_C_beta,
    Rcpp::Named("grad_D_beta") = grad_D_beta
  );
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

  std::vector< std::vector<int> > obs_by_subj(Nsubs);
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    obs_by_subj[subj].push_back(obs);
  }

  Rcpp::NumericMatrix theta_mode(Nsubs, K);
  Rcpp::NumericVector precision(K * K * Nsubs);
  precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision_chol(K * K * Nsubs);
  precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector logdet_precision(Nsubs);
  Rcpp::IntegerVector niter(Nsubs);
  Rcpp::LogicalVector converged(Nsubs);
  Rcpp::NumericVector objective(Nsubs);
  Rcpp::NumericVector covariance;
  if (keep_covariance) {
    covariance = Rcpp::NumericVector(K * K * Nsubs);
    covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }

  struct BigIRTLaplacePersonBlockWorker : public RcppParallel::Worker {
    const std::vector< std::vector<int> >& obs_by_subj;
    RcppParallel::RVector<int> score;
    RcppParallel::RMatrix<double> theta_init;
    RcppParallel::RMatrix<double> person_pred;
    RcppParallel::RMatrix<int> fixed_ability;
    RcppParallel::RMatrix<double> fixed_ability_value;
    RcppParallel::RMatrix<int> A_ref;
    RcppParallel::RMatrix<double> A_fixed_value;
    RcppParallel::RMatrix<int> A_beta_row;
    RcppParallel::RMatrix<double> A_pred;
    RcppParallel::RVector<int> B_ref;
    RcppParallel::RVector<double> B_fixed_value;
    RcppParallel::RVector<int> B_beta_row;
    RcppParallel::RMatrix<double> B_pred;
    RcppParallel::RVector<int> C_ref;
    RcppParallel::RVector<double> C_fixed_value;
    RcppParallel::RVector<int> C_beta_row;
    RcppParallel::RMatrix<double> C_pred;
    RcppParallel::RVector<int> D_ref;
    RcppParallel::RVector<double> D_fixed_value;
    RcppParallel::RVector<int> D_beta_row;
    RcppParallel::RMatrix<double> D_pred;
    RcppParallel::RMatrix<double> Abilitybeta;
    RcppParallel::RVector<double> invspApars;
    RcppParallel::RMatrix<double> invspAbeta;
    RcppParallel::RVector<double> Bpars;
    RcppParallel::RMatrix<double> Bbeta;
    RcppParallel::RVector<double> logitCpars;
    RcppParallel::RMatrix<double> logitCbeta;
    RcppParallel::RVector<double> logitDpars;
    RcppParallel::RMatrix<double> logitDbeta;
    RcppParallel::RMatrix<double> prior_mean;
    RcppParallel::RVector<double> prior_precision;
    RcppParallel::RMatrix<int> free_mask;
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
    RcppParallel::RVector<double> objective;
    RcppParallel::RVector<double> covariance;

    BigIRTLaplacePersonBlockWorker(
      const std::vector< std::vector<int> >& obs_by_subj,
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
        objective(objective), covariance(covariance) {}

    inline double softplus(const double x) const {
      if (x > 0.0) return x + std::log1p(std::exp(-x));
      return std::log1p(std::exp(x));
    }

    inline double add_beta_term(const int row_idx, const RcppParallel::RMatrix<double>& pred,
      const RcppParallel::RMatrix<double>& beta, const int beta_row) const {
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
        MatrixXd prior_prec = bigirt_matrix_from_array_parallel(prior_precision, K, subj);
        std::vector<int> active;
        active.reserve(K);
        for (int k = 0; k < K; ++k) {
          theta(k) = theta_init(subj, k);
          mu(k) = prior_mean(subj, k);
          if (free_mask(subj, k) != 0) active.push_back(k);
        }

        const std::vector<int>& obs_idx = obs_by_subj[subj];
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
              const double raw_a = invspApars[A_ref(obs, k) - 1] + add_beta_term(obs, A_pred, invspAbeta, A_beta_row(obs, k));
              a_val = softplus(raw_a);
            }
            loadings[oi](k) = a_val;
          }

          b_row[oi] = B_fixed_value[obs];
          if (B_ref[obs] > 0) b_row[oi] = Bpars[B_ref[obs] - 1] + add_beta_term(obs, B_pred, Bbeta, B_beta_row[obs]);

          c_row[oi] = C_fixed_value[obs];
          if (C_ref[obs] > 0) {
            const double raw_c = logitCpars[C_ref[obs] - 1] + add_beta_term(obs, C_pred, logitCbeta, C_beta_row[obs]);
            c_row[oi] = 0.5 * bigirt_stable_inv_logit(raw_c);
          }

          d_row[oi] = D_fixed_value[obs];
          if (D_ref[obs] > 0) {
            const double raw_d = logitDpars[D_ref[obs] - 1] + add_beta_term(obs, D_pred, logitDbeta, D_beta_row[obs]);
            d_row[oi] = 0.5 * bigirt_stable_inv_logit(raw_d) + 0.5;
          }
        }

        auto logpost_and_grad = [&](const VectorXd& theta_eval, VectorXd& grad_out,
          MatrixXd& prec_out, double& lp_out) {
          grad_out = -prior_prec * (theta_eval - mu);
          prec_out = prior_prec;
          lp_out = -0.5 * (theta_eval - mu).dot(prior_prec * (theta_eval - mu));
          for (std::size_t oi = 0; oi < obs_idx.size(); ++oi) {
            const int obs = obs_idx[oi];
            VectorXd ability = theta_eval + ability_offset[oi];
            for (int k = 0; k < K; ++k) if (fixed_ability(obs, k) != 0) ability(k) = fixed_ability_value(obs, k);
            double eta = -b_row[oi];
            for (int k = 0; k < K; ++k) eta += loadings[oi](k) * ability(k);
            BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta, c_row[oi], d_row[oi]);
            lp_out += rt.ll;
            for (int k = 0; k < K; ++k) grad_out(k) += rt.grad_eta * loadings[oi](k);
            for (int r = 0; r < K; ++r) {
              const double ar = loadings[oi](r);
              if (ar == 0.0) continue;
              for (int cc = 0; cc < K; ++cc) {
                const double ac = loadings[oi](cc);
                if (ac == 0.0) continue;
                prec_out(r, cc) += rt.w * ar * ac;
              }
            }
          }
        };

        VectorXd grad(K);
        MatrixXd prec(K, K);
        double lp = NA_REAL;
        logpost_and_grad(theta, grad, prec, lp);

        bool subj_converged = active.empty();
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
          VectorXd sub_grad(active.size());
          for (size_t r = 0; r < active.size(); ++r) {
            sub_grad(r) = grad(active[r]);
            for (size_t cc = 0; cc < active.size(); ++cc) sub_prec(r, cc) = prec(active[r], active[cc]);
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
          if (!success) Rcpp::stop("Cholesky failed while updating person modes.");

          VectorXd step = llt.solve(sub_grad);
          double damping = 1.0;
          bool accepted = false;
          VectorXd theta_try = theta;
          VectorXd grad_try(K);
          MatrixXd prec_try(K, K);
          double lp_try = lp;
          for (int bt = 0; bt < 12; ++bt) {
            theta_try = theta;
            for (size_t ai = 0; ai < active.size(); ++ai) theta_try(active[ai]) = theta(active[ai]) + damping * step(ai);
            logpost_and_grad(theta_try, grad_try, prec_try, lp_try);
            if (R_finite(lp_try) && lp_try >= lp - 1e-10) {
              accepted = true;
              break;
            }
            damping *= 0.5;
          }
          if (!accepted) break;
          theta = theta_try;
          grad = grad_try;
          prec = prec_try;
          lp = lp_try;
        }

        logpost_and_grad(theta, grad, prec, lp);
        objective[subj] = lp;
        niter[subj] = used_iter;
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
    keep_covariance, theta_mode, precision, precision_chol, logdet_precision, niter, converged, objective, covariance
  );
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
      Rcpp::Named("converged") = converged
    );
  }

  return Rcpp::List::create(
      Rcpp::Named("theta_mode") = theta_mode,
      Rcpp::Named("precision") = precision,
      Rcpp::Named("precision_chol") = precision_chol,
      Rcpp::Named("logdet_precision") = logdet_precision,
      Rcpp::Named("objective") = objective,
      Rcpp::Named("niter") = niter,
      Rcpp::Named("converged") = converged
    );
}

extern "C" SEXP _bigIRT_laplace_materialize_block_cpp_impl(
    SEXP itemSEXP,
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP theta_baseSEXP,
    SEXP person_predSEXP,
    SEXP item_predSEXP,
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
    SEXP logitDbetaSEXP) {

  Rcpp::IntegerVector item(itemSEXP);
  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix theta_base(theta_baseSEXP);
  Rcpp::NumericMatrix person_pred(person_predSEXP);
  Rcpp::NumericMatrix item_pred(item_predSEXP);
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

  const int Nobs = id.size();
  const int Nsubs = theta_base.nrow();
  const int K = theta_base.ncol();
  if (item.size() != Nobs || score.size() != Nobs) Rcpp::stop("item, id, and score must have length Nobs.");
  if (person_pred.nrow() != Nobs || fixed_ability.nrow() != Nobs || fixed_ability.ncol() != K ||
      fixed_ability_value.nrow() != Nobs || fixed_ability_value.ncol() != K ||
      A_ref.nrow() != Nobs || A_ref.ncol() != K || A_fixed_value.nrow() != Nobs || A_fixed_value.ncol() != K ||
      A_beta_row.nrow() != Nobs || A_beta_row.ncol() != K) {
    Rcpp::stop("Unexpected dimensions in Laplace materialize block inputs.");
  }
  const int Nitems = Rcpp::max(item);
  const int pPerson = person_pred.ncol();
  const int pItem = item_pred.ncol();

  auto softplus = [](const double x) {
    if (x > 0.0) return x + std::log1p(std::exp(-x));
    return std::log1p(std::exp(x));
  };
  auto add_beta_term = [](const int row_idx, const Rcpp::NumericMatrix& pred,
    const Rcpp::NumericMatrix& beta, const int beta_row) {
    if (beta_row <= 0 || pred.ncol() == 0 || beta.nrow() == 0) return 0.0;
    const int brow = beta_row - 1;
    double out = 0.0;
    for (int j = 0; j < pred.ncol(); ++j) out += pred(row_idx, j) * beta(brow, j);
    return out;
  };

  Rcpp::NumericMatrix personPredsMean(Nsubs, pPerson);
  Rcpp::NumericMatrix itemPredsMean(Nitems, pItem);
  Rcpp::IntegerVector personCount(Nsubs);
  Rcpp::IntegerVector itemCount(Nitems);

  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    const int itm = item[obs] - 1;
    if (subj < 0 || subj >= Nsubs || itm < 0 || itm >= Nitems) Rcpp::stop("id/item must be coded from 1.");
    personCount[subj] += 1;
    itemCount[itm] += 1;
    for (int j = 0; j < pPerson; ++j) personPredsMean(subj, j) += person_pred(obs, j);
    for (int j = 0; j < pItem; ++j) itemPredsMean(itm, j) += item_pred(obs, j);
  }
  for (int subj = 0; subj < Nsubs; ++subj) {
    const double denom = std::max(personCount[subj], 1);
    for (int j = 0; j < pPerson; ++j) personPredsMean(subj, j) /= denom;
  }
  for (int itm = 0; itm < Nitems; ++itm) {
    const double denom = std::max(itemCount[itm], 1);
    for (int j = 0; j < pItem; ++j) itemPredsMean(itm, j) /= denom;
  }

  Rcpp::NumericMatrix Ability(Nsubs, K);
  for (int subj = 0; subj < Nsubs; ++subj) {
    for (int k = 0; k < K; ++k) {
      double val = theta_base(subj, k);
      for (int j = 0; j < pPerson; ++j) val += personPredsMean(subj, j) * Abilitybeta(k, j);
      Ability(subj, k) = val;
    }
  }
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    for (int k = 0; k < K; ++k) {
      if (fixed_ability(obs, k) != 0) Ability(subj, k) = fixed_ability_value(obs, k);
    }
  }

  Rcpp::NumericMatrix A(Nitems, K);
  Rcpp::NumericVector B(Nitems);
  Rcpp::NumericVector C(Nitems);
  Rcpp::NumericVector D(Nitems);
  Rcpp::LogicalVector seen_item(Nitems, false);

  Rcpp::NumericMatrix row_loadings(Nobs, K);
  Rcpp::NumericMatrix row_ability(Nobs, K);
  Rcpp::NumericVector b_row(Nobs), c_row(Nobs), d_row(Nobs), eta_row(Nobs), pcorrect(Nobs), p_obs(Nobs);

  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    const int itm = item[obs] - 1;

    for (int k = 0; k < K; ++k) {
      double aval = A_fixed_value(obs, k);
      if (A_ref(obs, k) > 0) {
        const double raw_a = invspApars[A_ref(obs, k) - 1] + add_beta_term(obs, A_pred, invspAbeta, A_beta_row(obs, k));
        aval = softplus(raw_a);
      }
      row_loadings(obs, k) = aval;
      row_ability(obs, k) = Ability(subj, k);
    }

    double bval = B_fixed_value[obs];
    if (B_ref[obs] > 0) bval = Bpars[B_ref[obs] - 1] + add_beta_term(obs, B_pred, Bbeta, B_beta_row[obs]);
    double cval = C_fixed_value[obs];
    if (C_ref[obs] > 0) {
      const double raw_c = logitCpars[C_ref[obs] - 1] + add_beta_term(obs, C_pred, logitCbeta, C_beta_row[obs]);
      cval = 0.5 * bigirt_stable_inv_logit(raw_c);
    }
    double dval = D_fixed_value[obs];
    if (D_ref[obs] > 0) {
      const double raw_d = logitDpars[D_ref[obs] - 1] + add_beta_term(obs, D_pred, logitDbeta, D_beta_row[obs]);
      dval = 0.5 * bigirt_stable_inv_logit(raw_d) + 0.5;
    }

    b_row[obs] = bval;
    c_row[obs] = cval;
    d_row[obs] = dval;

    double eta = -bval;
    for (int k = 0; k < K; ++k) eta += row_loadings(obs, k) * row_ability(obs, k);
    eta_row[obs] = eta;
    const double pc = cval + (dval - cval) * bigirt_stable_inv_logit(eta);
    pcorrect[obs] = pc;
    p_obs[obs] = score[obs] ? pc : (1.0 - pc);

    if (!seen_item[itm]) {
      seen_item[itm] = true;
      for (int k = 0; k < K; ++k) A(itm, k) = row_loadings(obs, k);
      B[itm] = bval;
      C[itm] = cval;
      D[itm] = dval;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("A") = A,
    Rcpp::Named("B") = B,
    Rcpp::Named("C") = C,
    Rcpp::Named("D") = D,
    Rcpp::Named("Ability") = Ability,
    Rcpp::Named("itemPredsMean") = itemPredsMean,
    Rcpp::Named("personPredsMean") = personPredsMean,
    Rcpp::Named("b_row") = b_row,
    Rcpp::Named("c_row") = c_row,
    Rcpp::Named("d_row") = d_row,
    Rcpp::Named("eta_row") = eta_row,
    Rcpp::Named("row_loadings") = row_loadings,
    Rcpp::Named("row_ability") = row_ability,
    Rcpp::Named("p") = p_obs,
    Rcpp::Named("pcorrect") = pcorrect
  );
}

extern "C" SEXP _bigIRT_laplace_item_objective_fixed_cov_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP row_abilitySEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP loadingsSEXP,
    SEXP covarianceSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix row_ability(row_abilitySEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::NumericMatrix loadings(loadingsSEXP);
  Rcpp::NumericVector covariance(covarianceSEXP);

  const int Nobs = id.size();
  const int K = row_ability.ncol();
  Rcpp::IntegerVector cov_dim = covariance.attr("dim");
  if (cov_dim.size() != 3 || cov_dim[0] != K || cov_dim[1] != K) {
    Rcpp::stop("covariance must have dimensions K x K x Nsubs.");
  }
  const int Nsubs = cov_dim[2];
  if (row_ability.nrow() != Nobs || loadings.nrow() != Nobs || loadings.ncol() != K) {
    Rcpp::stop("row_ability and loadings must have dimensions Nobs x K.");
  }

  Rcpp::NumericMatrix grad_loadings(Nobs, K);
  Rcpp::NumericVector grad_b(Nobs);
  Rcpp::NumericVector grad_c(Nobs);
  Rcpp::NumericVector grad_d(Nobs);
  Rcpp::NumericVector eta(Nobs);
  Rcpp::NumericVector p_row(Nobs);
  Rcpp::NumericVector w_row(Nobs);

  double objective = 0.0;
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    MatrixXd Sigma = bigirt_matrix_from_array(covariance, K, subj);
    VectorXd a(K);
    VectorXd z(K);
    double eta_obs = -b[obs];
    for (int k = 0; k < K; ++k) {
      a(k) = loadings(obs, k);
      z(k) = row_ability(obs, k);
      eta_obs += a(k) * z(k);
    }
    BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta_obs, c[obs], d[obs]);
    const VectorXd Sa = Sigma * a;
    const double aSa = a.dot(Sa);
    objective += rt.ll - 0.5 * rt.w * aSa;
    eta[obs] = eta_obs;
    p_row[obs] = rt.p;
    w_row[obs] = rt.w;

    for (int k = 0; k < K; ++k) {
      grad_loadings(obs, k) =
        rt.grad_eta * z(k) -
        0.5 * (rt.dw_deta * z(k) * aSa + 2.0 * rt.w * Sa(k));
    }
    grad_b[obs] = -rt.grad_eta + 0.5 * rt.dw_deta * aSa;
    grad_c[obs] = rt.grad_c - 0.5 * rt.dw_dc * aSa;
    grad_d[obs] = rt.grad_d - 0.5 * rt.dw_dd * aSa;
  }

  return Rcpp::List::create(
    Rcpp::Named("objective") = objective,
    Rcpp::Named("grad_loadings") = grad_loadings,
    Rcpp::Named("grad_b") = grad_b,
    Rcpp::Named("grad_c") = grad_c,
    Rcpp::Named("grad_d") = grad_d,
    Rcpp::Named("eta") = eta,
    Rcpp::Named("p_row") = p_row,
    Rcpp::Named("w_row") = w_row
  );
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
    SEXP grain_sizeSEXP) {
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

  Rcpp::List item_fg = Rcpp::as<Rcpp::List>(_bigIRT_laplace_item_block_objective_cpp_impl(
    idSEXP, scoreSEXP, row_ability,
    A_refSEXP, A_fixed_valueSEXP, A_beta_rowSEXP, A_predSEXP,
    B_refSEXP, B_fixed_valueSEXP, B_beta_rowSEXP, B_predSEXP,
    C_refSEXP, C_fixed_valueSEXP, C_beta_rowSEXP, C_predSEXP,
    D_refSEXP, D_fixed_valueSEXP, D_beta_rowSEXP, D_predSEXP,
    invspAparsSEXP, invspAbetaSEXP, BparsSEXP, BbetaSEXP,
    logitCparsSEXP, logitCbetaSEXP, logitDparsSEXP, logitDbetaSEXP,
    prior_precisionSEXP, jitterSEXP, max_attemptsSEXP, grain_sizeSEXP
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

extern "C" SEXP _bigIRT_laplace_direct_objective_cpp_impl(
    SEXP idSEXP,
    SEXP scoreSEXP,
    SEXP theta_initSEXP,
    SEXP ability_offsetSEXP,
    SEXP bSEXP,
    SEXP cSEXP,
    SEXP dSEXP,
    SEXP loadingsSEXP,
    SEXP prior_meanSEXP,
    SEXP prior_precisionSEXP,
    SEXP free_maskSEXP,
    SEXP jitterSEXP,
    SEXP max_attemptsSEXP,
    SEXP max_iterSEXP,
    SEXP tolSEXP,
    SEXP keep_covarianceSEXP) {

  Rcpp::IntegerVector id(idSEXP);
  Rcpp::IntegerVector score(scoreSEXP);
  Rcpp::NumericMatrix theta_init(theta_initSEXP);
  Rcpp::NumericMatrix ability_offset(ability_offsetSEXP);
  Rcpp::NumericVector b(bSEXP);
  Rcpp::NumericVector c(cSEXP);
  Rcpp::NumericVector d(dSEXP);
  Rcpp::NumericMatrix loadings(loadingsSEXP);
  Rcpp::NumericMatrix prior_mean(prior_meanSEXP);
  Rcpp::NumericVector prior_precision(prior_precisionSEXP);
  Rcpp::IntegerMatrix free_mask(free_maskSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  const int max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  const int max_iter = Rcpp::as<int>(max_iterSEXP);
  const double tol = Rcpp::as<double>(tolSEXP);
  const bool keep_covariance = Rcpp::as<bool>(keep_covarianceSEXP);

  const int Nobs = id.size();
  const int Nsubs = theta_init.nrow();
  const int K = theta_init.ncol();
  if (ability_offset.nrow() != Nobs || ability_offset.ncol() != K) {
    Rcpp::stop("ability_offset must have dimensions Nobs x K.");
  }
  if (loadings.nrow() != Nobs || loadings.ncol() != K) {
    Rcpp::stop("loadings must have dimensions Nobs x K.");
  }
  if (prior_mean.nrow() != Nsubs || prior_mean.ncol() != K) {
    Rcpp::stop("prior_mean must have dimensions Nsubs x K.");
  }

  Rcpp::IntegerVector prior_dim = prior_precision.attr("dim");
  if (prior_dim.size() != 3 || prior_dim[0] != K || prior_dim[1] != K || prior_dim[2] != Nsubs) {
    Rcpp::stop("prior_precision must have dimensions K x K x Nsubs.");
  }

  std::vector< std::vector<int> > obs_by_subj(Nsubs);
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = id[obs] - 1;
    if (subj < 0 || subj >= Nsubs) Rcpp::stop("id must be coded from 1 to Nsubs.");
    obs_by_subj[subj].push_back(obs);
  }

  Rcpp::NumericMatrix theta_mode(Nsubs, K);
  Rcpp::NumericVector precision(K * K * Nsubs);
  precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector precision_chol(K * K * Nsubs);
  precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  Rcpp::NumericVector logdet_precision(Nsubs);
  Rcpp::IntegerVector niter(Nsubs);
  Rcpp::LogicalVector converged(Nsubs);
  Rcpp::NumericVector objective_by_subj(Nsubs);
  Rcpp::NumericVector covariance;
  if (keep_covariance) {
    covariance = Rcpp::NumericVector(K * K * Nsubs);
    covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }

  double objective = 0.0;
  const MatrixXd eye = MatrixXd::Identity(K, K);
  for (int subj = 0; subj < Nsubs; ++subj) {
    VectorXd theta(K);
    VectorXd mu(K);
    MatrixXd prior_prec = bigirt_matrix_from_array(prior_precision, K, subj);
    std::vector<int> active;
    for (int k = 0; k < K; ++k) {
      theta(k) = theta_init(subj, k);
      mu(k) = prior_mean(subj, k);
      if (free_mask(subj, k) != 0) active.push_back(k);
    }

    auto logpost_and_grad = [&](const VectorXd& theta_eval, VectorXd& grad_out,
      MatrixXd& prec_out, double& lp_out) {
      grad_out = -prior_prec * (theta_eval - mu);
      prec_out = prior_prec;
      lp_out = -0.5 * (theta_eval - mu).dot(prior_prec * (theta_eval - mu));
      for (size_t oi = 0; oi < obs_by_subj[subj].size(); ++oi) {
        const int obs = obs_by_subj[subj][oi];
        VectorXd ability = theta_eval;
        for (int k = 0; k < K; ++k) ability(k) += ability_offset(obs, k);
        double eta = -b[obs];
        for (int k = 0; k < K; ++k) eta += loadings(obs, k) * ability(k);
        BigIRTRowTerms rt = bigirt_row_terms(static_cast<double>(score[obs]), eta, c[obs], d[obs]);
        lp_out += rt.ll;
        for (int k = 0; k < K; ++k) grad_out(k) += rt.grad_eta * loadings(obs, k);
        for (int r = 0; r < K; ++r) {
          const double ar = loadings(obs, r);
          if (ar == 0.0) continue;
          for (int cc = 0; cc < K; ++cc) {
            const double ac = loadings(obs, cc);
            if (ac == 0.0) continue;
            prec_out(r, cc) += rt.w * ar * ac;
          }
        }
      }
    };

    VectorXd grad(K);
    MatrixXd prec(K, K);
    double lp = NA_REAL;
    logpost_and_grad(theta, grad, prec, lp);

    bool subj_converged = active.empty();
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
      VectorXd sub_grad(active.size());
      for (size_t r = 0; r < active.size(); ++r) {
        sub_grad(r) = grad(active[r]);
        for (size_t cc = 0; cc < active.size(); ++cc) sub_prec(r, cc) = prec(active[r], active[cc]);
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
      if (!success) Rcpp::stop("Cholesky failed while updating direct Laplace person modes.");

      VectorXd step = llt.solve(sub_grad);
      double damping = 1.0;
      bool accepted = false;
      VectorXd theta_try = theta;
      VectorXd grad_try(K);
      MatrixXd prec_try(K, K);
      double lp_try = lp;
      for (int bt = 0; bt < 12; ++bt) {
        theta_try = theta;
        for (size_t ai = 0; ai < active.size(); ++ai) theta_try(active[ai]) = theta(active[ai]) + damping * step(ai);
        logpost_and_grad(theta_try, grad_try, prec_try, lp_try);
        if (R_finite(lp_try) && lp_try >= lp - 1e-10) {
          accepted = true;
          break;
        }
        damping *= 0.5;
      }
      if (!accepted) break;
      theta = theta_try;
      grad = grad_try;
      prec = prec_try;
      lp = lp_try;
    }

    logpost_and_grad(theta, grad, prec, lp);
    niter[subj] = used_iter;
    converged[subj] = subj_converged;
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
    if (!success) Rcpp::stop("Cholesky failed while finalizing direct Laplace precision.");

    MatrixXd L = llt_full.matrixL();
    double logdet = 0.0;
    for (int k = 0; k < K; ++k) logdet += 2.0 * std::log(std::max(L(k, k), 1e-12));
    logdet_precision[subj] = logdet;

    double laplace_subj = lp + 0.5 * static_cast<double>(K) * std::log(2.0 * M_PI) - 0.5 * logdet;
    objective_by_subj[subj] = laplace_subj;
    objective += laplace_subj;

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

  if (keep_covariance) {
    return Rcpp::List::create(
      Rcpp::Named("objective") = objective,
      Rcpp::Named("objective_by_subj") = objective_by_subj,
      Rcpp::Named("theta_mode") = theta_mode,
      Rcpp::Named("precision") = precision,
      Rcpp::Named("precision_chol") = precision_chol,
      Rcpp::Named("logdet_precision") = logdet_precision,
      Rcpp::Named("covariance") = covariance,
      Rcpp::Named("niter") = niter,
      Rcpp::Named("converged") = converged
    );
  }

  return Rcpp::List::create(
    Rcpp::Named("objective") = objective,
    Rcpp::Named("objective_by_subj") = objective_by_subj,
    Rcpp::Named("theta_mode") = theta_mode,
    Rcpp::Named("precision") = precision,
    Rcpp::Named("precision_chol") = precision_chol,
    Rcpp::Named("logdet_precision") = logdet_precision,
    Rcpp::Named("niter") = niter,
    Rcpp::Named("converged") = converged
  );
}

extern "C" SEXP _bigIRT_laplace_corr_grad_cpp_impl(
    SEXP theta_modeSEXP,
    SEXP covarianceSEXP,
    SEXP precisionSEXP,
    SEXP prior_meanSEXP,
    SEXP ability_sdSEXP,
    SEXP corr_parSEXP,
    SEXP corr_paramizationSEXP,
    SEXP jitterSEXP) {

  Rcpp::NumericMatrix theta_mode(theta_modeSEXP);
  Rcpp::NumericVector covariance(covarianceSEXP);
  Rcpp::NumericVector precision(precisionSEXP);
  Rcpp::NumericVector prior_mean(prior_meanSEXP);
  Rcpp::NumericVector ability_sd(ability_sdSEXP);
  Rcpp::NumericVector corr_par(corr_parSEXP);
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
  }
  GQ = 0.5 * (GQ + GQ.transpose());

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
