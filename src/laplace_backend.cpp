#include <RcppEigen.h>
#include <RcppParallel.h>
#include <chrono>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>
using Eigen::MatrixXd;
using Eigen::VectorXd;

// Worker inputs must not retain an R object.  RcppParallel's RVector/RMatrix
// views are only safe for a narrow set of access patterns and were the source
// of process-level exits in predictor-bearing fits on Windows.  These compact
// column-major copies are made once at the .Call boundary and are ordinary C++
// data thereafter.
template <typename T>
// The buffer is shared, not copied. RcppParallel splits the worker body once
// per stealing thread, and the default member-wise copy duplicated every one of
// these -- all of them read-only, and several of them sized by the number of
// observations rather than the number of subjects. On a sparse fit with
// millions of responses that is hundreds of megabytes per split, for data no
// split ever writes to. A shared_ptr copy is O(1) and every accessor below is
// unchanged, so no call site has to know.
struct BigIRTOwnedVector {
  std::shared_ptr< std::vector<T> > values;
  BigIRTOwnedVector() : values(std::make_shared< std::vector<T> >()) {}
  template <typename V> explicit BigIRTOwnedVector(const V& x)
      : values(std::make_shared< std::vector<T> >(static_cast<std::size_t>(x.size()))) {
    std::copy(x.begin(), x.end(), values->begin());
  }
  inline int length() const { return static_cast<int>(values->size()); }
  inline int size() const { return static_cast<int>(values->size()); }
  inline const T& operator[](const int i) const { return (*values)[static_cast<std::size_t>(i)]; }
};

template <typename T>
// Shared for the same reason as BigIRTOwnedVector above.
struct BigIRTOwnedMatrix {
  int nr = 0, nc = 0;
  std::shared_ptr< std::vector<T> > values;
  BigIRTOwnedMatrix() : values(std::make_shared< std::vector<T> >()) {}
  template <typename M> explicit BigIRTOwnedMatrix(const M& x)
      : nr(x.nrow()), nc(x.ncol()),
        values(std::make_shared< std::vector<T> >(static_cast<std::size_t>(nr) * static_cast<std::size_t>(nc))) {
    for (int j = 0; j < nc; ++j) for (int i = 0; i < nr; ++i)
      (*values)[static_cast<std::size_t>(i + nr * j)] = x(i, j);
  }
  inline int nrow() const { return nr; }
  inline int ncol() const { return nc; }
  inline const T& operator()(const int i, const int j) const {
    return (*values)[static_cast<std::size_t>(i + nr * j)];
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
  BEGIN_RCPP

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
  Rcpp::IntegerVector status_code(Nsubs);
  Rcpp::NumericVector objective(Nsubs);
  Rcpp::NumericVector covariance;
  if (keep_covariance) {
    covariance = Rcpp::NumericVector(K * K * Nsubs);
    covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
  }

  struct BigIRTLaplacePersonWorker : public RcppParallel::Worker {
    const std::vector< std::vector<int> >& obs_by_subj;
    BigIRTOwnedVector<int> score;
    BigIRTOwnedMatrix<double> theta_init;
    BigIRTOwnedMatrix<double> ability_offset;
    BigIRTOwnedVector<double> b;
    BigIRTOwnedVector<double> c;
    BigIRTOwnedVector<double> d;
    BigIRTOwnedMatrix<double> loadings;
    BigIRTOwnedMatrix<double> prior_mean;
    BigIRTOwnedVector<double> prior_precision;
    BigIRTOwnedMatrix<int> free_mask;
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
    Rcpp::IntegerVector& status_code;
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
      Rcpp::IntegerVector& status_code,
      Rcpp::NumericVector& objective,
      Rcpp::NumericVector& covariance)
      : obs_by_subj(obs_by_subj), score(score), theta_init(theta_init),
        ability_offset(ability_offset), b(b), c(c), d(d), loadings(loadings),
        prior_mean(prior_mean), prior_precision(prior_precision), free_mask(free_mask),
        jitter(jitter), max_attempts(max_attempts), max_iter(max_iter), tol(tol),
        keep_covariance(keep_covariance), K(K), theta_mode(theta_mode),
        precision(precision), precision_chol(precision_chol),
        logdet_precision(logdet_precision), niter(niter), converged(converged), status_code(status_code),
        objective(objective), covariance(covariance) {}

    void operator()(std::size_t begin, std::size_t end) {
      const MatrixXd eye = MatrixXd::Identity(K, K);
      for (std::size_t subj_idx = begin; subj_idx < end; ++subj_idx) {
        const int subj = static_cast<int>(subj_idx);
        VectorXd theta(K);
        VectorXd mu(K);
        MatrixXd prior_prec = MatrixXd::Zero(K, K);
        for (int r = 0; r < K; ++r) for (int cc = 0; cc < K; ++cc)
          prior_prec(r, cc) = prior_precision[r + K * cc + K * K * subj];
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
        int subj_status = subj_converged ? 0 : 1;
        int used_iter = 0;
        for (int it = 0; it < max_iter && !subj_converged; ++it) {
          used_iter = it + 1;
          double gnorm = 0.0;
          for (size_t ai = 0; ai < active.size(); ++ai) gnorm += grad(active[ai]) * grad(active[ai]);
          gnorm = std::sqrt(gnorm);
          if (gnorm < tol) {
            subj_converged = true;
            subj_status = 0;
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
          if (!success) { subj_status = 4; break; }

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
          if (!accepted) { subj_status = 2; break; }
          theta = theta_try;
          grad = grad_try;
          prec = prec_try;
          lp = lp_try;
        }

        logpost_and_grad(theta, grad, prec, lp);
        if (!R_finite(lp) && subj_status < 3) subj_status = 3;
        objective[subj] = lp;
        niter[subj] = used_iter;
        converged[subj] = subj_converged;
        status_code[subj] = subj_status;

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
          status_code[subj] = 5;
          prec = MatrixXd::Identity(K, K) / jitter;
          llt_full.compute(prec);
        }

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
    logdet_precision, niter, converged, status_code, objective, covariance
  );
  worker(0, static_cast<std::size_t>(Nsubs));

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
    const double dq_deta = q * (1.0 - 2.0 * g);
    const double ds_deta = u * dq_deta;
    const double dr_deta = s * (1.0 - 2.0 * p);
    const double dw_deta = (2.0 * s * ds_deta * r - s * s * dr_deta) / (r * r);

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
    const double dq_deta = q * (1.0 - 2.0 * g);
    const double ds_deta = u * dq_deta;
    const double dr_deta = sc * (1.0 - 2.0 * pp);
    const double dw_deta = (2.0 * sc * ds_deta * r - sc * sc * dr_deta) / (r * r);
    const double hess_w = sc * sc / r;

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
    const double dq_deta = q * (1.0 - 2.0 * g);
    const double ds_deta = u * dq_deta;
    const double dr_deta = sc * (1.0 - 2.0 * pp);
    const double dw_deta = (2.0 * sc * ds_deta * r - sc * sc * dr_deta) / (r * r);
    const double hess_w = sc * sc / r;

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

extern "C" SEXP _bigIRT_laplace_ability_beta_mstep_cpp_impl(
    SEXP theta_residualSEXP, SEXP person_predSEXP, SEXP ability_betaSEXP,
    SEXP free_maskSEXP, SEXP beta_scaleSEXP, SEXP jitterSEXP) {
  BEGIN_RCPP
  Rcpp::NumericMatrix residual(theta_residualSEXP);
  Rcpp::NumericMatrix x(person_predSEXP);
  Rcpp::NumericMatrix beta(ability_betaSEXP);
  Rcpp::IntegerMatrix free_mask(free_maskSEXP);
  const int n = residual.nrow(), k = residual.ncol(), p = x.ncol();
  if(x.nrow() != n || beta.nrow() != k || beta.ncol() != p ||
     free_mask.nrow() != n || free_mask.ncol() != k)
    Rcpp::stop("Unexpected dimensions in Laplace ability-beta M-step.");
  const double scale = Rcpp::as<double>(beta_scaleSEXP);
  const double jitter = Rcpp::as<double>(jitterSEXP);
  Rcpp::NumericMatrix updated = Rcpp::clone(beta);
  Rcpp::NumericMatrix correction(k, p);
  double grad_sq = 0.0;
  for(int factor = 0; factor < k; ++factor) {
    Eigen::MatrixXd xtx = Eigen::MatrixXd::Zero(p, p);
    Eigen::VectorXd rhs = Eigen::VectorXd::Zero(p);
    for(int i = 0; i < n; ++i) {
      if(free_mask(i, factor) == 0) continue;
      for(int a = 0; a < p; ++a) {
        const double xa = x(i, a);
        rhs(a) += xa * residual(i, factor);
        for(int b = 0; b < p; ++b) xtx(a, b) += xa * x(i, b);
      }
    }
    grad_sq += rhs.squaredNorm();
    if(p == 0) continue;
    xtx.diagonal().array() += 1.0 / (scale * scale) + jitter;
    Eigen::LDLT<Eigen::MatrixXd> solver(xtx);
    if(solver.info() != Eigen::Success) Rcpp::stop("Ability-beta M-step normal equations failed.");
    const Eigen::VectorXd delta = solver.solve(rhs);
    if(solver.info() != Eigen::Success || !delta.allFinite()) Rcpp::stop("Ability-beta M-step solve failed.");
    for(int a = 0; a < p; ++a) {
      correction(factor, a) = delta(a);
      updated(factor, a) += delta(a);
    }
  }
  return Rcpp::List::create(Rcpp::Named("beta") = updated,
    Rcpp::Named("correction") = correction,
    Rcpp::Named("grad_norm") = std::sqrt(grad_sq));
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
    const std::vector< std::vector<int> >& obs_by_subj,
    const Rcpp::IntegerVector& B_ref) {
  const std::size_t n = obs_by_subj.size();
  std::vector<int> order(n);
  for (std::size_t i = 0; i < n; ++i) order[i] = static_cast<int>(i);
  if (n < 2 || B_ref.size() == 0) return order;

  const int nref = static_cast<int>(B_ref.size());
  std::vector< std::vector<int> > sig(n);
  for (std::size_t i = 0; i < n; ++i) {
    const std::vector<int>& obs = obs_by_subj[i];
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
  const std::vector< std::vector<int> >& obs_by_subj;
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

      const std::vector<int>& obs_idx = obs_by_subj[subj];
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
        for (int k = 0; k < K; ++k) trace_by_theta(k) += row_terms[oi].dw_deta * a(k) * aSa;
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
        const double ua = mode_adjoint.dot(a);

        for (int k = 0; k < K; ++k) {
          const double g_load = rt.grad_eta * z(k) -
            logdet_scale * 0.5 * (rt.dw_deta * z(k) * aSa + 2.0 * rt.w * Sa(k)) -
            0.5 * (mode_adjoint(k) * rt.grad_eta + ua * rt.dgrad_eta_deta * z(k));
          const double g_raw = g_load * row_a_mult[oi](k);
          if (A_ref(obs, k) > 0) grad_A[static_cast<std::size_t>(A_ref(obs, k) - 1)] += g_raw;
          const int a_beta_idx = A_ref(obs, k) > 0 ? (nA_beta_row == 1 ? 1 : A_ref(obs, k)) : 0;
          add_beta_grad(grad_A_beta, nA_beta_row, pA, A_pred, obs, a_beta_idx, g_raw);
        }

        const double g_b = -rt.grad_eta + logdet_scale * 0.5 * rt.dw_deta * aSa +
          0.5 * ua * rt.dgrad_eta_deta;
        if (B_ref[obs] > 0) grad_B[static_cast<std::size_t>(B_ref[obs] - 1)] += g_b;
        add_beta_grad(grad_B_beta, nB_beta_row, pB, B_pred, obs, b_beta_idx, g_b);

        const double g_c_raw = (rt.grad_c - logdet_scale * 0.5 * rt.dw_dc * aSa -
          0.5 * ua * rt.dgrad_eta_dc) * row_c_mult[oi];
        if (C_ref[obs] > 0) grad_C[static_cast<std::size_t>(C_ref[obs] - 1)] += g_c_raw;
        add_beta_grad(grad_C_beta, nC_beta_row, pC, C_pred, obs, c_beta_idx, g_c_raw);

        const double g_d_raw = (rt.grad_d - logdet_scale * 0.5 * rt.dw_dd * aSa -
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
  std::vector< std::vector<int> > obs_by_subj;
  std::vector<int> subject_order;
};

extern "C" SEXP _bigIRT_laplace_item_prepare_cpp_impl(
    SEXP idSEXP, SEXP scoreSEXP, SEXP row_abilitySEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_beta_rowSEXP, SEXP A_predSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_beta_rowSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_beta_rowSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_beta_rowSEXP, SEXP D_predSEXP,
    SEXP prior_precisionSEXP, SEXP jitterSEXP, SEXP max_attemptsSEXP) {
  BEGIN_RCPP
  BigIRTItemPrepared* d = new BigIRTItemPrepared();
  d->id = Rcpp::IntegerVector(idSEXP);
  d->score = Rcpp::IntegerVector(scoreSEXP);
  d->row_ability = Rcpp::NumericMatrix(row_abilitySEXP);
  d->A_ref = Rcpp::IntegerMatrix(A_refSEXP);
  d->A_fixed_value = Rcpp::NumericMatrix(A_fixed_valueSEXP);
  d->A_beta_row = Rcpp::IntegerMatrix(A_beta_rowSEXP);
  d->A_pred = Rcpp::NumericMatrix(A_predSEXP);
  d->B_ref = Rcpp::IntegerVector(B_refSEXP);
  d->B_fixed_value = Rcpp::NumericVector(B_fixed_valueSEXP);
  d->B_beta_row = Rcpp::IntegerVector(B_beta_rowSEXP);
  d->B_pred = Rcpp::NumericMatrix(B_predSEXP);
  d->C_ref = Rcpp::IntegerVector(C_refSEXP);
  d->C_fixed_value = Rcpp::NumericVector(C_fixed_valueSEXP);
  d->C_beta_row = Rcpp::IntegerVector(C_beta_rowSEXP);
  d->C_pred = Rcpp::NumericMatrix(C_predSEXP);
  d->D_ref = Rcpp::IntegerVector(D_refSEXP);
  d->D_fixed_value = Rcpp::NumericVector(D_fixed_valueSEXP);
  d->D_beta_row = Rcpp::IntegerVector(D_beta_rowSEXP);
  d->D_pred = Rcpp::NumericMatrix(D_predSEXP);
  d->prior_precision = Rcpp::NumericVector(prior_precisionSEXP);
  d->jitter = Rcpp::as<double>(jitterSEXP);
  d->max_attempts = Rcpp::as<int>(max_attemptsSEXP);
  d->K = d->row_ability.ncol();
  d->Nsubs = Rcpp::as<Rcpp::IntegerVector>(d->prior_precision.attr("dim"))[2];

  const int Nobs = d->id.size();
  d->obs_by_subj.assign(static_cast<std::size_t>(d->Nsubs), std::vector<int>());
  for (int obs = 0; obs < Nobs; ++obs) {
    const int subj = d->id[obs] - 1;
    if (subj < 0 || subj >= d->Nsubs) {
      delete d;
      Rcpp::stop("id must be coded from 1 to Nsubs.");
    }
    d->obs_by_subj[static_cast<std::size_t>(subj)].push_back(obs);
  }
  // Natural subject order by default.
  //
  // Grouping subjects by the items they answered was an attempt to shrink the
  // slice of the item bank each chunk touches. Measured, it costs 8 to 18 per
  // cent and lowers the speedup on 16 cores from 2.84x to 2.67x, so it is off.
  // The reasoning behind it was wrong: the item parameter arrays are a couple
  // of thousand doubles and already sit in cache, while permuting subjects
  // destroys sequential access to the per-observation arrays, which have a row
  // per response and are what actually loads the memory system. It optimised
  // locality on the small arrays at the expense of the large ones.
  //
  // Set BIGIRT_LOCALITY_ORDER to re-enable it, for a design where the item bank
  // is large enough that the trade might reverse.
  if (std::getenv("BIGIRT_LOCALITY_ORDER") != nullptr) {
    d->subject_order = bigIRT_subject_locality_order(d->obs_by_subj, d->B_ref);
  } else {
    d->subject_order.resize(d->obs_by_subj.size());
    for (std::size_t z = 0; z < d->subject_order.size(); ++z)
      d->subject_order[z] = static_cast<int>(z);
  }

  Rcpp::XPtr<BigIRTItemPrepared> ptr(d, true);
  return ptr;
  END_RCPP
}

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
  Rcpp::IntegerVector status_code(Nsubs);
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
    RcppParallel::RVector<int> status_code;
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
          // Rcpp::stop here would longjmp out of a worker thread, which is
          // undefined behaviour. Record it instead, as the serial kernel does.
          if (!success) { subj_status = 4; break; }

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
          if (!accepted) { subj_status = 2; break; }
          theta = theta_try;
          grad = grad_try;
          prec = prec_try;
          lp = lp_try;
        }

        logpost_and_grad(theta, grad, prec, lp);
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
  const bool has_item_predictors =
    A_pred.ncol() > 0 || B_pred.ncol() > 0 || C_pred.ncol() > 0 || D_pred.ncol() > 0;
  if (has_item_predictors) {
    worker(0, static_cast<std::size_t>(Nsubs));
  } else {
    RcppParallel::parallelFor(static_cast<std::size_t>(0), static_cast<std::size_t>(Nsubs), worker, grain_size);
  }

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
  BEGIN_RCPP

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
  END_RCPP
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
