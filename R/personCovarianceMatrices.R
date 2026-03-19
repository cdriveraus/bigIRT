#' Convert row loadings to a sparse row representation
#'
#' Converts an \eqn{N_{obs} \times K} matrix of effective row loadings into a
#' compact sparse representation suitable for fast row-wise accumulation in C++.
#' Each row can correspond to a fully moderated loading vector after any item or
#' person covariate effects have already been applied.
#'
#' @param loadings A numeric matrix with one row per observation and one column
#'   per latent dimension.
#' @param tol Values with absolute magnitude less than or equal to \code{tol} are
#'   treated as structural zeros.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{row_ptr}}{Integer vector of length \code{Nobs + 1}. Entries mark
#'   the start of each row in \code{load_idx} and \code{load_val}.}
#'   \item{\code{load_idx}}{1-based latent-dimension indices for non-zero
#'   loadings.}
#'   \item{\code{load_val}}{Non-zero loading values.}
#'   \item{\code{n_dim}}{Number of latent dimensions.}
#' }
#'
#' @export
rowLoadingsToSparse <- function(loadings, tol = 0){
  if(!is.matrix(loadings)) stop("loadings must be a matrix.")
  if(!is.numeric(loadings)) stop("loadings must be numeric.")

  Nobs <- nrow(loadings)
  K <- ncol(loadings)
  row_ptr <- integer(Nobs + 1L)
  load_idx <- integer()
  load_val <- numeric()
  cursor <- 1L
  row_ptr[1] <- cursor

  for(i in seq_len(Nobs)){
    nz <- which(abs(loadings[i,]) > tol)
    if(length(nz)){
      load_idx <- c(load_idx, nz)
      load_val <- c(load_val, loadings[i, nz])
      cursor <- cursor + length(nz)
    }
    row_ptr[i + 1L] <- cursor
  }

  list(
    row_ptr = as.integer(row_ptr),
    load_idx = as.integer(load_idx),
    load_val = as.numeric(load_val),
    n_dim = as.integer(K)
  )
}

bigIRT_prior_precision_array <- function(prior_precision, Nsubs, K){
  if(is.null(prior_precision)){
    prior_precision <- diag(K)
  }

  if(is.matrix(prior_precision)){
    if(!all(dim(prior_precision) == c(K, K))){
      stop("prior_precision matrix must have dimensions K x K.")
    }
    out <- array(0, dim = c(K, K, Nsubs))
    for(i in seq_len(Nsubs)) out[,,i] <- prior_precision
    return(out)
  }

  if(is.array(prior_precision) && length(dim(prior_precision)) == 3L){
    if(!all(dim(prior_precision) == c(K, K, Nsubs))){
      stop("prior_precision array must have dimensions K x K x Nsubs.")
    }
    return(prior_precision)
  }

  stop("prior_precision must be NULL, a K x K matrix, or a K x K x Nsubs array.")
}

bigIRT_normalise_sparse_loadings <- function(loadings, Nobs, K){
  if(is.matrix(loadings)){
    return(rowLoadingsToSparse(loadings))
  }

  if(is.list(loadings) && all(c("row_ptr", "load_idx", "load_val") %in% names(loadings))){
    if(length(loadings$row_ptr) != (Nobs + 1L)){
      stop("Sparse loadings$row_ptr must have length Nobs + 1.")
    }
    if(length(loadings$load_idx) != length(loadings$load_val)){
      stop("Sparse loadings$load_idx and loadings$load_val must have the same length.")
    }
    if(any(loadings$load_idx < 1L | loadings$load_idx > K)){
      stop("Sparse loadings$load_idx contains invalid latent-dimension indices.")
    }
    return(list(
      row_ptr = as.integer(loadings$row_ptr),
      load_idx = as.integer(loadings$load_idx),
      load_val = as.numeric(loadings$load_val),
      n_dim = as.integer(K)
    ))
  }

  stop("loadings must be either an Nobs x K matrix or a sparse list from rowLoadingsToSparse().")
}

bigIRT_attach_covariance_dimnames <- function(out, theta_mean, id_levels){
  K <- ncol(theta_mean)
  dim_names <- colnames(theta_mean)
  if(is.null(dim_names)) dim_names <- paste0("dim", seq_len(K))
  person_names <- as.character(id_levels)

  dimnames(out$covariance) <- list(dim_names, dim_names, person_names)
  dimnames(out$precision) <- list(dim_names, dim_names, person_names)
  dimnames(out$precision_chol) <- list(dim_names, dim_names, person_names)
  out
}

bigIRT_get_person_covariance_cpp <- local({
  cpp_fun <- NULL

  function(){
    if(!is.null(cpp_fun)) return(cpp_fun)

    Rcpp::cppFunction(
      depends = "RcppEigen",
      env = environment(),
      code = '
        #include <RcppEigen.h>
        using Eigen::MatrixXd;
        using Eigen::VectorXd;

        inline double stable_inv_logit(const double x) {
          if (x >= 0.0) {
            const double z = std::exp(-x);
            return 1.0 / (1.0 + z);
          } else {
            const double z = std::exp(x);
            return z / (1.0 + z);
          }
        }

        // [[Rcpp::export]]
        Rcpp::List person_covariance_cpp_impl(
            const Rcpp::IntegerVector& id,
            const Rcpp::NumericMatrix& theta_mean,
            const Rcpp::NumericVector& b,
            const Rcpp::NumericVector& c,
            const Rcpp::NumericVector& d,
            const Rcpp::IntegerVector& row_ptr,
            const Rcpp::IntegerVector& load_idx,
            const Rcpp::NumericVector& load_val,
            const Rcpp::NumericVector& prior_precision,
            const double jitter = 1e-8,
            const int max_attempts = 8) {

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
            std::vector<int> dims;
            std::vector<double> vals;
            dims.reserve(std::max(1, end - start));
            vals.reserve(std::max(1, end - start));

            double eta = -b[obs];
            for (int ptr = start; ptr < end; ++ptr) {
              const int k = load_idx[ptr] - 1;
              const double aval = load_val[ptr];
              dims.push_back(k);
              vals.push_back(aval);
              eta += aval * theta_mean(subj, k);
            }

            const double g = stable_inv_logit(eta);
            const double p = c[obs] + (d[obs] - c[obs]) * g;
            const double dp = (d[obs] - c[obs]) * g * (1.0 - g);
            const double denom = std::max(p * (1.0 - p), 1e-12);
            const double w = (dp * dp) / denom;

            if (w <= 0.0 || !R_finite(w)) continue;

            for (std::size_t ii = 0; ii < dims.size(); ++ii) {
              const int ki = dims[ii];
              const double avi = vals[ii];
              for (std::size_t jj = 0; jj < dims.size(); ++jj) {
                const int kj = dims[jj];
                precision_store[subj](ki, kj) += w * avi * vals[jj];
              }
            }
          }

          Rcpp::NumericVector covariance(K * K * Nsubs);
          covariance.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
          Rcpp::NumericVector precision(K * K * Nsubs);
          precision.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
          Rcpp::NumericVector precision_chol(K * K * Nsubs);
          precision_chol.attr("dim") = Rcpp::IntegerVector::create(K, K, Nsubs);
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

            MatrixXd L = llt.matrixL();
            MatrixXd Sigma = llt.solve(eye);
            chol_jitter_used[s] = chol_jitter;

            for (int r = 0; r < K; ++r) {
              for (int cidx = 0; cidx < K; ++cidx) {
                covariance[r + K * cidx + K * K * s] = Sigma(r, cidx);
                precision[r + K * cidx + K * K * s] = Q(r, cidx);
                precision_chol[r + K * cidx + K * K * s] = L(r, cidx);
              }
            }
          }

          return Rcpp::List::create(
            Rcpp::Named("covariance") = covariance,
            Rcpp::Named("precision") = precision,
            Rcpp::Named("precision_chol") = precision_chol,
            Rcpp::Named("chol_jitter_used") = chol_jitter_used
          );
        }
      '
    )

    cpp_fun <<- person_covariance_cpp_impl
    cpp_fun
  }
})

#' Compute person covariance matrices with a pure R reference implementation
#'
#' This function mirrors \code{\link{personCovarianceMatrices}} but performs the
#' row-wise precision accumulation and per-person matrix inversions in R. It is
#' intended for sanity checks against the embedded C++ implementation.
#'
#' @inheritParams personCovarianceMatrices
#'
#' @return A list with per-person posterior \code{covariance},
#'   \code{precision}, and \code{precision_chol} arrays.
#'
#' @export
personCovarianceMatrices_R <- function(id, theta_mean, b, loadings, c = NULL, d = NULL,
  prior_precision = NULL, jitter = 1e-8, max_attempts = 8){

  theta_mean <- as.matrix(theta_mean)
  if(!is.numeric(theta_mean)) stop("theta_mean must be numeric.")
  Nsubs <- nrow(theta_mean)
  K <- ncol(theta_mean)
  Nobs <- length(id)

  if(length(b) != Nobs) stop("b must have length Nobs.")
  if(is.null(c)) c <- rep(0, Nobs)
  if(is.null(d)) d <- rep(1, Nobs)
  if(length(c) != Nobs || length(d) != Nobs) stop("c and d must have length Nobs.")

  id_factor <- factor(id)
  id_index <- as.integer(id_factor)
  if(length(levels(id_factor)) != Nsubs){
    stop("theta_mean must have one row per unique person id.")
  }

  sparse <- bigIRT_normalise_sparse_loadings(loadings, Nobs = Nobs, K = K)
  prior_precision_arr <- bigIRT_prior_precision_array(prior_precision, Nsubs = Nsubs, K = K)

  precision <- prior_precision_arr
  for(obs in seq_len(Nobs)){
    subj <- id_index[obs]
    idx <- seq.int(sparse$row_ptr[obs], sparse$row_ptr[obs + 1L] - 1L)
    eta <- -b[obs]
    if(length(idx)){
      dims <- sparse$load_idx[idx]
      vals <- sparse$load_val[idx]
      eta <- eta + sum(vals * theta_mean[subj, dims])
    } else {
      dims <- integer()
      vals <- numeric()
    }

    g <- plogis(eta)
    p <- c[obs] + (d[obs] - c[obs]) * g
    dp <- (d[obs] - c[obs]) * g * (1 - g)
    w <- (dp * dp) / max(p * (1 - p), 1e-12)
    if(!is.finite(w) || w <= 0) next

    if(length(dims)){
      precision[dims, dims, subj] <- precision[dims, dims, subj] + w * tcrossprod(vals)
    }
  }

  covariance <- array(0, dim = c(K, K, Nsubs))
  precision_chol <- array(0, dim = c(K, K, Nsubs))
  chol_jitter_used <- numeric(Nsubs)

  for(subj in seq_len(Nsubs)){
    chol_jitter <- jitter
    chol_ok <- FALSE
    Q <- precision[,,subj]
    for(attempt in seq_len(max_attempts)){
      Q_try <- Q
      diag(Q_try) <- diag(Q_try) + chol_jitter
      Rchol <- try(chol(Q_try), silent = TRUE)
      if(!inherits(Rchol, "try-error")){
        Q <- Q_try
        covariance[,,subj] <- chol2inv(Rchol)
        precision_chol[,,subj] <- t(Rchol)
        chol_jitter_used[subj] <- chol_jitter
        precision[,,subj] <- Q
        chol_ok <- TRUE
        break
      }
      chol_jitter <- chol_jitter * 10
    }
    if(!chol_ok) stop("Cholesky failed while building person covariance matrices.")
  }

  out <- list(
    covariance = covariance,
    precision = precision,
    precision_chol = precision_chol,
    chol_jitter_used = chol_jitter_used,
    id_levels = levels(id_factor),
    backend = "R"
  )
  bigIRT_attach_covariance_dimnames(out, theta_mean, levels(id_factor))
}

#' Compute person covariance matrices from row-level moderated IRT parameters
#'
#' Builds a posterior precision matrix for each person by combining a prior
#' precision matrix with row-wise Fisher information contributions. The intended
#' input is the row-level moderated parameter state already produced inside the
#' Stan likelihood:
#' \itemize{
#'   \item a current person mean matrix \code{theta_mean}
#'   \item row-level effective difficulties \code{b}
#'   \item row-level effective lower and upper asymptotes \code{c} and \code{d}
#'   \item row-level effective loading vectors, supplied either densely or in
#'   sparse row form
#' }
#'
#' For each response row \eqn{r} attached to person \eqn{j}, the function
#' computes
#' \deqn{Q_j = Q_{0,j} + \sum_r w_r a_r a_r^\top}
#' where \eqn{Q_{0,j}} is the prior precision for person \eqn{j}, \eqn{a_r} is
#' the row-level effective loading vector, and
#' \deqn{w_r = \frac{(p_r'(\eta_r))^2}{p_r(\eta_r)(1-p_r(\eta_r))}}
#' is the Bernoulli Fisher-information weight. This is stable for 2PL, 3PL, 4PL,
#' and sparse multidimensional loadings.
#'
#' The embedded C++ backend returns the posterior precision, its lower Cholesky
#' factor, and the implied covariance matrix for each person. The pure R version
#' \code{\link{personCovarianceMatrices_R}} mirrors the same calculations for
#' sanity checks.
#'
#' @param id A vector of person ids with one entry per observation row.
#' @param theta_mean A numeric \eqn{N_{subs} \times K} matrix of current person
#'   means. Rows must correspond to the sorted unique values of \code{id}.
#' @param b Numeric vector of row-level effective difficulties.
#' @param loadings Either an \eqn{N_{obs} \times K} numeric matrix of row-level
#'   effective loadings, or a sparse list returned by
#'   \code{\link{rowLoadingsToSparse}}.
#' @param c Numeric vector of row-level lower asymptotes. Defaults to all zeros.
#' @param d Numeric vector of row-level upper asymptotes. Defaults to all ones.
#' @param prior_precision Either \code{NULL}, a shared \eqn{K \times K} prior
#'   precision matrix, or a person-specific \eqn{K \times K \times N_{subs}}
#'   array.
#' @param jitter Initial diagonal jitter added before Cholesky factorisation.
#' @param max_attempts Maximum number of jitter expansions before failing.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{covariance}}{Posterior covariance array with dimensions
#'   \code{K x K x Nsubs}.}
#'   \item{\code{precision}}{Posterior precision array with dimensions
#'   \code{K x K x Nsubs}.}
#'   \item{\code{precision_chol}}{Lower Cholesky factor of the posterior
#'   precision for each person.}
#'   \item{\code{chol_jitter_used}}{The diagonal jitter needed for each
#'   person-specific factorisation.}
#'   \item{\code{id_levels}}{Unique person ids corresponding to the third array
#'   dimension.}
#'   \item{\code{backend}}{Either \code{"cpp"} or \code{"R"}.}
#' }
#'
#' @examples
#' # Two-dimensional row-level example with one cross-loading row.
#' theta_mean <- rbind(
#'   c(-0.3, 0.4),
#'   c(0.6, -0.2)
#' )
#' colnames(theta_mean) <- c("math", "verbal")
#'
#' row_loadings <- rbind(
#'   c(1.2, 0.0),
#'   c(0.8, 0.3),
#'   c(0.0, 1.1),
#'   c(0.5, 0.6)
#' )
#'
#' ids <- c("p1", "p1", "p2", "p2")
#' b <- c(-0.5, 0.1, 0.0, -0.2)
#' cpar <- rep(0, length(ids))
#' dpar <- rep(1, length(ids))
#'
#' sparse_loadings <- rowLoadingsToSparse(row_loadings)
#' out_cpp <- personCovarianceMatrices(
#'   id = ids,
#'   theta_mean = theta_mean,
#'   b = b,
#'   c = cpar,
#'   d = dpar,
#'   loadings = sparse_loadings,
#'   prior_precision = diag(2)
#' )
#'
#' out_r <- personCovarianceMatrices_R(
#'   id = ids,
#'   theta_mean = theta_mean,
#'   b = b,
#'   c = cpar,
#'   d = dpar,
#'   loadings = sparse_loadings,
#'   prior_precision = diag(2)
#' )
#'
#' max(abs(out_cpp$covariance - out_r$covariance))
#' out_cpp$covariance[, , "p1"]
#'
#' @export
personCovarianceMatrices <- function(id, theta_mean, b, loadings, c = NULL, d = NULL,
  prior_precision = NULL, jitter = 1e-8, max_attempts = 8){

  theta_mean <- as.matrix(theta_mean)
  if(!is.numeric(theta_mean)) stop("theta_mean must be numeric.")
  Nsubs <- nrow(theta_mean)
  K <- ncol(theta_mean)
  Nobs <- length(id)

  if(length(b) != Nobs) stop("b must have length Nobs.")
  if(is.null(c)) c <- rep(0, Nobs)
  if(is.null(d)) d <- rep(1, Nobs)
  if(length(c) != Nobs || length(d) != Nobs) stop("c and d must have length Nobs.")

  id_factor <- factor(id)
  id_index <- as.integer(id_factor)
  if(length(levels(id_factor)) != Nsubs){
    stop("theta_mean must have one row per unique person id.")
  }

  sparse <- bigIRT_normalise_sparse_loadings(loadings, Nobs = Nobs, K = K)
  prior_precision_arr <- bigIRT_prior_precision_array(prior_precision, Nsubs = Nsubs, K = K)

  cpp_fun <- bigIRT_get_person_covariance_cpp()
  out <- cpp_fun(
    id = id_index,
    theta_mean = theta_mean,
    b = as.numeric(b),
    c = as.numeric(c),
    d = as.numeric(d),
    row_ptr = sparse$row_ptr,
    load_idx = sparse$load_idx,
    load_val = sparse$load_val,
    prior_precision = prior_precision_arr,
    jitter = jitter,
    max_attempts = as.integer(max_attempts)
  )

  out$id_levels <- levels(id_factor)
  out$backend <- "cpp"
  bigIRT_attach_covariance_dimnames(out, theta_mean, levels(id_factor))
}
