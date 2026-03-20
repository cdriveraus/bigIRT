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
  if(!is.null(out$precision)){
    dimnames(out$precision) <- list(dim_names, dim_names, person_names)
  }
  if(!is.null(out$precision_chol)){
    dimnames(out$precision_chol) <- list(dim_names, dim_names, person_names)
  }
  out
}

bigIRT_person_covariance_cpp_impl <- function(id, theta_mean, b, c, d, row_ptr,
  load_idx, load_val, prior_precision, jitter, max_attempts, return_precision = TRUE){
  .Call(
    `_bigIRT_person_covariance_cpp_impl`,
    id, theta_mean, b, c, d, row_ptr, load_idx, load_val, prior_precision,
    jitter, max_attempts, as.logical(return_precision)
  )
}

bigIRT_person_sigma_points_cpp_impl <- function(id, theta_mean, b, c, d, row_ptr,
  load_idx, load_val, prior_precision, jitter, max_attempts, sigma_scale){
  .Call(
    `_bigIRT_person_sigma_points_cpp_impl`,
    id, theta_mean, b, c, d, row_ptr, load_idx, load_val, prior_precision,
    jitter, max_attempts, as.numeric(sigma_scale)
  )
}

bigIRT_person_sigma_points_cpp_dense_impl <- function(id, theta_mean, b, c, d, loadings,
  prior_precision, jitter, max_attempts, sigma_scale){
  .Call(
    `_bigIRT_person_sigma_points_cpp_dense_impl`,
    id, theta_mean, b, c, d, loadings, prior_precision,
    jitter, max_attempts, as.numeric(sigma_scale)
  )
}

bigIRT_person_sigma_points_cpp <- function(id, theta_mean, b, loadings, c = NULL, d = NULL,
  prior_precision = NULL, jitter = 1e-8, max_attempts = 8, sigma_scale = 0.5){
  theta_mean <- as.matrix(theta_mean)
  if(!is.numeric(theta_mean)) stop("theta_mean must be numeric.")
  Nsubs <- nrow(theta_mean)
  K <- ncol(theta_mean)
  Nobs <- length(id)

  if(length(b) != Nobs) stop("b must have length Nobs.")
  if(is.null(c)) c <- rep(0, Nobs)
  if(is.null(d)) d <- rep(1, Nobs)
  if(length(c) != Nobs || length(d) != Nobs) stop("c and d must have length Nobs.")

  if(is.integer(id) && length(id) == Nobs && all(id >= 1L) && max(id) == Nsubs){
    id_index <- as.integer(id)
    id_levels <- as.character(seq_len(Nsubs))
  } else {
    id_factor <- factor(id)
    id_index <- as.integer(id_factor)
    id_levels <- levels(id_factor)
    if(length(id_levels) != Nsubs){
      stop("theta_mean must have one row per unique person id.")
    }
  }

  prior_precision_arr <- bigIRT_prior_precision_array(prior_precision, Nsubs = Nsubs, K = K)
  if(is.matrix(loadings)){
    if(nrow(loadings) != Nobs || ncol(loadings) != K){
      stop("loadings matrix must have dimensions Nobs x K.")
    }
    out <- bigIRT_person_sigma_points_cpp_dense_impl(
      id = id_index,
      theta_mean = theta_mean,
      b = as.numeric(b),
      c = as.numeric(c),
      d = as.numeric(d),
      loadings = loadings,
      prior_precision = prior_precision_arr,
      jitter = jitter,
      max_attempts = as.integer(max_attempts),
      sigma_scale = sigma_scale
    )
  } else {
    sparse <- bigIRT_normalise_sparse_loadings(loadings, Nobs = Nobs, K = K)
    out <- bigIRT_person_sigma_points_cpp_impl(
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
      max_attempts = as.integer(max_attempts),
      sigma_scale = sigma_scale
    )
  }

  out$id_levels <- id_levels
  out$backend <- "cpp_sigma"
  out
}

#' Compute person covariance matrices with a pure R reference implementation
#'
#' This function mirrors \code{\link{personCovarianceMatrices}} but performs the
#' row-wise precision accumulation and per-person matrix inversions in R. It is
#' intended for sanity checks against the embedded C++ implementation.
#'
#' @inheritParams personCovarianceMatrices
#'
#' @return A list with posterior \code{covariance}; and if
#'   \code{return_precision=TRUE}, also \code{precision} and
#'   \code{precision_chol} arrays.
#'
#' @export
personCovarianceMatrices_R <- function(id, theta_mean, b, loadings, c = NULL, d = NULL,
  prior_precision = NULL, jitter = 1e-8, max_attempts = 8, return_precision = TRUE){

  theta_mean <- as.matrix(theta_mean)
  if(!is.numeric(theta_mean)) stop("theta_mean must be numeric.")
  Nsubs <- nrow(theta_mean)
  K <- ncol(theta_mean)
  Nobs <- length(id)

  if(length(b) != Nobs) stop("b must have length Nobs.")
  if(is.null(c)) c <- rep(0, Nobs)
  if(is.null(d)) d <- rep(1, Nobs)
  if(length(c) != Nobs || length(d) != Nobs) stop("c and d must have length Nobs.")

  if(is.integer(id) && length(id) == Nobs && all(id >= 1L) && max(id) == Nsubs){
    id_index <- as.integer(id)
    id_levels <- as.character(seq_len(Nsubs))
  } else {
    id_factor <- factor(id)
    id_index <- as.integer(id_factor)
    id_levels <- levels(id_factor)
    if(length(id_levels) != Nsubs){
      stop("theta_mean must have one row per unique person id.")
    }
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
  precision_chol <- if(isTRUE(return_precision)) array(0, dim = c(K, K, Nsubs)) else NULL
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
        if(isTRUE(return_precision)) precision_chol[,,subj] <- t(Rchol)
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
    precision = if(isTRUE(return_precision)) precision else NULL,
    precision_chol = precision_chol,
    chol_jitter_used = chol_jitter_used,
    id_levels = id_levels,
    backend = "R"
  )
  bigIRT_attach_covariance_dimnames(out, theta_mean, id_levels)
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
#' @param return_precision Logical. If \code{TRUE}, also return posterior
#'   precision and precision Cholesky arrays. If \code{FALSE}, compute and return
#'   only covariance (plus jitter diagnostics).
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
  prior_precision = NULL, jitter = 1e-8, max_attempts = 8, return_precision = TRUE){

  theta_mean <- as.matrix(theta_mean)
  if(!is.numeric(theta_mean)) stop("theta_mean must be numeric.")
  Nsubs <- nrow(theta_mean)
  K <- ncol(theta_mean)
  Nobs <- length(id)

  if(length(b) != Nobs) stop("b must have length Nobs.")
  if(is.null(c)) c <- rep(0, Nobs)
  if(is.null(d)) d <- rep(1, Nobs)
  if(length(c) != Nobs || length(d) != Nobs) stop("c and d must have length Nobs.")

  if(is.integer(id) && length(id) == Nobs && all(id >= 1L) && max(id) == Nsubs){
    id_index <- as.integer(id)
    id_levels <- as.character(seq_len(Nsubs))
  } else {
    id_factor <- factor(id)
    id_index <- as.integer(id_factor)
    id_levels <- levels(id_factor)
    if(length(id_levels) != Nsubs){
      stop("theta_mean must have one row per unique person id.")
    }
  }

  sparse <- bigIRT_normalise_sparse_loadings(loadings, Nobs = Nobs, K = K)
  prior_precision_arr <- bigIRT_prior_precision_array(prior_precision, Nsubs = Nsubs, K = K)

  out <- bigIRT_person_covariance_cpp_impl(
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
    max_attempts = as.integer(max_attempts),
    return_precision = return_precision
  )

  out$id_levels <- id_levels
  out$backend <- "cpp"
  bigIRT_attach_covariance_dimnames(out, theta_mean, id_levels)
}
