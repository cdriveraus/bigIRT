if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("direct cpp sigma points agree with covariance-based routes", {
    theta_mean <- rbind(
      c(-0.2, 0.3),
      c(0.5, -0.1),
      c(0.0, 0.4)
    )
    colnames(theta_mean) <- c("s1", "s2")

    id <- c("p1", "p1", "p2", "p2", "p3", "p3")
    b <- c(-0.4, 0.2, 0.1, -0.3, 0.0, 0.25)
    cpar <- c(0.00, 0.05, 0.00, 0.02, 0.01, 0.00)
    dpar <- c(1.00, 0.95, 1.00, 0.98, 0.97, 1.00)
    row_loadings <- rbind(
      c(1.1, 0.0),
      c(0.7, 0.2),
      c(0.0, 1.2),
      c(0.4, 0.6),
      c(0.9, 0.1),
      c(0.2, 0.8)
    )
    sparse <- rowLoadingsToSparse(row_loadings)
    prior_precision <- diag(c(1.2, 0.9))
    jitter <- 1e-8
    sigma_scale <- 0.5

    sigma_cpp <- bigIRT:::bigIRT_person_sigma_points_cpp(
      id = id,
      theta_mean = theta_mean,
      b = b,
      c = cpar,
      d = dpar,
      loadings = sparse,
      prior_precision = prior_precision,
      jitter = jitter,
      sigma_scale = sigma_scale
    )

    cov_cpp <- personCovarianceMatrices(
      id = id,
      theta_mean = theta_mean,
      b = b,
      c = cpar,
      d = dpar,
      loadings = sparse,
      prior_precision = prior_precision,
      jitter = jitter,
      return_precision = FALSE
    )
    cov_r <- personCovarianceMatrices_R(
      id = id,
      theta_mean = theta_mean,
      b = b,
      c = cpar,
      d = dpar,
      loadings = sparse,
      prior_precision = prior_precision,
      jitter = jitter,
      return_precision = FALSE
    )

    Nsubs <- nrow(theta_mean)
    K <- ncol(theta_mean)
    Nsamp <- 2 * K + 1
    old_sigma_from_cov_cpp <- array(0, dim = c(Nsubs, K, Nsamp))
    old_sigma_from_cov_r <- array(0, dim = c(Nsubs, K, Nsamp))
    for(i in seq_len(Nsubs)){
      sp_cpp <- bigIRT:::bigIRT_sigma_points(theta_mean[i,], cov_cpp$covariance[,,i], jitter = jitter, sigmaScale = sigma_scale)
      sp_r <- bigIRT:::bigIRT_sigma_points(theta_mean[i,], cov_r$covariance[,,i], jitter = jitter, sigmaScale = sigma_scale)
      old_sigma_from_cov_cpp[i,,] <- t(sp_cpp$points)
      old_sigma_from_cov_r[i,,] <- t(sp_r$points)
    }

    expect_equal(as.numeric(sigma_cpp$weights), as.numeric(sp_cpp$weights), tolerance = 1e-12)
    expect_equal(sigma_cpp$ability_samples, old_sigma_from_cov_cpp, tolerance = 1e-8)
    expect_equal(sigma_cpp$ability_samples, old_sigma_from_cov_r, tolerance = 1e-8)
    expect_equal(
      unname(as.matrix(sigma_cpp$cov_mean)),
      unname(apply(cov_cpp$covariance, c(1,2), mean)),
      tolerance = 1e-8
    )
    expect_equal(
      unname(as.matrix(sigma_cpp$posterior_sd)),
      unname(t(sapply(seq_len(Nsubs), function(i) sqrt(pmax(diag(cov_cpp$covariance[,,i]), 0))))),
      tolerance = 1e-8
    )
  })
}
