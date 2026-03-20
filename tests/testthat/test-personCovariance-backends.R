if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("personCovarianceMatrices cpp and R backends agree on sparse MIRT fixture", {
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

    out_cpp <- personCovarianceMatrices(
      id = id,
      theta_mean = theta_mean,
      b = b,
      c = cpar,
      d = dpar,
      loadings = sparse,
      prior_precision = prior_precision
    )
    out_r <- personCovarianceMatrices_R(
      id = id,
      theta_mean = theta_mean,
      b = b,
      c = cpar,
      d = dpar,
      loadings = sparse,
      prior_precision = prior_precision
    )

    expect_equal(out_cpp$backend, "cpp")
    expect_equal(out_r$backend, "R")
    expect_equal(out_cpp$id_levels, out_r$id_levels)
    expect_equal(out_cpp$covariance, out_r$covariance, tolerance = 1e-8)
    expect_equal(out_cpp$precision, out_r$precision, tolerance = 1e-8)
    expect_equal(out_cpp$precision_chol, out_r$precision_chol, tolerance = 1e-8)
  })
}
