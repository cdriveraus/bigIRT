if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("wleIRT returns scale-specific finite estimates for MIRT fits", {
    set.seed(404)
    sim <- simIRT(
      Nsubs = 40,
      Nitems = 6,
      Nscales = 2,
      NitemsAnswered = 6,
      ASD = 0.1,
      AMean = 1,
      BSD = 0.7,
      BMean = 0,
      logitCSD = 0,
      logitCMean = -20
    )

    fit <- fitIRT(
      sim$dat,
      pl = 2,
      cores = 1,
      iter = 25,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    out <- wleIRT(fit)
    expect_equal(dim(out$wle), c(fit$dat$Nsubs, fit$dat$Nscales))
    expect_equal(dim(out$wleSE), c(fit$dat$Nsubs, fit$dat$Nscales))
    expect_true(all(is.finite(out$wle)))
    expect_true(all(is.finite(out$wleSE)))
  })
}
