if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  ## The design has to be able to support a weighted likelihood estimate at all.
  ## An earlier version of this test used six items across two scales, which is
  ## three per scale: the gradient has no interior root for most people there,
  ## and the test passed only because a failed bracket used to return the
  ## midpoint of the search interval. That is a finite number but not an
  ## estimate, so the assertion below was satisfied by a fabricated value.
  test_that("wleIRT returns scale-specific finite estimates for MIRT fits", {
    set.seed(404)
    sim <- simIRT(
      Nsubs = 200,
      Nitems = 40,
      Nscales = 2,
      NitemsAnswered = 20,
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

    ## Most people should have an estimate on each scale. Some may not, and a
    ## person with no interior root is reported as NA rather than as a number.
    expect_gt(mean(is.finite(out$wle)), 0.9)
    expect_gt(mean(is.finite(out$wleSE)), 0.9)

    ## Scale-specific means the two columns are estimated separately, not that
    ## one is copied into the other.
    expect_false(isTRUE(all.equal(out$wle[, 1], out$wle[, 2])))

    ## And the estimates should track the abilities that generated them.
    truth <- as.matrix(sim$Ability)
    for(k in seq_len(ncol(out$wle))){
      ok <- is.finite(out$wle[, k])
      expect_gt(stats::cor(out$wle[ok, k], truth[ok, k]), 0.5)
    }
  })
}
