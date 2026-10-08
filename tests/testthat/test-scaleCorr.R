if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("a six-dimensional latent correlation is recovered, and joint MAP refuses to try", {
    set.seed(1)
    Np <- 500
    dat <- simIRT(Nsubs = Np, Nitems = 20, Nscales = 6,
      logitCMean = -20, logitCSD = .0, AMean = 1, ASD = .0,
      BMean = 0, BSD = .5, personPreds = matrix(rnorm(Np)),
      AbilityPredEffects = matrix(c(1, -0.5, .5), 6, 1), AbilityMean = 0)

    fit <- suppressWarnings(fitIRT(dat$dat, cores = 2, pl = 1, priors = TRUE,
      estimateAbilityCorr = TRUE, normalise = FALSE))
    ## The latent correlation is fit$abilityPrior$corr; the correlation of the
    ## ability point estimates is a different, biased quantity. Measured over
    ## the 15 pairs: mean absolute error .058, worst .109 (about two standard
    ## errors, as the worst of 15 should be); the identity it starts from is
    ## .272 out, so the bounds can fail.
    err <- abs(fit$abilityPrior$corr - cor(dat$Ability))[lower.tri(diag(6))]
    expect_lt(mean(err), .1)
    expect_lt(max(err), .2)

    ## Joint MAP cannot estimate it (see estimateAbilityCorr in ?fitIRT).
    expect_error(fitIRT(dat$dat, cores = 2, pl = 1, marginalApprox = "map",
      estimateAbilityCorr = TRUE), "needs marginalApprox = \"laplace\"")
  })
}
