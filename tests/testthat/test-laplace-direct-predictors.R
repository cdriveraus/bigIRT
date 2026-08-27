if(identical(Sys.getenv("NOT_CRAN"), "true") && .Machine$sizeof.pointer != 4 &&
   identical(Sys.getenv("BIGIRT_ENABLE_EXPERIMENTAL_DIRECT_PREDICTORS"), "true")){
  library(bigIRT)
  library(testthat)

  predictor_rmse <- function(x, y) sqrt(mean((as.numeric(x) - as.numeric(y))^2))

  test_that("laplace matches JML for 1d person and A/B item predictors", {
    set.seed(11)

    Np <- 700
    Ni <- 70
    itempreds <- matrix(rnorm(Ni * 2, sd = 0.3), Ni, 2)
    personpreds <- matrix(rnorm(Np * 2, sd = 0.3), Np, 2)
    colnames(itempreds) <- c("I1", "I2")
    colnames(personpreds) <- c("P1", "P2")

    sim <- simIRT(
      Nsubs = Np,
      Nitems = Ni,
      Nscales = 1,
      AMean = 1.2,
      ASD = 0.15,
      BMean = 0,
      BSD = 0.7,
      logitCMean = -20,
      logitCSD = 0,
      itemPreds = itempreds,
      personPreds = personpreds,
      AitemPredEffects = matrix(c(0.18, -0.12), 1, 2),
      BitemPredEffects = matrix(c(0.28, -0.16), 1, 2),
      AbilityPredEffects = matrix(c(0.42, -0.24), 1, 2)
    )

    fit_args <- list(
      dat = sim$dat,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      plot = FALSE,
      verbose = 0,
      AitemPreds = c("I1", "I2"),
      BitemPreds = c("I1", "I2"),
      personPreds = c("P1", "P2"),
      betaScale = 25
    )

    fit_jml <- do.call(fitIRT, c(fit_args, list(marginalApprox = "none")))
    fit_lap <- do.call(fitIRT, c(fit_args, list(marginalApprox = "laplace_direct", laplaceOuterIter = 80)))

    expect_equal(dim(fit_lap$pars$Abilitybeta), c(1, 2))
    expect_equal(dim(fit_lap$pars$Abeta), c(1, 2))
    expect_equal(dim(fit_lap$pars$Bbeta), c(1, 2))

    expect_lt(predictor_rmse(fit_lap$covariateEffects$Ability, fit_jml$covariateEffects$Ability), 0.12)
    expect_lt(predictor_rmse(fit_lap$covariateEffects$A, fit_jml$covariateEffects$A), 0.18)
    expect_lt(predictor_rmse(fit_lap$covariateEffects$B, fit_jml$covariateEffects$B), 0.12)
  })

  test_that("laplace matches JML for 1d 4PL predictor effects", {
    set.seed(12)

    Np <- 900
    Ni <- 80
    itempreds <- matrix(rnorm(Ni * 2, sd = 0.25), Ni, 2)
    personpreds <- matrix(rnorm(Np * 2, sd = 0.25), Np, 2)
    colnames(itempreds) <- c("I1", "I2")
    colnames(personpreds) <- c("P1", "P2")

    sim <- simIRT(
      Nsubs = Np,
      Nitems = Ni,
      Nscales = 1,
      AMean = 1.1,
      ASD = 0.12,
      BMean = 0,
      BSD = 0.8,
      logitCMean = -2.8,
      logitCSD = 0.15,
      logitDMean = 2.4,
      logitDSD = 0.12,
      itemPreds = itempreds,
      personPreds = personpreds,
      BitemPredEffects = matrix(c(0.22, -0.14), 1, 2),
      logitCitemPredEffects = matrix(c(0.30, -0.20), 1, 2),
      logitDitemPredEffects = matrix(c(0.18, -0.10), 1, 2),
      AbilityPredEffects = matrix(c(0.35, -0.22), 1, 2)
    )

    fit_args <- list(
      dat = sim$dat,
      pl = 4,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      plot = FALSE,
      verbose = 0,
      BitemPreds = c("I1", "I2"),
      CitemPreds = c("I1", "I2"),
      DitemPreds = c("I1", "I2"),
      personPreds = c("P1", "P2"),
      betaScale = 25
    )

    fit_jml <- do.call(fitIRT, c(fit_args, list(marginalApprox = "none")))
    fit_lap <- do.call(fitIRT, c(fit_args, list(marginalApprox = "laplace_direct", laplaceOuterIter = 100)))

    expect_equal(dim(fit_lap$pars$Cbeta), c(1, 2))
    expect_equal(dim(fit_lap$pars$Dbeta), c(1, 2))

    expect_lt(predictor_rmse(fit_lap$covariateEffects$Ability, fit_jml$covariateEffects$Ability), 0.15)
    expect_lt(predictor_rmse(fit_lap$covariateEffects$B, fit_jml$covariateEffects$B), 0.15)
    expect_lt(predictor_rmse(fit_lap$covariateEffects$C, fit_jml$covariateEffects$C), 0.05)
    expect_lt(predictor_rmse(fit_lap$covariateEffects$D, fit_jml$covariateEffects$D), 0.05)
  })

  test_that("laplace reports multivariate predictor effects with matrix dimensions", {
    set.seed(13)

    Np <- 500
    Ni <- 45
    K <- 2
    itempreds <- matrix(rnorm(Ni * 2, sd = 0.25), Ni, 2)
    personpreds <- matrix(rnorm(Np * 2, sd = 0.25), Np, 2)
    colnames(itempreds) <- c("I1", "I2")
    colnames(personpreds) <- c("P1", "P2")
    loadings <- matrix(0, nrow = Ni, ncol = K)
    loadings[seq_len(Ni / 2), 1] <- 1
    loadings[(Ni / 2 + 1):Ni, 2] <- 1

    sim <- simIRT(
      Nsubs = Np,
      Nitems = Ni,
      Nscales = K,
      mirt = TRUE,
      loadings = loadings,
      AbilityCorr = matrix(c(1, 0.35, 0.35, 1), 2, 2),
      itemPreds = itempreds,
      personPreds = personpreds,
      AitemPredEffects = c(0.15, -0.08),
      BitemPredEffects = c(0.20, -0.10),
      AbilityPredEffects = matrix(c(0.30, -0.18, -0.12, 0.22), 2, 2, byrow = TRUE),
      logitCMean = -20,
      logitCSD = 0
    )

    fit <- fitIRT(
      sim$dat,
      pl = 2,
      marginalApprox = "laplace_direct",
      priors = TRUE,
      ebayes = FALSE,
      estimateAbilityCorr = TRUE,
      cores = 1,
      loadings = loadings,
      AitemPreds = c("I1", "I2"),
      BitemPreds = c("I1", "I2"),
      personPreds = c("P1", "P2"),
      dropPerfectScores = FALSE,
      normalise = FALSE,
      plot = FALSE,
      verbose = 0,
      betaScale = 25,
      laplaceOuterIter = 60
    )

    expect_equal(dim(fit$pars$Abilitybeta), c(K, 2))
    expect_equal(dim(fit$covariateEffects$Ability), c(K, 2))
    expect_true(all(is.finite(fit$covariateEffects$Ability)))
    expect_true(all(is.finite(fit$covariateEffects$A)))
    expect_true(all(is.finite(fit$covariateEffects$B)))
  })
}
