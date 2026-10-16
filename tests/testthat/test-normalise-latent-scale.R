## Normalisation must standardise a solution by its latent distribution, not by
## the spread of its ability point estimates.
##
## Point estimates are shrunk toward the prior mean: at ten responses per
## person their SD is about .75 on a metric whose latent SD is 1. Until October
## 2026 normaliseIRT/normaliseMIRT, compareIRTmodels' "normalized" form and
## fitIRT(normalise = TRUE) all standardised by that spread, flattening a fit's
## discriminations by about a quarter -- on a metric the ability prior had
## already fixed -- while a simulated truth, standardised by its true
## abilities, was left alone. The paper's simulations scored curve recovery
## that way and so favoured whichever engine's slopes were steeper. Separately,
## normaliseIRT(robust = TRUE) set the interquartile range, not a standard
## deviation, to normaliseScale.

test_that("normaliseIRT's robust scale is a standard deviation, not an interquartile range", {
  set.seed(1)
  th <- stats::rnorm(1e5) * 2 + 3
  out <- normaliseIRT(B = 0, Ability = th, A = 1, robust = TRUE)
  expect_lt(abs(stats::sd(out$Ability) - 1), .02)
  expect_lt(abs(stats::median(out$Ability)), .02)
  ## Whatever the scale, every person's linear predictor A * theta - B, and so
  ## every response probability, is unchanged.
  expect_equal(out$A * out$Ability - out$B, 1 * th - 0, tolerance = 1e-10)
})

if(identical(Sys.getenv("NOT_CRAN"), "true")){
  set.seed(3)
  sim <- simIRT(Nsubs = 2000, Nitems = 40, Nscales = 1, NitemsAnswered = 10,
    AMean = 1, ASD = .2, BMean = 0, BSD = 1)
  fit_args <- list(dat = sim$dat, pl = 2, cores = 1, marginalApprox = "laplace", ebayes = FALSE,
    verbose = 0, dropPerfectScores = FALSE)
  f <- suppressWarnings(do.call(fitIRT, fit_args))

  test_that("a fit's latent scale comes from its posterior, not its point estimates", {
    e <- extractMIRTpars(f)
    ## Asserted first, so the test cannot pass by the shrinkage not happening.
    expect_lt(stats::sd(e$Ability[, 1]), .85)
    expect_lt(abs(e$latent_sd - 1), .05)
  })

  test_that("normalising a fit already on the prior's metric leaves its loadings in place", {
    z <- compareIRTmodels(list(truth = sim, estimated = f))
    a_raw <- z$itempars[form == "raw" & source == "estimated" & parameter == "A", value]
    a_norm <- z$itempars[form == "normalized" & source == "estimated" & parameter == "A", value]
    expect_lt(abs(mean(a_norm) / mean(a_raw) - 1), .05)
    ## The normalized curve RMSE is then no worse than the raw one by more
    ## than the small scale correction; before, it was inflated by the
    ## flattening.
    c_raw <- mean(unique(z$itempars[form == "raw" & source == "estimated", .(item, itemCurveRMSE)])$itemCurveRMSE)
    c_norm <- mean(unique(z$itempars[form == "normalized" & source == "estimated", .(item, itemCurveRMSE)])$itemCurveRMSE)
    expect_lt(c_norm, c_raw * 1.15)

    fn <- suppressWarnings(do.call(fitIRT, c(fit_args, list(normalise = TRUE))))
    expect_lt(abs(mean(fn$pars$A) / mean(f$pars$A) - 1), .05)
  })

  test_that("a mirt fit is standardised by its estimated latent distribution", {
    skip_if_not_installed("mirt")
    w <- as.matrix(data.table::dcast(data.table::as.data.table(sim$dat), id ~ Item,
      value.var = "score")[, -1])
    m <- mirt::mirt(w, 1, itemtype = "2PL", verbose = FALSE)
    e <- extractMIRTpars(m, score_method = "MAP")
    expect_equal(unname(e$latent_sd), 1)
    expect_lt(stats::sd(e$Ability[, 1]), .9)
  })
}

## normalise = TRUE is a reparameterisation: nothing a fit predicts or
## reports about measurement may change, and every part of the fit must move
## with the abilities. Until October 2026 it moved the abilities and the item
## parameters only. In one dimension the person posterior stayed on the old
## metric, so reliability() changed (.859 to .868 in one check); in any
## dimension the covariate coefficients stayed too, so the abilities stopped
## being base + xbar' beta; the row-level parameters looIRT() reads were not
## moved; and logLik() read the ability prior's normalising constant from the
## rewritten abilityPrior, so it moved as well.
if(identical(Sys.getenv("NOT_CRAN"), "true")){
  test_that("normalise = TRUE changes nothing a fit predicts or reports about measurement", {
    ## (With an estimated correlation the normalising map whitens the
    ## abilities -- theta' = (theta - centre) L^-1, which mixes the dimensions
    ## -- so per-dimension reliability is a property of different variables
    ## and is checked only where the map is diagonal; everything else must
    ## hold either way.)
    for(cfg in list(list(K = 1L, ecorr = FALSE), list(K = 2L, ecorr = FALSE), list(K = 2L, ecorr = TRUE))){
      K <- cfg$K
      set.seed(70 + K)
      s <- simIRT(Nsubs = 600, Nitems = 15 * K, Nscales = K, NitemsAnswered = 15 * K,
        AMean = 1.5, ASD = .3, BMean = .5, BSD = 1, logitCMean = -20, logitCSD = 0)
      d <- data.table::as.data.table(s$dat)
      d[, x := stats::rnorm(1), by = id]
      d[, score := as.integer(stats::runif(.N) <
        stats::plogis(stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6)) + .8 * x))]
      args <- list(dat = d, pl = 2, cores = 1, personPreds = "x", laplaceKeepCovariance = TRUE,
        dropPerfectScores = FALSE, laplaceRefine = "none", laplaceCertify = FALSE,
        estimateAbilityCorr = cfg$ecorr)
      f0 <- suppressWarnings(do.call(fitIRT, c(args, list(normalise = FALSE))))
      f1 <- suppressWarnings(do.call(fitIRT, c(args, list(normalise = TRUE, normaliseMean = .3))))
      lab <- paste0("K = ", K, if(cfg$ecorr) ", correlation estimated" else "")
      ## Asserted first, so the rest cannot pass by nothing having moved.
      expect_gt(max(abs(f1$pars$Ability - f0$pars$Ability)), .1, label = lab)
      if(!cfg$ecorr)
        expect_equal(reliability(f1)$empirical, reliability(f0)$empirical, tolerance = 1e-8, label = lab)
      expect_equal(as.numeric(logLik(f1)), as.numeric(logLik(f0)), tolerance = 1e-10, label = lab)
      ## Abilities are still base + xbar' beta, on the new metric.
      recon <- f1$personPosterior$mode + f1$pars$personPredsMean %*% t(f1$pars$Abilitybeta)
      expect_equal(unname(as.matrix(f1$pars$Ability)), unname(recon), tolerance = 1e-10, label = lab)
      ## Every row's linear predictor is unchanged, from the moved rows.
      eta1 <- rowSums(as.matrix(f1$pars$row_loadings) * as.matrix(f1$pars$row_ability)) - f1$pars$b_row
      expect_equal(eta1, f0$pars$eta_row, tolerance = 1e-8, label = lab)
      expect_true(all(paste0(colnames(f1$pars$Ability), "_SD") %in% names(f1$personPars)), label = lab)
      if(K == 1L)
        expect_equal(f1$covariateEffects$AbilityStd, f0$covariateEffects$AbilityStd, tolerance = 1e-8)
      if(requireNamespace("loo", quietly = TRUE))
        expect_equal(suppressWarnings(looIRT(f1, draws = 40L, seed = 9))$estimates,
          suppressWarnings(looIRT(f0, draws = 40L, seed = 9))$estimates, tolerance = 1e-8, label = lab)
    }
  })
}
