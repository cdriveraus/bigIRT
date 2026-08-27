## The direct Laplace gradient against central finite differences.
##
## Every slot here was wrong at some point and the errors were quiet ones: the
## ability_beta slot was missing the Q H^-1 that maps the log-determinant slope
## through the posterior, and separately had its beta prior overwritten rather
## than accumulated; the corr slots were missing the adjoint entirely; and the
## symmetrisation feeding the correlation chain rule aliased in Eigen, which
## stayed invisible for as long as every contribution to it was symmetric. None
##
## The tolerance is 1e-6 on purpose. The observed error is about 2e-8, and the
## aliasing bug above was worth 2.5e-4, so a looser 1e-3 would have let it
## straight back in.
## of those stopped a fit from running or looking healthy, so they are checked
## numerically rather than by eye.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## Assemble the pieces optimize_direct would, and return value/gradient at par.
  direct_evaluator <- function(sim, sdat_args = list(), ecorr = FALSE) {
    f <- do.call(fitIRT, c(list(dat = sim$dat, pl = 2, cores = 1, priors = TRUE,
      ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace_fast", laplaceOuterIter = 2, verbose = 0,
      plot = FALSE, keepInternals = TRUE), sdat_args))
    sdat <- f$internals$sdat
    st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6,
      corr_paramization = "normalized_chol")
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = ecorr)
    ilay <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, layout = ilay)
    ctx$grain_size <- bigIRT:::bigIRT_laplace_subject_grain(sdat$Nsubs, 1L)
    ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- ecorr
    ctx$corr_paramization <- "normalized_chol"
    st$laplaceCorrParam <- "normalized_chol"
    list(layout = lay,
         par = bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay),
         eval = function(v) bigIRT:::bigIRT_laplace_direct_objective(par = v,
           state = st, sdat = sdat, prior_precision = pp,
           theta_init = st$AbilityBase, jitter = 1e-6, max_attempts = 8L,
           max_iter = 4000L, tol = 1e-13, keep_covariance = TRUE, context = ctx))
  }

  ## Relative error of the analytic gradient against a central difference.
  slot_relerr <- function(d, slots, h = 1e-5) {
    g <- d$eval(d$par)$approx_grad
    fd <- vapply(slots, function(k) {
      up <- d$par; up[k] <- up[k] + h
      dn <- d$par; dn[k] <- dn[k] - h
      (d$eval(up)$value - d$eval(dn)$value) / (2 * h)
    }, numeric(1))
    max(abs(g[slots] - fd) / pmax(1, abs(fd)))
  }

  test_that("direct Laplace ability_beta gradient matches finite differences", {
    set.seed(4001)
    Np <- 400
    sim <- simIRT(Nsubs = Np, Nitems = 40, Nscales = 2, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
      personPreds = matrix(stats::rnorm(Np * 2), Np, 2),
      AbilityPredEffects = matrix(c(.5, -.3, .4, -.2), 2, 2))
    ## betaScale is deliberately tight: the prior term that used to be
    ## overwritten scales as 1/betaScale^2 and vanishes under a loose prior.
    d <- direct_evaluator(sim, list(personPreds = c("V1", "V2"), betaScale = 2))
    expect_gt(length(d$layout$ability_beta), 0L)
    expect_lt(slot_relerr(d, d$layout$ability_beta), 1e-6)
  })

  test_that("direct Laplace correlation gradient matches finite differences", {
    set.seed(4002)
    sim <- simIRT(Nsubs = 500, Nitems = 45, Nscales = 3, NitemsAnswered = 24,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    d <- direct_evaluator(sim, ecorr = TRUE)
    expect_gt(length(d$layout$corr), 0L)
    ## Three scales, and away from zero correlation. At rho = 0 the prior log
    ## determinant is stationary, which zeroes two of the four contributions and
    ## would let a wrong gradient pass; the Eigen aliasing also only shows up
    ## with more than one correlation parameter.
    d$par[d$layout$corr] <- seq(0.5, by = 0.2, length.out = length(d$layout$corr))
    expect_lt(slot_relerr(d, d$layout$corr), 1e-6)
  })

  test_that("direct Laplace item gradients match finite differences", {
    set.seed(4003)
    ip <- matrix(stats::rnorm(40 * 2, 0, .3), 40, 2)
    sim <- simIRT(Nsubs = 400, Nitems = 40, Nscales = 1, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
      itemPreds = ip, BitemPredEffects = matrix(c(.8, -.5), 1, 2))
    d <- direct_evaluator(sim, list(BitemPreds = c("V1", "V2"),
      itemSpecificBetas = FALSE, betaScale = 2))
    expect_lt(slot_relerr(d, head(d$layout$B, 4L)), 1e-6)
    expect_lt(slot_relerr(d, head(d$layout$A, 4L)), 1e-6)
    expect_gt(length(d$layout$B_beta), 0L)
    expect_lt(slot_relerr(d, d$layout$B_beta), 1e-6)
  })

  test_that("direct Laplace ability_mean gradient matches finite differences", {
    ## The prior mean needs the same adjoint as the regression coefficients --
    ## it is the intercept they are measured against -- and without it this ran
    ## about two per cent short. The slots only exist when the ability mean is
    ## actually estimated, hence estMeans.
    set.seed(4004)
    sim <- simIRT(Nsubs = 700, Nitems = 50, Nscales = 2, NitemsAnswered = 25,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0.4, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    d <- direct_evaluator(sim, list(estMeans = c("Ability", "B")))
    expect_gt(length(d$layout$ability_mean), 0L)
    expect_lt(slot_relerr(d, d$layout$ability_mean), 1e-6)
  })

  test_that("laplace results do not depend on response row order", {
    ## Per-person and per-item sums were built with rowsum(reorder = FALSE),
    ## which returns groups in the order they are encountered rather than in
    ## level order. Response data usually arrive sorted by person, so the two
    ## coincided for person sums and the error stayed invisible; grouping by
    ## item does not coincide, and the item information used by the empirical
    ## Bayes update came back permuted. It showed up as a Schur complement with
    ## one negative eigenvalue. Shuffling the rows is the cheap way to catch a
    ## whole class of these.
    skip_on_cran()
    set.seed(5)
    sim <- simIRT(Nsubs = 400, Nitems = 30, Nscales = 2, NitemsAnswered = 15,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
      personPreds = matrix(stats::rnorm(400), 400, 1),
      AbilityPredEffects = matrix(c(.5, -.3), 2, 1))
    d <- data.table::as.data.table(sim$dat)
    args <- list(pl = 2, cores = 1, priors = TRUE, personPreds = "V1",
      marginalApprox = "laplace", dropPerfectScores = FALSE, normalise = FALSE,
      verbose = 0, plot = FALSE, laplaceOuterIter = 25)
    fit_a <- do.call(fitIRT, c(list(dat = d), args))
    set.seed(9)
    fit_b <- do.call(fitIRT, c(list(dat = d[sample(.N)]), args))
    expect_equal(fit_a$optim$logLik, fit_b$optim$logLik, tolerance = 1e-8)
    expect_equal(as.numeric(fit_a$pars$Abilitybeta),
                 as.numeric(fit_b$pars$Abilitybeta), tolerance = 1e-6)
    expect_equal(sort(as.numeric(fit_a$pars$B)),
                 sort(as.numeric(fit_b$pars$B)), tolerance = 1e-6)
  })
}
