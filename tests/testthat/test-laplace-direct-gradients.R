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
      marginalApprox = "laplace_fast", laplaceOuterIter = 2, verbose = 0, keepInternals = TRUE), sdat_args))
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

  test_that("direct Laplace ability_beta gradient is right with correlations estimated", {
    ## Together, not separately: the beta adjoint used the prior precision at
    ## the starting correlation rather than the current one, which only shows
    ## when both blocks are live and rho is away from its start.
    set.seed(4005)
    Np <- 400
    sim <- simIRT(Nsubs = Np, Nitems = 40, Nscales = 2, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
      personPreds = matrix(stats::rnorm(Np * 2), Np, 2),
      AbilityPredEffects = matrix(c(.5, -.3, .4, -.2), 2, 2))
    d <- direct_evaluator(sim, list(personPreds = c("V1", "V2"), betaScale = 2), ecorr = TRUE)
    expect_gt(length(d$layout$corr), 0L)
    d$par[d$layout$corr] <- 0.5
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

  ## Everything above runs pl = 2 on 2PL data, so none of it reaches the C or
  ## D slots, and none of it could have detected the curvature bug that the
  ## next test guards: for the 2PL the observed Hessian and Fisher
  ## information are the same number, so the adjoint got the right matrix by
  ## accident.
  direct_evaluator_pl <- function(sim, pl) {
    f <- fitIRT(dat = sim$dat, pl = pl, cores = 1, priors = TRUE,
      ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace_fast", laplaceOuterIter = 2, verbose = 0, keepInternals = TRUE)
    sdat <- f$internals$sdat
    st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6,
      corr_paramization = "normalized_chol")
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
    ilay <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, layout = ilay)
    ctx$grain_size <- bigIRT:::bigIRT_laplace_subject_grain(sdat$Nsubs, 1L)
    ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- FALSE
    ctx$corr_paramization <- "normalized_chol"
    st$laplaceCorrParam <- "normalized_chol"
    list(layout = lay,
         par = bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay),
         eval = function(v) bigIRT:::bigIRT_laplace_direct_objective(par = v,
           state = st, sdat = sdat, prior_precision = pp,
           theta_init = st$AbilityBase, jitter = 1e-6, max_attempts = 8L,
           max_iter = 4000L, tol = 1e-13, keep_covariance = TRUE, context = ctx))
  }

  test_that("3PL and 4PL item gradients match finite differences, 1 to 3 dimensions", {
    ## Everything above runs pl = 2 on 2PL data. That exercises neither the C
    ## and D slots nor the curvature that feeds the mode adjoint, and for
    ## years it passed at 2e-8 while the 3PL and 4PL gradient was wrong by
    ## 1e-3 to 4e-2. The cause was Fisher information being inverted where
    ## the implicit function theorem needs the actual Hessian; the 2PL could
    ## not see it because there the two curvatures are the same number.
    ##
    ## Fisher is no longer used for the Laplace curvature, so this asserts
    ## the property directly. If it ever fails, something has put an
    ## expected-information quantity back into log|H| or the adjoint.
    for (K in 1:3) {
      for (pl in c(3L, 4L)) {
        set.seed(5200 + 10 * K + pl)
        sim <- simIRT(Nsubs = 250, Nitems = 20 * K, Nscales = K,
          NitemsAnswered = rep(12L, K), AMean = 1, ASD = .3, BMean = 0,
          BSD = 1, logitCMean = -1.4, logitCSD = .25,
          logitDMean = if (pl >= 4) 2.5 else 20,
          logitDSD = if (pl >= 4) .25 else 0)
        d <- direct_evaluator_pl(sim, pl)
        for (nm in if (pl >= 4) c("A", "B", "C", "D") else c("A", "B", "C")) {
          slots <- d$layout[[nm]]
          if (is.null(slots) || !length(slots)) next
          expect_lt(slot_relerr(d, head(slots, 3L)), 1e-6,
                    label = sprintf("K=%d pl=%d slot=%s", K, pl, nm))
        }
      }
    }
  })

  test_that("the gradient is right with some abilities fixed (personDat)", {
    ## Two faults, one behind the other. The item kernel took each person's
    ## log-determinant over every dimension, while the person kernel -- whose
    ## log-determinant the value uses -- pins fixed abilities out: with 40 of
    ## 150 persons fixed the gradient was off by 1.06 relative (1.15 in two
    ## dimensions), against 2e-7 now. And the positive-semidefinite guard ran
    ## over every dimension, so a fixed ability's curvature could clip the
    ## free block and drop a log-determinant term that was not flat: 7.8e-2
    ## in two dimensions with one ability fixed for some persons, once the
    ## first was fixed. Every case has free asymptotes, covariates and an
    ## estimated ability mean, and second-order Laplace inherits the same terms.
    skip_on_cran()
    set.seed(3)
    lap_eval <- function(dat, args){
      f <- suppressWarnings(do.call(fitIRT, c(list(dat = dat, pl = 3, cores = 1, priors = TRUE,
        dropPerfectScores = FALSE, laplaceOuterIter = 2, laplaceCertify = FALSE,
        laplaceRefine = "none", keepInternals = TRUE), args)))
      sdat <- f$internals$sdat
      st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
      pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
      lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
      ctx <- bigIRT:::bigIRT_laplace_item_context(sdat,
        layout = lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))])
      ctx$grain_size <- 16L; ctx$direct_layout <- lay; ctx$corr_paramization <- "normalized_chol"
      st$laplaceCorrParam <- "normalized_chol"
      set.seed(1)
      par <- bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
      par <- par + stats::rnorm(length(par), 0, .2)
      make <- function(l2) function(v){
        cx <- ctx; cx$laplace2 <- l2
        bigIRT:::bigIRT_laplace_direct_objective(par = v, state = st, sdat = sdat,
          prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6, max_iter = 4000L,
          tol = 1e-13, keep_covariance = TRUE, context = cx)
      }
      list(par = par, make = make)
    }
    relerr <- function(f, par, h = 1e-5){
      g <- f(par)$approx_grad
      fd <- vapply(seq_along(par), function(k){
        u <- par; u[k] <- u[k] + h; d <- par; d[k] <- d[k] - h
        (f(u)$value - f(d)$value) / (2 * h)
      }, numeric(1))
      max(abs(g - fd) / pmax(1, abs(fd)))
    }
    ## One dimension, 40 of 150 persons fixed.
    s1 <- simIRT(Nsubs = 150, Nitems = 15, Nscales = 1, NitemsAnswered = 12, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d1 <- s1$dat; d1[, x := stats::rnorm(1), by = id]
    pd1 <- data.frame(id = unique(d1$id)[1:40], a = stats::rnorm(40))
    names(pd1)[2] <- as.character(unique(d1$Scale))
    ## Two dimensions: 40 persons with the first ability fixed only, 10 with both.
    s2 <- simIRT(Nsubs = 150, Nitems = 16, Nscales = 2, NitemsAnswered = 16, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d2 <- s2$dat; d2[, x := stats::rnorm(1), by = id]
    pd2 <- data.frame(id = unique(d2$id)[1:50], a = stats::rnorm(50), b = c(rep(NA, 40), stats::rnorm(10)))
    names(pd2)[2:3] <- sort(unique(as.character(d2$Scale)))
    for(case in list(list(d = d1, pd = pd1, nm = "1D"), list(d = d2, pd = pd2, nm = "2D"))){
      e <- lap_eval(case$d, list(personDat = case$pd, personPreds = "x",
        estMeans = c("ability", "A", "C")))
      expect_lt(relerr(e$make(FALSE), e$par), 1e-5, label = paste(case$nm, "Laplace"))
      expect_lt(relerr(e$make(TRUE), e$par), 1e-5, label = paste(case$nm, "second-order Laplace"))
    }
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
      verbose = 0, laplaceOuterIter = 25)
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
