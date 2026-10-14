## abilityBetaMap: person-predictor coefficients shared across dimensions or
## held at zero on one (R/laplaceBackend.R, bigIRT_beta_*).
##
## The map changes which coefficients the optimiser carries, so every path
## that assembles their gradient is checked against central differences, at
## the 1e-6 bar of test-laplace-direct-gradients.R: the Laplace objective
## (2PL and 3PL, correlations estimated away from zero, partly fixed persons,
## a predictor varying within person), fixed-node quadrature and the joint
## MAP. A shared coefficient's gradient is the sum over the entries it stands
## for, and an entry held at zero has none; a path that kept one entry's
## gradient, or the last one written, fails here. Then what a fit, the score
## refit and the covariance report.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  B <- asNamespace("bigIRT")

  ## Two scales, V1 shared by both, V2 on the first only.
  bmap <- function(scales) matrix(c(1, 1, 2, 0), 2, 2, dimnames = list(scales, c("V1", "V2")))

  sim2 <- function(seed, Np = 400, pl = 2){
    set.seed(seed)
    s <- simIRT(Nsubs = Np, Nitems = 40, Nscales = 2, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) .3 else 0,
      logitDMean = 20, logitDSD = 0,
      personPreds = matrix(stats::rnorm(Np * 2), Np, 2),
      AbilityPredEffects = matrix(c(.5, .5, .4, 0), 2, 2))
    s
  }
  scales_of <- function(dat) sort(unique(as.character(dat$Scale)))

  ## The objective as the optimiser sees it, at a point away from the start.
  evaluator <- function(dat, args, ecorr = FALSE, map = FALSE, jitter_par = 0.15){
    f <- suppressWarnings(do.call(fitIRT, c(list(dat = dat, cores = 1, priors = TRUE, ebayes = FALSE,
      dropPerfectScores = FALSE, normalise = FALSE, marginalApprox = if(map) "map" else "laplace",
      laplaceOuterIter = 2, laplaceCertify = FALSE, verbose = 0, keepInternals = TRUE), args)))
    sdat <- f$internals$sdat
    st <- B$bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
    pp <- B$bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- B$bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = ecorr)
    ctx <- B$bigIRT_laplace_item_context(sdat, layout = lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))])
    ctx$grain_size <- 16L
    ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- ecorr
    ctx$corr_paramization <- "normalized_chol"
    if(map) ctx$map <- TRUE
    st$laplaceCorrParam <- "normalized_chol"
    par <- B$bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    set.seed(99); par <- par + stats::rnorm(length(par), 0, jitter_par)
    if(length(lay$corr)) par[lay$corr] <- 0.5
    ev <- function(v, context = ctx) B$bigIRT_laplace_direct_objective(par = v, state = st, sdat = sdat,
      prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6, max_attempts = 8L,
      max_iter = 4000L, tol = 1e-13, keep_covariance = TRUE, context = context)
    list(layout = lay, par = par, ev = ev, sdat = sdat, ctx = ctx)
  }
  relerr <- function(f, par, slots, h = 1e-5){
    g <- f(par)$approx_grad
    fd <- vapply(slots, function(k){
      up <- par; up[k] <- up[k] + h; dn <- par; dn[k] <- dn[k] - h
      (f(up)$value - f(dn)$value) / (2 * h)
    }, numeric(1))
    max(abs(g[slots] - fd) / pmax(1, abs(fd)))
  }

  test_that("the map is read by name, renumbered, and the identity is no map", {
    s <- sim2(1, Np = 150)
    sc <- scales_of(s$dat)
    m <- B$bigIRT_ability_beta_map_input(bmap(sc), sc, c("V1", "V2"))
    expect_identical(m, c(1L, 1L, 2L, 0L))
    ## Rows given in another order, and numbers not starting at 1.
    m2 <- bmap(rev(sc)); m2[] <- c(7, 7, 0, 9)
    expect_identical(B$bigIRT_ability_beta_map_input(m2, sc, c("V1", "V2")), c(1L, 1L, 2L, 0L))
    ## A predictor the matrix leaves out is free on every scale.
    m3 <- bmap(sc)[, "V1", drop = FALSE]
    expect_identical(B$bigIRT_ability_beta_map_input(m3, sc, c("V1", "V2")), c(1L, 1L, 2L, 3L))
    expect_null(B$bigIRT_ability_beta_map_input(matrix(1:4, 2, 2, dimnames = list(sc, c("V1", "V2"))),
      sc, c("V1", "V2")))
    expect_error(B$bigIRT_ability_beta_map_input(matrix(1, 1, 1, dimnames = list("x", "V1")), sc, "V1"),
      "unknown")
    expect_error(B$bigIRT_ability_beta_map_input(matrix(-1, 1, 1, dimnames = list(sc[1], "V1")), sc, "V1"),
      "whole numbers")
  })

  test_that("Laplace ability-coefficient gradient with shared and zeroed coefficients (2PL, correlations)", {
    s <- sim2(4001)
    d <- evaluator(s$dat, list(pl = 2, personPreds = c("V1", "V2"), betaScale = 2,
      abilityBetaMap = bmap(scales_of(s$dat))), ecorr = TRUE)
    expect_identical(as.integer(d$sdat$abilityBetaMap), c(1L, 1L, 2L, 0L))
    expect_length(d$layout$ability_beta, 2L)
    expect_lt(relerr(d$ev, d$par, d$layout$ability_beta), 1e-6)
    expect_lt(relerr(d$ev, d$par, d$layout$corr), 1e-6)
    ## The state the optimiser sees holds the map: shared entries equal, the
    ## held one at zero.
    ab <- B$bigIRT_laplace_unpack_direct_state(d$par, B$bigIRT_laplace_initial_state(d$sdat, eps = 1e-6,
      corr_paramization = "normalized_chol"), d$sdat, layout = d$layout)$Abilitybeta
    expect_equal(ab[1, 1], ab[2, 1]); expect_identical(ab[2, 2], 0)
  })

  test_that("Laplace gradient with the map holds in a 3PL and with partly fixed persons", {
    s <- sim2(4011, pl = 3)
    sc <- scales_of(s$dat)
    d <- evaluator(s$dat, list(pl = 3, personPreds = c("V1", "V2"), betaScale = 2, abilityBetaMap = bmap(sc)))
    expect_lt(relerr(d$ev, d$par, d$layout$ability_beta), 1e-6)
    expect_lt(relerr(d$ev, d$par, head(d$layout$C, 3L)), 1e-6)
    ## Forty persons with the first ability fixed (personDat), ten with both:
    ## the shared coefficient's gradient must skip their fixed dimensions.
    ## With the ability mean estimated, and under second-order Laplace too,
    ## whose gradient is assembled separately.
    ids <- unique(s$dat$id)[1:50]
    pdat <- data.frame(id = ids, a = stats::rnorm(50), b = c(rep(NA, 40), stats::rnorm(10)))
    names(pdat)[2:3] <- sc
    d2 <- evaluator(s$dat, list(pl = 3, personPreds = c("V1", "V2"), betaScale = 2, abilityBetaMap = bmap(sc),
      personDat = pdat, estMeans = c("ability", "A", "C")))
    expect_lt(relerr(d2$ev, d2$par, d2$layout$ability_beta), 1e-6)
    expect_lt(relerr(d2$ev, d2$par, d2$layout$ability_mean), 1e-6)
    ctx2 <- d2$ctx; ctx2$laplace2 <- TRUE
    expect_lt(relerr(function(v) d2$ev(v, ctx2), d2$par, d2$layout$ability_beta), 1e-6)
  })

  test_that("Laplace gradient with a shared coefficient on a predictor varying within person", {
    s <- sim2(4021, Np = 300)
    dat <- data.table::copy(s$dat)
    dat[, x1 := stats::rnorm(.N)]; dat[, x2 := stats::rnorm(1), by = id]
    sc <- scales_of(dat)
    m <- matrix(c(1, 1, 2, 3), 2, 2, dimnames = list(sc, c("x1", "x2")))
    d <- evaluator(dat, list(pl = 2, personPreds = c("x1", "x2"), betaScale = 2, abilityBetaMap = m))
    expect_length(d$layout$ability_beta, 3L)
    expect_lt(relerr(d$ev, d$par, d$layout$ability_beta), 1e-6)
  })

  test_that("fixed-node quadrature gradient with shared and zeroed coefficients (3PL)", {
    s <- sim2(5103, Np = 300, pl = 3)
    d <- evaluator(s$dat, list(pl = 3, personPreds = c("V1", "V2"), betaScale = 2,
      abilityBetaMap = bmap(scales_of(s$dat))), ecorr = TRUE)
    lap <- d$ev(d$par)
    ctx <- d$ctx; ctx$agq <- B$bigIRT_agq_rules(d$sdat$Nscales, nodes = 5L); ctx$agq_centre <- lap$posterior
    f <- function(v) d$ev(v, ctx)
    expect_lt(relerr(f, d$par, d$layout$ability_beta), 1e-6)
    expect_lt(relerr(f, d$par, d$layout$corr), 1e-6)
  })

  test_that("joint-MAP gradient with shared and zeroed coefficients", {
    s <- sim2(6001, Np = 200)
    d <- evaluator(s$dat, list(pl = 2, personPreds = c("V1", "V2"), betaScale = 2,
      abilityBetaMap = bmap(scales_of(s$dat))), map = TRUE, jitter_par = 0.3)
    expect_lt(relerr(d$ev, d$par, d$layout$ability_beta), 1e-5)
  })

  test_that("a fit keeps the map, and the score refit and covariance respect it", {
    s <- sim2(7001, Np = 800)
    sc <- scales_of(s$dat)
    f <- suppressWarnings(fitIRT(s$dat, pl = 2, personPreds = c("V1", "V2"), normalise = FALSE,
      abilityBetaMap = bmap(sc), cores = 1, verbose = 0, keepInternals = TRUE))
    ab <- as.matrix(f$pars$Abilitybeta)
    expect_equal(ab[1, "V1"], ab[2, "V1"])
    expect_identical(unname(ab[2, "V2"]), 0)
    ## Truth: V1 .5 on both, V2 .4 on the first and nothing on the second.
    expect_lt(abs(ab[1, "V1"] - .5), .15); expect_lt(abs(ab[1, "V2"] - .4), .15)
    ## The ability re-estimate that purification and `adjust` make keeps it.
    sco <- B$bigIRT_score_compute(f, exclude = unique(s$dat$Item)[1:5], refit = TRUE, cores = 1)
    rb <- as.matrix(sco$ability_beta)
    expect_true(isTRUE(sco$refit_converged))
    expect_equal(rb[1, 1], rb[2, 1], tolerance = 1e-10)
    expect_identical(unname(rb[2, 2]), 0)
    expect_lt(max(abs(rb - ab)), .05)
    ## One row per entry of the K x P matrix; shared entries alike, the held one zero.
    V <- vcov(f, what = "covariate")
    expect_equal(dim(V), c(4L, 4L))
    expect_equal(V[1, 1], V[2, 2]); expect_equal(V[1, 2], V[1, 1])
    expect_identical(unname(V[4, 4]), 0)
  })
}
