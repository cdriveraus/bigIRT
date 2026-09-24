## Adaptive Gauss-Hermite quadrature for the person integrals (R/agq.R,
## src/agq_backend.cpp).
##
## The quadrature objective's gradient is the Fisher identity -- a posterior
## expectation of each response's score -- and it is the EXACT gradient of the
## objective only while the nodes are held fixed. So the gradient tests hold
## them fixed (`agq_centre`) and demand agreement with central differences to
## 1e-6, the same bar the Laplace gradient tests use. A re-centred objective
## would agree only to the quadrature error and would let a wrong term hide
## in that slack.
##
## Every case runs a 3PL or 4PL with free asymptotes: the 2PL is where Laplace
## is least wrong and where a gradient test proves least (see
## test-laplace-direct-gradients.R on what a 2PL-only test missed for years).
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  agq_evaluator <- function(sim, pl, fit_args = list(), ecorr = FALSE) {
    ## Two iterations only, to get the prepared data; the fit itself is unused.
    f <- suppressWarnings(do.call(fitIRT, c(list(dat = sim$dat, pl = pl, cores = 1,
      priors = TRUE, ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace", laplaceOuterIter = 2, verbose = 0,
      plot = FALSE, keepInternals = TRUE), fit_args)))
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
    par <- bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    ## Away from the start, so no gradient block is trivially zero.
    set.seed(99); par <- par + stats::rnorm(length(par), 0, 0.15)
    ev <- function(v, context) bigIRT:::bigIRT_laplace_direct_objective(par = v,
      state = st, sdat = sdat, prior_precision = pp, theta_init = st$AbilityBase,
      jitter = 1e-6, max_attempts = 8L, max_iter = 4000L, tol = 1e-12,
      keep_covariance = TRUE, context = context)
    list(layout = lay, par = par, sdat = sdat, ctx = ctx, ev = ev)
  }

  ## The fixed-node quadrature objective at Q nodes, centred where the
  ## Laplace posterior sits at d$par.
  fixed_agq <- function(d, Q) {
    lap <- d$ev(d$par, d$ctx)
    ctx <- d$ctx; ctx$agq <- bigIRT:::bigIRT_agq_rules(d$sdat$Nscales, nodes = Q)
    ctx$agq_centre <- lap$posterior
    function(v) d$ev(v, ctx)
  }

  slot_relerr <- function(f, par, slots, h = 1e-5) {
    g <- f(par)$approx_grad
    fd <- vapply(slots, function(k) {
      up <- par; up[k] <- up[k] + h; dn <- par; dn[k] <- dn[k] - h
      (f(up)$value - f(dn)$value) / (2 * h)
    }, numeric(1))
    max(abs(g[slots] - fd) / pmax(1, abs(fd)))
  }

  test_that("the Gauss-Hermite rule integrates polynomials of degree 2Q - 1 exactly", {
    for(Q in c(3L, 5L, 9L)) {
      r <- bigIRT:::bigIRT_gh_rule(Q)
      w <- exp(r$log_weights)
      mom <- vapply(0:(2 * Q - 1), function(k) sum(w * r$nodes^k), numeric(1))
      ## E[z^k] for a standard normal: 0 for odd k, (k - 1)!! for even k.
      ref <- vapply(0:(2 * Q - 1), function(k)
        if(k %% 2) 0 else prod(seq(1, max(k - 1, 1), by = 2)), numeric(1))
      expect_lt(max(abs(mom - ref) / pmax(1, ref)), 1e-10)
    }
  })

  test_that("quadrature with one node is the Laplace value", {
    set.seed(5101)
    sim <- simIRT(Nsubs = 300, Nitems = 30, Nscales = 2, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d <- agq_evaluator(sim, pl = 3)
    lap <- d$ev(d$par, d$ctx)
    ctx <- d$ctx; ctx$agq <- bigIRT:::bigIRT_agq_rules(d$sdat$Nscales, nodes = 1L)
    one <- d$ev(d$par, ctx)
    expect_lt(abs(one$value - lap$value), 1e-8 * abs(lap$value))
  })

  test_that("quadrature item gradients match finite differences (3PL and 4PL, K = 1)", {
    set.seed(5102)
    ip <- matrix(stats::rnorm(30 * 2, 0, .3), 30, 2)
    sim <- simIRT(Nsubs = 300, Nitems = 30, Nscales = 1, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3,
      logitDMean = 2, logitDSD = .3, itemPreds = ip, BitemPredEffects = matrix(c(.6, -.4), 1, 2))
    for(pl in 3:4) {
      d <- agq_evaluator(sim, pl = pl, list(BitemPreds = c("V1", "V2"),
        itemSpecificBetas = FALSE, betaScale = 2))
      f <- fixed_agq(d, 9L)
      L <- d$layout
      for(b in c("A", "B", "C", "D")) if(length(L[[b]]))
        expect_lt(slot_relerr(f, d$par, head(L[[b]], 4L)), 1e-6, label = paste(pl, b))
      for(b in c("A_mean", "B_mean", "C_mean", "D_mean", "B_beta")) if(length(L[[b]]))
        expect_lt(slot_relerr(f, d$par, L[[b]]), 1e-6, label = paste(pl, b))
      expect_gt(length(L$C), 0L)
      if(pl == 4) expect_gt(length(L$D), 0L)
    }
  })

  test_that("quadrature person-side gradients match finite differences (K = 2)", {
    set.seed(5103)
    Np <- 300
    sim <- simIRT(Nsubs = Np, Nitems = 30, Nscales = 2, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = .3,
      logitCMean = -1.5, logitCSD = .3,
      personPreds = matrix(stats::rnorm(Np * 2), Np, 2),
      AbilityPredEffects = matrix(c(.5, -.3, .4, -.2), 2, 2))
    d <- agq_evaluator(sim, pl = 3, list(personPreds = c("V1", "V2"), betaScale = 2,
      estMeans = c("Ability", "A", "C")), ecorr = TRUE)
    L <- d$layout
    expect_gt(length(L$ability_beta), 0L)
    expect_gt(length(L$ability_mean), 0L)
    expect_gt(length(L$corr), 0L)
    ## Away from zero correlation, where the prior log determinant is
    ## stationary and a missing term could hide.
    d$par[L$corr] <- 0.5
    f <- fixed_agq(d, 5L)
    expect_lt(slot_relerr(f, d$par, L$ability_beta), 1e-6)
    expect_lt(slot_relerr(f, d$par, L$ability_mean), 1e-6)
    expect_lt(slot_relerr(f, d$par, L$corr), 1e-6)
    expect_lt(slot_relerr(f, d$par, head(L$A, 3L)), 1e-6)
    expect_lt(slot_relerr(f, d$par, head(L$C, 3L)), 1e-6)
  })

  test_that("quadrature converges to the integral and moves off the Laplace value", {
    ## One dimension, so the integral can be done by brute force on a grid.
    set.seed(5104)
    sim <- simIRT(Nsubs = 60, Nitems = 15, Nscales = 1, NitemsAnswered = 15,
      AMean = 1.5, ASD = .3, BMean = 0, BSD = 1, logitCMean = -1, logitCSD = .3)
    d <- agq_evaluator(sim, pl = 3)
    lap <- d$ev(d$par, d$ctx)
    q41 <- { ctx <- d$ctx; ctx$agq <- bigIRT:::bigIRT_agq_rules(1L, nodes = 41L); d$ev(d$par, ctx) }
    st <- lap$state; sdat <- d$sdat
    re <- bigIRT:::bigIRT_laplace_row_effective(state = st, sdat = sdat,
      thetaBase = lap$posterior$theta_mode, rows = d$ctx$train_rows, context = d$ctx$row_context)
    lo <- re$loadings[, 1]; int <- re$eta_row - lo * re$row_ability[, 1]
    ids <- as.integer(d$ctx$row_context$ids); y <- sdat$score[d$ctx$train_rows]
    prec <- as.numeric(lap$prior_mats$precision)[1]
    grid <- seq(-10, 10, length.out = 8001); h <- diff(grid)[1]
    brute <- sum(vapply(seq_len(sdat$Nsubs), function(i) {
      r <- which(ids == i)
      lp <- -0.5 * prec * grid^2
      for(k in r) {
        p <- re$c_row[k] + (re$d_row[k] - re$c_row[k]) * plogis(lo[k] * grid + int[k])
        lp <- lp + if(y[k] == 1) log(p) else log1p(-p)
      }
      mx <- max(lp); mx + log(sum(exp(lp - mx)) * h)
    }, numeric(1)))
    ## The two objectives share the prior kernel without its normalising
    ## constant, so they are directly comparable.
    q_only <- q41$value - q41$prior$value
    expect_lt(abs(q_only - brute), 1e-6 * abs(brute))
    expect_gt(abs((lap$value - lap$prior$value) - brute), 1e-3)
  })
}
