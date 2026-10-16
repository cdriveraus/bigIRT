## Latent correlations that ran to exactly 1 and stayed there.
##
## Under rho = tanh(par) a step that overshoots lands in the flat tail, where
## the gradient is numerically zero and nothing brings the parameter back: the
## fit reports convergence with a correlation of 1.000, short of its optimum
## (on a Mindsteps subsample, 154 nats). The first step overshot because the
## metric's correlation block was Fisher information alone, several times
## smaller than the actual curvature at a cold start (R/laplaceOptimizer.R,
## bigIRT_laplace_corr_metric); and with that fixed a later step could still
## reach the tail on some threaded runs, so the optimiser now holds each
## partial correlation within +-.999 (bigIRT_laplace_corr_bounds). Each test
## below fails on the code before the fix.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## Three correlated subdomains, 4PL, every person answering many items, and
  ## a person predictor whose effect is common to the dimensions: the shape
  ## of the Mindsteps build, where the saturation was found.
  sat_data <- function(N = 1000L, J = 300L, per = 175L, r = .75, Pc = 6L, beta = 1, seed = 1L){
    set.seed(seed); K <- 3L
    sub <- rep(1:3, length.out = J)
    a1 <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J, 0, 1.5)
    cc <- stats::rbeta(J, 1, 12); dd <- 1 - stats::rbeta(J, 1, 20)
    R <- matrix(r, K, K); diag(R) <- 1
    Xc <- matrix(stats::rnorm(N * Pc), N)
    th <- matrix(stats::rnorm(N * K), N) %*% chol(R) + beta * Xc[, 1]
    id <- rep(seq_len(N), each = per)
    it <- as.integer(vapply(seq_len(N), function(i) sample.int(J, per), integer(per)))
    d <- data.table::data.table(id = id, Item = it, Scale = paste0("s", sub[it]))
    for(m in seq_len(Pc)) data.table::set(d, j = paste0("pc", m), value = Xc[id, m])
    eta <- a1[it] * th[cbind(id, sub[it])] - b[it]
    data.table::set(d, j = "score", value = stats::rbinom(nrow(d), 1, cc[it] + (dd[it] - cc[it]) * stats::plogis(eta)))
    d
  }
  sat_args <- function(d, ...){
    pp <- grep("^pc", names(d), value = TRUE)
    c(list(dat = d, score = "score", id = "id", item = "Item", scale = "Scale", pl = 4L,
      estimateAbilityCorr = TRUE, cores = 4, verbose = 0, laplaceCertify = FALSE, laplaceRefine = "none",
      dropPerfectScores = FALSE, personPreds = pp,
      abilityBetaMap = matrix(rep(seq_along(pp), each = 3L), 3L, length(pp), dimnames = list(paste0("s", 1:3), pp))),
      list(...))
  }

  test_that("a cold fit of correlated subdomains does not run its correlations to 1", {
    ## Before the fix: every correlation's parameter near 40 after the first
    ## step, one correlation 1.0000 at the end, 364 nats short.
    d <- sat_data()
    f <- suppressWarnings(do.call(fitIRT, sat_args(d)))
    cr <- f$abilityPrior$corr
    expect_lt(max(abs(cr[lower.tri(cr)])), .9)
    expect_lt(max(abs(cr[lower.tri(cr)] - .75)), .1)
    ## And it is the optimum the finite-difference metric reaches.
    old <- options(bigIRT.corr_metric = "fd"); on.exit(options(old))
    fd <- suppressWarnings(do.call(fitIRT, sat_args(d)))
    expect_lt(abs(as.numeric(logLik(f)) - as.numeric(logLik(fd))), .5)
  })

  test_that("the correlation curvature matches the objective's own at a cold start", {
    ## At the optimiser's first metric build: the starting items, the
    ## correlations at zero, and the persons' modes already correlated as the
    ## data say. The observed information of the Gaussian measurement model
    ## must match finite differences of the objective's own gradient in the
    ## correlation parameters, and the metric block must be no flatter than
    ## that. Fisher alone is several times too flat there -- which is what let
    ## the first step overshoot -- so the test checks that the case is one
    ## where it matters. (Zeroing the correlations at fitted items is not
    ## such a case: with the items known, Fisher is about right.)
    d <- sat_data()
    first <- NULL
    orig <- bigIRT:::bigIRT_laplace_metric_solver
    local_mocked_bindings(bigIRT_laplace_metric_solver = function(e, sdat, context, layout, gfun, ...){
      if(is.null(first) && length(layout$corr)){
        ci <- layout$corr; ps <- attr(layout, "par_scale")[ci]
        cc <- bigIRT:::bigIRT_laplace_corr_curvature(e$ev, sdat, context$corr_paramization)
        ## gfun is the gradient of the cost (minus the objective), in
        ## optimiser coordinates, so its differences in u are s_j s_l times
        ## the curvature in the natural parameters.
        FD <- sapply(seq_along(ci), function(k){
          hk <- 1e-4; xp <- e$x; xp[ci[k]] <- xp[ci[k]] + hk
          (gfun(xp) - e$g)[ci] / hk
        }) / outer(ps, ps)
        M <- bigIRT:::bigIRT_laplace_corr_metric(e$ev, sdat, layout, context$corr_paramization) / outer(ps, ps)
        first <<- list(cc = cc, FD = (FD + t(FD)) / 2, M = M)
      }
      orig(e, sdat, context, layout, gfun, ...)
    }, .package = "bigIRT")
    invisible(suppressWarnings(do.call(fitIRT, sat_args(d))))
    expect_false(is.null(first))
    FD <- first$FD
    expect_gt(min(diag(FD) / diag(first$cc$fisher)), 2)
    expect_lt(max(abs(first$cc$observed - FD)) / max(abs(FD)), .1)
    expect_gt(min(eigen(first$M - .9 * FD, symmetric = TRUE)$values), 0)
  })

  test_that("the optimiser brings a correlation back from the flat tail", {
    ## A bounded parameter started where tanh is flat: unbounded, the gradient
    ## is ~1e-16 and the search stops at once; with the box it is clamped to
    ## .999, where the gradient is still informative, and reaches the optimum.
    fg <- function(x){ r <- tanh(x); list(f = 500 * (r - .5)^2, g = 1000 * (r - .5) * (1 - r^2)) }
    minv <- function(v) v / 10
    free <- bigIRT:::bigIRT_laplace_lbfgs(19, fg, minv = minv, gain_tol = 1e-10)
    expect_gt(tanh(free$par), .999999)
    bd <- bigIRT:::bigIRT_laplace_corr_bounds(list(corr = 1L), "stan_corsqrt")
    boxed <- bigIRT:::bigIRT_laplace_lbfgs(19, fg, minv = minv, gain_tol = 1e-10, bounds = bd)
    expect_lt(abs(tanh(boxed$par) - .5), 1e-4)
    ## A boundary optimum stays on the bound rather than past it.
    fg1 <- function(x){ r <- tanh(x); list(f = 500 * (r - 1)^2, g = 1000 * (r - 1) * (1 - r^2)) }
    at <- bigIRT:::bigIRT_laplace_lbfgs(0, fg1, minv = minv, gain_tol = 1e-12, bounds = bd)
    expect_lt(abs(tanh(at$par) - .999), 1e-9)
  })

  test_that("fitIRT says so when a latent correlation ends on its bound", {
    ## Two dimensions with the same abilities: the data cannot tell them
    ## apart, so the estimate goes to the bound and the fit warns.
    set.seed(4)
    N <- 1500L; J <- 60L
    th <- stats::rnorm(N)
    sc <- rep(1:2, length.out = J); a1 <- stats::runif(J, 1, 1.8); b <- stats::rnorm(J)
    d <- data.table::CJ(id = seq_len(N), Item = seq_len(J))
    d[, Scale := paste0("s", sc[Item])]
    d[, score := stats::rbinom(.N, 1, stats::plogis(a1[Item] * th[id] - b[Item]))]
    w <- character()
    f <- withCallingHandlers(fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale", pl = 2L,
      estimateAbilityCorr = TRUE, cores = 4, verbose = 0, laplaceRefine = "none"),
      warning = function(m){ w <<- c(w, conditionMessage(m)); invokeRestart("muffleWarning") })
    expect_true(any(grepl("latent correlation is at the bound", w)))
    expect_gt(f$abilityPrior$corr[1, 2], .99)
  })
}
