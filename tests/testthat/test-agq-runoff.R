## The quadrature refinement must not walk an item out along its own ray
## (R/laplaceBackend.R, "Quadrature refinement").
##
## Held nodes are a fixed discrete ability distribution away from where they
## were placed, and an item made a step between two of them fits it better
## the steeper it is. On an unregularised 2PL the refinement used that: each
## stretch of steps gained on the held nodes and lost on the objective, and a
## run of them took an item out along its ray. The reported 3-D case went
## from a discrimination of 58 to 6197 and ended 46 nats below where the
## refinement started; the guard let that through, since it only looked at
## gains past half a nat per person.
##
## Both fixtures are frozen data, not re-simulated, so a change to the
## simulator cannot quietly turn these into cases that no longer run off:
##   agq-runoff-1d.rds  training rows of paper/Submission2 task
##                      F_2PL_sparse_i150_r014 (600 persons, 80 items, ten
##                      responses each)
##   agq-runoff-3d.rds  training rows and loading mask of task
##                      M_misspecified_N450_r004 (450 persons, 24 items, one
##                      loading each on three dimensions)
## Fitted as the benchmark's unregularised arm fits them.
##
## Every check is against something the code does not compute itself: the
## exact marginal, a sum of one-dimensional integrals done on a grid
## (abilities independent, one loading per item), and its maximum, found once
## with its analytic gradient by L-BFGS-B on the same grid (2026-10-08).

## The exact marginal log-likelihood of a 2PL, p = plogis(A theta - B), with
## one loading per item and independent N(0, 1) abilities, on bigIRT's scale
## (which leaves out K N log(2 pi) / 2). `dim_of` gives each item's dimension
## by item label; itemPars carries the loadings as A (one dimension) or
## A_1 ... A_K.
bigirt_exact_marginal <- function(d, itemPars, dim_of = NULL, N = length(unique(d$id)),
                                  grid = seq(-7, 7, length.out = 1401)){
  K <- if(is.null(dim_of)) 1L else max(dim_of)
  lab <- as.character(itemPars$Item)
  Acols <- if(K == 1L) "A" else paste0("A_", seq_len(K))
  if(is.null(dim_of)) dim_of <- setNames(rep(1L, length(lab)), lab)
  A <- as.matrix(itemPars[, Acols, drop = FALSE])[cbind(seq_along(lab), dim_of[lab])]
  names(A) <- lab
  B <- setNames(itemPars$B, lab)
  val <- 0
  for(k in seq_len(K)){
    dk <- d[dim_of[as.character(d$Item)] == k, , drop = FALSE]
    it <- as.character(dk$Item)
    eta <- outer(A[it], grid) - B[it]
    L <- rowsum(stats::plogis((2 * dk$score - 1) * eta, log.p = TRUE), dk$id, reorder = TRUE)
    L <- sweep(L, 2, stats::dnorm(grid, log = TRUE) + log(grid[2] - grid[1]), "+")
    mx <- apply(L, 1, max)
    val <- val + sum(mx + log(rowSums(exp(L - mx))))
  }
  val + K * N * log(2 * pi) / 2
}

test_that("the exact marginal agrees with the quadrature objective at a tame point", {
  ## The check on the check: at a regularised fit, where no item is steep,
  ## adaptive quadrature and the grid must agree. A wrong sign convention for
  ## B, or a constant counted differently, would show here.
  d <- readRDS(test_path("fixtures", "agq-runoff-1d.rds")); d$Scale <- 1L
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = TRUE, estMeans = character(0),
    dropPerfectScores = FALSE, normalise = FALSE, laplaceRefine = "agq",
    cores = 1, verbose = 0, plot = FALSE))
  expect_lt(max(fit$itemPars$A), 5)
  ## The fitted objective includes the item prior; take it out.
  ll <- fit$optim$logLik - fit$laplaceStatus$item_prior_value
  expect_equal(ll, bigirt_exact_marginal(d, fit$itemPars), tolerance = 0.05 / abs(ll))
})

if(identical(Sys.getenv("NOT_CRAN"), "true") && .Machine$sizeof.pointer != 4){

test_that("a 1-D refinement ends no lower than it started, and where its approximation holds", {
  d <- readRDS(test_path("fixtures", "agq-runoff-1d.rds")); d$Scale <- 1L
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    dropPerfectScores = FALSE, normalise = FALSE, laplaceRefine = "agq",
    cores = 1, verbose = 0, plot = FALSE))
  st <- fit$laplaceStatus; rf <- st$refine
  ## Two of its loadings sit on the flat end of their softplus, which also
  ## threw the optimiser's metric (see the test on that below); with that
  ## fixed, no stretch on held nodes loses here, and the 3-D case below is
  ## the one that still exercises the judge.
  ## Before: 15.8 nats lower than the start, item 22 at a discrimination of
  ## 3335 -- where quadrature put the objective 19.7 nats below the exact
  ## marginal, and the exact marginal was 1.17 below its maximum.
  expect_gte(rf$agq_end - rf$agq_start, -1e-3)
  expect_equal(rf$agq_end, fit$optim$logLik)
  ex <- bigirt_exact_marginal(d, fit$itemPars)
  expect_lt(abs(fit$optim$logLik - ex), 0.25)
  ## The exact maximum is -2994.174, with item 22 at 17.5 on a profile that
  ## is within a tenth of a nat of flat from 6 on.
  expect_gt(ex, -2994.174 - 0.25)
  ## Nothing is left run off that pulling back would improve.
  expect_lt(max(fit$itemPars$A), bigIRT:::bigIRT_runaway_slope())
  expect_true(is.null(st$diverged) || !any(st$diverged$pullback_gain > 1e-3, na.rm = TRUE))
})

test_that("the reported 3-D case is pulled off its ridge, and ends no lower than it started", {
  fx <- readRDS(test_path("fixtures", "agq-runoff-3d.rds"))
  d <- fx$dat
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    loadings = fx$loadings, dropPerfectScores = FALSE, normalise = FALSE,
    estimateAbilityCorr = FALSE, itemSpecificBetas = FALSE, laplaceRefine = "agq",
    cores = 1, verbose = 0, plot = FALSE))
  st <- fit$laplaceStatus; rf <- st$refine
  dim_of <- setNames(apply(is.na(fx$loadings), 1, which), rownames(fx$loadings))
  ## Before: item 12 from 58 to 6197, the objective 46 nats below its start,
  ## and there 62 nats below the exact marginal. Pulling item 12 back gained
  ## 61, and the certificate called it diverged.
  expect_gte(rf$agq_end - rf$agq_start, -1e-3)
  expect_true(isTRUE(rf$held_nodes_lost > 0))
  ex <- bigirt_exact_marginal(d, fit$itemPars, dim_of = dim_of)
  expect_lt(abs(fit$optim$logLik - ex), 1)
  ## The ridge is a local optimum: pulling item 12 back alone loses there,
  ## because its neighbours have adapted to it. Only pulling the run-off
  ## items back together and refitting finds the better optimum, so this
  ## case is the one that needs the retry.
  expect_true(any(rf$pulled_back$kept & grepl("^retry", rf$pulled_back$at)))
  expect_null(st$diverged)
  expect_lt(max(abs(as.matrix(fit$itemPars[, paste0("A_", 1:3)]))), bigIRT:::bigIRT_runaway_slope())
  ## The exact maximum is -1044.30 (largest loading 7.4, item 12 at 3.3). The
  ## refinement's own optimum sits 1.5 below it, with smaller loadings, which
  ## is the quadrature's error for steep items rather than the optimiser's;
  ## the old end was 6.1 below.
  expect_gt(ex, -1044.30 - 3)
})

test_that("the refinement does not park a run-off item on the screen", {
  ## Task M_misspecified_N450_r007 (agq-runoff-3d-screen.rds): the Laplace
  ## stage ends with items 20 and 21 run off, and from there the refinement
  ## cannot take a step. Its pull-back used to aim at the screen itself among
  ## other points, and put item 20 at exactly 20 logits per SD -- where the
  ## screen, which looks for steeper than that, no longer saw it -- so the
  ## fit was reported not a maximum with nothing named.
  fx <- readRDS(test_path("fixtures", "agq-runoff-3d-screen.rds"))
  fit <- suppressWarnings(fitIRT(fx$dat, pl = 2, priors = FALSE, estMeans = character(0),
    loadings = fx$loadings, dropPerfectScores = FALSE, normalise = FALSE,
    estimateAbilityCorr = FALSE, itemSpecificBetas = FALSE, laplaceRefine = "agq",
    cores = 1, verbose = 0, plot = FALSE))
  st <- fit$laplaceStatus
  slope <- bigIRT:::bigIRT_runaway_slope()
  ## The ability prior SD is 1 here, so steepness is |A|.
  A <- abs(as.matrix(fit$itemPars[, paste0("A_", 1:3)]))
  expect_false(any(abs(A - slope) < 1e-6))
  ## Every item steeper than the screen is named, and only those.
  steep <- as.character(fit$itemPars$Item[apply(A, 1, max) > slope])
  expect_setequal(as.character(unique(st$diverged$item)), steep)
  expect_gte(st$refine$agq_end - st$refine$agq_start, -1e-3)
})

}

test_that("a loading on the flat tail of its softplus is not thrown by the metric", {
  ## Unregularised fit M_misspecified_N450_r007 ended its Laplace stage with
  ## one raw loading at -124: slope 1e-54, curvature 1e-108. Every metric
  ## moved that coordinate by its coupling to the other gradients over that
  ## curvature, a step of 9e53, so no step length along the direction gained
  ## and neither Laplace nor the quadrature refinement took a step. Put there
  ## by hand on the 1-D fixture, each metric must now leave it essentially
  ## where it is and still point uphill.
  d <- readRDS(test_path("fixtures", "agq-runoff-1d.rds")); d$Scale <- 1L
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    dropPerfectScores = FALSE, normalise = FALSE, laplaceRefine = "none",
    laplaceCertify = FALSE, keepInternals = TRUE, cores = 1, verbose = 0, plot = FALSE))
  ev <- bigIRT:::bigIRT_direct_eval_from_fit(fit)
  lay <- ev$layout; par <- ev$par
  k <- lay$A[5]
  par[k] <- -124
  run <- function(p) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = ev$state,
    sdat = ev$sdat, prior_precision = fit$internals$priorPrecision,
    theta_init = fit$internals$thetaBase, jitter = 1e-6, keep_covariance = TRUE,
    context = ev$context)
  r0 <- run(par)
  e <- list(f = -r0$value, g = -r0$approx_grad, ev = r0, x = par)
  gfun <- function(p) -run(p)$approx_grad
  for(impl in c("cpp", "R")) for(type in c("dense", "blockdiag")){
    withr::local_options(bigIRT.dense_impl = impl, bigIRT.blockdiag_impl = impl)
    minv <- bigIRT:::bigIRT_laplace_metric_solver(e, ev$sdat, ev$context, lay, gfun, type = type)
    step <- -minv(e$g)
    expect_true(all(is.finite(step)), label = paste(impl, type))
    expect_lt(abs(step[k]), 1, label = paste(impl, type, "step on the flat loading"))
    gain <- max(vapply(10^(0:-4), function(a) run(par + a * step)$value - r0$value, numeric(1)))
    expect_gt(gain, 0, label = paste(impl, type, "best gain along the step"))
  }
})

test_that("a refresh can end the L-BFGS search", {
  ## A quadratic the search would take many iterations over; the refresh
  ## ends it at the first call, which is how the refinement undoes a stretch
  ## of held-node steps that lost.
  H <- diag(c(1, 100))
  fg <- function(x) list(f = 0.5 * sum(x * (H %*% x)), g = as.numeric(H %*% x))
  calls <- 0L
  r <- bigIRT:::bigIRT_laplace_lbfgs(c(5, 5), fg, maxit = 200L, gain_tol = 1e-14,
    refresh = function(e){ calls <<- calls + 1L; list(stop = TRUE) }, refresh_every = 3L)
  expect_identical(r$terminate$what, "refresh_stop")
  expect_identical(r$iter, 3L)
  expect_identical(calls, 1L)
  ## Without it the same search runs to the gain criterion.
  r2 <- bigIRT:::bigIRT_laplace_lbfgs(c(5, 5), fg, maxit = 200L, gain_tol = 1e-14,
    refresh = function(e) list(minv = NULL), refresh_every = 3L)
  expect_identical(r2$terminate$what, "gain")
  expect_gt(r2$iter, 3L)
})
