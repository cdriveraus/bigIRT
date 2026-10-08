## Unregularised fits whose item parameters run off (R/certify.R, "Items that
## run off").
##
## Found on a three-dimensional 2PL with priors = FALSE and laplaceRefine =
## "agq": an item's raw discrimination reached 6197, the R side formed the
## loading as log1p(exp(6197)) = Inf, its intercept was 3041, the linear
## predictor took Inf * 0 = NaN for every person whose ability on that
## dimension was exactly the prior mean, and laplaceStatus said only that the
## fit had not converged. The native kernels, whose softplus was always the
## stable form, saw a finite loading throughout.

## One item answered correctly exactly when ability is above zero: complete
## separation, which nothing without an item prior bounds. Abilities on a
## quantile grid with a gap around the threshold, ten ordinary items.
bigirt_separated_data <- function(N = 200, nitems = 10, gap = 0.1, seed = 1){
  set.seed(seed)
  th <- stats::qnorm(seq(0.5, N - 0.5) / N)
  th <- th[abs(th) > gap]
  B <- seq(-1.5, 1.5, length.out = nitems)
  d <- data.table::CJ(id = seq_along(th), Item = seq_len(nitems))
  d[, score := as.integer(stats::runif(.N) < stats::plogis(th[id] - B[Item]))]
  sep <- data.table::data.table(id = seq_along(th), Item = nitems + 1L, score = as.integer(th > 0))
  d <- rbind(d, sep)[order(id, Item)]
  d[, Scale := 1L]
  d
}

test_that("the softplus link cannot overflow, and is the native kernels' to the bit", {
  afunc <- bigIRT:::afunc
  x <- c(-1e300, -800, -40, -1, 0, 1e-12, 0.5, 1, 30, 709, 710, 6197, 1e300)
  expect_true(all(is.finite(afunc(x))))
  expect_identical(afunc(6197), 6197)
  expect_identical(afunc(1e300), 1e300)
  ## The C++ branch: x + log1p(exp(-x)) above zero, log1p(exp(x)) below.
  expect_identical(afunc(x), ifelse(x > 0, x + log1p(exp(-x)), log1p(exp(x))))
  expect_equal(afunc(c(-3, 0.5, 3)), log1p(exp(c(-3, 0.5, 3))), tolerance = 1e-15)
  ## Dimensions survive, and the inverse inverts where the old one overflowed.
  m <- matrix(c(-2, 0.5, 800, 6197), 2)
  expect_identical(dim(afunc(m)), dim(m))
  expect_equal(bigIRT:::afunci(afunc(c(-5, 0.3, 4, 800, 6197))), c(-5, 0.3, 4, 800, 6197),
    tolerance = 1e-12)
})

test_that("an unregularised fit that runs off says so by name, with every probability finite", {
  d <- bigirt_separated_data()
  ## One person held out entirely: with no training response their ability is
  ## the prior mean, exactly 0, which is what turned an infinite loading into
  ## NaN probabilities.
  out_id <- 7L
  train <- which(d$id != out_id)
  ws <- character(0)
  ## On Laplace, which takes the separated item to a discrimination of about
  ## 42. This ran on laplaceRefine = "agq" until the quadrature refinement
  ## stopped walking items out along their rays (test-agq-runoff.R); it took
  ## this item to 1e8 and so ran off too. See the next test for what the
  ## refinement does with it now.
  fit <- withCallingHandlers(
    fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0), dropPerfectScores = FALSE,
      laplaceRefine = "none", trainingRows = train, cores = 1, verbose = 0, plot = FALSE),
    warning = function(w){ ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
  st <- fit$laplaceStatus

  ## What went wrong before: an infinite loading and NaN probabilities.
  expect_true(all(is.finite(fit$pars$pcorrect)))
  expect_true(all(is.finite(fit$pars$p)))
  expect_true(all(is.finite(as.matrix(fit$itemPars[, c("A", "B")]))))
  expect_equal(fit$pars$Ability[as.character(out_id), 1], 0)

  ## The separated item has run off, and is named as such -- item and
  ## parameter as itemPars spells them -- with no other item alongside it.
  expect_false(st$converged)
  expect_identical(st$reason, "diverged")
  expect_identical(st$certificate$status, "diverged")
  expect_s3_class(st$diverged, "data.frame")
  expect_setequal(st$diverged$item, "11")
  expect_setequal(st$diverged$parameter, c("A", "B"))
  sep <- fit$itemPars[fit$itemPars$Item == "11", ]
  expect_equal(st$diverged$value[st$diverged$parameter == "A"], sep$A)
  expect_equal(st$diverged$value[st$diverged$parameter == "B"], sep$B)
  expect_gt(sep$A, bigIRT:::bigIRT_runaway_slope())
  expect_true(all(abs(fit$itemPars$A[fit$itemPars$Item != "11"]) < 5))
  expect_match(st$certificate$reason, "item 11 (A = ", fixed = TRUE)
  expect_match(st$certificate$reason, "run off", fixed = TRUE)
  expect_true(any(grepl("diverged", ws, fixed = TRUE)))

  ## With the certificate off the screen still names it.
  fit0 <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    dropPerfectScores = FALSE, laplaceRefine = "none", trainingRows = train,
    laplaceCertify = FALSE, cores = 1, verbose = 0, plot = FALSE))
  expect_null(fit0$laplaceStatus$certificate)
  expect_identical(fit0$laplaceStatus$reason, "diverged")
  expect_false(fit0$laplaceStatus$converged)
  expect_setequal(fit0$laplaceStatus$diverged$item, "11")
  expect_true(all(is.finite(fit0$pars$pcorrect)))
})

test_that("the quadrature refinement does not exploit a separated item", {
  ## On the exact marginal (a grid, as in test-agq-runoff.R) the separated
  ## item's maximum is the step itself: its profile along its own ray rises
  ## all the way, though by less than half a nat beyond a discrimination of
  ## 9. Quadrature cannot integrate a step, so its objective has a maximum
  ## near 9 instead, and the refinement now stops there. Before, held nodes
  ## took the item to 3e11, where the reported objective was 17 nats ABOVE
  ## the exact marginal at the same point -- the approximation exploited, and
  ## by chance landing on the right verdict. What is required now is what can
  ## be: no loss on the refinement's own objective, and the objective it
  ## reports is the exact one, to within quadrature's error, where it ends.
  d <- bigirt_separated_data()
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    dropPerfectScores = FALSE, laplaceRefine = "agq", cores = 1, verbose = 0, plot = FALSE))
  rf <- fit$laplaceStatus$refine
  expect_gte(rf$agq_end - rf$agq_start, -1e-3)
  grid <- seq(-7, 7, length.out = 1401)
  ip <- fit$itemPars
  j <- match(as.character(d$Item), as.character(ip$Item))
  eta <- outer(ip$A[j], grid) - ip$B[j]
  L <- rowsum(stats::plogis((2 * d$score - 1) * eta, log.p = TRUE), d$id, reorder = TRUE)
  L <- sweep(L, 2, stats::dnorm(grid, log = TRUE) + log(grid[2] - grid[1]), "+")
  mx <- apply(L, 1, max)
  exact <- sum(mx + log(rowSums(exp(L - mx)))) + nrow(L) * log(2 * pi) / 2
  expect_lt(abs(fit$optim$logLik - exact), 1)
  expect_true(all(is.finite(as.matrix(ip[, c("A", "B")]))))
  expect_true(all(is.finite(fit$pars$pcorrect)))
})

test_that("a well-behaved fit is not called diverged", {
  d <- bigirt_separated_data()
  d <- d[Item != 11L]
  fit <- suppressWarnings(fitIRT(d, pl = 2, cores = 1, verbose = 0, plot = FALSE))
  expect_null(fit$laplaceStatus$diverged)
  expect_false(identical(fit$laplaceStatus$reason, "diverged"))
  expect_false(identical(fit$laplaceStatus$certificate$status, "diverged"))
})

test_that("an item pushed onto a step is pulled back with a gain, and named", {
  ## An ordinary item put on a cliff by hand, the way the quadrature rounds
  ## put the one that found this: the certificate must call it diverged, and
  ## pulling it back along its ray must gain, since the data do not want a
  ## step there. The same state, with an ability set to exactly zero, must
  ## also materialise finite probabilities.
  d <- bigirt_separated_data()
  d <- d[Item != 11L]
  fit <- suppressWarnings(fitIRT(d, pl = 2, priors = FALSE, estMeans = character(0),
    dropPerfectScores = FALSE, laplaceRefine = "agq", keepInternals = TRUE,
    cores = 1, verbose = 0, plot = FALSE))
  expect_null(fit$laplaceStatus$diverged)
  e <- bigIRT:::bigIRT_direct_eval_from_fit(fit)
  par <- e$par
  j <- 4L
  ## The ray through item j's own threshold, at a raw discrimination where
  ## log1p(exp(raw)) overflows.
  thr <- fit$pars$B[j] / fit$pars$A[j, 1]
  par[e$layout$A[j]] <- 800
  par[e$layout$B[j]] <- 800 * thr
  final <- bigIRT:::bigIRT_laplace_direct_objective(par = par, state = e$state, sdat = e$sdat,
    prior_precision = fit$internals$priorPrecision, theta_init = fit$internals$thetaBase,
    jitter = 1e-6, keep_covariance = TRUE, context = e$context)
  cert <- bigIRT:::bigIRT_certify(final = final, par = par, context = e$context,
    state = e$state, sdat = e$sdat, prior_precision = fit$internals$priorPrecision,
    layout = e$layout, tolerance = 1e-3)
  expect_identical(cert$status, "diverged")
  expect_false(cert$converged)
  expect_setequal(cert$diverged$item, as.character(j))
  expect_setequal(cert$diverged$parameter, c("A", "B"))
  expect_equal(cert$diverged$value[cert$diverged$parameter == "A"], 800)
  expect_gt(min(cert$diverged$pullback_gain), 1)
  expect_match(cert$reason, "it gains up to", fixed = TRUE)

  ## An intercept far beyond every ability (a constant item) is named alone;
  ## one inside the screen is not named at all.
  screen <- function(b){
    p <- e$par; p[e$layout$B[j]] <- b
    st <- bigIRT:::bigIRT_laplace_unpack_direct_state(p, e$state, e$sdat, layout = e$layout)
    bigIRT:::bigIRT_runaway_items(st, e$sdat, e$layout)
  }
  far <- screen(-60)
  expect_identical(far$item, j)
  expect_identical(far$parameter, "B")
  expect_equal(far$value, -60)
  expect_identical(nrow(screen(-8)), 0L)

  state <- final$state
  state$AbilityBase[1, ] <- 0
  m <- bigIRT:::bigIRT_laplace_constrained_pars(state, e$sdat)
  expect_equal(m$A[j, 1], 800)
  expect_true(all(is.finite(m$eta_row)))
  expect_true(all(is.finite(m$pcorrect)))
  expect_true(all(m$pcorrect >= 0 & m$pcorrect <= 1))
})
