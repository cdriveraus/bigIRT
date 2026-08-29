## Ability covariates that vary within a person.
##
## This path was wrong for a long time and the failure was quiet: the
## likelihood applied the per-response covariate value correctly while the
## gradient assumed the value was constant within a person, so the optimiser
## converged, reported convergence, and returned a confident wrong answer. A
## true within-person effect of 0.50 came back as -0.31, sign included, while
## between-person effects in the same fit recovered correctly.
##
## Recovery alone did not catch the first attempt at a fix -- it returned 0.5038
## for a true 0.50 with the gradient still wrong -- so the gradient is checked
## against central differences of the objective directly, away from the optimum
## where it is large enough for a relative comparison to mean anything.

sim_within <- function(seed = 11, N = 800, J = 150, per = 40, occ = 4L,
                       span = 2, beta_w = 0, beta_b = 0){
  set.seed(seed)
  A <- stats::rlnorm(J, 0, .3); B <- stats::rnorm(J, 0, 1)
  th <- stats::rnorm(N); z <- stats::rnorm(N)
  d <- data.table::data.table(id = rep(seq_len(N), each = per))
  d[, occasion := rep(seq_len(occ), length.out = .N), by = id]
  d[, t_raw := (occasion - 1) / (occ - 1) * span]
  d[, t_dev := t_raw - mean(t_raw), by = id]     # varies only within a person
  d[, zb := z[id]]                                # varies only between persons
  d[, item := sample.int(J, .N, replace = TRUE)]
  d[, Scale := "s1"]
  ability <- th[d$id] + beta_w * d$t_dev + beta_b * d$zb
  p <- 1 / (1 + exp(-A[d$item] * (ability - B[d$item])))
  d[, score := stats::rbinom(.N, 1, p)]
  d[, code := paste0("i", item)]
  d[]
}

fit_within <- function(d, preds, ...){
  bigIRT::fitIRT(dat = d, score = "score", id = "id", item = "code",
    scale = "Scale", pl = 2L, personPreds = preds, marginalApprox = "laplace",
    priors = TRUE, normalise = FALSE, dropPerfectScores = FALSE, verbose = 0L,
    plot = FALSE, cores = 2L, laplaceTol = 1e-5, laplaceTolScale = "per_obs", ...)
}

test_that("a within-person ability covariate is recovered, not just a between one", {
  skip_on_cran()
  f <- fit_within(sim_within(beta_w = 0.5), "t_dev")
  expect_equal(as.numeric(f$pars$Abilitybeta), 0.5, tolerance = 0.12)

  g <- fit_within(sim_within(seed = 12, beta_b = 0.5), "zb")
  expect_equal(as.numeric(g$pars$Abilitybeta), 0.5, tolerance = 0.12)

  ## Fitted together, a wrong within-person gradient also dragged the
  ## between-person estimate with it, so check the pair rather than each alone.
  h <- fit_within(sim_within(seed = 13, beta_w = 0.5, beta_b = 0.5), c("t_dev", "zb"))
  expect_equal(as.numeric(as.matrix(h$pars$Abilitybeta)), c(0.5, 0.5), tolerance = 0.12)
})

test_that("the ability-beta gradient matches central differences of the objective", {
  skip_on_cran()
  d <- sim_within(seed = 7, N = 400, J = 100, per = 30, beta_w = 0.4, beta_b = 0.3)
  d[, zt := t_dev * zb]      # a second within-varying predictor
  f <- fit_within(d, c("t_dev", "zb", "zt"), keepInternals = TRUE)

  dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE, cores = 1L)
  idx <- dd$layout$ability_beta
  expect_gt(length(idx), 0L)
  obj <- function(par)
    bigIRT:::bigIRT_laplace_direct_objective(par = par, state = dd$state,
      sdat = dd$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = dd$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = dd$context, tol = 1e-10, max_iter = 400L)

  ## Away from the optimum, where the gradient is not near zero and a relative
  ## comparison is informative.
  par <- dd$par
  par[idx] <- par[idx] + c(0.6, -0.5, 0.4)[seq_along(idx)]
  analytic <- obj(par)$approx_grad[idx]

  eps <- 1e-5
  numeric_grad <- vapply(seq_along(idx), function(i){
    pu <- pd <- par
    pu[idx[i]] <- pu[idx[i]] + eps
    pd[idx[i]] <- pd[idx[i]] - eps
    (obj(pu)$value - obj(pd)$value) / (2 * eps)
  }, numeric(1))

  expect_gt(max(abs(numeric_grad)), 1)          # the test would be vacuous otherwise
  expect_equal(analytic, numeric_grad, tolerance = 1e-5)
})

test_that("the native within-person kernel agrees with the R reference", {
  skip_on_cran()
  d <- sim_within(seed = 9, N = 400, J = 100, per = 30, beta_w = 0.4, beta_b = 0.3)
  f <- fit_within(d, c("t_dev", "zb"), keepInternals = TRUE)
  dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE, cores = 1L)
  ev <- bigIRT:::bigIRT_laplace_direct_objective(par = dd$par, state = dd$state,
    sdat = dd$sdat, prior_precision = f$internals$priorPrecision,
    theta_init = dd$eval$posterior$theta_mode, keep_covariance = TRUE,
    context = dd$context, tol = 1e-8, max_iter = 200L)
  rc <- dd$context$row_context
  re <- bigIRT:::bigIRT_laplace_row_effective(dd$state, dd$sdat,
    thetaBase = ev$posterior$theta_mode, rows = dd$context$train_rows, context = rc)
  vi <- which(bigIRT:::bigIRT_laplace_person_pred_within_varying(rc))
  expect_gt(length(vi), 0L)

  pt <- bigIRT:::bigIRT_laplace_person_row_terms(dd$sdat, ev$posterior, re, rc)
  cpp <- bigIRT:::bigIRT_laplace_ability_beta_rows(dd$sdat, ev$posterior, re, rc,
           gacc = pt$slope, vary_idx = vi, impl = "cpp")
  rref <- bigIRT:::bigIRT_laplace_ability_beta_rows(dd$sdat, ev$posterior, re, rc,
            gacc = pt$slope, vary_idx = vi, impl = "R")
  expect_equal(cpp, rref, tolerance = 1e-8)

  ## and the grouped single-sweep kernel against the same
  fu <- bigIRT:::bigIRT_laplace_person_beta_fused(dd$sdat, ev$posterior, re, rc, vi)
  expect_false(is.null(fu))
  expect_equal(fu$score, pt$score, tolerance = 1e-10)
  expect_equal(fu$slope, pt$slope, tolerance = 1e-10)
  expect_equal(fu$beta, cpp, tolerance = 1e-8)
})

test_that("the within-varying check is cached and correct", {
  skip_on_cran()
  d <- sim_within(seed = 21, N = 200, J = 60, per = 20, beta_w = 0.4)
  f <- fit_within(d, c("t_dev", "zb"), keepInternals = TRUE)
  dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE, cores = 1L)
  rc <- dd$context$row_context
  expect_false(is.null(rc$person_pred_varies))
  expect_equal(unname(rc$person_pred_varies), c(TRUE, FALSE))
  ## the cached answer must match one derived from scratch
  expect_equal(unname(bigIRT:::bigIRT_laplace_person_pred_varies(rc$person_pred, rc$ids)),
               c(TRUE, FALSE))
})

test_that("group means match a row-at-a-time accumulation", {
  set.seed(3)
  n <- 20000L; N <- 500L; P <- 3L
  id <- sample.int(N, n, replace = TRUE)
  X <- matrix(stats::rnorm(n * P), n, P)
  ref <- matrix(0, N, P)
  for(i in seq_len(n)) ref[id[i], ] <- ref[id[i], ] + X[i, , drop = FALSE]
  ref <- ref / pmax(tabulate(id, nbins = N), 1)
  expect_equal(bigIRT:::bigIRT_laplace_group_means(X, id, N, P), ref)
  ## groups with no rows stay at zero rather than becoming NaN
  expect_true(all(bigIRT:::bigIRT_laplace_group_means(X, id, N + 5L, P)[(N + 1L):(N + 5L), ] == 0))
})
