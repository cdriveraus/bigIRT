library(bigIRT)

test_that("laplace item objective gradient matches finite differences", {
  id <- c(1L, 1L, 2L, 2L)
  score <- c(1L, 0L, 1L, 0L)
  row_ability <- matrix(c(
    0.2, -0.1,
    0.2, -0.1,
    -0.3, 0.4,
    -0.3, 0.4
  ), ncol = 2, byrow = TRUE)
  loadings <- matrix(c(
    0.8, 0.1,
    0.7, 0.0,
    0.2, 0.6,
    0.0, 0.9
  ), ncol = 2, byrow = TRUE)
  b <- c(0.1, -0.3, 0.2, 0.4)
  c <- rep(0, 4)
  d <- rep(1, 4)
  prior <- array(0, dim = c(2, 2, 2))
  prior[, , 1] <- diag(2)
  prior[, , 2] <- diag(2)

  obj <- bigIRT:::bigIRT_laplace_item_objective_cpp_impl(
    id = id,
    score = score,
    row_ability = row_ability,
    b = b,
    c = c,
    d = d,
    loadings = loadings,
    prior_precision = prior,
    jitter = 1e-8,
    max_attempts = 8L
  )

  fd_grad <- function(i, j, eps = 1e-6) {
    plus <- loadings
    minus <- loadings
    plus[i, j] <- plus[i, j] + eps
    minus[i, j] <- minus[i, j] - eps
    obj_plus <- bigIRT:::bigIRT_laplace_item_objective_cpp_impl(
      id = id, score = score, row_ability = row_ability,
      b = b, c = c, d = d, loadings = plus,
      prior_precision = prior, jitter = 1e-8, max_attempts = 8L
    )$objective
    obj_minus <- bigIRT:::bigIRT_laplace_item_objective_cpp_impl(
      id = id, score = score, row_ability = row_ability,
      b = b, c = c, d = d, loadings = minus,
      prior_precision = prior, jitter = 1e-8, max_attempts = 8L
    )$objective
    (obj_plus - obj_minus) / (2 * eps)
  }

  expect_equal(obj$grad_loadings[1, 1], fd_grad(1, 1), tolerance = 1e-6)
  expect_equal(obj$grad_loadings[1, 2], fd_grad(1, 2), tolerance = 1e-6)
  expect_equal(obj$grad_loadings[3, 2], fd_grad(3, 2), tolerance = 1e-6)
})

test_that("laplace_em returns posterior outputs and finite MIRT parameters", {
  set.seed(123)
  sim <- simIRT(
    Nsubs = 120,
    Nitems = 12,
    Nscales = 2,
    NitemsAnswered = 6,
    mirt = TRUE,
    loadingSparsity = 0.4
  )

  fit <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_em",
    laplaceOuterIter = 3,
    noptimsteps = 10,
    verbose = 0,
    plot = FALSE
  )

  expect_true(is.list(fit$personPosterior))
  expect_true(all(c("mode", "precision", "precision_chol", "logdet_precision") %in% names(fit$personPosterior)))
  expect_true(is.list(fit$laplaceStatus))
  expect_true(is.matrix(fit$pars$A))
  expect_true(all(is.finite(fit$pars$A)))
  expect_true(all(fit$pars$A >= 0))
  expect_equal(dim(fit$pars$row_loadings), c(fit$dat$Nobs, fit$dat$Nscales))
  expect_equal(dim(fit$personPosterior$mode), c(fit$dat$Nsubs, fit$dat$Nscales))
  expect_equal(dim(fit$personPosterior$precision), c(fit$dat$Nscales, fit$dat$Nscales, fit$dat$Nsubs))
  expect_false("samples" %in% names(fit$personPosterior))
})

test_that("laplace_em does not call the legacy JML optimizer", {
  local_mocked_bindings(
    optimIRT = function(...) stop("legacy optimIRT/JML path should not run for laplace_em"),
    .package = "bigIRT"
  )

  set.seed(456)
  sim <- bigIRT::simIRT(
    Nsubs = 40,
    Nitems = 8,
    Nscales = 2,
    NitemsAnswered = 4,
    mirt = TRUE,
    loadingSparsity = 0.5
  )

  fit <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_em",
    laplaceOuterIter = 1,
    noptimsteps = 3,
    verbose = 0,
    plot = FALSE
  )

  expect_true(is.list(fit$personPosterior))
  expect_identical(fit$laplaceStatus$initialized_from, "prior_anchored")
})

test_that("laplace_direct returns posterior outputs and avoids legacy JML", {
  local_mocked_bindings(
    optimIRT = function(...) stop("legacy optimIRT/JML path should not run for laplace_direct"),
    .package = "bigIRT"
  )

  set.seed(789)
  sim <- bigIRT::simIRT(
    Nsubs = 40,
    Nitems = 8,
    Nscales = 2,
    NitemsAnswered = 4,
    mirt = TRUE,
    loadingSparsity = 0.4
  )

  fit <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_direct",
    laplaceOuterIter = 4,
    noptimsteps = 4,
    verbose = 0,
    plot = FALSE
  )

  expect_true(is.list(fit$personPosterior))
  expect_true(all(c("mode", "precision", "precision_chol", "logdet_precision") %in% names(fit$personPosterior)))
  expect_true(all(is.finite(fit$pars$A)))
  expect_identical(fit$laplaceStatus$initialized_from, "prior_anchored")
  expect_true(isTRUE(fit$laplaceStatus$direct_objective))
  expect_true(isTRUE(fit$laplaceStatus$approximate_gradient))
})

test_that("laplace_direct final person parameters use the resolved posterior modes", {
  set.seed(20260329)
  sim <- bigIRT::simIRT(
    Nsubs = 80,
    Nitems = 12,
    Nscales = 2,
    NitemsAnswered = c(4, 4),
    mirt = TRUE,
    loadingSparsity = 0.3
  )

  fit <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_direct",
    laplaceOuterIter = 12,
    laplaceGradTol = 1e-3,
    verbose = 0,
    plot = FALSE
  )

  expect_equal(unname(as.matrix(fit$pars$Ability)), unname(as.matrix(fit$personPosterior$mode)), tolerance = 1e-8)
  expect_gt(sd(as.numeric(fit$pars$Ability)), 0)
})
