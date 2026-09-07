library(bigIRT)

## `laplaceStatus$beta_frozen` used to be set from `sdat$NpersonPreds > 0`, so
## it read TRUE for every fit that had person predictors at all -- exactly the
## fits in which the coefficients are estimated -- and `print()` then announced
## that the effects "were held fixed". The flag has to describe what the
## optimiser did with the coefficients, not whether any exist.

test_that("a fit that moves Abilitybeta off its start does not report frozen effects", {
  ## Deliberately not skipped on CRAN: the fit is a second, and a flag that
  ## contradicts the fit it describes is exactly what a check should catch.
  set.seed(7)
  N <- 400L; J <- 50L; per <- 15L
  A <- rlnorm(J, 0, 0.25); B <- rnorm(J, 0, 1)
  theta <- rnorm(N); sesv <- rnorm(N)
  d <- data.table::data.table(id = rep(seq_len(N), each = per))
  d[, ses := sesv[id]]
  d[, Item := sample.int(J, .N, replace = TRUE)]
  d[, Scale := "s1"]
  ab <- theta[d$id] + 0.6 * d$ses
  d[, score := rbinom(.N, 1L, 1 / (1 + exp(-A[Item] * (ab - B[Item]))))]

  fit <- fitIRT(d, pl = 2, cores = 1, marginalApprox = "laplace",
    personPreds = "ses", dropPerfectScores = FALSE, normalise = FALSE,
    keepInternals = TRUE, verbose = 0, plot = FALSE)

  ## The premise of the test, asserted rather than assumed: the Laplace path
  ## starts every ability coefficient at exactly zero, so a coefficient that
  ## ends up far from zero can only have got there by being estimated.
  start <- bigIRT:::bigIRT_laplace_initial_state(fit$internals$sdat)$Abilitybeta
  expect_true(all(start == 0))

  beta <- drop(fit$pars$Abilitybeta)
  expect_length(beta, 1L)
  expect_gt(abs(beta - 0), 0.3)

  expect_false(isTRUE(fit$laplaceStatus$beta_frozen))
  expect_false(isTRUE(fit$laplaceStatus$frozen_effects))
  expect_false(any(grepl("held fixed", capture.output(print(fit)), fixed = TRUE)))
})

test_that("beta_frozen tracks the free layout, not the presence of predictors", {
  free <- list(ability_beta = 4:6)
  none <- list(ability_beta = integer())

  ## predictors exist and their coefficients are free -> nothing is frozen
  expect_false(bigIRT:::bigIRT_laplace_beta_frozen(list(NpersonPreds = 2L), free))
  ## predictors exist but were kept out of the optimised vector -> frozen
  expect_true(bigIRT:::bigIRT_laplace_beta_frozen(list(NpersonPreds = 2L), none))
  expect_true(bigIRT:::bigIRT_laplace_beta_frozen(list(NpersonPreds = 2L), NULL))
  ## no predictors -> there is nothing that could be frozen
  expect_false(bigIRT:::bigIRT_laplace_beta_frozen(list(NpersonPreds = 0L), none))
  expect_false(bigIRT:::bigIRT_laplace_beta_frozen(list(NpersonPreds = 0L), free))
})
