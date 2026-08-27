## Latent-correlation estimation on the fast Laplace backend.
##
## The M-step is Sigma = mean_j ( thetahat_j thetahat_j' + V_j ), rescaled to
## unit diagonal.  The V_j term is easy to drop by accident -- it only appears
## when the person posterior covariances are retained -- and dropping it leaves
## the correlation sitting at its starting value while the fit still reports a
## healthy stopping status.  These tests pin down both the recovery and the
## failure mode.

test_that("laplace recovers a known latent correlation", {
  skip_on_cran()
  trueR <- matrix(c(1, .7, .5, .7, 1, .6, .5, .6, 1), 3, 3)
  set.seed(31)
  sim <- simIRT(Nsubs = 4000, Nitems = 60, Nscales = 3, NitemsAnswered = c(8, 8, 8),
    AMean = 1, ASD = .2, BMean = 0, BSD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
    AbilityCorr = trueR)

  fit <- suppressMessages(fitIRT(sim$dat, pl = 2, cores = 1, priors = TRUE, ebayes = FALSE,
    marginalApprox = "laplace_fast", dropPerfectScores = FALSE, normalise = FALSE,
    estimateAbilityCorr = TRUE, verbose = 0, plot = FALSE,
    laplaceTol = 0.1, laplaceOuterIter = 300L))

  est <- fit$abilityPrior$corr
  expect_true(isTRUE(fit$abilityPrior$estimated))
  expect_equal(dim(est), c(3L, 3L))
  expect_equal(diag(est), rep(1, 3), tolerance = 1e-8)
  expect_equal(est, t(est), tolerance = 1e-10)
  expect_true(min(eigen(est, symmetric = TRUE, only.values = TRUE)$values) > 0)
  ## The M-step must actually have run; a silent no-op is the failure mode.
  ## The estimate must actually have moved; a silent no-op is the failure
  ## mode. The backend no longer runs a separate correlation M-step to count,
  ## so this asks the estimate itself how far it travelled from the identity.
  expect_gt(max(abs(est - diag(3))), .05)
  expect_lt(max(abs(est - trueR)), .12)
})

test_that("the latent correlation stays at the identity when not requested", {
  skip_on_cran()
  set.seed(31)
  sim <- simIRT(Nsubs = 1500, Nitems = 36, Nscales = 3, NitemsAnswered = c(6, 6, 6),
    AMean = 1, ASD = .2, BMean = 0, BSD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
    AbilityCorr = matrix(c(1, .6, .6, .6, 1, .6, .6, .6, 1), 3, 3))

  fit <- suppressMessages(fitIRT(sim$dat, pl = 2, cores = 1, priors = TRUE, ebayes = FALSE,
    marginalApprox = "laplace_fast", dropPerfectScores = FALSE, normalise = FALSE,
    estimateAbilityCorr = FALSE, verbose = 0, plot = FALSE,
    laplaceTol = 0.1, laplaceOuterIter = 100L))

  expect_false(isTRUE(fit$abilityPrior$estimated))
  expect_equal(fit$abilityPrior$corr, diag(3), tolerance = 1e-10)
  ## Nothing to count any more; the identity assertion above is the check.
})

test_that("the correlation estimate corrects the bias in the person modes", {
  ## Reading a correlation off the person modes does not work, and once the
  ## prior itself is correlated the modes are pulled toward each other and
  ## overstate it: across seeds 21 to 23 the modes gave .79, .78 and .76 against
  ## a truth of .60, while the estimate gave .65, .63 and .63. The estimate has
  ## to be the closer of the two.
  skip_on_cran()
  set.seed(21)
  trueR <- matrix(c(1, .6, .6, 1), 2, 2)
  sim <- simIRT(Nsubs = 1500, Nitems = 40, Nscales = 2, NitemsAnswered = c(10, 10),
    AMean = 1, ASD = .2, BMean = 0, BSD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
    AbilityCorr = trueR)
  fit <- suppressMessages(fitIRT(sim$dat, pl = 2, cores = 1, priors = TRUE,
    ebayes = FALSE, marginalApprox = "laplace", dropPerfectScores = FALSE,
    normalise = FALSE, estimateAbilityCorr = TRUE, verbose = 0, plot = FALSE,
    laplaceOuterIter = 60))

  est <- as.matrix(fit$abilityPrior$corr)[1, 2]
  modes_only <- stats::cor(fit$personPosterior$mode)[1, 2]
  expect_lt(abs(est - trueR[1, 2]), abs(modes_only - trueR[1, 2]))
  expect_lt(abs(est - trueR[1, 2]), .15)
})
