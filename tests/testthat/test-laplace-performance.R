library(bigIRT)

test_that("laplace_em records timing and optimizer diagnostics", {
  set.seed(20260320)
  sim <- simIRT(
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
    marginalApprox = "laplace_em",
    laplaceOuterIter = 3,
    noptimsteps = 6,
    laplaceDiagnostics = TRUE,
    verbose = 0,
    plot = FALSE
  )

  dx <- fit$laplaceDiagnostics
  expect_s3_class(dx, "data.table")
  expect_true(all(c(
    "personStepSec",
    "itemStepSec",
    "refreshStepSec",
    "objectiveEvalSec",
    "outerIterSec",
    "itemTargetEvals",
    "itemGradNorm"
  ) %in% names(dx)))
  expect_true(all(is.finite(dx$itemTargetEvals)))
  expect_true(all(dx$itemTargetEvals >= 1))
  expect_true(all(dx$outerIterSec >= 0))
  expect_true(all(dx$itemStepSec >= 0))
  expect_true(all(dx$personStepSec >= 0))
})
