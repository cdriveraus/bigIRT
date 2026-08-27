library(bigIRT)

test_that("laplace records timing and optimizer diagnostics", {
  set.seed(20260320)
  sim <- simIRT(
    Nsubs = 80,
    Nitems = 12,
    Nscales = 2,
    NitemsAnswered = c(4, 4),
    mirt = TRUE,
    loadingSparsity = 0.3
  )

  fit <- expect_warning(fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_fast",
    laplaceOuterIter = 3,
    noptimsteps = 6,
    laplaceDiagnostics = TRUE,
    verbose = 0,
    plot = FALSE
  ), "iteration limit")

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
  ## personStepSec and refreshStepSec are NA on this backend: the person modes
  ## are solved inside the objective rather than in a separate step, and there
  ## is no posterior refresh to time. The columns stay so that diagnostics from
  ## either era line up; the timings that exist must still be sane.
  expect_true(all(is.na(dx$personStepSec) | dx$personStepSec >= 0))
  expect_true(all(is.na(dx$refreshStepSec) | dx$refreshStepSec >= 0))
  expect_true(all(dx$objectiveEvalSec >= 0))
})
