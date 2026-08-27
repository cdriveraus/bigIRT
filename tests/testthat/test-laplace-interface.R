test_that("fit input validation rejects malformed fast-path inputs", {
  dat <- data.frame(id = c(1, 1, 2, 2), Item = c("a", "b", "a", "b"),
    Scale = 1, score = c(0, 1, 1, 0))
  expect_error(fitIRT(dat, pl = 0), "pl")
  bad <- dat; bad$score[1] <- 2
  expect_error(fitIRT(bad), "binary")
  expect_error(fitIRT(dat, trainingRows = c(1, 1)), "unique integer")
  expect_error(fitIRT(dat, trainingRows = 9), "original input")
})

test_that("fit validation accepts data frames and data tables consistently", {
  dat <- data.frame(id = c(2, 1, 2, 1), Item = c("a", "a", "b", "b"),
    Scale = 1, score = c(0, 1, 1, 0))
  expect_silent(bigIRT:::bigIRT_validate_fit_inputs(
    dat, "score", "id", "Item", "Scale", 2,
    list(iter = 1, cores = 1), c(1L, 2L, 3L, 4L)))
  expect_silent(bigIRT:::bigIRT_validate_fit_inputs(
    data.table::as.data.table(dat), "score", "id", "Item", "Scale", 2,
    list(iter = 1, cores = 1), c(1L, 2L, 3L, 4L)))
})

test_that("direct Laplace fits both item and person predictor models", {
  ## Both were refused until the Abilitybeta gradient was derived. It now agrees
  ## with central finite differences to the printed precision across one and two
  ## scales, one and two predictors, and tight or loose beta priors, so the
  ## coefficients are checked against the values they were simulated from.
  set.seed(9)
  ip <- matrix(stats::rnorm(40 * 2, 0, .3), 40, 2)
  simI <- simIRT(Nsubs = 600, Nitems = 40, Nscales = 1, NitemsAnswered = 20,
    AMean = 1, ASD = .2, BMean = 0, BSD = .5, AbilityMean = 0, AbilitySD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
    itemPreds = ip, BitemPredEffects = matrix(c(.8, -.5), 1, 2))
  fitI <- fitIRT(simI$dat, pl = 2, marginalApprox = "laplace_direct",
    BitemPreds = c("V1", "V2"), itemSpecificBetas = FALSE,
    dropPerfectScores = FALSE, normalise = FALSE, cores = 1,
    laplaceOuterIter = 30, verbose = 0, plot = FALSE)
  expect_length(as.numeric(fitI$pars$Bbeta), 2)
  expect_true(all(is.finite(as.numeric(fitI$pars$Bbeta))))

  set.seed(11)
  eff <- matrix(c(.6, -.35), 1, 2)
  simP <- simIRT(Nsubs = 1500, Nitems = 60, Nscales = 1, NitemsAnswered = 25,
    AMean = 1, ASD = .2, BMean = 0, BSD = .8, AbilityMean = 0, AbilitySD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0,
    personPreds = matrix(stats::rnorm(1500 * 2), 1500, 2),
    AbilityPredEffects = eff)
  fitP <- fitIRT(simP$dat, pl = 2, marginalApprox = "laplace_direct",
    personPreds = c("V1", "V2"), dropPerfectScores = FALSE, normalise = FALSE,
    cores = 1, laplaceOuterIter = 40, verbose = 0, plot = FALSE)
  ab <- as.numeric(fitP$pars$Abilitybeta)
  expect_length(ab, 2)
  expect_true(all(is.finite(ab)))
  expect_equal(ab, as.numeric(eff), tolerance = .12)
})
