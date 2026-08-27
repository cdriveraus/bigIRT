## fitIRT sorts the data by person internally.  Everything a user indexes with
## their own row numbers must survive that, and the row-ordering convention
## documented in ?fitIRT must hold.  A regression here is silent and severe:
## before this was fixed, `pcorrect` came back in internal order, so held-out
## evaluation on any data set that was not already sorted by person scored
## predictions against the wrong responses.

test_that("per-response predictions are invariant to input row order", {
  skip_on_cran()
  set.seed(11)
  sim <- simIRT(Nsubs = 120, Nitems = 30, Nscales = 1, NitemsAnswered = 12,
    AMean = 1, ASD = .2, BMean = 0, BSD = 1,
    logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
  dat <- data.table::copy(sim$dat)
  n <- nrow(dat)
  set.seed(5)
  perm <- sample(n)

  fit_it <- function(d) suppressMessages(fitIRT(d, pl = 2, cores = 1, priors = TRUE,
    ebayes = FALSE, marginalApprox = "laplace_fast", dropPerfectScores = FALSE,
    normalise = FALSE, estimateAbilityCorr = FALSE, verbose = 0, plot = FALSE))

  f0 <- fit_it(dat)
  f1 <- fit_it(dat[perm])

  ## Row k of the permuted input is row perm[k] of the original.
  expect_equal(as.numeric(f1$pars$pcorrect), as.numeric(f0$pars$pcorrect)[perm],
    tolerance = 1e-3)
  expect_equal(as.numeric(f1$pars$p), as.numeric(f0$pars$p)[perm], tolerance = 1e-3)
  expect_length(as.numeric(f0$pars$pcorrect), n)

  ## The predictions must actually predict the responses they are paired with.
  expect_gt(stats::cor(as.numeric(f0$pars$pcorrect), dat$score), .3)
  expect_gt(stats::cor(as.numeric(f1$pars$pcorrect), dat[perm]$score), .3)

  ## originalRow is the documented bridge between the two orderings.
  o0 <- as.integer(f0$pars$originalRow)
  expect_length(o0, n)
  expect_setequal(o0, seq_len(n))
})

test_that("training rows and predictors are applied to the intended responses", {
  skip_on_cran()
  set.seed(11)
  sim <- simIRT(Nsubs = 150, Nitems = 30, Nscales = 1, NitemsAnswered = 12,
    AMean = 1, ASD = .2, BMean = 0, BSD = 1,
    logitCMean = -2.4, logitCSD = .3, logitDMean = 2.4, logitDSD = .3,
    itemPreds = matrix(scale(seq_len(30)), ncol = 1, dimnames = list(NULL, "item_x")),
    BitemPredEffects = matrix(.35, 1, 1),
    personPreds = matrix(scale(seq_len(150)), ncol = 1, dimnames = list(NULL, "person_x")),
    AbilityPredEffects = matrix(-.25, 1, 1))
  dat <- data.table::copy(sim$dat)
  n <- nrow(dat)
  set.seed(5)
  perm <- sample(n)
  set.seed(3)
  train <- sort(sample(n, floor(n / 2)))

  fit_it <- function(d, tr) suppressMessages(fitIRT(d, pl = 4, cores = 1, priors = TRUE,
    ebayes = FALSE, trainingRows = tr, BitemPreds = "item_x", personPreds = "person_x",
    marginalApprox = "laplace_fast", dropPerfectScores = FALSE, normalise = FALSE,
    estimateAbilityCorr = FALSE, verbose = 0, plot = FALSE))

  f0 <- fit_it(dat, train)
  ## The same physical responses, addressed in the permuted numbering.
  f1 <- fit_it(dat[perm], which(perm %in% train))

  for(nm in c("A", "B", "C", "D")){
    expect_equal(as.numeric(f0$pars[[nm]]), as.numeric(f1$pars[[nm]]), tolerance = 1e-6,
      info = nm)
  }
  expect_equal(as.numeric(f0$pars$Bbeta), as.numeric(f1$pars$Bbeta), tolerance = 1e-6)
  expect_equal(as.numeric(f0$pars$Abilitybeta), as.numeric(f1$pars$Abilitybeta),
    tolerance = 1e-6)
})

test_that("inv_logit does not overflow for extreme linear predictors", {
  ## exp(x)/(1+exp(x)) returns NaN for x above about 710.  Fitted
  ## discriminations that large do occur, and a single NaN silently removed a
  ## whole replication from benchmark summaries.
  expect_equal(bigIRT:::inv_logit(800), 1)
  expect_equal(bigIRT:::inv_logit(-800), 0)
  expect_equal(bigIRT:::inv_logit(0), .5)
  p <- IRTcurve(A = matrix(618, 1, 1), B = -12.85, C = .25, D = .878,
    theta = matrix(c(-3, 0, 3), ncol = 1), plot = FALSE)
  expect_true(all(is.finite(p)))
})
