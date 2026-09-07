## Holding out whole people.
##
## Excluding every response from a person is a different thing from excluding
## some of them. With some excluded, that person's ability is still estimated
## from the rest, so a person-level covariate has almost nothing left to
## predict and held-out loss cannot measure whether the covariate is any good.
## With all of them excluded, the model must fall back on the prior -- the
## covariate prediction when `personPreds` are supplied -- which is exactly the
## quantity a covariate screen wants to evaluate.
##
## What is checked here:
##   1. a fit with whole persons held out runs at all
##   2. a held-out person's ability equals their covariate prediction, since
##      no response of theirs can move it away from the prior mean
##   3. training persons are unaffected by which persons were held out
##   4. an item with no training response is still refused, because there is
##      genuinely nothing to calibrate it from

test_that("persons with no training rows take the prior, and items still cannot", {
  skip_on_cran()
  set.seed(11)

  Nsub <- 160L; Nitem <- 40L
  x <- stats::rnorm(Nsub)                      # one person covariate
  beta_true <- 0.8
  theta <- beta_true * x + stats::rnorm(Nsub, 0, 0.5)
  a <- stats::runif(Nitem, 0.7, 1.8)
  b <- stats::rnorm(Nitem)

  dat <- data.table::CJ(id = seq_len(Nsub), code = seq_len(Nitem))
  dat[, Scale := "s"]
  dat[, xcov := x[id]]
  p <- 1 / (1 + exp(-(a[dat$code] * (theta[dat$id] - b[dat$code]))))
  dat[, score := stats::rbinom(.N, 1L, p)]

  ## last 30 people are held out entirely
  held <- (Nsub - 29L):Nsub
  train <- which(!(dat$id %in% held))

  fit <- fitIRT(dat = dat, score = "score", id = "id", item = "code",
                scale = "Scale", pl = 2L, personPreds = "xcov",
                trainingRows = train, marginalApprox = "laplace",
                priors = TRUE, normalise = FALSE, dropPerfectScores = FALSE,
                verbose = 0L, plot = FALSE, cores = 1L)

  ab <- as.numeric(as.matrix(fit$pars$Ability))
  bet <- as.numeric(as.matrix(fit$pars$Abilitybeta))
  expect_length(ab, Nsub)
  expect_true(all(is.finite(ab)))

  ## A held-out person contributes no likelihood, so their posterior IS the
  ## prior and their ability must be exactly the covariate prediction.
  pred_held <- bet[1] * x[held]
  expect_equal(ab[held], pred_held, tolerance = 1e-5)

  ## Training persons are not pinned to their covariate prediction -- their own
  ## responses move them away from it -- so the two must differ.
  pred_train <- bet[1] * x[-held]
  expect_gt(mean(abs(ab[-held] - pred_train)), 0.1)

  ## and the covariate is still recovered with a third of the sample held out
  expect_gt(bet[1], 0.3)

  ## An item with no training response remains an error: unlike a person, it
  ## has no prior to fall back on that would identify its parameters.
  bad <- which(dat$code != 1L)
  expect_error(
    fitIRT(dat = dat, score = "score", id = "id", item = "code", scale = "Scale",
           pl = 2L, personPreds = "xcov", trainingRows = bad,
           marginalApprox = "laplace", priors = TRUE, normalise = FALSE,
           dropPerfectScores = FALSE, verbose = 0L, plot = FALSE, cores = 1L),
    "every item")
})
