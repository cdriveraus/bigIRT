## looIRT: PSIS-LOO over the training responses.
##
## Two properties are checked directly rather than by eyeballing plausible
## output (see CLAUDE.md, "Plausible output is not evidence"):
##
##   1. looIRT must score only the rows the fit was trained on. `sdat` (aka
##      fit$prepared) carries the training flag as `trainingLogical`, not
##      `trainingRows` -- a fit's prepared data has no `trainingRows` field at
##      all, so a filter that reads it is a silent no-op and every held-out
##      response gets scored as though it were part of the likelihood.
##   2. looIRT's per-row ability must include the person-covariate offset
##      x_row'beta when personPreds are present, the same way
##      bigIRT_row_ability() and the reported `fit$pars$Ability` do. Omitting
##      it scores every response at the wrong ability whenever a person's
##      covariate value is not exactly at the reference level.

test_that("looIRT scores only the training rows, not the held-out ones", {
  skip_on_cran()
  skip_if_not_installed("loo")
  set.seed(21)

  Nsub <- 120L; Nitem <- 30L
  a <- stats::runif(Nitem, 0.8, 1.6)
  b <- stats::rnorm(Nitem)
  theta <- stats::rnorm(Nsub)

  dat <- data.table::CJ(id = seq_len(Nsub), code = seq_len(Nitem))
  dat[, Scale := "s"]
  p <- 1 / (1 + exp(-(a[dat$code] * (theta[dat$id] - b[dat$code]))))
  dat[, score := stats::rbinom(.N, 1L, p)]

  ## Hold out a random fifth of the RESPONSES (not whole persons: every item
  ## and every person keeps at least some training rows, so this is a
  ## legitimate design under the trainingRows semantics in CLAUDE.md).
  set.seed(22)
  train <- sort(sample.int(nrow(dat), floor(0.8 * nrow(dat))))

  fit <- fitIRT(dat = dat, score = "score", id = "id", item = "code",
    scale = "Scale", pl = 2L, trainingRows = train,
    marginalApprox = "laplace", priors = TRUE, normalise = FALSE,
    dropPerfectScores = FALSE, verbose = 0L, plot = FALSE, cores = 1L,
    laplaceKeepCovariance = TRUE)

  res <- suppressWarnings(looIRT(fit, draws = 50L, seed = 1L))
  info <- attr(res, "bigIRT")

  ## The defect: sdat$trainingRows does not exist (fitIRT stores
  ## trainingLogical), so the filter silently matches nothing and every row
  ## for a kept person -- trained or not -- is scored.
  expect_equal(info$responses, length(train))
  expect_lt(info$responses, nrow(dat))
})

test_that("looIRT's row ability includes the person-covariate offset", {
  skip_on_cran()
  skip_if_not_installed("loo")
  set.seed(31)

  Nsub <- 150L; Nitem <- 40L
  x <- stats::rnorm(Nsub)                 # a person covariate, varies by person
  beta_true <- 1.4                        # large relative to the ability SD
  theta <- beta_true * x + stats::rnorm(Nsub, 0, 0.4)
  a <- stats::runif(Nitem, 0.8, 1.6)
  b <- stats::rnorm(Nitem)

  dat <- data.table::CJ(id = seq_len(Nsub), code = seq_len(Nitem))
  dat[, Scale := "s"]
  dat[, xcov := x[id]]
  p <- 1 / (1 + exp(-(a[dat$code] * (theta[dat$id] - b[dat$code]))))
  dat[, score := stats::rbinom(.N, 1L, p)]

  fit <- fitIRT(dat = dat, score = "score", id = "id", item = "code",
    scale = "Scale", pl = 2L, personPreds = "xcov",
    marginalApprox = "laplace", priors = TRUE, normalise = FALSE,
    dropPerfectScores = FALSE, verbose = 0L, plot = FALSE, cores = 1L,
    laplaceKeepCovariance = TRUE, keepInternals = TRUE)

  expect_gt(abs(as.numeric(fit$pars$Abilitybeta)[1]), 0.3)

  ## Collapse the person posterior to (numerically) a point mass at the mode,
  ## so looIRT's sampled draws equal the mode regardless of the RNG stream --
  ## this isolates the offset question from sampling noise. With no sampling
  ## variability, PSIS-LOO's pointwise elpd_loo reduces exactly to the raw
  ## per-row log-likelihood at that point: log(mean(exp(c))) == c for
  ## constant c, no importance weighting needed.
  fit$personPosterior$covariance[] <- 1e-12

  ## Near-zero posterior variance by construction (see above), so the wide
  ## Pareto-k / capped-ESS diagnostics loo emits here are an artefact of the
  ## test rig, not information about the fit.
  res <- suppressWarnings(looIRT(fit, draws = 40L, seed = 2L))
  elpd <- res$pointwise[, "elpd_loo"]

  sdat <- fit$prepared
  ids <- as.integer(sdat$id)
  rows <- which(as.integer(sdat$trainingLogical) == 1L)
  pid <- ids[rows]
  re <- fit$rowEffective
  K <- as.integer(sdat$Nscales)

  ## Reference ability INCLUDES the covariate offset, exactly as
  ## bigIRT_row_ability() (R/inference.R) and the reported fit$pars$Ability
  ## both build it: AbilityBase[person] + x_row'beta.
  mode <- as.matrix(fit$personPosterior$mode)
  pp <- as.matrix(sdat$personPreds)[rows, , drop = FALSE]
  beta <- as.matrix(fit$pars$Abilitybeta)
  ability_ref <- mode[pid, , drop = FALSE] + pp %*% t(beta)

  lo <- as.matrix(re$loadings)[rows, , drop = FALSE]
  eta_ref <- rowSums(lo * ability_ref) - as.numeric(re$b)[rows]
  g_ref <- 1 / (1 + exp(-eta_ref))
  cc <- as.numeric(re$c)[rows]; dd <- as.numeric(re$d)[rows]
  p_ref <- pmin(pmax(cc + (dd - cc) * g_ref, 1e-12), 1 - 1e-12)
  y <- as.numeric(sdat$score)[rows]
  ll_ref <- y * log(p_ref) + (1 - y) * log(1 - p_ref)

  ## The defect: looIRT's eta omits the x_row'beta term entirely, so its
  ## reported per-row predictive density is the one at the BASE ability
  ## (mode[pid,] alone), which differs from ll_ref by exactly the offset's
  ## effect on the log-likelihood -- large here because beta_true is large
  ## relative to the ability SD.
  expect_equal(as.numeric(elpd), as.numeric(ll_ref), tolerance = 1e-3)
})
