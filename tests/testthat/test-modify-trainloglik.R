## improveModel(trainLogLik = TRUE) records, for every batch, the training
## persons' marginal log-likelihood and the number of free parameters, so that
## the held-out gate can be compared with in-sample criteria (likelihood
## ratio, AIC, BIC) on the same batches.

test_that("training log-likelihood is the fit's marginal and parameters count only what can move", {
  skip_on_cran()
  set.seed(3)
  N <- 800; J <- 12
  g <- rbinom(N, 1, .5); th <- rnorm(N) + .6 * g
  a <- runif(J, .8, 1.6); b <- rnorm(J)
  d <- data.table::CJ(id = 1:N, Item = 1:J)
  d[, g := g[id]]
  d[, score := rbinom(.N, 1, plogis(a[Item] * th[id] - b[Item] + (Item <= 2) * 1.2 * g))]
  d[, Scale := 1L]
  r <- improveModel(d, pl = 2, personCovariates = "g", dif = "B", asymptotes = character(), nsimDim = 0L,
    maxRounds = 1L, cores = 1, verbose = FALSE, trainLogLik = TRUE)
  p <- r$path
  expect_true(all(c("train_loglik_before", "train_loglik_after", "df_before", "df_after", "train_persons") %in% names(p)))
  ## The quadrature marginal over the training persons is the fit's own
  ## log-likelihood (the fit's objective is that marginal, with laplaceRefine
  ## = "agq"), and the same as fitting the training persons alone.
  bf <- r$base_fit
  tl <- bigIRT:::bigIRT_modify_train_loglik(bf)
  expect_equal(tl$loglik, as.numeric(logLik(bf, effective = FALSE)), tolerance = 1e-6)
  expect_equal(p$train_loglik_before[1], tl$loglik)
  expect_equal(p$train_persons[1], N - length(r$holdout_persons))
  ## A DIF batch adds one coefficient per item it names (g is binary), not
  ## one per item in the bank: the other items' columns are zero.
  dif <- p[grepl("^dif-g", batch)]
  expect_true(nrow(dif) >= 1L)
  expect_equal(dif$df_after - dif$df_before, dif$terms)
  ## Training and held-out persons together are every person.
  h <- heldoutLogLik(bf)
  expect_equal(tl$persons + h$persons, N)
})
