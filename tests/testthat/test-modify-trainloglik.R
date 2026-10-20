## improveModel records, for every batch, the training persons' marginal
## log-likelihood and the number of free parameters, so that the held-out gate
## can be compared with in-sample criteria (likelihood ratio, AIC, BIC) on the
## same batches.

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
    maxRounds = 1L, cores = 1, verbose = FALSE)
  p <- r$path
  expect_true(all(c("train_loglik_before", "train_loglik_after", "df_before", "df_after", "train_persons") %in% names(p)))
  ## The fit's own log-likelihood is the quadrature marginal over the training
  ## persons (the fit's objective is that marginal, with laplaceRefine =
  ## "agq"), by the same computation heldoutLogLik() makes for the held-out.
  bf <- r$base_fit
  trn <- which(as.integer(bf$prepared$trainingLogical) == 1L)
  quad <- bigIRT:::bigIRT_modify_marginal(bf, trn)$loglik
  expect_equal(p$train_loglik_before[1], quad, tolerance = 1e-6)
  expect_equal(p$train_persons[1], N - length(r$holdout_persons))
  ## A DIF batch adds one coefficient per item it names (g is binary), not
  ## one per item in the bank: the other items' columns are zero.
  dif <- p[grepl("^dif-g", batch)]
  expect_true(nrow(dif) >= 1L)
  expect_equal(dif$df_after - dif$df_before, dif$terms)
  ## Training and held-out persons together are every person.
  expect_equal(p$train_persons[1] + heldoutLogLik(bf)$persons, N)
})

test_that("a clustered gate equals the default with singleton clusters, and is CR1 otherwise", {
  set.seed(5)
  a <- list(by_person = stats::setNames(rnorm(300), as.character(1:300)))
  b <- list(by_person = a$by_person + rnorm(300, .05, .3))
  g0 <- bigIRT:::bigIRT_modify_gain(a, b)
  g1 <- bigIRT:::bigIRT_modify_gain(a, b, clusters = stats::setNames(as.character(1:300), as.character(1:300)))
  expect_equal(g1$z, g0$z, tolerance = 1e-12)
  ## Clusters of ten, with a shared shock: the clustered z is smaller.
  cl <- stats::setNames(as.character((0:299) %/% 10), as.character(1:300))
  b2 <- list(by_person = b$by_person + rep(rnorm(30, 0, .5), each = 10))
  d <- b2$by_person - a$by_person
  u <- tapply(d - mean(d), cl, sum)
  expect_equal(bigIRT:::bigIRT_modify_gain(a, b2, clusters = cl)$z, sum(d) / sqrt(30 / 29 * sum(u^2)), tolerance = 1e-12)
  expect_lt(abs(bigIRT:::bigIRT_modify_gain(a, b2, clusters = cl)$z), abs(bigIRT:::bigIRT_modify_gain(a, b2)$z))
})
