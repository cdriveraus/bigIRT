## A group gap specific to one subdomain with true DIF inside the same
## subdomain, against the gap. With the effect by dimension and item DIF only
## as alternatives, item DIF won: flagged against a common effect, it covered
## the subdomain's items wholesale, took in the gap, and the next round could
## not undo it. combineBydim offers the effect by dimension with DIF screened
## again at the model that has it, which keeps the gap as a gap and the DIF
## as DIF.

combo_data <- function(seed){
  set.seed(seed)
  N <- 4000L; J <- 20L; K <- 3L
  R <- matrix(.7, K, K); diag(R) <- 1
  g <- stats::rbinom(N, 1, .5)
  th <- matrix(stats::rnorm(N * K), N) %*% chol(R) + outer(g, c(.2, .6, .2))
  subs <- c("A", "B", "C"); sub <- rep(subs, each = J); JJ <- K * J
  a <- stats::runif(JJ, .9, 1.6); b <- stats::rnorm(JJ, 0, .8)
  true_dif <- J + c(2L, 7L, 12L, 17L)
  dshift <- (seq_len(JJ) %in% true_dif) * -.6
  ii <- rep(seq_len(N), JJ); jj <- rep(seq_len(JJ), each = N)
  eta <- a[jj] * th[cbind(ii, match(sub[jj], subs))] - b[jj] + dshift[jj] * g[ii]
  list(d = data.table::data.table(id = ii, Item = jj, Scale = 1L, subdomain = sub[jj], g = g[ii],
    score = stats::rbinom(N * JJ, 1, stats::plogis(eta))), true_dif = true_dif)
}

test_that("an effect by dimension and DIF inside the dimension are both kept", {
  skip_on_cran()
  x <- combo_data(809049)
  run <- function(cb) suppressMessages(improveModel(x$d, pl = 2, personPreds = "g", personCovariates = "g",
    difCovariates = "g", itemSets = "subdomain", dif = "B", asymptotes = character(), nsimDim = 0L, maxRounds = 2L,
    cores = 1, verbose = FALSE, fitArgs = list(laplaceRefine = "agq", dropPerfectScores = FALSE), combineBydim = cb))
  items_of <- function(r) as.integer(unlist(lapply(Filter(function(t) t$covariate == "g", r$spec$dif), `[[`, "items")))
  r1 <- run(TRUE)
  it1 <- items_of(r1)
  expect_true("g" %in% r1$spec$bydim)
  expect_lte(sum(!it1 %in% x$true_dif), 2L)
  expect_gte(sum(x$true_dif %in% it1), 3L)
  ## The gap in the second subdomain: truth .6 in residual-SD units.
  expect_lt(abs(as.matrix(r1$fit$pars$Abilitybeta)[2, "g"] - .6), .15)
  ## Without the combination, item DIF takes in the gap on this data set.
  r0 <- run(FALSE)
  expect_gt(sum(!items_of(r0) %in% x$true_dif), 3L)
})
