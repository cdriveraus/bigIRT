## Purification that flags every item had nothing left to anchor on and
## stopped with an error, which modificationTests (and so improveModel)
## swallowed: a failed DIF screen looked like a covariate with no DIF
## candidates. Data that do it: a group gap in one of two correlated
## subdomains, fitted with one dimension, so every item shows DIF along the
## group (one subdomain's items easier, the other's harder).
floor_data <- function(seed = 7L, N = 3000L, J = 10L){
  set.seed(seed)
  R <- matrix(c(1, .5, .5, 1), 2)
  g <- stats::rbinom(N, 1, .5)
  th <- matrix(stats::rnorm(2 * N), N) %*% chol(R) + outer(g, c(0, 1))
  sub <- rep(1:2, each = J)
  ii <- rep(seq_len(N), 2 * J); jj <- rep(seq_len(2 * J), each = N)
  b <- stats::rnorm(2 * J, 0, .5)
  data.table::data.table(id = ii, Item = jj, Scale = 1L, g = g[ii],
    score = stats::rbinom(N * 2 * J, 1, stats::plogis(1.2 * th[cbind(ii, sub[jj])] - b[jj])))
}
floor_fit <- function(d) fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale", pl = 2,
  personPreds = "g", cores = 1, verbose = 0)
floor_cov <- function(d){
  cv <- data.frame(id = as.character(unique(d$id)))
  cv$g <- factor(d[, list(g = g[1]), by = id]$g)
  cv
}

test_that("purification keeps an anchor floor instead of failing when every item is flagged", {
  d <- floor_data()
  f <- floor_fit(d)
  cv <- floor_cov(d)
  ## Unpurified, every item is significant: the case the floor is for.
  u <- scoreTest(f, cv, parameters = "B", purify = FALSE, cores = 1)
  expect_equal(sum(stats::p.adjust(u$tests$p, "BH") < .05), nrow(u$tests))
  expect_warning(r <- scoreTest(f, cv, parameters = "B", purify = TRUE, cores = 1),
    class = "bigIRT_anchor_floor")
  expect_equal(nrow(r$tests), 20L)
  ## A tenth of the tested items stay anchors, and they are the items with
  ## the least evidence of DIF in the final stage.
  expect_gte(sum(r$tests$anchor), 2L)
  expect_gte(min(r$tests$p[r$tests$anchor]), max(r$tests$p[!r$tests$anchor]))
  expect_true(all(is.finite(r$tests$p)))
  ## A count works as well as a share.
  expect_warning(r5 <- scoreTest(f, cv, parameters = "B", purify = TRUE, cores = 1, purifyMinAnchor = 5),
    class = "bigIRT_anchor_floor")
  expect_gte(sum(r5$tests$anchor), 5L)
})

test_that("modificationTests passes the anchor floor on, and reports a screen that fails", {
  d <- floor_data()
  f <- floor_fit(d)
  expect_warning(s <- modificationTests(f, d, personCovariates = "g", difCovariates = "g", dif = "B",
    asymptotes = character(), itemSets = character(), exploratory = FALSE), class = "bigIRT_anchor_floor")
  ## The DIF screen is there, not silently absent.
  expect_true(!is.null(s$dif) && nrow(s$dif[covariate == "g"]) == 20L)
  ## A screen that errors is reported by name, not dropped in silence (an
  ## infinite covariate value stops the score computation).
  hv <- stats::rnorm(length(unique(d$id))); hv[1] <- Inf
  d$h <- hv[match(d$id, unique(d$id))]
  w <- character()
  s2 <- withCallingHandlers(modificationTests(f, d, personCovariates = c("g", "h"), difCovariates = "h",
    dif = "B", asymptotes = character(), itemSets = character(), exploratory = FALSE),
    warning = function(x){ w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("failed and are left out", w) & grepl("along h", w)))
})

test_that("the anchor floor stays out of the way when nothing is flagged or nothing is tested", {
  set.seed(2); N <- 600L; J <- 10L
  th <- stats::rnorm(N); ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
  d <- data.table::data.table(id = ii, Item = jj, Scale = 1L, score = stats::rbinom(N * J, 1, stats::plogis(th[ii])))
  f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale", pl = 2, cores = 1, verbose = 0)
  cv <- data.frame(id = as.character(seq_len(N)))
  ## A covariate with no variation: nothing is tested, so nothing is anchored by force.
  cv$h <- rep(1, N)
  expect_no_warning(r <- scoreTest(f, cv, parameters = "B", purify = TRUE, cores = 1))
  expect_true(all(is.na(r$tests$p)))
  ## No DIF: nothing flagged, no floor.
  cv$h <- stats::rnorm(N)
  w <- character()
  withCallingHandlers(scoreTest(f, cv, parameters = "B", purify = TRUE, cores = 1),
    warning = function(x){ w <<- c(w, class(x)); invokeRestart("muffleWarning") })
  expect_false("bigIRT_anchor_floor" %in% w)
})
