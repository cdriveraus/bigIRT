## A covariate added to the ability regression after a fit (scoreTest's
## `adjust`) must enter the way the fit's own covariates do. With a group
## dimension and commonEffects (improveModel), a covariate has an effect on
## the general dimension and none on the group dimension. The adjustment
## used to share the added coefficient across every dimension, so it gave the
## group dimension an effect the model does not have, and DIF screened under
## it no longer matched a refit with the covariate: on the testlet design
## below the two correlated .31 on items without DIF. In one dimension they
## agree to three decimals, which is what this test asks of two dimensions.

test_that("post-fit impact follows the fit's sharing pattern and matches a refit", {
  skip_on_cran()
  set.seed(11)
  N <- 2500; J <- 24
  g <- rbinom(N, 1, .5); z <- rnorm(N)
  th <- rnorm(N) + .8 * g + .5 * z
  u <- rnorm(N, 0, .8)
  a <- runif(J, .9, 1.6); b <- rnorm(J, 0, .8)
  tl <- seq_len(J) %in% 9:13
  d <- data.table::CJ(id = seq_len(N), Item = seq_len(J))
  d[, `:=`(g = g[id], z = z[id], Scale = 1L)]
  d[, score := rbinom(.N, 1, plogis(a[Item] * th[id] - b[Item] + tl[Item] * u[id] + (Item <= 2) * .6 * g[id]))]
  fa <- list(cores = 1, verbose = 0, laplaceCertify = FALSE, laplaceRefine = "agq", dropPerfectScores = FALSE)
  base <- list(score = "score", id = "id", item = "Item", scale = "Scale", pl = 2L, fitArgs = fa,
    commonEffects = TRUE, abilityTerms = list())
  spec <- function(with_g) list(personPreds = if(with_g) c("g", "z") else "z", abil = list(), impact = character(),
    dif = list(), lower = character(), upper = character(), pooled = character(),
    groups = list(list(name = "T", items = as.character(9:13))), partition = character(), bydim = character())
  fit <- function(s){ a <- bigIRT:::bigIRT_modify_build(d, s, base); suppressWarnings(do.call(fitIRT, a)) }
  f1 <- fit(spec(TRUE)); f0 <- fit(spec(FALSE))
  pg <- d[, .(g = g[1]), by = id]
  cv <- data.frame(id = pg$id, g = factor(pg$g))
  t1 <- suppressWarnings(scoreTest(f1, cv, parameters = "B"))
  t0 <- suppressWarnings(scoreTest(f0, cv, parameters = "B"))
  ## The added coefficient: none on the group dimension, and on the general
  ## one the refit's. (The adjustment holds the items, so its scale is theirs
  ## and the residual SD shrinks below the refit's 1: compared per residual
  ## SD.)
  adj <- t0$adjustment
  bb <- as.matrix(f1$pars$Abilitybeta)
  gdim <- which(bb[, "g"] == 0)
  expect_length(gdim, 1L)
  expect_equal(adj$estimate[adj$dim == gdim], 0)
  expect_equal(adj$estimate[adj$dim != gdim] / adj$ability_sd[adj$dim != gdim], unname(bb[-gdim, "g"]), tolerance = .03)
  ## The DIF statistics match the refit's, item by item.
  x <- merge(t0$tests[, .(item, LM0 = LM)], t1$tests[, .(item, LM1 = LM)], by = "item")
  expect_gt(stats::cor(sqrt(x$LM0), sqrt(x$LM1)), .98)
  expect_gt(stats::cor(sqrt(x$LM0[!x$item %in% c("1", "2")]), sqrt(x$LM1[!x$item %in% c("1", "2")])), .95)
})
