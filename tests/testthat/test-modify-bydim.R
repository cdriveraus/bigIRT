## improveModel with the covariates' effects common to dimensions
## (commonEffects, R/modify.R): a dimension is judged on the dependence it
## explains, and a covariate's effect by dimension competes with its item DIF
## in the DIF stage. With each dimension's effects free as soon as it was
## added, a partition took in item DIF on a few of a subdomain's items: on a
## simulated bank recall of true DIF fell from 66 to 34 per cent.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  test_that("the build shares effects on a partition, has none on a group dimension, and frees a covariate by dimension", {
    set.seed(1)
    n <- 40L
    d <- data.frame(id = rep(seq_len(n), each = 6), Item = rep(1:6, n), Scale = 1,
      score = stats::rbinom(6 * n, 1, .5), sub = rep(c("a", "a", "b", "b", "c", "c"), n),
      g = rep(stats::rbinom(n, 1, .5), each = 6), x1 = rep(stats::rnorm(n), each = 6),
      x2 = stats::rnorm(6 * n))
    base <- list(score = "score", id = "id", item = "Item", scale = "Scale", pl = 2L, fitArgs = list(),
      commonEffects = TRUE, abilityTerms = list(x = c("x1", "x2")))
    spec <- list(personPreds = c("g", "x1", "x2"), abil = list(), impact = character(), dif = list(),
      lower = character(), upper = character(), pooled = character(), groups = list(), partition = "sub",
      bydim = character())
    m <- bigIRT:::bigIRT_modify_build(d, spec, base)$abilityBetaMap
    expect_identical(dimnames(m), list(c("a", "b", "c"), c("g", "x1", "x2")))
    expect_true(all(apply(m, 2, function(v) length(unique(v))) == 1L))
    expect_setequal(as.integer(m), 1:3)
    ## x, freed by dimension, is carried by x1 and x2 (abilityTerms).
    spec$bydim <- "x"
    m <- bigIRT:::bigIRT_modify_build(d, spec, base)$abilityBetaMap
    expect_length(unique(m[, "g"]), 1L)
    expect_length(unique(c(m[, "x1"], m[, "x2"])), 6L)
    expect_setequal(as.integer(m), 1:7)
    ## Group dimensions: the general dimension's own effects, none on a group
    ## dimension unless freed.
    spec$partition <- character(); spec$bydim <- character()
    spec$groups <- list(list(name = "sub=a", items = c("1", "2")))
    m <- bigIRT:::bigIRT_modify_build(d, spec, base)$abilityBetaMap
    expect_identical(rownames(m), c("1", ".g1"))
    expect_true(all(m["1", ] > 0L)); expect_true(all(m[".g1", ] == 0L))
    spec$bydim <- "g"
    m <- bigIRT:::bigIRT_modify_build(d, spec, base)$abilityBetaMap
    expect_gt(m[".g1", "g"], 0L); expect_false(m[".g1", "g"] == m["1", "g"])
    expect_true(all(m[".g1", c("x1", "x2")] == 0L))
    ## Without commonEffects, no map: every dimension's effects free.
    base$commonEffects <- FALSE
    expect_null(bigIRT:::bigIRT_modify_build(d, spec, base)$abilityBetaMap)
    ## A covariate's terms: by name, its own added terms, and not another
    ## covariate's whose name begins the same way.
    expect_identical(bigIRT:::bigIRT_modify_cov_terms("g",
      c("g", ".imp_g_1", ".imp_g_x_1", ".abil_g_2", ".abil_gx_1"), list()), c("g", ".imp_g_1", ".abil_g_2"))
  })


  ## Three subdomains correlated .7 and a group g; the group gap `gap` by
  ## subdomain, and item DIF `dif` along g.
  sim_sub <- function(seed, gap, dif = 0, N = 3000L, J = 10L, scale = "sub"){
    set.seed(seed)
    R <- matrix(.7, 3, 3); diag(R) <- 1
    g <- stats::rbinom(N, 1, .5)
    th <- matrix(stats::rnorm(3 * N), N) %*% chol(R) + outer(g, gap)
    sub <- rep(c("A", "B", "C"), each = J)
    a <- stats::runif(3 * J, .9, 1.6); b <- stats::rnorm(3 * J, 0, .8)
    dif <- rep_len(dif, 3 * J)
    ii <- rep(seq_len(N), 3 * J); jj <- rep(seq_len(3 * J), each = N)
    d <- data.table::data.table(id = ii, Item = jj, Scale = 1, sub = sub[jj], g = g[ii],
      score = stats::rbinom(N * 3 * J, 1, stats::plogis(a[jj] * th[cbind(ii, match(sub[jj], c("A", "B", "C")))] -
        b[jj] + dif[jj] * g[ii])))
    if(scale == "sub") d[, Scale := sub]
    d
  }

  ## The screen of an effect by dimension is the score test of freeing it, so
  ## it must agree with the likelihood ratio of the fits with the effect
  ## shared and free. (Its first version read about half the ratio: the
  ## stored prior precision had the starting correlation;
  ## test-score-estimated-corr.R.) At the free fit there is nothing to test.
  test_that("the screen of an effect by dimension agrees with the likelihood ratio", {
    skip_on_cran()
    d <- sim_sub(101, c(.2, .6, .2))
    tied <- matrix(1, 3, 1, dimnames = list(c("A", "B", "C"), "g"))
    fit <- function(map) suppressWarnings(fitIRT(d, pl = 2, personPreds = "g", normalise = FALSE,
      estimateAbilityCorr = TRUE, abilityBetaMap = map, cores = 2, laplaceRefine = "agq", keepInternals = TRUE))
    ft <- fit(tied); ff <- fit(NULL)
    bt <- bigIRT:::bigIRT_modify_bydim_tests(scoreContributions(ft), list(g = "g"))
    expect_identical(bt$df, 2L)
    lr <- 2 * (as.numeric(logLik(ff)) - as.numeric(logLik(ft)))
    expect_gt(lr, 50)
    expect_lt(abs(bt$LM / lr - 1), .1)
    expect_null(bigIRT:::bigIRT_modify_bydim_tests(scoreContributions(ff), list(g = "g")))
  })

  run_sub <- function(d) suppressWarnings(suppressMessages(improveModel(d, pl = 2, personPreds = "g",
    personCovariates = "g", difCovariates = "g", itemSets = "sub", dif = "B", asymptotes = character(),
    nsimDim = 0L, maxRounds = 2L, cores = 1, verbose = FALSE)))

  ## A gap specific to subdomain B and no DIF. Each subdomain's abilities take
  ## up most of the gap, so item DIF screens at the 3-D model barely see it;
  ## the screen of g's effect by dimension does, and that is what is kept.
  ## (With the alternative offered only after DIF flags, nothing was kept and
  ## the gap was reported as .4 in every subdomain against .2/.6/.2.) The
  ## gate, not the screen, is the limit: with a .2/.6/.2 gap at these sizes
  ## the screen read p = 2e-14 and the batch gained 6.6 held-out nats (z
  ## 1.56), about what the screen predicts, so the gap here is 0/.8/0.
  test_that("a gap specific to one subdomain is kept as the covariate's effect by dimension", {
    skip_on_cran()
    r <- run_sub(sim_sub(5103, c(0, .8, 0), scale = "one"))
    expect_identical(r$spec$partition, "sub")
    expect_identical(r$spec$bydim, "g")
    expect_length(r$spec$dif, 0L)
    ## (Round 1's DIF screen: the last one, in round 2, has g freed already.)
    expect_lt(r$screens_all[["round1_screens-dif"]]$bydim[covariate == "g", p], 1e-6)
    bb <- as.matrix(r$fit$pars$Abilitybeta)[, "g"]
    expect_gt(bb[2] - max(bb[c(1, 3)]), .4)
  })

  ## A group difference common to the subdomains and DIF on three of
  ## subdomain B's ten items. The partition is kept with g's effect common
  ## to it, and the DIF is kept as DIF: g's effect by dimension would spread
  ## it over all of B's items.
  test_that("item DIF on a few of a subdomain's items is kept as DIF, not as an effect by dimension", {
    skip_on_cran()
    J <- 10L
    r <- run_sub(sim_sub(11, c(.3, .3, .3), dif = (seq_len(3 * J) %in% (J + 1:3)) * .8, scale = "one"))
    expect_identical(r$spec$partition, "sub")
    expect_length(r$spec$bydim, 0L)
    di <- as.integer(unlist(lapply(r$spec$dif, `[[`, "items")))
    expect_true(all((J + 1:3) %in% di))
    bb <- as.matrix(r$fit$pars$Abilitybeta)[, "g"]
    expect_equal(max(bb) - min(bb), 0)
  })
}
