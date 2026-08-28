## Information curves, reliability, item fit, empirical response curves, plots.
##
## These are all one-pass row-wise or item-wise computations, which is the whole
## reason they are native rather than delegated: they work on data that never
## has to become a wide person-by-item matrix.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  fit_m <- function(K = 1L, ni = 30L, ns = 1500L, seed = 4){
    set.seed(seed)
    s <- simIRT(Nsubs = ns, Nitems = ni, Nscales = K, NitemsAnswered = rep(18L, K),
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    n <- nrow(s$dat)
    set.seed(seed + 1); tr <- sort(sample.int(n, floor(0.85 * n)))
    fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      trainingRows = tr, keepInternals = TRUE, laplaceKeepCovariance = TRUE)
  }

  test_that("test information is positive and the SEM is its inverse root", {
    f <- fit_m()
    ti <- testInformation(f, theta = seq(-3, 3, 0.25))
    expect_true(all(ti$test_information > 0))
    expect_equal(ti$sem, 1 / sqrt(ti$test_information), tolerance = 1e-10)
    ## Information sums over items, so it cannot be below any single item's.
    expect_true(all(ti$test_information >= apply(ti$item_information, 1, max)))
    ## With difficulties centred near zero, the test measures best near zero.
    expect_lt(abs(ti$theta[which.max(ti$test_information)]), 1.5)
    expect_output(print(ti), "Peak information")
  })

  test_that("reliability lies in the unit interval and matches its own pieces", {
    f <- fit_m()
    r <- reliability(f)
    expect_true(all(r$empirical > 0 & r$empirical < 1))
    expect_true(all(r$marginal > 0 & r$marginal < 1))
    expect_equal(r$empirical,
      r$observed_variance / (r$observed_variance + r$mean_error_variance),
      tolerance = 1e-10)
    expect_equal(r$mean_sem, sqrt(r$mean_error_variance), tolerance = 1e-10)
  })

  test_that("item fit sits near one when the model generated the data", {
    f <- fit_m()
    it <- itemFit(f)
    expect_equal(nrow(it), as.integer(f$dat$Nitems))
    expect_true(all(it$n > 0))
    ## Correctly specified data: both statistics should centre on 1.
    expect_gt(median(it$infit), 0.8);  expect_lt(median(it$infit), 1.2)
    expect_gt(median(it$outfit), 0.8); expect_lt(median(it$outfit), 1.2)
  })

  test_that("empirical curves track the model and their intervals are valid", {
    f <- fit_m()
    e <- empiricalICC(f, bins = 8L)
    expect_true(all(e$lower >= 0 & e$upper <= 1))
    expect_true(all(e$lower <= e$observed & e$observed <= e$upper))
    expect_true(all(e$n > 0))
    ## Correctly specified data: the model should sit inside most intervals.
    ## Not the nominal 95 per cent, though. Bins are formed on estimated
    ## ability, which is shrunk and noisy, so people sort partly by noise and
    ## the observed curve flattens against the model. Measured at about 0.80
    ## here; the bound guards against a real collapse, not the artefact.
    inside <- mean(e$expected >= e$lower & e$expected <= e$upper)
    expect_gt(inside, 0.7)
    ## Bins are ordered by ability, so observed success should trend upward.
    agg <- tapply(e$observed, e$bin, mean)
    expect_gt(stats::cor(as.numeric(names(agg)), as.numeric(agg)), 0.8)
  })

  test_that("the plot method draws each view without error", {
    f <- fit_m()
    pdf(NULL); on.exit({while(dev.cur() > 1) dev.off()}, add = TRUE)
    for(ty in c("icc", "information", "wright")) expect_silent(plot(f, type = ty))
  })

  test_that("discrimination standard errors survive more than one dimension", {
    ## Regression test. A_ref carries one parameter index per (row, dimension),
    ## and reading only its first column leaves every item on a later dimension
    ## with no likelihood information, so its standard error collapses to the
    ## prior. That showed up as profile/hessian ratios above 2.
    f <- fit_m(K = 2L, ni = 40L, ns = 2000L, seed = 5)
    ip <- itemInformation(f, method = "profile")
    ih <- itemInformation(f, method = "hessian")
    expect_true("A" %in% ip$active)
    r <- ip$se[, "A"] / ih$se[, "A"]
    expect_gt(median(r), 0.8)
    expect_lt(median(r), 1.15)
    ## No item may be sitting at the prior standard deviation.
    expect_lt(max(ip$se[, "A"]), 0.9 * as.numeric(f$dat$invspASD)[1])
  })

  test_that("summary reports convergence, errors, reliability and held-out fit", {
    f <- fit_m()
    s <- summary(f)
    expect_true(!is.null(s$reliability))
    expect_true(!is.null(s$heldout))
    expect_true(is.finite(s$aic))
    expect_output(print(s), "Reliability")
    expect_output(print(s), "Held out")
  })
}
