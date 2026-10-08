## Empirical Bayes with a prior centre held fixed (estMeans leaving it out).
##
## The prior is centred on the fixed value, so the SD empirical Bayes fits must
## be the items' spread about that value. The hyperparameter objective used the
## spread about the estimates' own mean whatever the centre, and the fit then
## applied that SD about the fixed centre: with the loading centre placed at a
## raw .542 + 2 (a = 2.6) against items generated at a = 1, a tight SD of about
## .45 pulled the loadings to a mean of 1.94-2.15. Measured with the objective
## corrected: SD 1.86-2.03, loadings 1.03-1.13 (three seeds).
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("a fixed prior centre's SD is the spread about that centre", {
    set.seed(1)
    s <- simIRT(Nsubs = 400, Nitems = 40, Nscales = 1, AMean = 1, ASD = .2, BMean = 0, BSD = 1,
      logitCMean = -20, logitCSD = 0)
    f <- suppressWarnings(fitIRT(s$dat, pl = 2, cores = 1, ebayes = TRUE, estMeans = character(0),
      invspAMeandat = 2.5, dropPerfectScores = FALSE))
    m <- merge(s$itemPars, f$itemPars, by = "Item", suffixes = c(".true", ".fit"))
    raw <- log(expm1(m$A.fit))
    ## The centre stayed where it was put, and the SD covers the distance to it.
    expect_equal(as.numeric(f$prepared$invspAMeandat), 2.5)
    expect_gt(f$prepared$invspASD, abs(mean(raw) - 2.5))
    ## So the loadings are not dragged to the misplaced centre.
    expect_lt(abs(mean(m$A.fit) - mean(m$A.true)), .25)
  })

  test_that("an estimated centre is still profiled out as the estimates' mean", {
    ## The unchanged case: with the centre estimated the objective measures the
    ## spread about the estimates' mean, and the update moves the centre there.
    set.seed(2)
    s <- simIRT(Nsubs = 400, Nitems = 40, Nscales = 1, AMean = 1, ASD = .2, BMean = .5, BSD = 1,
      logitCMean = -20, logitCSD = 0)
    f <- suppressWarnings(fitIRT(s$dat, pl = 2, cores = 1, ebayes = TRUE, dropPerfectScores = FALSE,
      keepInternals = TRUE))
    expect_lt(abs(as.numeric(f$prepared$BMeandat) - mean(as.numeric(f$pars$B))), .1)
    expect_lt(abs(f$prepared$BSDx - sd(as.numeric(f$pars$B))), .25)
  })
}
