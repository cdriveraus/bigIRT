## What laplaceRefine = "auto" finishes a two-parameter fit on, by dimension.
##
## Up to 0.1.9 the multidimensional 2PL was finished on second-order Laplace
## at every K >= 2. With few responses per person and dimension that was
## measurably off the quadrature optimum in every fit, and in 2 to 4 per cent
## of the paper's three-dimensional fits it certified a spurious optimum with
## one item at a discrimination of 4 to 10 (paper/Submission2, Study 2). Now:
## nothing at K = 1, quadrature at K = 2 and 3, second-order Laplace beyond.
## The first two expectations fail on the old rule.
if(identical(Sys.getenv("NOT_CRAN"), "true")){
  auto_fit <- function(K, refine = "auto", seed = 3100 + K) {
    set.seed(seed)
    s <- simIRT(Nsubs = 200, Nitems = 6 * K, Nscales = K, NitemsAnswered = rep(4L, K), BSD = 1)
    suppressWarnings(fitIRT(s$dat, pl = 2, cores = 1, verbose = 0,
      dropPerfectScores = FALSE, laplaceRefine = refine))
  }

  test_that("auto finishes a 2PL by quadrature at two and three dimensions", {
    for(K in 2:3) {
      st <- auto_fit(K)$laplaceStatus
      expect_identical(st$marginal, "agq", label = sprintf("K = %d marginal", K))
      expect_false(isTRUE(st$refine$reverted))
    }
  })

  test_that("auto at three dimensions is the explicit quadrature fit", {
    a <- auto_fit(3L); q <- auto_fit(3L, refine = "agq")
    expect_equal(a$itemPars, q$itemPars, tolerance = 1e-10)
    expect_equal(as.numeric(logLik(a)), as.numeric(logLik(q)), tolerance = 1e-10)
  })

  test_that("auto leaves the one-dimensional 2PL on Laplace and uses second-order Laplace beyond three", {
    expect_identical(auto_fit(1L)$laplaceStatus$marginal, "laplace")
    expect_identical(auto_fit(4L)$laplaceStatus$marginal, "laplace2")
  })
}
