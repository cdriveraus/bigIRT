if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  library(data.table)

  ## What the loading arguments become in the prepared data. Two optimiser
  ## iterations of the joint-MAP estimator are enough to build it; the
  ## estimates themselves are not under test here.
  quick_fit <- function(...) suppressWarnings(fitIRT(..., marginalApprox = "map",
    laplaceOuterIter = 2, laplaceCertify = FALSE))

  test_that("legacy scale bridge builds one-hot loading standata", {
    set.seed(11)
    sim <- simIRT(Nsubs = 80, Nitems = 5, Nscales = 2, NitemsAnswered = 5)
    fit <- quick_fit(
      sim$dat,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE
    )

    items <- sim$dat[!duplicated(Item), .(Item, Scale)][order(Item)]
    nitems <- nrow(items)
    nscales <- length(unique(sim$dat$Scale))
    idx <- function(itemi, scalei) as.integer((itemi - 1) * nscales + scalei)

    expect_equal(fit$prepared$NitemScales, as.integer(nitems * nscales))
    expect_equal(fit$prepared$NfixedA, as.integer(nitems * (nscales - 1)))
    expect_equal(length(fit$prepared$fixedAlog), nitems * nscales)
    expect_equal(dim(fit$pars$A), c(nitems, nscales))

    for(i in seq_len(nitems)){
      active_scale <- items$Scale[i]
      expect_equal(fit$prepared$fixedAlog[idx(i, active_scale)], 0L)
      inactive <- setdiff(seq_len(nscales), active_scale)
      for(si in inactive){
        expect_equal(fit$prepared$fixedAlog[idx(i, si)], 1L)
        expect_equal(fit$prepared$Adata[idx(i, si)], 0, tolerance = 1e-12)
      }
    }
  })

  test_that("custom loadings support mixed fixed/free entries", {
    set.seed(12)
    sim <- simIRT(Nsubs = 60, Nitems = 4, Nscales = 2, NitemsAnswered = 4)
    items <- sim$dat[!duplicated(Item), .(Item, Scale)][order(Item)]
    scale_levels <- sort(unique(sim$dat$Scale))
    nitems <- nrow(items)
    nscales <- length(scale_levels)

    loadings <- matrix(NA_real_, nrow = nitems, ncol = nscales)
    rownames(loadings) <- as.character(items$Item)
    colnames(loadings) <- as.character(scale_levels)
    loadings[1, 1] <- 0.7
    loadings[1, 2] <- 0.2
    loadings[2, 2] <- 0.0
    loadings[5, 1] <- 0.33
    loadings[4, 2] <- 0.5

    loadingsFixed <- matrix(NA, nrow = nitems, ncol = nscales)
    rownames(loadingsFixed) <- rownames(loadings)
    colnames(loadingsFixed) <- colnames(loadings)
    loadingsFixed[4, 2] <- FALSE
    loadingsFixed[5, 1] <- TRUE

    fit <- quick_fit(
      sim$dat,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      loadings = loadings,
      loadingsFixed = loadingsFixed
    )

    idx <- function(itemi, scalei) as.integer((itemi - 1) * nscales + scalei)
    expect_equal(dim(fit$pars$A), c(nitems, nscales))
    expect_equal(fit$prepared$fixedAlog[idx(1, 1)], 1L)
    expect_equal(fit$prepared$fixedAlog[idx(1, 2)], 1L)
    expect_equal(fit$prepared$fixedAlog[idx(2, 2)], 1L)
    expect_equal(fit$prepared$fixedAlog[idx(5, 1)], 1L)
    expect_equal(fit$prepared$fixedAlog[idx(4, 2)], 0L)

    expect_equal(fit$prepared$Adata[idx(1, 1)], 0.7, tolerance = 1e-12)
    expect_equal(fit$prepared$Adata[idx(1, 2)], 0.2, tolerance = 1e-12)
    expect_equal(fit$prepared$Adata[idx(2, 2)], 0.0, tolerance = 1e-12)
    expect_equal(fit$prepared$Adata[idx(5, 1)], 0.33, tolerance = 1e-12)
    expect_equal(fit$prepared$Adata[idx(4, 2)], -99, tolerance = 1e-12)

    expect_true(all(c("A", "A_1", "A_2") %in% colnames(fit$itemPars)))
  })
}
