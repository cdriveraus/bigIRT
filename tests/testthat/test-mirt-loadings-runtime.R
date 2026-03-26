if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  library(data.table)

  test_that("runtime fit supports legacy multi-scale loading bridge", {
    set.seed(101)
    sim <- simIRT(
      Nsubs = 40,
      Nitems = 4,
      Nscales = 2,
      NitemsAnswered = 4,
      ASD = 0.1,
      AMean = 1,
      BSD = 0.7,
      BMean = 0,
      logitCSD = 0,
      logitCMean = -20
    )

    fit <- fitIRT(
      sim$dat,
      pl = 2,
      cores = 1,
      iter = 20,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    nitems <- nrow(sim$dat[!duplicated(Item), .(Item)])
    nscales <- length(unique(sim$dat$Scale))
    expect_equal(dim(fit$pars$A), c(nitems, nscales))
    expect_equal(fit$dat$NitemScales, as.integer(nitems * nscales))
  })

  test_that("runtime fit supports custom mixed fixed/free loadings", {
    set.seed(102)
    sim <- simIRT(
      Nsubs = 40,
      Nitems = 4,
      Nscales = 2,
      NitemsAnswered = 4,
      ASD = 0.1,
      AMean = 1,
      BSD = 0.7,
      BMean = 0,
      logitCSD = 0,
      logitCMean = -20
    )

    items <- sim$dat[!duplicated(Item), .(Item)][order(Item)]
    scales <- sort(unique(sim$dat$Scale))
    nitems <- nrow(items)
    nscales <- length(scales)
    idx <- function(itemi, scalei) as.integer((itemi - 1) * nscales + scalei)

    loadings <- matrix(NA_real_, nrow = nitems, ncol = nscales)
    rownames(loadings) <- as.character(items$Item)
    colnames(loadings) <- as.character(scales)
    loadings[1, 1] <- 0.8
    loadings[1, 2] <- 0.1
    loadings[2, 2] <- 0.0
    loadings[4, 2] <- 0.6

    loadingsFixed <- matrix(NA, nrow = nitems, ncol = nscales)
    rownames(loadingsFixed) <- rownames(loadings)
    colnames(loadingsFixed) <- colnames(loadings)
    loadingsFixed[4, 2] <- FALSE

    fit <- fitIRT(
      sim$dat,
      pl = 2,
      cores = 1,
      iter = 20,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE,
      loadings = loadings,
      loadingsFixed = loadingsFixed
    )

    expect_equal(dim(fit$pars$A), c(nitems, nscales))
    expect_equal(fit$dat$fixedAlog[idx(1, 1)], 1L)
    expect_equal(fit$dat$fixedAlog[idx(1, 2)], 1L)
    expect_equal(fit$dat$fixedAlog[idx(2, 2)], 1L)
    expect_equal(fit$dat$fixedAlog[idx(4, 2)], 0L)
    expect_true(all(c("A", "A_1", "A_2") %in% colnames(fit$itemPars)))
  })

  test_that("runtime laplace fit exposes row-effective outputs", {
    set.seed(103)
    sim <- simIRT(
      Nsubs = 24,
      Nitems = 4,
      Nscales = 2,
      NitemsAnswered = 4,
      ASD = 0.1,
      AMean = 1,
      BSD = 0.7,
      BMean = 0,
      logitCSD = 0,
      logitCMean = -20
    )

    fit <- fitIRT(
      sim$dat,
      pl = 2,
      cores = 1,
      iter = 10,
      priors = TRUE,
      ebayes = FALSE,
      marginalApprox = "laplace_em",
      laplaceOuterIter = 1,
      noptimsteps = 5,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    expectedRowEff <- c("b_row", "c_row", "d_row", "eta_row", "row_loadings")
    expect_true(
      all(expectedRowEff %in% names(fit$pars)),
      info = paste(
        "Missing row-effective generated quantities on fitted pars:",
        paste(setdiff(expectedRowEff, names(fit$pars)), collapse = ", ")
      )
    )
    expect_equal(length(fit$pars$b_row), fit$dat$Nobs)
    expect_equal(dim(fit$pars$row_loadings), c(fit$dat$Nobs, fit$dat$Nscales))
    expect_true(!is.null(fit$rowEffective))
    expect_equal(length(fit$rowEffective$b), fit$dat$Nobs)
    expect_equal(dim(fit$rowEffective$loadings), c(fit$dat$Nobs, fit$dat$Nscales))
    expect_true(is.matrix(fit$personPosterior$precision))
    expect_true(is.array(fit$personPosterior$covariance) || is.null(fit$personPosterior$covariance))
  })
}
