if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("sampled ability appends ability SD columns", {
    set.seed(123)
    require(data.table)

    dat <- simIRT(
      Nsubs = 60, Nitems = 20, Nscales = 1,
      logitCMean = -20, logitCSD = 0,
      AMean = 1, ASD = .1,
      BMean = 0, BSD = 1,
      AbilityMean = 0, AbilitySD = 1
    )

    fitSampled <- fitIRT(
      dat$dat, cores = 1, pl = 2,
      priors = FALSE, ebayes = FALSE,
      sampledAbilityStep = TRUE,
      sampledAbilityOuterIter = 8,
      noptimsteps = 3,
      sampledAbilityDiagnostics = TRUE,
      dropPerfectScores = TRUE
    )

    abilityCol <- grep("^(X1|1)$", colnames(fitSampled$personPars))
    sdCol <- grep("_SD$", colnames(fitSampled$personPars))

    expect_length(abilityCol, 1)
    expect_gte(length(sdCol), 1)
    expect_true(all(is.finite(fitSampled$personPars[[sdCol]])))
    expect_true(min(fitSampled$personPars[[sdCol]]) >= 0)
  })
}
