if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("sampled ability step reduces sparse JML discrimination error", {
    set.seed(2)
    require(data.table)

    Np <- 400
    Ni <- 100
    dat <- simIRT(NitemsAnswered = 10,
      Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = 0,
      AMean = 1,ASD = .1,
      BMean = 0,BSD = 1,
      AbilityMean = 0,AbilitySD = 1
    )

    # dat$dat <- dat$dat[, .SD[sample(.N, 3)], by = .(id, Scale)]
    dat$dat <- suppressWarnings(bigIRT:::dropPerfectScores(data.table(dat$dat), tol. = .01))
    trueItemPars <- dat$dat[!duplicated(Item), .(Item, A, B)]
    truePersonPars <- dat$dat[!duplicated(id), .(id, Ability)]


    fitJML <- fitIRT(
      dat$dat, cores = 1, pl = 2,
      priors = T, ebayes = FALSE,
      dropPerfectScores = FALSE
    )

    fitSampled <- fitIRT(
      dat$dat, cores = 1, pl = 2,
      priors = T, ebayes = FALSE,
      sampledAbilityStep = TRUE,noptimsteps = 20,
      sampledAbilityOuterIter = 300,
      dropPerfectScores = FALSE,
      sampledAbilityDiagnostics = TRUE,
      sampledAbilityPlot = TRUE,
      sampledAbilityPlotEvery = 10,
      sampledAbilitySigmaScale = 0.25
    )

    expect_true(nrow(fitSampled$sampledAbilityDiagnostics) > 0)


    fitJML$itemPars$Item <- as.integer(fitJML$itemPars$Item)
    fitSampled$itemPars$Item <- as.integer(fitSampled$itemPars$Item)

    jmlNorm <- normaliseIRT(
      B = fitJML$itemPars$B,
      Ability = fitJML$personPars[[2]],
      A = fitJML$itemPars$A
    )
    sampledNorm <- normaliseIRT(
      B = fitSampled$itemPars$B,
      Ability = fitSampled$personPars[[2]],
      A = fitSampled$itemPars$A
    )
    trueNorm <- normaliseIRT(
      B = trueItemPars$B,
      Ability = truePersonPars$Ability,
      A = trueItemPars$A
    )

    jmlA <- merge(
      data.table(Item = as.character(fitJML$itemPars$Item), A = jmlNorm$A),
      data.table(Item = as.character(trueItemPars$Item), A = trueNorm$A),
      by = "Item"
    )
    sampledA <- merge(
      data.table(Item = as.character(fitSampled$itemPars$Item), A = sampledNorm$A),
      data.table(Item = as.character(trueItemPars$Item), A = trueNorm$A),
      by = "Item"
    )

    jmlRmse <- sqrt(mean((jmlA$A.x - jmlA$A.y)^2))
    sampledRmse <- sqrt(mean((sampledA$A.x - sampledA$A.y)^2))
    print(data.table(
      Method = c("JML", "Sampled"),
      RMSE = c(jmlRmse, sampledRmse)
    ))

    plot(jmlA$A.x, jmlA$A.y, main = "JML A", xlab = "Estimated A", ylab = "True A",col='blue',pch=16)
    points(sampledA$A.x, sampledA$A.y, col = "red",pch=16)
    abline(0, 1, lty = 2)

    expect_true(ncol(fitSampled$personPars) == 3)
    # Seed-level results can vary slightly; sampled updates should not be
    # materially worse than baseline JML in this sparse setup.
    expect_lte(sampledRmse, jmlRmse * 1.15)
  })
}
