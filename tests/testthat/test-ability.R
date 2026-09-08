if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ability", {
    set.seed(1)

    require(data.table)
    Np=500
    dat <- simIRT(Nsubs = Np,Nitems = 20,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
      itemDat = dat$dat[unique(Item) & id %in% '1',],stochastic=T,
      normalise = T,ebayes = T,ebayesmultiplier = 2)


    ##single item test
    dat$dat$B[1]=-5
    dat$dat$score[1]=1
    fit <- fitIRT(dat$dat[1,],cores=1,pl=1,plot=F,verbose=0,priors=T,dropPerfectScores = F,
      itemDat = dat$dat[1,],
      normalise = F,ebayes = F,ebayesmultiplier = 2,AbilitySD = array(10))
    fit$personPars

    # cor(fit$personPars[,-1])
    # cov2cor(fit$pars$AbilityCov)
    # cor(dat$Ability)

    # testthat::expect_equivalent(
    #   cor(fit$personPars[,-1]),
    #   cor(dat$Ability),
    #   tol=1e-1)




  })


}

