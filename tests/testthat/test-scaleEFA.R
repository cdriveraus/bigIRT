if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ScaleEFA", { #placeholder, not working
    set.seed(1)

    require(data.table)
    Np=500
    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = 20,Nscales = 6,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
      normalise = T,ebayes = T,ebayesmultiplier = 2)

    # cor(fit$personPars[,-1])
    # cov2cor(fit$pars$AbilityCov)
    # cor(dat$Ability)

    # testthat::expect_equivalent(
    #   cor(fit$personPars[,-1]),
    #   cor(dat$Ability),
    #   tol=1e-1)


  })


}

