if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
cores=2

  test_that("personPreds1PL", {
    set.seed(1)

    require(data.table)
    Np=5000
    Ni=50
    personpreds = matrix(rnorm(Np*2,0,.2),Np,2)
    predBeta=matrix(c(-1,2),byrow=TRUE,2,1)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = 0,
      BMean=0,BSD = 1,
      # itemPreds = itempreds,
      # AitemPredEffects = predBeta,
      # BitemPredEffects = predBeta,
      personPreds = personpreds, AbilityPredEffects = t(predBeta),
      AbilityMean = 0)

    dat$dat <- bigIRT:::dropPerfectScores(data.table(dat$dat))

    itemdat <- dat$dat[!duplicated(Item),]

    fit <- fitIRT(dat$dat,cores=cores,pl=1,plot=F,verbose=1,priors=T,
      personPreds = c('V1','V2'),
      itemDat = itemdat,
      betaScale = 10,
      normalise = F,ebayes = T,ebayesmultiplier = 2)

    # apply(fit$pars$invspAbeta,1,mean)
    # apply(fit$pars$Bbeta,2,mean)
    #
    # plot(fit$pars$Bbeta[,2])
    #
    # plot(fit$pars$Ability,dat$dat[!duplicated(id),Ability])
    # plot(fit$pars$B,dat$B)
    # plot(fit$pars$A,dat$A)
    # abline(0,1)


    testthat::expect_equivalent(
      c(fit$pars$Abilitybeta),
      c(predBeta),
      tol=.1)
  })

}
