if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
cores=2

  test_that("itemPreds1PL", {
    set.seed(1)

    require(data.table)
    Np=5000
    Ni=50
    itempreds = matrix(rnorm(Ni*3,0,.2),Ni,3)
    predBeta=matrix(c(1,2,-2),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 2,ASD = .25,
      BMean=0,BSD = .1,
      itemPreds = itempreds,
      AitemPredEffects = predBeta,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    BpredBetaStd <- predBeta * apply(dat$dat[,c('V1','V2','V3')],2,sd) / sd(dat$B)
    ApredBetaStd <- predBeta * apply(dat$dat[,c('V1','V2','V3')],2,sd) / sd(dat$A)

    # dat$dat <- bigIRT:::dropPerfectScores(dat$dat)

    persondat <- dat$dat[!duplicated(id),]
    setnames(persondat,'Ability','1')

    fit <- fitIRT(dat$dat,cores=cores,pl=2,plot=F,verbose=10,priors=T,
      BitemPreds = c('V1','V2','V3'),
      AitemPreds = c('V1','V2','V3'),
      # personPreds = c('V1','V2','V3'),
      personDat = persondat,
      betaScale = 100,
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
      c(BpredBetaStd),
      c(fit$CovariateEffects$BStd),
      tol=.05)

    testthat::expect_equivalent(
      c(ApredBetaStd),
      c(fit$CovariateEffects$AStd),
      tol=.1)
  })




  test_that("itemPreds2PL", {
    set.seed(1)

    require(data.table)
    Np=500
    Ni=500
    itempreds = matrix(rnorm(Ni*3,0,.2),Ni,3)
    predBeta=matrix(c(.1,.2,-.2),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .1,
      BMean=0,BSD = .1,
      itemPreds = itempreds,
      AitemPredEffects = predBeta,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    persondat <- dat$dat[!duplicated(id),]
    setnames(persondat,'Ability','1')

    fit <- fitIRT(dat$dat,cores=cores,pl=2,plot=F,verbose=1,priors=T,
      AitemPreds = c('V1','V2','V3'),
      BitemPreds = c('V1','V2','V3'),
      personDat = persondat,
      betaScale = 10,
      normalise = F,ebayes = T,ebayesmultiplier = 2,itemSpecificBetas = F)


    fit$pars$Bbeta
    fit$pars$invspAbeta
    fit$pars$Abeta

    # apply(fit$pars$invspAbeta,1,mean)
    # apply(fit$pars$Bbeta,2,mean)
    #
    # plot(fit$pars$Bbeta[,2])
    #
    # plot(fit$pars$Ability,dat$Ability)
    # plot(fit$pars$B,dat$B)
    # plot(fit$pars$A,dat$A)
    # abline(0,1)


    testthat::expect_equivalent(
      c(fit$pars$Bbeta),
      c(predBeta),
      tol=.05)

    testthat::expect_equivalent(
      c(fit$pars$Abeta),
      c(predBeta),
      tol=.1)
  })


  test_that("itemPreds3PL", {
    set.seed(1)

    require(data.table)
    Np=500
    Ni=500
    itempreds = matrix(rnorm(Ni*3,0,.2),Ni,3)
    predBeta=matrix(c(.1,.2,-.2),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -1,logitCSD = .2,AMean = 1,ASD = .1,
      BMean=0,BSD = 1,
      itemPreds = itempreds,
      logitCitemPredEffects = predBeta,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    persondat <- dat$dat[!duplicated(id),]
    setnames(persondat,'Ability','1')

    fit <- fitIRT(dat$dat,cores=cores,pl=3,plot=F,verbose=1,priors=T,
      CitemPreds = c('V1','V2','V3'),
      BitemPreds = c('V1','V2','V3'),
      personDat = persondat,
      betaScale = 10,
      normalise = F,ebayes = F,ebayesmultiplier = 2,itemSpecificBetas = F)


    fit$pars$C
    fit$pars$logitCbeta
    fit$pars$Cbeta

    # apply(fit$pars$invspAbeta,1,mean)
    # apply(fit$pars$Bbeta,2,mean)
    #
    # plot(fit$pars$Bbeta[,2])
    #
    # plot(fit$pars$Ability,dat$Ability)
    # plot(fit$pars$B,dat$B)
    # plot(fit$pars$A,dat$A)
    # abline(0,1)


    testthat::expect_equivalent(
      c(fit$pars$Bbeta),
      c(predBeta),
      tol=.05)

    testthat::expect_equivalent(
      c(fit$pars$logitCbeta),
      c(predBeta),
      tol=.1)
  })

}
