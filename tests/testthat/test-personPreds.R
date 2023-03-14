if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("personPreds2PL", {
    cores=1
    set.seed(1)

    require(data.table)
    Np=5000
    Ni=500
    itemsAnswered=25
    personpreds = matrix(rnorm(Np*2,0,.2),Np,2)
    predBeta=matrix(c(-1,2),byrow=TRUE,2,1)
    AbilitySD = .8


    dat <- simIRT(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 2,ASD = .25,
      BMean=0,BSD = .5,AbilitySD = AbilitySD,
      # itemPreds = itempreds,
      # AitemPredEffects = predBeta,
      # BitemPredEffects = predBeta,
      personPreds = personpreds, AbilityPredEffects = t(predBeta),
      AbilityMean = 0)

    #sparsity
    dat$dat[,random:=sample(1:length(unique(Item))),by=id]
    dat$dat=dat$dat[random <=itemsAnswered,]
    dat$dat <- bigIRT:::dropPerfectScores(data.table(dat$dat),tol.=.01)
    dat$Ability=dat$Ability[dat$dat[!duplicated(id),id]]

    predBetaStd <- predBeta * apply(dat$dat[,c('V1','V2')],2,sd) / sd(dat$Ability)

    dat$dat[,percent:=mean(score),by=id]
    range(dat$dat$percent)

    itemdat <- dat$dat[!duplicated(Item),]

    # dat$dat[,c('V1','V2')] <- dat$dat[,c('V1','V2')] * 10

    fit <- fitIRT(dat$dat,cores=cores,pl=2,plot=F,verbose=1,priors=T,
      personPreds = c('V1','V2'),dropPerfectScores = T,
      integrateAbility = T,integrateWidth = .5,
      # itemDat = itemdat,
      betaScale = 100,tol=1e-2,
      normalise = F,ebayes = T,ebayesmultiplier = 2)

    parsfit<-bigIRT:::normaliseIRT(B = fit$itemPars$B,Ability = fit$personPars$X1,A = fit$itemPars$A)
    parstrue<-bigIRT:::normaliseIRT(B = dat$B,Ability = dat$Ability,A = dat$A)

    # apply(fit$pars$invspAbeta,1,mean)
    # apply(fit$pars$Bbeta,2,mean)
    #
    # plot(fit$pars$Bbeta[,2])
    #
     # plot(parsfit$Ability,parstrue$Ability)
     # plot(parsfit$A,parstrue$A)
     # plot(parsfit$B,parstrue$B)
    # abline(0,1)

    lmf=lm(fit$pars$Abilitypars ~ fit$dat$personPreds[!duplicated(fit$dat$id),])
    lm(parstrue$Ability ~ fit$dat$personPreds[!duplicated(fit$dat$id),])
    testthat::expect_equivalent(
      c(predBetaStd),
      c(fit$CovariateEffects$AbilityStd),
      tol=.1)
  })

}
