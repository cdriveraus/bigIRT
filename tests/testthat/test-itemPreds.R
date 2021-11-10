if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ability", {
    set.seed(1)

    require(data.table)
    Np=500
    Ni=50
    itempreds = matrix(rnorm(Ni*3),Ni,3)
    predBeta=matrix(c(.1,.2,-.2),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .1,
      BMean=0,BSD = 2,
      itemPreds = itempreds,
      AitemPredEffects = predBeta,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    dat$dat[,Ncorrect:=sum(score),by=id]
    range(dat$dat$Ncorrect)

    plot(dat$dat$V1,dat$dat$B)

    fit <- fitIRT(dat$dat,cores=2,pl=2,plot=F,verbose=1,priors=T,
      # itemDat = dat$dat[unique(Item) & id %in% '1',],
      itemPreds = c('V1','V2','V3'),
      normalise = F,ebayes = T,ebayesmultiplier = 2,itemSpecificBetas = F)

apply(fit$pars$invspAbeta,1,mean)
apply(fit$pars$Bbeta,1,mean)

plot(fit$pars$Bbeta[2,])

plot(fit$pars$B,dat$B)
plot(fit$pars$A,dat$A)
abline(0,1)


    testthat::expect_equivalent(
      c(fit$pars$Bbeta),
      c(predBeta),
      tol=5e-1)
})
