if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ability", {
    set.seed(1)

    require(data.table)
    Np=5000
    Ni=50
    itempreds = matrix(rnorm(Ni*3),Ni,3)
    predBeta=matrix(c(.1,0.2,-.1),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,
      itemPreds = itempreds,
      AitemPredEffects = predBeta,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    fit <- fitIRT(dat$dat,cores=1,pl=2,plot=F,verbose=0,priors=T,
      # itemDat = dat$dat[unique(Item) & id %in% '1',],
      itemPreds = c('V1','V2','V3'),
      normalise = T,ebayes = T,ebayesmultiplier = 2,itemSpecificBetas = T)

apply(fit$pars$invspAbeta,1,mean)
apply(fit$pars$Bbeta,1,mean)

plot(fit$pars$Bbeta[2,])

    testthat::expect_equivalent(
      c(fit$pars$Bbeta),
      c(predBeta),
      tol=5e-1)
})
