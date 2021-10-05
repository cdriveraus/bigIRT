if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  cores=2


  test_that("looSimple", {
    set.seed(1)

    require(data.table)
    dat <- bigIRT:::IRTsim(Nsubs = 100,Nitems = 100,Nscales = 1,
      logitCMean = -20,logitCSD = .01,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,
      AbilityMean = 0,AbilitySD = 1)

    wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])

    trainingSet <- (1:nrow(dat$dat))[sample(-1:-(nrow(dat$dat)),50)]

    fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T)
    fit2 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,trainingRows = trainingSet)

    # plot(fit$pars$p, fit2$pars$p)

    testthat::expect_equivalent(
      exp((sum(log(fit2$pars$p[-trainingSet]))-fit2$optim$f)/fit$dat$Nobs),
      exp(-fit$optim$f/fit$dat$Nobs),
      tol=1e-4)


    #parallel check
    fit <- fitIRT(dat$dat,cores=4,pl=1,plot=F,verbose=0,priors=T)
    fit2 <- fitIRT(dat$dat,cores=4,pl=1,plot=F,verbose=0,priors=T,trainingRows = trainingSet)

    testthat::expect_equivalent(
      exp((sum(log(fit2$pars$p[-trainingSet]))-fit2$optim$f)/fit$dat$Nobs),
      exp(-fit$optim$f/fit$dat$Nobs),
      tol=1e-4)

  })


}

