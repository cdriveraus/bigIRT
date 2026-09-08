if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  cores=2


  test_that("ScaleCorrSimple", {
    set.seed(1)

    require(data.table)
    Np=500
    dat <- simIRT(Nsubs = Np,Nitems = 20,Nscales = 6,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
      normalise = T,ebayes = T,ebayesmultiplier = 2)

    ## The correlation is not readable off personPars. normalise = TRUE whitens
    ## the abilities -- normaliseMIRT applies inv_chol of the fitted ability
    ## covariance and pushes the correlation into the loadings -- so the reported
    ## abilities have identity covariance by construction and cor(personPars) is
    ## zero however well the correlation was estimated. The estimate survives in
    ## pars$AbilityCov, which is what this checks.
    testthat::expect_equivalent(
      cov2cor(fit$pars$AbilityCov),
      cor(dat$Ability),
      tol=1e-1)

  })


}

