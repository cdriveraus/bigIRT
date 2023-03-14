if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  cores=2


  test_that("looSimple", {
    set.seed(1)

    require(data.table)
    dat <- simIRT(Nsubs = 100,Nitems = 100,Nscales = 1,
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

if(FALSE){#CV comparison

  trainingSets1 <- trainingSets2 <- trainingSets3 <- list()
  out <- list()
  n <-200

  for(i in 1:n){
  nsubs=1000
  intelligence <- rnorm(nsubs)
  dat <- simIRT(Nsubs = nsubs,Nitems = 20,Nscales = 1,
    logitCMean = -20,logitCSD = .01,AMean = 1,ASD = .0,
    personPreds = cbind(intelligence),AbilityPredEffects = matrix(1),
    BMean=0,BSD = .5,
    AbilityMean = 0,AbilitySD = 1)

  dat$dat <- dropPerfectScores(dat$dat)


  trainingSets1[[i]] <- sample(1:(nrow(dat$dat)),ceiling(.9*nrow(dat$dat)))

  trainingSets2[[i]] <- dat$dat[,c(paste0('set2')):=
      sample(rep(c(rep(FALSE,1),rep(TRUE,9)),.N/10),replace=FALSE,size=.N),by=id][,c(paste0('set2')),with=FALSE]
  trainingSets2[[i]] <- which(trainingSets2[[i]]$set2)

  trainingSets3[[i]] <- sample(unique(dat$dat$id),ceiling(.5*length(unique(dat$dat$id))))
  trainingSets3[[i]] <- unlist(lapply(trainingSets3[[i]],function(idi) (1:nrow(dat$dat))[dat$dat$id %in% idi]))


  fit1 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,personPreds = 'intelligence',
    trainingRows = trainingSets1[[i]],itemDat = dat$dat[!duplicated(Item),])
  fit2 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,personPreds = 'intelligence',
    trainingRows = trainingSets2[[i]],itemDat = dat$dat[!duplicated(Item),])
  fit3 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,personPreds = 'intelligence',
    trainingRows = trainingSets3[[i]],itemDat = dat$dat[!duplicated(Item),])
  fit4 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
    trainingRows = trainingSets1[[i]],itemDat = dat$dat[!duplicated(Item),])
  fit5 <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
    trainingRows = trainingSets3[[i]],itemDat = dat$dat[!duplicated(Item),])

  out[['random']][[i]] <- fit1
  out[['partialSubject']][[i]] <- fit2
  out[['wholeSubject']][[i]] <- fit3
  out[['noCovRandom']][[i]] <- fit4
  out[['noCovWholeSubject']][[i]] <- fit5
  }

  cvp<-data.table(random=sapply(1:n, function(x) exp(mean(log(out$random[[x]]$pars$p[-trainingSets1[[x]] ])))),
    partialSubject=sapply(1:n, function(x) exp(mean(log(out$partialSubject[[x]]$pars$p[-trainingSets2[[x]] ])))),
    wholeSubject=sapply(1:n, function(x) exp(mean(log(out$wholeSubject[[x]]$pars$p[-trainingSets3[[x]] ])))),
    noCovRandom=sapply(1:n, function(x) exp(mean(log(out$noCovRandom[[x]]$pars$p[-trainingSets1[[x]] ])))),
    noCovWholeSubject=sapply(1:n, function(x) exp(mean(log(out$noCovWholeSubject[[x]]$pars$p[-trainingSets3[[x]] ])))))

  apply(cvp,2,mean)

}
