if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  cores=2
  #cores = 6

  test_that("fixedItemPars", {
    set.seed(1)

    require(data.table)
    Np=2000
    Ni=500
    itempreds = matrix(rnorm(Ni*3,0,.2),Ni,3)
    predBeta=matrix(c(1,2,-2),1,3)

    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .1,
      BMean=0,BSD = .1,
      itemPreds = itempreds,
      AitemPredEffects = predBeta*.1,
      BitemPredEffects = predBeta,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    persondat <- dat$dat[!duplicated(id),]
    setnames(persondat,'Ability','1')

    itemdat <- dat$dat[!duplicated(Item),][30:40,]

    itemPreds = c('V1','V2','V3')

    fit <- fitIRT(dat$dat,cores=cores,pl=2,plot=F,verbose=0,priors=T,
      itemPreds = itemPreds,
      personDat = persondat,
      itemDat = itemdat,
      # betaScale = 1,
      normalise = F,ebayes = T,ebayesmultiplier = 2,itemSpecificBetas = FALSE)


    fitis <- fitIRT(dat$dat,cores=cores,pl=2,plot=F,verbose=0,priors=TRUE,
      itemPreds = itemPreds,
      personDat = persondat,
      itemDat = itemdat,
      # betaScale = 1,
      normalise = F,ebayes = T,ebayesmultiplier = 2,itemSpecificBetas = TRUE)

    pis=bigIRT:::birtCheckParsOutput(fitis)
    p=bigIRT:::birtCheckParsOutput(fit)

    #check that output pars give same likelihood as computed likelihood
    testthat::expect_equivalent(0, sum(p^2- fit$pars$p^2),tol=1e-8)
    testthat::expect_equivalent(0, sum(pis^2- fitis$pars$p^2),tol=1e-8)

    #and that item specific betas give similar likelihood to non specific betas
    testthat::expect_equivalent(0, mean(fitis$pars$p^2 - fit$pars$p^2),tol=1e-1)

    # o=order(abs(p-pis),decreasing = T)
    #
    # fit$dat$item[o[1:20]]
    # cbind(p,pis)[o[1:20],]

    # apply(fit$pars$invspAbeta,1,mean)
    # apply(fit$pars$Bbeta,2,mean)
    #
    # plot(fit$pars$Bbeta[,2])
    #
    # plot(fit$pars$Ability,dat$Ability)
    # plot(fit$pars$B,c(dat$B))
    # plot(fitis$pars$B,c(dat$B))
    # plot(fitis$pars$A,dat$A)
    # abline(0,1)


    #check correlation between B and item preds
    corrs=round(cor(cbind(fit$pars$B,fitis$pars$B,dat$B, dat$dat[!duplicated(Item),itemPreds,with=FALSE])),2)
    testthat::expect_equivalent(corrs[4:6,1],corrs[4:6,2],tol=.03)
    #and between data
    testthat::expect_equivalent(corrs[4:6,1],corrs[4:6,3],tol=.03)

    #check correlation between A and item preds
    corrs=round(cor(cbind(fit$pars$A,fitis$pars$A,dat$A, dat$dat[!duplicated(Item),itemPreds,with=FALSE])),2)
    testthat::expect_equivalent(corrs[4:6,1],corrs[4:6,2],tol=.05)
    #and between data
    testthat::expect_equivalent(corrs[4:6,1],corrs[4:6,3],tol=.1)

    #check regression weights between true and estimated

    testthat::expect_equivalent(
      fit$pars$Bbeta,
      c(predBeta),
      tol=.2)

    if(F){ #doesn't work for unique item betas-- rely on correlations instead
    testthat::expect_equivalent(
      apply(fitis$pars$Bbeta,2,mean),
      c(predBeta),
      tol=.1)
    }

    #check pars between true and estimated
    testthat::expect_equivalent(fit$pars$B, c(dat$B),  tol=.3)
    testthat::expect_equivalent(fit$pars$A, c(dat$A),  tol=.3)

    testthat::expect_equivalent(fitis$pars$B, c(dat$B),  tol=.3)
    testthat::expect_equivalent(fitis$pars$A, c(dat$A),  tol=.3)

    testthat::expect_equivalent(0, mean(fit$pars$B^2- c(dat$B)^2),  tol=.02)
    testthat::expect_equivalent(0, mean(fitis$pars$B^2- c(dat$B)^2),  tol=.02)

    testthat::expect_equivalent(0, mean(fit$pars$A^2- c(dat$A)^2),  tol=.01)
    testthat::expect_equivalent(0, mean(fitis$pars$A^2- c(dat$A)^2),  tol=.01)
    }
  })


}
