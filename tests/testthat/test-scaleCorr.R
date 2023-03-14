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

    # cor(fit$personPars[,-1])
    # cov2cor(fit$pars$AbilityCov)
    # cor(dat$Ability)

    testthat::expect_equivalent(
      cor(fit$personPars[,-1]),
      cor(dat$Ability),
      tol=1e-1)

    if(FALSE){ #plot ebayes corr performance, compare against TAM -- TAM overestimates correlations, but less so than bigIRT underestimates

      of <- NULL
      for(m in c(1,1.5,2)){
        for(Nitems in c(5,10,20,50,100)){
          for(i in 1:2){

      Np=100
      dat <- simIRT(Nsubs = Np,Nitems = Nitems,Nscales = 2,
        logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
        BMean=0,BSD = .5,personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1),2,1),
        AbilityMean = 0)

      wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])

      fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
        normalise = T,ebayes = T,ebayesmultiplier = m)


      require(TAM)
      pl=1
      if(pl==3) ttam <- system.time(tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
        guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6))))#, acceleration="Ramsay")))
      if(pl==2) ttam <- system.time(tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE))
      if(pl==1) ttam <- system.time(tfit <-tam.mml(resp = wdat,est.variance = TRUE))

      tammodel <- paste0("
  LAVAAN MODEL:
    F1=~ 1*X1__X",Nitems,"
    F2=~ 1*X",(Nitems+1),"__X",(Nitems*2),"
        # Alternatively to the factor 1 one can use the item type Rasch
    F1 ~~ F1
    F2 ~~ F2
    F1 ~~ F2")

      tfit <- TAM::tamaan( tammodel, wdat)

      tamAbility <- IRT.factor.scores(tfit)

      o <- data.frame(ebayesmulti=m,Nitems=Nitems,
        corrTrue = cor(dat$Ability)[2], corrEst = cor(fit$personPars[,-1])[2],
        corrTAM=cor(tamAbility[,c(1,3)])[2],
        pcorrTAM=cov2cor(tfit$variance)[2])
      if(is.null(of)) of = o else of <- rbind(of,o)
        }
        }
      }
      of$corrDiff = of$corrEst-of$corrTrue
      of$corrDiffTAM = of$corrTAM-of$corrTrue
      of$pcorrDiffTAM = of$pcorrTAM-of$corrTrue
      require(ggplot2)
      of <- melt(data.table(of),id.vars = c('ebayesmulti','Nitems'))
      ggplot(data = of[variable %in% c('corrDiff','corrDiffTAM','pcorrDiffTAM'),],
        mapping = aes(x=ebayesmulti, y=value,colour=factor(variable),shape=factor(Nitems)))+
        geom_point()+theme_bw()

} #end sim / plots

  })


}

