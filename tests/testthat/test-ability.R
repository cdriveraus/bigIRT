if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ability", {
    set.seed(1)

    require(data.table)
    Np=500
    dat <- simIRT(Nsubs = Np,Nitems = 20,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
      BMean=0,BSD = .5,
      # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
      AbilityMean = 0)

    fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
      itemDat = dat$dat[unique(Item) & id %in% '1',],
      normalise = T,ebayes = T,ebayesmultiplier = 2)


    ##single item test
    dat$dat$B[1]=-5
    dat$dat$score[1]=1
    fit <- fitIRT(dat$dat[1,],cores=1,pl=1,plot=F,verbose=0,priors=T,dropPerfectScores = F,
      itemDat = dat$dat[1,],
      normalise = F,ebayes = F,ebayesmultiplier = 2,AbilitySD = array(10))
    fit$personPars

    # cor(fit$personPars[,-1])
    # cov2cor(fit$pars$AbilityCov)
    # cor(dat$Ability)

    # testthat::expect_equivalent(
    #   cor(fit$personPars[,-1]),
    #   cor(dat$Ability),
    #   tol=1e-1)




    if(FALSE){ #WLE comparison


# sim ---------------------------------------------------------------------


      library(future)
      plan(strategy='multisession',workers=20)
      # library(bigIRT)
      require(mirt)
      require(TAM)
      require(bigIRT)
      doTAM <- FALSE


      simFunc <- function(Nitems, truePL,fitpl,knownItems,priorMultiply){
        # require(bigIRT)
        if(truePL==3) logitCMean = -2

        Np=500
        dat <- simIRT(Nsubs = Np,Nitems = Nitems,Nscales = 1,
          logitCMean = ifelse(truePL > 2,-2,-20),logitCSD = .5,
          AMean = 1,ASD = ifelse(truePL > 1,.2,0),
          BMean=0,BSD = 1,
          AbilitySD = 1,
          # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
          AbilityMean = 0)

        # dat$dat <- dropPerfectScores(dat$dat)
# print(dat)
        persondat = dat$dat[!duplicated(id) & Item %in% '1',]
        itemdat=dat$dat[!duplicated(Item) & id %in% '1',]

        if(!knownItems) itemdat<-itemdat[0:0,]

        fit <- fitIRT(dat$dat,cores=1,pl=fitpl,plot=F,verbose=0,priors=T,estMeans = T,
          itemDat = itemdat,dropPerfectScores = FALSE,AbilitySD = priorMultiply,
          normalise = F,ebayes = F)

        # fitml <- fitIRT(dat$dat,cores=1,pl=fitpl,plot=F,verbose=0,priors=F,estMeans=T,
        #   itemDat = fit$itemPars,dropPerfectScores = FALSE,AbilitySD = priorMultiply,
        #   normalise = F,ebayes = F)

        wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score'))


        if(doTAM){
          if(knownItems) B.fixed <- cbind(itemdat$id,1,1,itemdat$B) else B.fixed <- matrix(NA,ncol=4,nrow=1)

          if(fitpl==3) tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
            guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6)))
          if(fitpl==2) tfit <-tam.mml.2pl(resp = wdat,B.fixed = B.fixed)
          if(fitpl==1) tfit <-tam.mml(resp = wdat[,-1],pid = wdat$id,B.fixed = B.fixed)

          wleAbility <- IRT.factor.scores(tfit,type = 'WLE')$theta
        } else { #do MIRT

          mitemdat <- data.table(mirt(data = wdat[,-1],itemtype = switch(fitpl,'Rasch','2PL','3PL'),pars='values'))
          if(knownItems){
            mitemdat[name %in% 'a1',c('value','est'):=list(itemdat$A,FALSE)]
            mitemdat[name %in% 'd',c('value','est'):=list(-itemdat$B*itemdat$A,FALSE)]
            mitemdat[name %in% 'g',c('value','est'):=list(itemdat$C,FALSE)]
          }
          mfit <- mirt(data = wdat[,-1],itemtype = switch(fitpl,'Rasch','2PL','3PL'),pars=mitemdat)
          wleAbility <- c(fscores(object = mfit,method = 'WLE'))
        } #end MIRT / TAM


        o <- data.frame(ebayesmulti=priorMultiply,Nitems=Nitems,
          truePL=truePL,fitPL=fitpl,knownItems=knownItems,
          # ML = fitml$personPars$X1,
          MAPbigIRT=fit$personPars$X1,WLE = wleAbility,
          Ability=persondat$Ability)
        return(o)
      }



      of <- list()
      for(i in 1:40){
        of[[i]] <- future({
          oi <- NULL
          for(priorMultiply in c(2.75)){ #2.75
            for(Nitems in c(50,20)){
              for(truePL in c(3,2,1)){
                for(fitpl in c(3,2,1)){
                  for(knownItems in c(TRUE,FALSE)){
                    o <- try(simFunc(Nitems = Nitems,truePL = truePL,fitpl = fitpl,knownItems = knownItems,priorMultiply=priorMultiply))
                    if(!'try-error' %in% class(o)){
                      if(is.null(oi)) oi <- o else oi <- rbind(oi,o)
                    } else print(o)
                  }
                }
              }
            }
          }
          return(oi)
        }) #end future
      }





# sim plots ---------------------------------------------------------------


      of <- value(of)
      of <- rbindlist(of)

      require(ggplot2)
      require(ggpointdensity)
      ofb=copy(of)
      # of[,MAPbigIRT:=MAPbigIRT-Ability]
      # of[,WLETAM:=WLETAM-Ability]
      of <- melt(data.table(of),id.vars = c('ebayesmulti','Nitems','truePL','fitPL','knownItems','Ability'))
      of$Estimator=factor(of$variable)
      # of[variable %in% 'MAPbigIRT',Estimator:=paste0('MAPbigIRT_',ebayesmulti)]
      # of$variable <- factor(of$variable)
      # of$Bias <- of$value
      of$Nitems <- factor(of$Nitems)
      of$estAbility <- of$value
      of$residualAbility <- of$value-of$Ability
      of$PriorWeight = paste0('Prior weight = ',1/of$ebayesmulti)
      of$knownItems <- ifelse(of$knownItems,'knownItems','unknownItems')
      of[Estimator %in% 'MAPbigIRT',Estimator:= paste0(Estimator,'_',ebayesmulti)]

      # of[,rmse:=sqrt(mean(residualAbility^2)),by=interaction(Nitems,ebayesmulti, Estimator)]

      ggplot(of,aes(y=abs(residualAbility),x=Nitems,colour=Estimator,group=Estimator,linetype=factor(fitPL)))+
        stat_summary(fun=mean,geom='line',size=1)+
        stat_summary(fun.data=mean_sdl, geom="ribbon", alpha=0.1)+
        geom_point(alpha=.1,position=position_jitter(width = 0.1))+
        facet_wrap(vars(knownItems))+
        theme_bw()



      for(truePLi in 1:3){
        print(ggplot(of[!Estimator %in% 'ML' & truePL %in% truePLi,],
          aes(y=abs(residualAbility),x=Ability,linetype=Estimator,colour=factor(fitPL)))+
            geom_smooth(se=F)+
            facet_wrap(vars(interaction(Nitems,knownItems)))+
            theme_bw()+ggtitle(paste0('True PL = ',truePLi)))
      }

      for(truePLi in 1:3){
      print(ggplot(of[!Estimator %in% 'ML' & truePL %in% truePLi,],
        aes(y=residualAbility,x=Ability,linetype=Estimator,colour=factor(fitPL)))+
        # geom_pointdensity(alpha=.1)+
        geom_smooth(se=F)+
        geom_hline(yintercept = 0)+
        facet_wrap(vars(interaction(Nitems,knownItems)))+
        theme_bw()+ggtitle(paste0('True PL = ',truePLi)))
      }



    } #end sim
  })


}

