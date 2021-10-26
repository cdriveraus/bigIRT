if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ability", {
    set.seed(1)

    require(data.table)
    Np=500
    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = 20,Nscales = 1,
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
      require(bigIRT)
      of <- NULL
      for(m in c(2,2.75,4)){
        for(Nitems in c(5,10,20,40)){
          for(i in 1:10){

            Np=5000
            dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Nitems,Nscales = 1,
              logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .0,
              BMean=0,BSD = 1,
              AbilitySD = 1,
              # personPreds = matrix(rnorm(Np)), AbilityPredEffects = matrix(c(1,-0.5,.5),6,1),
              AbilityMean = 0)

            # dat$dat <- dropPerfectScores(dat$dat)

            persondat = dat$dat[unique(id) & Item %in% '1',]
            itemdat=dat$dat[unique(Item) & id %in% '1',]

            fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=0,priors=T,
              itemDat = itemdat,dropPerfectScores = FALSE,AbilitySD = m,
              normalise = F,ebayes = F)

            wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score'))


            require(TAM)
            pl=1
            if(pl==3) ttam <- system.time(tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
              guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6))))#, acceleration="Ramsay")))
            if(pl==2) ttam <- system.time(tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE))
            if(pl==1) tfit <-tam.mml(resp = wdat[,-1],pid = wdat$id,B.fixed = cbind(itemdat$id,1,1,itemdat$B))

            tamAbility <- IRT.factor.scores(tfit,type = 'WLE')

            o <- data.frame(ebayesmulti=m,Nitems=Nitems,
              MAPbigIRT=fit$personPars$X1,WLETAM = tamAbility$theta,Ability=persondat$Ability)
            # rmseBIRT=sqrt(sum((persondat$Ability-fit$personPars$X1)^2)),
            # rmseTAM=sqrt(sum((persondat$Ability-tamAbility$theta)^2)),
            if(is.null(of)) of = o else of <- rbind(of,o)
          }
        }
      }



      require(ggplot2)
      ofb=of
      of=data.table(of)
      of[,MAPbigIRT:=MAPbigIRT-Ability]
      of[,WLETAM:=WLETAM-Ability]
      of <- melt(data.table(of),id.vars = c('ebayesmulti','Nitems','Ability'))
      of$Estimator=factor(of$variable)
      # of[variable %in% 'MAPbigIRT',Estimator:=paste0('MAPbigIRT_',ebayesmulti)]
      # of$variable <- factor(of$variable)
      of$Bias <- of$value
      of$Nitems <- factor(of$Nitems)
      of$PriorWeight = paste0('Prior weight = ',1/of$ebayesmulti)

      of[,rmse:=sqrt(mean(Bias^2)),by=interaction(Nitems,ebayesmulti, Estimator)]

      ggplot(of[!duplicated(rmse),],aes(y=rmse,x=Nitems,colour=Estimator,group=Estimator))+geom_line()+
        geom_point()+facet_wrap(vars(PriorWeight))+theme_bw()


      ggplot(data = of,
        mapping = aes(x=Ability, y=Bias,colour=Estimator,linetype=Nitems))+
        geom_smooth(data=of[Estimator %in% 'WLETAM',-c('PriorWeight'),with=F],
          method = 'gam',formula=y~s(x,k=6,bs='cs'),se = FALSE)+
        geom_smooth(data=of[!Estimator %in% 'WLETAM',],
          method = 'gam',formula=y~s(x,k=6,bs='cs'),se = FALSE)+
        theme_bw()+facet_wrap(vars(PriorWeight))+
        ggtitle('bigIRT w different prior weights vs WLE') +
        coord_cartesian(xlim=c(-2,2),ylim=c(-.5,.5))



    })


  }

