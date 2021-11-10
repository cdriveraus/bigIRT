if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)


  test_that("ScaleEFA", { #placeholder, not working
    set.seed(1)

    require(data.table)
    require(ggplot2)
    Np=5000
    Ni=30
    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 1,ASD = .2,
      BMean=0,BSD = 1,
      # personPreds = matrix(rnorm(Np)),
      # AbilityPredEffects = matrix(c(1),6,1),
      AbilityMean = 0)

    dat=dat$dat
    items <- dat[!duplicated(Item),c('Scale','Item','A','B','C')]
    items <- items[Scale %in% 1 | as.logical(rbinom(Ni*1,1,.1)),]
    dat=dat[Item %in% items$Item,]
    dat2=copy(dat)[,Scale:=1]

    fit <- fitIRT(dat2,cores=4,pl=1,plot=F,verbose=0,priors=T,
      normalise = T,ebayes = F,ebayesmultiplier = 2)

    s=bigIRT:::scoreIRT(fit)
    score = cbind(data.table(id=fit$dat$id,Item=dat$Item[fit$dat$originalRow],response=fit$dat$score,
      abilityest=fit$pars$AbilityNobs, Aest=fit$pars$A, Best=fit$pars$B, Cest=fit$pars$C),s)
    score[,abilityG:=rowSums(.SD),.SDcols=colnames(score)[grep('^ability\\d+',colnames(score))]]
    score[,AG:=rowSums(.SD),.SDcols=colnames(score)[grep('^a\\d+',colnames(score))]]
    score[,BG:=rowSums(.SD),.SDcols=colnames(score)[grep('^b\\d+',colnames(score))]]
    score[,CG:=rowSums(.SD),.SDcols=colnames(score)[grep('^c\\d+',colnames(score))]]

    score=score[,c('id','response','Item','abilityest','Aest','Best','Cest','abilityG','AG','BG','CG'),with=FALSE]
    score <- merge(score,items,all = TRUE)

    ggplot(score[as.integer(factor(Item)) < 150,],aes(y=BG,x=abilityest,colour=A))+
      geom_point(alpha=.5)+
      scale_color_gradient(low = 'red',high='blue')+
      facet_wrap(vars(response))+
      # geom_smooth(se=F)+
      theme_bw()

    cors=cor(scorep[,-1])
    cors[diag(nrow(cors))==1]=0
    corsorder=apply(cors,1,function(x) order(abs(x),decreasing=TRUE))
    round(corp[corsorder[1:10],corsorder[1:10]],2)

    scorep=copy(score)
    scorei=copy(score)
    vcols=colnames(score)[-1:-3] #skip first 3 id columns
    scorep=scorep[,lapply(.SD,sum),by=id,.SDcols=vcols]
    scorep$ability=score[!duplicated(ability),ability]

    corp=cor(scorep[,-1])
    corp[diag(nrow(corp))==1]=0
    corporder=apply(corp,1,function(x) order(abs(x),decreasing=TRUE))
    round(corp[corporder[1:10],corporder[1:10]],2)

    scorei=scorei[,lapply(.SD,sum),by=item,.SDcols=vcols]
    cori=cor(scorei[,-1])
    cori[diag(nrow(cori))==1]=0
    coriorder=apply(cori,1,function(x) order(abs(x),decreasing=TRUE))
    round(cori[coriorder[1:10],coriorder[1:10]],2)

    plot(scorei$abilityG,col=dat$Scale[!duplicated(dat$A)])
    plot(scorep$abilityG)

    dat[,pcorrectmodel:=fit$pars$pcorrect]
    dat[,pobservedmodel:=fit$pars$p]

    ggplot(dat,aes(x=pobservedmodel,colour=factor(Scale)))+
      geom_density()+theme_bw()

    ggplot(dat,aes(x=pcorrectmodel,y=pobservedmodel,colour=factor(Scale)))+
      geom_smooth(se=FALSE)+theme_bw()

    #compare mean prob per item with observed prob
    dat[,meanpcorrect:=mean(pcorrectmodel),by=Item]
    dat[,meanpobserved:=mean(pobservedmodel),by=Item]
    ggplot(dat[id==1,],aes(x=meanpcorrect,y=meanpobserved,colour=factor(Scale)))+geom_point()+theme_bw()

    plot(fit$pars$A,unique(dat$A),col=dat$Scale[!duplicated(dat$A)])

    plot(density(fit$pars$p))
    plot(density(fit$pars$p[dat$Scale==1]))
    plot(density(fit$pars$p[dat$Scale==2]))

    # cor(fit$personPars[,-1])
    # cov2cor(fit$pars$AbilityCov)
    # cor(dat$Ability)

    # testthat::expect_equivalent(
    #   cor(fit$personPars[,-1]),
    #   cor(dat$Ability),
    #   tol=1e-1)


  })


}

