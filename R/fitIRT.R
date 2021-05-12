if(FALSE){


  #Generate some data
  require(data.table)
  dat <- bigIRT:::IRTsim(Nsubs = 5000,Nitems = 100,Nscales = 1,
    logitCMean = -1,logitCSD = .3,AMean = 1,ASD = .3,
    BMean=0,BSD = .5,
    AbilityMean = 0,AbilitySD = 1)
  cdat<-dat
  wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])
  pl=3

  # #mirt
  # require(mirt)
  # tmirt <- system.time(mfit <- mirt(data = wdat,model = 1,
  #   itemtype = ifelse(pl==1,'Rasch',paste0(pl,'PL'))
  #   # ,technical=list(NCYCLES=1000)
  #   ))
  # print(tmirt)
  # mitem <- t(sapply(coef(mfit)[1:length(dat$B)],function(x) x))
  # mability <- fscores(mfit)

  require(TAM)
  if(pl==3) ttam <- system.time(tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
    guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6))))#, acceleration="Ramsay")))
  if(pl==2) ttam <- system.time(tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE))
  if(pl==1) ttam <- system.time(tfit <-tam.mml(resp = wdat,est.variance = TRUE))
  tfit$guess


  # empirical bayes estimates for regularized final pass
  fit <- fitIRT(dat$dat,cores=6,pl=pl,plot=F,verbose=1,priors=T)




  par(mfrow=c(2,2))
  plot(dat$A,mitem[,1],col='blue',pch=16)
  points(dat$A,fit$pars$A)
  abline(0,1,col='green')
  plot(dat$B,-mitem[,2],col='blue',pch=16)
  points(dat$B,fit$pars$B)
  abline(0,1,col='green')
  plot(dat$C,mitem[,3],col='blue',pch=16)
  points(dat$C,fit$pars$C)
  abline(0,1,col='green')
  plot(dat$Ability,mability,col='blue',pch=16)
  points(dat$Ability,fit$pars$Ability)
  abline(0,1,col='green')




  plot(mability,dat$Ability)
  points(fit$pars$Ability,dat$Ability,col='blue',pch=16)
  abline(0,1,col='green')

  plot(dat$Ability,abs(dat$Ability-fit$pars$Ability[,1])-abs(dat$Ability-mability),col='blue',pch=16) #above zero is worse from bigIRT
  abline(0,0)

  sqrt(mean((dat$Ability-mability)^2)) #mirt ability rms
  sqrt(mean((dat$Ability-fit$pars$Ability)^2)) #bigIRT ability rms

  sqrt(mean((dat$A-mitem[,1])^2)) #mirt A rms
  sqrt(mean((dat$A-c(fit$pars$A))^2)) #bigIRT A rms

  sqrt(mean((dat$B+mitem[,2])^2)) #mirt B rms
  sqrt(mean((c(dat$B)-fit$pars$B)^2)) #bigIRT B rms

  sqrt(mean((dat$C+mitem[,3])^2)) #mirt C rms
  sqrt(mean((c(dat$C)-fit$pars$C)^2)) #bigIRT C rms

  #fit using combined approach by fixing ability sd (fixed discrimination parameters, so 1pl model)
  system.time(fitc <- bigIRT:::fitIRT(dat$dat,ASD = .5,logitCSD=.2, logitCMeandat = -2, BSD=1, AbilitySD=1,cores=1,pl=3))
  plot(fitc$pars$B,dat$B)
  abline(0,1,col='green')
  plot(fitc$pars$A,dat$A)
  plot(c(fitc$pars$Ability),dat$Ability)




  par(mfrow=c(2,2))
  plot(fit$pars$B,dat$B)
  abline(0,1,col='green')
  plot(fit$pars$A,dat$A)
  abline(0,1,col='green')
  plot(fit$pars$C,dat$C)
  abline(0,1,col='green')
  plot(c(fit$pars$Ability),dat$Ability)
  abline(0,1,col='green')


  #comparison of combined vs step approach
  plot(fitc$pars$B,dat$B)
  points(fit$pars$B,dat$B,col='red')
  abline(a = 0,b=1)

  #ability error vs ability
  plot(dat$Ability,(fitc$pars$Ability-dat$Ability)^2)
  points(dat$Ability,(fit$pars$Ability-dat$Ability)^2,col='red')


  sqrt(mean((fitc$pars$Ability-dat$Ability)^2)) #rms error combined
  sqrt(mean((fit$pars$Ability-dat$Ability)^2)) #vs step

  #item error vs difficulty
  plot(c(dat$B),(fitc$pars$B-c(dat$B))^2)
  points(c(dat$B),(fit$pars$B-c(dat$B))^2,col='red')


  sqrt(mean((fitc$pars$B-c(dat$B))^2)) #rms error combined
  sqrt(mean((fit$pars$B-c(dat$B))^2)) #vs step



  #correlations
  cor(data.frame(True=dat$Ability,Combined=fitc$pars$Ability,Stepwise=fit$pars$Ability))



  score='score'; item='Item'; scale='Scale';Adata=c();Bdata=c();Abilitydata=c();
  AMeandat=1;ASD=0;BMeandat=0;BSD=1000; AbilityMeandat=0;AbilitySD=100;iter=2000;cores=6;id='id'

}

#' Title
#'
#' @param dat
#' @param score
#' @param id
#' @param item
#' @param scale
#' @param pl
#' @param Adata
#' @param Bdata
#' @param Cdata
#' @param Abilitydata
#' @param AMeandat
#' @param ASD
#' @param BMeandat
#' @param BSD
#' @param logitCMeandat
#' @param logitCSD
#' @param AbilityMeandat
#' @param AbilitySD
#' @param AMeanSD
#' @param BMeanSD
#' @param logitCMeanSD
#' @param AbilityMeanSD
#' @param iter
#' @param cores
#' @param carefulfit
#' @param ebayes
#' @param ebayesmultiplier
#' @param estMeans
#' @param priors
#' @param outlierfix
#' @param normalise
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' #Generate some data (here 2pl model
#' require(data.table)
#' dat <- bigIRT:::IRTsim(Nsubs = 5000,Nitems = 100,Nscales = 1,
#'   logitCMean = -10,logitCSD = 0,AMean = 1,ASD = .3,
#'   BMean=0,BSD = .5,
#'   AbilityMean = 0,AbilitySD = 1)
#'
#' #convert to wide for TAM
#' wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])
#'
#'
#' #fit using TAM
#' require(TAM)
#' tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE)
#'
#'
#' #fit using bigIRT
#' fit <- fitIRT(dat$dat,cores=2,score = 'score',id = 'id',
#'   scale = 'Scale',item = 'Item', pl=2)
fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',pl=1,
  Adata=NA,Bdata=NA,Cdata=NA,Abilitydata=NA,
  AMeandat=1,ASD=5,BMeandat=0,BSD=10, logitCMeandat=-2,logitCSD=10,
  AbilityMeandat=array(0,dim=c(length(unique(dat[[scale]])))),
  AbilitySD=array(10,dim=c(length(unique(dat[[scale]])))),
  AMeanSD=.01,BMeanSD=BSD,logitCMeanSD=logitCSD,
  AbilityMeanSD=array(.01,dim=c(length(unique(dat[[scale]])))),
  iter=2000,cores=6,carefulfit=TRUE,ebayes=TRUE,ebayesmultiplier=2,
  estMeans=FALSE,priors=TRUE,outlierfix=TRUE,
  normalise=TRUE,...){


  #add obs counts
  if(!'data.table' %in% class(dat)) dat <- as.data.table(dat)

  dat <- dat[,c((id),(score),(item),(scale)),with=FALSE]

  itemIndex <- data.table(original=unique(dat[[item]]))
  scaleIndex <- data.table(original=unique(dat[[scale]]))
  idIndex <- data.table(original=unique(dat[[id]]))


  indx <- c(id,item,scale)
for( ci in indx) set(dat,j = ci,value = as.integer(factor(dat[[ci]])))
for( ci in indx) set(dat,j = ci,value = as.integer((dat[[ci]])))

    itemIndex$new <- unique(dat[[item]])
  scaleIndex$new <-unique(dat[[scale]])
  idIndex$new <- unique(dat[[id]])


  if(any(is.na(dat))) stop('Missings found in data!')
  if(!is.numeric(dat[[score]])) stop('Found a non-numeric score column!')
  if(normalise && any(!is.na(c(Adata,Bdata,Cdata,Abilitydata)))) warning(
    'With fixed values provided you may want to set normalise= FALSE',immediate. = TRUE)


  # dat[,itemMean:=mean(eval(score)),by=item]
  # dat[,personMean:=mean(eval(score)),by=id]





  Nitems <- length(unique(dat[[item]]))
  Nsubs=length(unique(dat[[id]]))
  Nscales=length(unique(dat[[scale]]))
  if(pl<2) Adata <- rep(1,Nitems)
  if(pl < 3)  Cdata <- rep(0,Nitems)

  if(all(is.na(Adata))) Adata <- array(numeric(),dim=c(0))
  if(all(is.na(Bdata))) Bdata <- array(numeric(),dim=c(0))
  if(all(is.na(Cdata))) Cdata <- array(numeric(),dim=c(0))
  if(all(is.na(Abilitydata))) Abilitydata <- array(numeric(),dim=c(0,0))



  sdat <- list(
    Nobs=nrow(dat),
    Nsubs=Nsubs,
    Nitems=Nitems,
    Nscales=Nscales,
    id=as.integer(factor(dat[[id]])),
    dopriors=as.integer(priors||ebayes),
    outlierfix=as.integer(outlierfix),
    start=1L,
    end=as.integer(nrow(dat)),
    score=array(as.integer(dat[[score]])),
    item = array(as.integer(factor(dat[[item]]))),
    itemMean = dat$itemMean[!duplicated(dat[[item]])],
    personMean = dat$personMean[!duplicated(dat[[id]])],
    scale=array(as.integer(factor(dat[[scale]]))),
    Adata=Adata,Bdata=Bdata,Cdata=Cdata, Abilitydata=Abilitydata,
    fixedMeans=as.integer(!estMeans),
    AMeandat=AMeandat,ASD=ASD,BMeandat=BMeandat,BSD=BSD,logitCMeandat=logitCMeandat,logitCSD=logitCSD,AbilityMeandat=AbilityMeandat,AbilitySD=array(AbilitySD),
    fixedA=as.integer(length(Adata) > 0),fixedB=as.integer(length(Bdata) > 0),
    fixedC=as.integer(length(Cdata) > 0),fixedAbility=as.integer(length(Abilitydata) > 0),
    AMeanSD=AMeanSD,BMeanSD=BMeanSD,logitCMeanSD=logitCMeanSD,AbilityMeanSD=array(AbilityMeanSD))


    JMLfit <- function(est, sdat, ebayes=FALSE, fit=NA,narrowPriors=FALSE,...){
      init <- c()
      sdat$dopriors = as.integer(priors)
      if(pl < 3 || sdat$fixedC==1) est <- est[est!='C']
      if(pl < 2 || sdat$fixedA==1) est <- est[est!='A']
      if(sdat$fixedAbility==1) est <- est[est!='Ability']
      if(sdat$fixedB==1) est <- est[est!='B']

      # message(paste(paste(est,collapse=', '),ebayes))
      message(paste0(paste0(est,collapse=', '),' ',ifelse(narrowPriors,'Narrow priors ', ifelse(ebayes,'Empirical Bayes ','Free estimation ')),'step...'))

      if('A' %in% est){
        if(all(!is.na(fit))) init <- c(init,fit$pars$A)
        if(!all(!is.na(fit))) init <- c(init,rnorm(sdat$Nitems,1,.2))
      } else {
        # if(all(is.na(fit))) sdat$Adata = rep(1,sdat$Nitems)
        if(!all(is.na(fit))) sdat$Adata = fit$pars$A
        sdat$fixedA = 1L
      }

      if('B' %in% est){
        if(all(!is.na(fit))) init <- c(init,fit$pars$B)
        if(!all(!is.na(fit))) init <- c(init,rnorm(sdat$Nitems,0,.5))
      } else {
        # if(all(is.na(fit))) sdat$Bdata = rep(0,sdat$Nitems)
        if(!all(is.na(fit))) sdat$Bdata = fit$pars$B
        sdat$fixedB = 1L
      }

      if('C' %in% est){
        if(all(!is.na(fit))) init <- c(init,logit(fit$pars$C+1e-8))
        if(!all(!is.na(fit))) init <- c(init,rnorm(sdat$Nitems,0,.5))
      } else {
        # if(all(is.na(fit))) sdat$Cdata = rep(0,sdat$Nitems)
        if(!all(is.na(fit))) sdat$Cdata = fit$pars$C
        sdat$fixedC = 1L
      }

      if('Ability' %in% est){
        if(all(!is.na(fit))) init <- c(init,fit$pars$Ability)
        if(!all(!is.na(fit))) init <- c(init,rnorm(sdat$Nscales * sdat$Nsubs,0,.5))
      } else {
        # if(all(is.na(fit))) sdat$Abilitydata = matrix(0,sdat$Nsubs,sdat$Nscales)
        if(!all(is.na(fit))) sdat$Abilitydata = fit$pars$Ability
        sdat$fixedAbility = 1L
      }

      if(estMeans && !is.na(fit)){
        if('A' %in% est) init <- c(init, mean(fit$pars$A))
        if('B' %in% est) init <- c(init, mean(fit$pars$B))
        if('C' %in% est) init <- c(init, mean(logit(fit$pars$C+1e-8)))
        if('Ability' %in% est) init <- c(init, colMeans(fit$pars$Ability))
      }

      if(ebayes){

        # goodItems <- which(sdat$itemMean > 0.01 & sdat$itemMean < .99)
        # goodPersons <- which(sdat$personMean > 0.01 & sdat$personMean < .99)


        sdat$dopriors <- 1L
        sdat$outlierfix <- 0L

        # sdat$AMeandat <- mean(fit$pars$A)
        sdat$ASD <- sd(fit$pars$A)*ebayesmultiplier

        sdat$BMeandat <- mean(fit$pars$B)
        sdat$BSD <- sd(fit$pars$B)*ebayesmultiplier

        sdat$logitCMeandat <- mean(logit(fit$pars$C+1e-8))
        sdat$logitCSD <- sd(logit(fit$pars$C+1e-8))*ebayesmultiplier

        # sdat$AbilityMeandat <- array(apply(fit$pars$Ability,2,mean))
        sdat$AbilitySD <- array(apply(fit$pars$Ability,2,sd))*ebayesmultiplier #maybe need to better account for multiple scales here, but not that important...

      }

      if(narrowPriors){
        sdat$dopriors <- 1L
        sdat$ASD <- .5
        sdat$BSD <- 1
        sdat$logitCSD <- 1
        sdat$AbilitySD <- array(1,sdat$Nscales)
      }

      if(length(init)==0) init <- NA

      fit <- optimIRT(standata=sdat,Niter=iter,cores=cores,init = init,...)

      #normalise pars
      if(normalise){
      fit$pars$B <- fit$pars$B / mean(fit$pars$A)
      fit$pars$Ability <- fit$pars$Ability / mean(fit$pars$A)
      fit$pars$A <- fit$pars$A / mean(fit$pars$A)

      fit$pars$B <- fit$pars$B +mean(fit$pars$Ability)
      # fit$pars$A <- fit$pars$A /(1+mean(fit$pars$Ability))
      fit$pars$Ability <- fit$pars$Ability -mean(fit$pars$Ability)
      }

      if(exists('cdat')) try({
        par(mfrow=c(2,2))
        # plot(cdat$A,mitem[,1],col='blue',pch=16)
        plot(cdat$A,fit$pars$A)
        points(cdat$A,tfit$item_irt$alpha,col='red')
        abline(0,1,col='green',lwd=2)
        # plot(cdat$B,-mitem[,2],col='blue',pch=16)
        plot(cdat$B,fit$pars$B)
        points(cdat$B,tfit$item_irt$beta,col='red')
        abline(0,1,col='green',lwd=2)
        # plot(cdat$C,mitem[,3],col='blue',pch=16)
        plot(cdat$C,fit$pars$C,ylim=c(0,1))
        points(cdat$C,tfit$guess,col='red')
        abline(0,1,col='green',lwd=2)
        # plot(cdat$Ability,mability,col='blue',pch=16)
        plot(cdat$Ability,fit$pars$Ability)
        points(cdat$Ability,tfit$person$EAP,col='red')
        abline(0,1,col='green',lwd=2)
      })

      rownames(fit$pars$A) <- itemIndex$original
      rownames(fit$pars$B) <- itemIndex$original
      rownames(fit$pars$C) <- itemIndex$original
      rownames(fit$pars$Ability) <- idIndex$original
      colnames(fit$pars$Ability) <- scaleIndex$original

      return(fit)
    }

    JMLseq <- list(
      if(carefulfit) list(est=c('A','B','C','Ability'),ebayes=FALSE,narrowPriors=TRUE),
      list(est=c('A','B','C','Ability'),ebayes=FALSE,narrowPriors=FALSE),
      if(ebayes) list(est=c('A','B','C','Ability'),ebayes=TRUE,narrowPriors=FALSE)
    )

    fit <- NA
    for(i in 1:length(JMLseq)){
      if(!is.null(JMLseq[[i]])){
        if(JMLseq[[i]]$ebayes %in% 'TRUE') fitML <- fit #store fit before ebayes step
        stochastic <- F#(i == length(JMLseq))
        fit <- JMLfit(est = JMLseq[[i]]$est,sdat = sdat, ebayes=JMLseq[[i]]$ebayes,
          fit = fit,stochastic=stochastic,
          narrowPriors = JMLseq[[i]]$narrowPriors,...)
      }
    }

    fit$fitML <- fitML


  return(fit)
}



logit <- function(x) log(x/(1-x))
