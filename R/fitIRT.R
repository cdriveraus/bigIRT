birtCheckParsOutput <- function(fit){
  p=rep(NA,fit$dat$Nobs)
  for(i in 1:length(p)){
    p[i] <- fit$itemPars$C[fit$dat$item[i]] + (1.0-fit$itemPars$C[fit$dat$item[i]]) / ( 1.0 + exp(
      (-fit$itemPars$A[fit$dat$item[i]] * (
        fit$personPars[fit$dat$id[i], 1+fit$dat$scale[i]] -
          fit$itemPars$B[fit$dat$item[i]]
      ))));

    if(fit$dat$score[i]==0) p[i]= 1.0-p[i];
  }
  return(p)
}


birtRunGeneratedQuantities<- function(fit){

  log1p_exp=function(x) log1p(exp(x))

  genq <- function(){
    browser()

    A=rep(NA,Nitems) # item A values
    B=rep(NA,Nitems) #item B values
    C=rep(NA,Nitems) #item C values
      #put the user supplied fixed values into the item parameter objects
      A[fixedA] = Adata[fixedA];
      B[fixedB] = Bdata[fixedB];
      C[fixedC] = Cdata[fixedC];

      #put the free parameters into the item parameter objects
      A[notfixedA] = invspApars;
      B[notfixedB] = Bpars;
      C[notfixedC] = logitCpars;


      # for(i in 1:Nsubs){ #for every subject
      #   for(j in 1:Nscales){ #and every scale
      #     if(fixedAbilityLogical[i,j]==1){
      #       Ability[i,j] = Abilitydata[i,j];
      #     } else{ #if ability is user supplied, input it
      #       Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; # or input the free parameter
      #       if(NpersonPreds) {
      #         predsmean=rep(0, NpersonPreds); #compute mean of person predictors
      #         count=0;
      #         for( ri in 1:Nobs){
      #           if(id[i] == i){
      #             count=count+1;
      #             predsmean=predsmean+personPreds[i,];
      #           }
      #         }
      #         predsmean= predsmean/count;
      #         Ability[i,j] = Ability[i,j] +predsmean * Abilitybeta[j,]; #when there are person predictors, apply the effect
      #       }
      #     }
      #   }
      # }


        for(i in 1:Nitems){ #for every item
            count=0;
            predsmean=rep(0,NitemPreds);
            for( ri in 1:Nobs){
              if(item[ri] == i){
                count=count+1;
                predsmean=predsmean+itemPreds[ri,];
              }
            }
            predsmean= predsmean/count;
          if(fixedAlog[i]==0){ #if free A par and item predictors, compute average item effect
            A[i] =A[i]+ matrix(predsmean,1) %*% t(invspAbeta[ifelse(itemSpecificBetas==1,freeAref[item[i]],1),,drop=FALSE]); #when there are person predictors, apply the effect
            A[i]=log1p_exp(A[i]);
          }
          if(fixedBlog[i]==0){ #if free B par and item predictors, compute average item effect
            B[i] = B[i] + matrix(predsmean,1) %*% t(Bbeta[ifelse(itemSpecificBetas==1, freeBref[item[i]], 1),,drop=F]); #when there are person predictors, apply the effect
          }
        }


#
#       #linearised regression weights for reporting
#       if(doApreds){
#         if(size(Abeta)==1){
#           Abeta[1,] = ((log1p_exp(mean(invspApars)+invspAbeta[1,]*.01))-(log1p_exp(mean(invspApars)-invspAbeta[1,]*.01)))/.02;
#         }
#         if(size(Abeta)>1){
#           for(i in 1:size(Abeta)){
#             Abeta[i,] = ((log1p(exp(invspApars[i])+invspAbeta[i,]*.01))-(log1p(exp(invspApars[i])-invspAbeta[i,]*.01)))/.02;
#           }
#         }
#       }
#
#       if(doCpreds){
#         if(size(Cbeta)==1)   Cbeta[1,] = ((inv_logit(mean(logitCpars))+logitCbeta[1,]*.01)-(inv_logit(mean(logitCpars))-logitCbeta[1,]*.01))/.02;
#         if(size(Cbeta)>1){
#           for(i in 1:size(Cbeta)){
#             Cbeta[i,] = ((inv_logit(logitCpars[i])+logitCbeta[i,]*.01)-(inv_logit(logitCpars[i])-logitCbeta[i,]*.01))/.02;
#           }
#         }
#       }

  } #end internal genq function

  e <- list2env(c(fit$pars,fit$dat))
  environment(genq) <- e
  genq()
}


  normaliseIRT <- function(B,Ability, A,normaliseScale=1, normaliseMean=0){

  nsd <- sd(Ability) / (normaliseScale)
  nm <- mean(Ability)

  Ability <- (Ability -nm)/ nsd +normaliseMean
  B  <- ( B-nm) / nsd +normaliseMean
  A <-  A * nsd
  return(list(A=A,B=B,Ability=Ability))
}

if(FALSE){


  #Generate some data
  require(data.table)
  dat <- bigIRT:::IRTsim(Nsubs = 200,Nitems = 100,Nscales = 1,
    logitCMean = -10,logitCSD = .03,AMean = 1,ASD = .03,
    BMean=0,BSD = .5,
    AbilityMean = 0,AbilitySD = 1)
  cdat<-dat
  wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])
  pl=2

  # #mirt
  # require(mirt)
  # tmirt <- system.time(mfit <- mirt(data = wdat,model = 1,
  #   itemtype = ifelse(pl==1,'Rasch',paste0(pl,'PL'))
  #   # ,technical=list(NCYCLES=1000)
  #   ))
  # print(tmirt)
  # mitem <- t(sapply(coef(mfit)[1:length(dat$B)],function(x) x))
  # mAbility <- fscores(mfit)

  require(TAM)
  if(pl==3) ttam <- system.time(tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
    guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6))))#, acceleration="Ramsay")))
  if(pl==2) ttam <- system.time(tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE))
  if(pl==1) ttam <- system.time(tfit <-tam.mml(resp = wdat,est.variance = TRUE))
  tamAbility <- IRT.factor.scores(tfit,type = 'WLE')
  tfit$guess


  # empirical bayes estimates for regularized final pass
  fit <- fitIRT(dat$dat,cores=1,pl=1,plot=F,verbose=1,priors=T)




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
  plot(dat$Ability,mAbility,col='blue',pch=16)
  points(dat$Ability,fit$pars$Ability)
  abline(0,1,col='green')




  plot(mAbility,dat$Ability)
  points(fit$pars$Ability,dat$Ability,col='blue',pch=16)
  abline(0,1,col='green')

  plot(dat$Ability,abs(dat$Ability-fit$pars$Ability[,1])-abs(dat$Ability-mAbility),col='blue',pch=16) #above zero is worse from bigIRT
  abline(0,0)

  sqrt(mean((dat$Ability-mAbility)^2)) #mirt Ability rms
  sqrt(mean((dat$Ability-fit$pars$Ability)^2)) #bigIRT Ability rms

  sqrt(mean((dat$A-mitem[,1])^2)) #mirt A rms
  sqrt(mean((dat$A-c(fit$pars$A))^2)) #bigIRT A rms

  sqrt(mean((dat$B+mitem[,2])^2)) #mirt B rms
  sqrt(mean((c(dat$B)-fit$pars$B)^2)) #bigIRT B rms

  sqrt(mean((dat$C+mitem[,3])^2)) #mirt C rms
  sqrt(mean((c(dat$C)-fit$pars$C)^2)) #bigIRT C rms

  #fit using combined approach by fixing Ability sd (fixed discrimination parameters, so 1pl model)
  system.time(fitc <- bigIRT:::fitIRT(dat$dat,invspASD = .5,logitCSD=.2, logitCMeandat = -2, BSD=1, AbilitySD=1,cores=1,pl=3))
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

  #Ability error vs Ability
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



}


cfunc <- function(x) exp(-exp(-x))
cfunci <- function(x) -log(-log(x))

afunc <- function(x) log1p(exp(x))
afunci <- function(x) log(exp(x)-1)


dropPerfectScores <- function(dat,scoreref.='score',itemref.='Item',idref.='id',tol.=.001){
  if(!'data.table' %in% class(dat)) stop('Not a data.table!')
  dat[,.originalRow:=1:.N]
  dropping <- TRUE
  while(dropping){
    dropping <- FALSE
    dat[,itemMean:=mean(get(scoreref.)),by=itemref.]
    if(any((abs(dat$itemMean-.5)+tol.)>= .5)){
      dropping <- TRUE
      warning('Dropping items with all 0 or 1',immediate. = TRUE)
      dat <- dat[(abs(itemMean-.5)+tol.)< .5,]
    }
    dat[,personMean:= mean(get(scoreref.)),by=idref.]
      if(any((abs(dat$personMean-.5)+tol.)>= .5)){
      warning('Dropping subjects with all 0 or 1',immediate. = TRUE)
      dropping <- TRUE
      dat <-dat[(abs(personMean-.5)+tol.)< .5,]
    }
  }
  return(dat)
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
#' @param personDat
#' @param invspAMeandat
#' @param invspASD
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
#' @param normalise
#' @param trainingRows
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' #Generate some data (here 2pl model
#' require(data.table)
#' dat <- bigIRT:::IRTsim(Nsubs = 50,Nitems = 100,Nscales = 1,
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
#'   scale = 'Scale',item = 'Item', pl=2,dohess=TRUE)
fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',pl=1,
  personDat=NA, personPreds=character(),
  itemDat=NA, itemPreds=character(),
  statePreds=character(),
  itemSpecificBetas=FALSE,
  betaScale=10,
  invspAMeandat=.542,invspASD=.5,BMeandat=0,BSD=2, logitCMeandat=-4,logitCSD=2,
  AbilityMeandat=array(0,dim=c(length(unique(dat[[scale]])))),
  AbilitySD=array(10,dim=c(length(unique(dat[[scale]])))),
  AbilityCorr=diag(1,c(length(unique(dat[[scale]])))),
  AMeanSD=1,BMeanSD=BSD,logitCMeanSD=logitCSD,
  AbilityMeanSD=array(1,dim=c(length(unique(dat[[scale]])))),
  iter=2000,cores=6,carefulfit=FALSE,
  ebayes=TRUE,ebayesmultiplier=2,ebayesFromFixed=FALSE,ebayesiter=1,
  estMeans=FALSE,priors=TRUE,
  normalise=TRUE,normaliseScale=1,normaliseMean=0,
  dropPerfectScores=TRUE,trainingRows=1:nrow(dat),
  init=NA,Dpar=FALSE,...){

  sdat <-list() #initialize standata object

  #setup unlikely names to use in data.table calls to avoid overlap from user defined names
  idref. <- id; scaleref. <- scale; itemref. <- item;
  scoreref. <- score;
  personPredsref. <- personPreds;
  itemPredsref. <- itemPreds
  statePredsref. <- statePreds

  if(!'data.table' %in% class(dat)){  #drop unused columns from dat and set to data.table (copy if already data table)
    dat <- as.data.table(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
    itemPredsref.,personPredsref.,statePredsref.),with=FALSE])
    } else {
      dat <- data.table::copy(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
        itemPredsref.,personPredsref.,statePredsref.),with=FALSE])
    }


  #drop problem people and items
  if(dropPerfectScores)    dat <- dropPerfectScores(dat,scoreref. = scoreref.,itemref. = itemref.,idref. = idref.)


    #setup indices to map user specified categories to sequential integers for stan
    itemIndex <- data.table(original=as.character(dat[[itemref.]][!duplicated(dat[[itemref.]])]))
    scaleIndex <- data.table(original=as.character(dat[[scaleref.]][!duplicated(dat[[scaleref.]])]))
    idIndex <- data.table(original=as.character(dat[[idref.]][!duplicated(dat[[idref.]])]))


    #convert categories to sequential integers
    indx <- c(idref.,itemref.,scaleref.)
    for( ci in indx) set(dat,j = ci,value = as.integer(factor(dat[[ci]])))
    for( ci in indx) set(dat,j = ci,value = as.integer((dat[[ci]])))

    #include new sequential integers in index lists
    itemIndex$new <- dat[[itemref.]][!duplicated(dat[[itemref.]])]
    itemIndex$scale <- dat[[scaleref.]][!duplicated(dat[[itemref.]])]
    scaleIndex$new <-dat[[scaleref.]][!duplicated(dat[[scaleref.]])]
    idIndex$new <- dat[[idref.]][!duplicated(dat[[idref.]])]

    #order indices by new integer

    itemIndex=itemIndex[order(new),]
    scaleIndex=scaleIndex[order(new),]
    idIndex=idIndex[order(new),]


    #checks...
    if(any(is.na(dat))) stop('Missings found in data! Probably just remove the row/s...')
    if(!is.numeric(dat[[scoreref.]])) stop('Found a non-numeric score column!')
    if(normalise && any(!is.na(c(itemDat,personDat)))) warning(
      'With fixed values provided you might want to set normalise= FALSE',immediate. = TRUE)



    Nitems <- length(unique(dat[[itemref.]]))
    Nsubs=length(unique(dat[[idref.]]))
    Nscales=length(unique(dat[[scaleref.]]))

    #if getting priors from fixed pars, do this before dropping unnecessary items from itemSetup / AbilitySetup
    if(ebayesFromFixed){
      personDat <- as.data.table(personDat)
      itemDat <- as.data.table(itemDat)
      sdat$dopriors <- 1L

      sdat$invspAMeandat <- mean(afunci(itemDat$A),na.rm=TRUE)
      sdat$invspASD <- sd(afunci(itemDat$A),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$BMeandat <- mean(itemDat$B,na.rm=TRUE)
      sdat$BSD <- sd(itemDat$B,na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$logitCMeandat <- mean(cfunci(itemDat$C+1e-8),na.rm=TRUE)
      sdat$logitCSD <- sd(cfunci(itemDat$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$AbilityMeandat <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,mean,na.rm=TRUE))
      sdat$AbilitySD <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,sd,na.rm=TRUE))*ebayesmultiplier+1e-5 #maybe need to better account for multiple scales here, but not that important...
      sdat$AbilityCorr <- cor(personDat[,c(scaleIndex$original),with=FALSE],use='pairwise.complete.obs')
    }

    #setup item structure to define fixed / free pars
    itemSetup <- data.table(itemIndex,A=ifelse(pl>1,as.numeric(NA),1),B=as.numeric(NA),C=ifelse(pl>2,as.numeric(NA),0))

    if(!all(is.na(itemDat))){ #if fixed item pars
      if(!'data.table' %in% class(itemDat)) itemDat <- as.data.table(itemDat)
      itemDat <- itemDat[get(itemref.) %in% itemSetup$original,]
      setupRows <- match(itemDat[[itemref.]],itemSetup$original)
      itemSetup[setupRows,c('A','B','C'):=itemDat[,c('A','B','C')]]
      if(pl<2) itemSetup[,'A':=1]
      if(pl<3) itemSetup[,'C':=0]
    }
    itemSetup[,paste0(c('A','B','C'),'data'):= .SD, .SDcols=c('A','B','C')] #create data columns
    setnafill(itemSetup,fill = -99,cols = paste0(c('A','B','C'),'data')) #and fill with arbitrary value to avoid NA in stan

    #setup person structure to define fixed / free pars
    AbilitySetup <- data.table(idIndex)
    AbilitySetup[,c(scaleIndex$original):=as.numeric(NA)]

    if(!all(is.na(personDat))){ #if fixed person pars
      if(!'data.table' %in% class(personDat)) personDat <- as.data.table(personDat)
      personDat <- personDat[get(idref.) %in% AbilitySetup$original,]
      setupRows <- match(personDat[[idref.]],AbilitySetup$original)
      AbilitySetup[setupRows,c(scaleIndex$original):=personDat[,c(scaleIndex$original),with=FALSE]]
    }
    AbilitySetup[,paste0(c(scaleIndex$original),'data'):= .SD, .SDcols=c(scaleIndex$original)] #create data columns
    setnafill(AbilitySetup,fill = -99,cols = paste0(c(scaleIndex$original),'data')) #and fill with arbitrary value to avoid NA in stan

    #which abilities are fixed
    fixedAbilityLogical <- AbilitySetup[order(new),c(scaleIndex$original),with=FALSE]
    fixedAbilityLogical<-fixedAbilityLogical[,lapply(.SD,function(x) as.integer(!is.na(x)))]

    #which parameters do the unfixed Ability matrix slots need to refer to
    Abilityparsindex <- matrix(cumsum(1-unlist(fixedAbilityLogical)),Nsubs,Nscales)
    Abilityparsindex[fixedAbilityLogical==1] <- 0

    #which scale is each Ability par for
    Abilityparsscaleindex <- c(col(Abilityparsindex)[Abilityparsindex>0])

    # #include short predictors:
    #
    # if(length(itemPredsref.)==0){
    #   itemPreds <- array(0,dim = c(Nitems,0))
    # } else{
    #   itemPreds <- dat[!duplicated(get(itemref.)),itemPredsref.,with=FALSE]
    #   itemPreds <- itemPreds[order(unique(dat[[itemref.]])),]
    # }
    #
    # if(length(personPredsref.)==0){
    #   personPreds <- array(0,dim = c(Nsubs,0))
    # } else{
    #   personPreds <- dat[!duplicated(get(idref.)),personPredsref.,with=FALSE]
    #   personPreds <- personPreds[order(unique(dat[[idref.]])),]
    # }

    #include long predictors:

    if(length(itemPredsref.)==0){
      itemPreds <- array(0,dim = c(nrow(dat),0))
    } else{
      itemPreds <- dat[,itemPredsref.,with=FALSE]
    }

    if(length(personPredsref.)==0){
      personPreds <- array(0,dim = c(nrow(dat),0))
    } else{
      personPreds <- dat[,personPredsref.,with=FALSE]
    }

    sdat$NstatePreds <- length(statePreds)
    sdat$statePreds <- matrix(0, nrow(dat), sdat$NstatePreds)
    if(sdat$NstatePreds > 0) sdat$statePreds <- as.matrix(dat[,statePredsref.,with=FALSE])

    trainingLogical=array(rep(0L,nrow(dat)))
    trainingLogical[trainingRows] <- 1L


    sdat <- c(sdat,list(
      Nobs=nrow(dat),
      Nsubs=Nsubs,
      Nitems=Nitems,
      Nscales=Nscales,
      id=array(dat[[idref.]]),
      dopriors=as.integer(priors||ebayes),
      outlierfix=0L,
      outlierscale=2,
      NfixedA=as.integer(sum(!is.na(itemSetup$A))),
      NfixedB=as.integer(sum(!is.na(itemSetup$B))),
      NfixedC=as.integer(sum(!is.na(itemSetup$C))),
      NfixedAbility=as.integer(sum(!is.na(unlist(AbilitySetup[,scaleIndex$original,with=FALSE])))),
      fixedA=array(as.integer(which(!is.na(itemSetup$A)))),
      fixedB=array(as.integer(which(!is.na(itemSetup$B)))),
      fixedC=array(as.integer(which(!is.na(itemSetup$C)))),
      Dpar = as.integer(Dpar),
      fixedAlog=array(as.integer((!is.na(itemSetup$A)))),
      fixedBlog=array(as.integer((!is.na(itemSetup$B)))),
      fixedClog=array(as.integer((!is.na(itemSetup$C)))),
      notfixedA=array(as.integer(which(is.na(itemSetup$A)))),
      notfixedB=array(as.integer(which(is.na(itemSetup$B)))),
      notfixedC=array(as.integer(which(is.na(itemSetup$C)))),
      Abilityparsindex=array(as.integer(unlist(Abilityparsindex)),c(Nsubs,Nscales)),
      fixedAbilityLogical=array(unlist(fixedAbilityLogical),c(Nsubs,Nscales)),
      Abilityparsscaleindex=array(as.integer(Abilityparsscaleindex)),
      start=1L,
      end=as.integer(nrow(dat)),
      trainingLogical=trainingLogical,
      score=array(as.integer(dat[[scoreref.]])),
      incorrect=array(as.integer(which(dat[[scoreref.]]==0))),
      item = array(dat[[itemref.]]),
      itemMean = dat$itemMean[!duplicated(dat[[itemref.]])],
      personMean = dat$personMean[!duplicated(dat[[idref.]])],
      scale=array(dat[[scaleref.]]),
      Adata=array(itemSetup$Adata),Bdata=array(itemSetup$Bdata),Cdata=array(itemSetup$Cdata),
      Abilitydata=matrix(unlist(AbilitySetup[,paste0(c(scaleIndex$original),'data'),with=FALSE]),Nsubs,Nscales),
      NitemPreds=ncol(itemPreds), itemPreds=array(unlist(itemPreds),dim(itemPreds)),
      NpersonPreds=ncol(personPreds), personPreds=(array(unlist(personPreds),dim(personPreds))),
      itemSpecificBetas=as.integer(itemSpecificBetas),
      betaScale=betaScale,
      invspAMeandat=invspAMeandat,invspASD=invspASD,
      BMeandat=BMeandat,BSD=BSD,
      logitCMeandat=logitCMeandat,logitCSD=logitCSD,
      AbilityMeandat=AbilityMeandat,AbilitySD=array(AbilitySD),AbilityCorr=AbilityCorr,
      AMeanSD=AMeanSD,BMeanSD=BMeanSD,logitCMeanSD=logitCMeanSD,AbilityMeanSD=array(AbilityMeanSD),
      fixedAMean=1L,fixedBMean=1L,fixedCMean=1L,fixedAbilityMean=1L,
      restrictAMean=1L,restrictBMean=1L,restrictCMean=0L,restrictAbilityMean=1L,
      rowIndexPar=0L,
      originalRow=dat$`.originalRow`,
      doGenQuant=0L)
    )

    # browser()
    sdat$freeAref=array(as.integer(cumsum(1-as.numeric(sdat$fixedAlog))))
    sdat$freeBref=array(as.integer(cumsum(1-as.numeric(sdat$fixedBlog))))
    sdat$freeCref=array(as.integer(cumsum(1-as.numeric(sdat$fixedClog))))


    JMLfit <- function(est, sdat, ebayes=FALSE, fit=NA,narrowPriors=FALSE,...){
      skipebayes <- FALSE

      message(paste0(ifelse(narrowPriors,'Narrow priors ', ifelse(ebayes,'Empirical Bayes ','Free estimation ')),'step...'))

      if(!all(is.na(fit))){
        sdat$Adata = fit$pars$A
        sdat$Bdata = fit$pars$B
        sdat$Cdata = fit$pars$C
        sdat$Abilitydata = fit$pars$Ability
        init = fit$optim$par
      }

      if(ebayes){
        sdat$dopriors <- 1L

        if(pl > 1 &&  length(fit$pars$invspApars) > 2){
          sdat$invspAMeandat <- mean(fit$pars$invspApars) #mean(afunci(fit$pars$A))
          sdat$invspASD <- sd(fit$pars$invspApars)*ebayesmultiplier+1e-5 #afunci(fit$pars$A)
        }

        if(length(fit$pars$Bpars) > 2){
          sdat$BMeandat <- mean(fit$pars$Bpars)
          sdat$BSD <- sd(fit$pars$Bpars)*ebayesmultiplier+1e-5
        }

        if(pl > 2 && length(fit$pars$logitCpars) > 2){
          sdat$logitCMeandat <- mean(fit$pars$logitCpars) #mean(cfunci(fit$pars$C+1e-8))
          sdat$logitCSD <- sd(fit$pars$logitCpars,na.rm=TRUE) * ebayesmultiplier+1e-5 #sd(cfunci(fit$pars$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
        }

        if(length(fit$pars$Abilitypars) > 2){

          sdat$AbilityMeandat <- array(sapply(1:Nscales,function(x){
            mean(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x])
          }))

          sdat$AbilitySD <- array(sapply(1:Nscales,function(x){
            sd(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x],na.rm=TRUE)
          })) * ebayesmultiplier + 1e-5

          sdat$AbilityCorr= cor(fit$pars$Ability) #inconsistency here -- based on overall ability, rather than conditional ability as for sd / mean.
        }

        if(any(is.na(c(sdat$BSD,sdat$invspASD,sdat$logitCSD,sdat$AbilitySD)))){
          skipebayes <- TRUE
          warning('NA when computing item sd parameters, ebayes set to FALSE')
        }
      }

      if(narrowPriors){
        sdat$dopriors <- 1L
        sdat$ASD <- .5
        sdat$BSD <- 1
        sdat$logitCSD <- 1
        sdat$AbilitySD <- array(1,sdat$Nscales)
      }
      # browser()
      if(!skipebayes) fit <- optimIRT(standata=sdat,Niter=iter,cores=cores,init = init,...)






      # if(exists('cdat')) try({
      #   #normalise pars
      #   tmp <- fit
      #
      #   nsd <- sd(tmp$pars$Ability)
      #   nm <- mean(tmp$pars$Ability)
      #
      #   tmp$pars$Ability <- (tmp$pars$Ability -nm)/ nsd
      #   tmp$pars$B <- ( tmp$pars$B-nm) / nsd
      #   tmp$pars$A <-  tmp$pars$A * nsd
      #
      #
      #   par(mfrow=c(2,2))
      #   # plot(cdat$A,mitem[,1],col='blue',pch=16)
      #   plot(cdat$A,tmp$pars$A)
      #   try(points(cdat$A,tfit$item_irt$alpha,col='red'))
      #   abline(0,1,col='green',lwd=2)
      #   # plot(cdat$B,-mitem[,2],col='blue',pch=16)
      #   plot(cdat$B,tmp$pars$B)
      #   # if(AB) plot(cdat$B,fit$pars$B/fit$pars$A)
      #   try(points(cdat$B,tfit$item_irt$beta,col='red'))
      #   abline(0,1,col='green',lwd=2)
      #   # plot(cdat$C,mitem[,3],col='blue',pch=16)
      #   plot(cdat$C,tmp$pars$C,ylim=c(0,1))
      #   try(points(cdat$C,tfit$guess,col='red'))
      #   abline(0,1,col='green',lwd=2)
      #   # plot(cdat$Ability,mAbility,col='blue',pch=16)
      #   plot(cdat$Ability,tmp$pars$Ability)
      #   try(points(cdat$Ability,tamAbility$theta,col='red'))
      #   abline(0,1,col='green',lwd=2)
      # })


      #check these - seems right but check again...
      rownames(fit$pars$A)[itemIndex$new] <- itemIndex$original
      rownames(fit$pars$B)[itemIndex$new]<- itemIndex$original
      rownames(fit$pars$C)[itemIndex$new] <- itemIndex$original
      rownames(fit$pars$Ability)[idIndex$new] <- idIndex$original
      colnames(fit$pars$Ability)[scaleIndex$new] <- scaleIndex$original

      return(fit)
    }

    JMLseq <- list(
      if(carefulfit) list(est=c('A','B','C','Ability'),ebayes=FALSE,narrowPriors=TRUE),
      list(est=c('A','B','C','Ability'),ebayes=FALSE,narrowPriors=FALSE),
      if(ebayes) list(est=c('A','B','C','Ability'),ebayes=TRUE,narrowPriors=FALSE)
    )

    fit <- NA
    for(i in 1:length(JMLseq)){
      ebayescounter <- 0
      finished=FALSE
      if(!is.null(JMLseq[[i]])){
        if(JMLseq[[i]]$ebayes %in% 'TRUE') fitML <- fit #store fit before ebayes step
        while(!finished){
          ebayescounter <- ebayescounter + 1
          # stochastic <- F#(i == length(JMLseq))
          fit <- JMLfit(est = JMLseq[[i]]$est,sdat = sdat, ebayes=JMLseq[[i]]$ebayes,
            fit = fit,
            narrowPriors = JMLseq[[i]]$narrowPriors,...)
          if(ebayescounter >= ebayesiter || !JMLseq[[i]]$ebayes) finished=TRUE
        }
      }
    }

    if(ebayes) fit$fitML <- fitML


    #normalise pars
    if(normalise){

      for(i in 1:ncol(fit$pars$Ability)){
        selector <- rownames(fit$pars$B) %in% itemSetup$original[itemSetup$scale %in% i]

        normpars <- normaliseIRT(B = fit$pars$B[selector],
          Ability = fit$pars$Ability[,i],
          A=fit$pars$A[selector],normaliseScale = normaliseScale, normaliseMean = normaliseMean)

        fit$pars$Ability[,i] <- normpars$Ability

        fit$pars$B[selector]  <- normpars$B
        fit$pars$A[selector] <-  normpars$A
      }
    }
    # if(normalise){
    #   nsd <- apply(fit$pars$Ability,2,sd) / (normaliseScale)
    #   nm <- apply(fit$pars$Ability,2,mean)
    #
    #   for(i in 1:ncol(fit$pars$Ability)){
    #     fit$pars$Ability[,i] <- (fit$pars$Ability[,i] -nm[i])/ nsd[i] +normaliseMean
    #     selector <- rownames(fit$pars$B) %in% itemSetup$original[itemSetup$scale %in% i]
    #     fit$pars$B[selector]  <- ( fit$pars$B[selector]-nm[i]) / nsd[i] +normaliseMean
    #     fit$pars$A[selector] <-  fit$pars$A[selector] * nsd[i]
    #   }
    # }

    # browser()
    fit$itemPars <- data.frame(item=rownames(fit$pars$B),A=fit$pars$A,B=fit$pars$B,C=fit$pars$C)
    colnames(fit$itemPars)[1] <- item
    if(ncol(itemPreds)>0){
      colnames(fit$pars$itemPredsMean) <- colnames(itemPreds)
      fit$itemPars <- cbind(fit$itemPars, fit$pars$itemPredsMean)
    }

    fit$personPars <- data.frame(id=rownames(fit$pars$Ability),fit$pars$Ability)
    colnames(fit$personPars)[1] = id
    if(ncol(personPreds)>0){
      colnames(fit$pars$personPredsMean) <- colnames(personPreds)
      fit$personPars <- cbind(fit$personPars, fit$pars$personPredsMean)
    }


    return(fit)
  }



  logit <- function(x) log(x/(1-x))
