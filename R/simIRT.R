inv_logit <- function(x){
  exp(x)/(1 + exp(x))
}

inv_log1p_exp <- function(x){
  log(exp(x)-1)
}

#' Simulate a person given an IRT scale covariance cholesky, covariates, and covariate effects.
#'
#' @param scaleChol
#' @param covs
#' @param covFuncList
#'
#' @return
#' @export
#'
#' @examples
if(F){

# sim ---------------------------------------------------------------------


  scaleNames = c('maths','english')
  sc <- t(chol(matrix(c(2,1,1,.8),2,2,dimnames = list(scaleNames,NULL))))

  N=100

  pcovs=data.table(age=seq(-4,4,length.out=N),
    ses=rnorm(N,0,2),
    height=rnorm(N,0,.2))

  persons=simPersons(N=N,
    mu=c(0,.3),
    scaleChol=sc,
    covs=pcovs,
    beta=matrix(c(.3,.2,0, .4, .3, 0),byrow=TRUE,2,3)
  )

  cor(persons)
  cov2cor(sc)
  plot(persons$age,persons$maths)


NperScale=1000
  icovs=data.table(grade=seq(-4,4,length.out=NperScale),
    clarity=rnorm(NperScale,0,2),
    specificity=rnorm(NperScale,0,.2))

  items <- simItems(NperScale = 1000,scaleNames = colnames(sc),logAmu = inv_log1p_exp(c(3,3)),
    logASD = c(0,.2),Bmu = c(0,0),BSD = c(3,4),logitCmu = c(-10,-10),logitCSD = c(0,0),
    covs = icovs,
    logAbeta = matrix(c(.01,.2,0, 0,-.2,0),byrow=TRUE,2,3),
    Bbeta = matrix(c(1,0,.1, 0,-.2,0),byrow=TRUE,2,3),
    logitCbeta = matrix(c(.1,0,.1, 0,-.2,0),byrow=TRUE,2,3)
  )

  items$C <- 0 #2pl model

  items$Item <- paste0('i_',items$Scale,'_',1:nrow(items))

  itemmat=as.matrix(items[,c('A','B','C',colnames(icovs)),with=FALSE])
  cor(itemmat)
  cor(itemmat[1:NperScale,])
  cor(itemmat[(NperScale+1):(NperScale*2),])

  plot(icovs$clarity[1:NperScale],items$A[1:NperScale])


  #simulation loops

  Nassesments <- 50
  Nperassessment <- 50
  truepersons <- copy(persons)
  persons[,(scaleNames):= 0]
  setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
  write.csv(x = persons,file = 'persons.csv')
  write.csv(x = items,file = 'items.csv')
  a=Sys.time()

  rscript <- Sys.which("Rscript")
system2(rscript, paste0(" --vanilla ", getwd(),"/algoserver.R"),wait = FALSE) #start socket server

con <- socketConnection(host = "localhost", port = 8888, #connect to socket server
  blocking = FALSE, timeout = 30)

  for(ai in 1:Nassesments){
    person <- sample(1:nrow(persons),1) #random person selected
    scale <- sample(scaleNames,1)

    adat <- data.table(Item='xxxxxxxxx') #blank placeholder
    for(i in 1:Nperassessment){

      #select an item to present
      itemcode <-  selectItem(items[Scale %in% scale & !Item %in% adat$item,],
        ability = unlist(persons[person,scale,with=FALSE]),
        targetease = 0.1,samplesize = ifelse(ai > (Nassesments/2), 1000,1))
      # item <- (algoNewItem(person=person, scale=scale, targetease=.1)

      #assessment data
      rowdat <- cbind(data.table(id=person, trueability=unlist(truepersons[person,scale,with=FALSE]), AssessmentItemCount=i,
        item=itemcode, score=as.integer(NA), ability=0.0,items[Item %in% itemcode,]),pcovs[person,])
      if(i==1) adat <- rowdat else adat <- rbind(adat,rowdat)

      #get response from student
      adat[i,score:= simResponse(items[Item %in% itemcode,],unlist(truepersons[person,scale,with=FALSE])) ]

      #update ability estimate of student
      # save(adat,file='adat.rda')

      cmd <- paste0("setwd('",getwd(),"');
      load(file='adat.rda');
      # fit <- fitIRT(dat = adat,score='score',id = 'id',item = 'item',scale = 'Scale',pl = 2,cores=1,
      #   # AbilitySD = 5,
      #   priors = TRUE,ebayes = FALSE,
      #   # personPreds = colnames(pcovs), #need fixed betas here or else unidentified
      #   itemDat = adat,normalise = FALSE,dropPerfectScores = FALSE);
      #   save(fit,file='fit.rda');
      1+3
        "
      )

      cmd <- gsub('\\n','',cmd)
      cmd <- gsub(' ','',cmd)

      system.time(
      system(command = paste0(Sys.getenv("R_HOME"),'/bin/','Rscript --vanilla ',getwd(),'/algofit.R'),
        intern = T,show.output.on.console = TRUE)
      )





      persons[person,(scale):=fit$pars$Ability]
      adat[nrow(adat),ability:=fit$pars$Ability]

      # if( (adat$trueability[i]-adat$B[i]) > 3 && adat$score[i]==0) stop('too high')
      # if( (adat$trueability[i]-adat$B[i]) < -3 && adat$score[i]==1) stop('too low')
      # print(adat[nrow(adat),])

      # readline(prompt = '')
      # setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
      # write.csv(x = persons,file = 'persons.csv')


    }
    print(ai)
    print(Sys.time()-a)
    if(ai==1) record <- data.table(AssessmentID=ai,adat) else record <- rbind(record,data.table(AssessmentID=ai,adat))

  }
print(Sys.time()-a)

  require(ggplot2)
  ggplot(record,aes(y=ability, x=AssessmentItemCount,colour=factor(AssessmentID)))+
    geom_line()+
    theme_bw()+
    geom_hline(data = record,
          aes(yintercept=trueability,colour=factor(AssessmentID)),size=1,alpha=.5,linetype=2)

record[,Random:=ifelse(AssessmentID > (Nassesments/2),TRUE,FALSE)]
record[,RMSE:=sqrt(mean((trueability-ability)^2)),by=interaction(Random,AssessmentItemCount)]

ggplot(record,aes(y=RMSE,colour=Random,x=AssessmentItemCount))+geom_line()+theme_bw()


# not sim -----------------------------------------------------------------



}

# algoNewItem<-function(person, scale, targetease){
#   persons <- fread('persons.csv')
#   items <- fread('items.csv')
#   item <- selectItem(items, ability = unlist(persons[person,scale,with=FALSE]),targetease = targetease)
# }
#
# algoAbilityEst<-function(...){
#   fitIRT(...)
#   setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
#   write.csv(x = persons,file = 'persons.csv')
# }

simResponse <- function(items, ability,score=TRUE){
  p=items$C +
    (1-items$C) / (1+exp(
      -items$A * #discrimination of item=
        (ability - #Ability
            items$B)))
  if(score) return(rbinom(n = length(p),size = 1,prob=p)) else return(p)

}

simPersons <- function(N, mu, scaleChol, covs=numeric(), beta=numeric){
  d=sqrt(length(scaleChol))
  y <- (matrix(rnorm(d*N),N,d) %*% t(scaleChol) + mu)
  colnames(y) <- colnames(scaleChol)
  if(length(covs) > 0)      y <- y + as.matrix(covs) %*% t(beta)
  return(data.table(y,covs))
}


simItems <- function(NperScale, scaleNames, logAmu, logASD, Bmu, BSD, logitCmu, logitCSD,
  covs=numeric(), logAbeta, Bbeta, logitCbeta){

  items <- lapply(1:length(scaleNames),function(i){
    logA=rnorm(NperScale,logAmu[i],logASD[i])
    B = rnorm(NperScale,Bmu[i],BSD[i])
    logitC=rnorm(NperScale,logitCmu[i],logitCSD[i])

    if(length(covs) > 0){
      logA <- c(logA + as.matrix(covs) %*% t(logAbeta[i,,drop=FALSE]))
      B <- c(B + as.matrix(covs) %*% t(Bbeta[i,,drop=FALSE]))
      logitC <- c(logitC + as.matrix(covs) %*% t(logitCbeta[i,,drop=FALSE]))
    }

    o=data.table(check.names = FALSE,Scale = scaleNames[i],
      A=log1p(exp(logA)),
      B=B,
      C=inv_logit(logitC)
    )
    cbind(o,covs)
  })

  return(do.call(rbind,items))
}

selectItem <- function(items, ability, targetease, samplesize=1){
  sample(items$Item[order(abs(ability-items$B-targetease))[1:min(nrow(items),samplesize)]],size = 1) #could use expected information instead
}

IRTcurve <- function(a,b,c,theta=seq(-3,3,.01),plot=TRUE,rescale=FALSE,add=FALSE...){
  theta <- sort(theta)
  x <- c + (1-c)/(1+exp(-a*(theta-b)))
  if(rescale) theta=scale(theta)
  if(plot){
    if(!add) plot(theta, x,ylim=c(0,1),main=paste0('a = ',round(a,3),', b = ',round(b,3),', c = ',round(c,3)),type='l',...)
    if(add) points(theta, x,ylim=c(0,1),main=paste0('a = ',round(a,3),', b = ',round(b,3),', c = ',round(c,3)),type='l',...)
  }
  if(!plot) return(x)
}


#' Title
#'
#' @param Nsubs
#' @param Nitems
#' @param Nscales
#' @param ASD
#' @param AMean
#' @param BSD
#' @param BMean
#' @param AbilitySD
#' @param AbilityMean
#'
#' @return
#' @export
#'
#' @examples
IRTsim <- function(Nsubs=100,Nitems=200,Nscales=3,
  ASD=0,AMean=1,BSD=1,BMean=0,logitCSD=1,logitCMean=-2,AbilitySD=1,AbilityMean=0,
  itemPreds=NA, AitemPredEffects=NA,BitemPredEffects=NA,logitCitemPredEffects=NA,
  personPreds=NA, AbilityPredEffects=NA,
  statePreds=NA, statePredEffects=NA, normalise=TRUE){

  Ability <- matrix(rnorm(Nsubs*Nscales,AbilityMean,AbilitySD),Nsubs)
  A <- matrix(rnorm(Nitems*Nscales,AMean,ASD),Nitems)
  B <- matrix(rnorm(Nitems*Nscales,BMean,BSD),Nitems)
  logitC <- matrix(rnorm(Nitems*Nscales,logitCMean,logitCSD),Nitems)

  if(!all(is.na(itemPreds))){
    if(all(!is.na(AitemPredEffects))) A <- A + apply(itemPreds,1,function(x) sum(AitemPredEffects*x))
    if(all(!is.na(BitemPredEffects))) B <- B + apply(itemPreds,1,function(x) sum(BitemPredEffects*x))
    if(all(!is.na(logitCitemPredEffects))) logitC <- logitC + apply(itemPreds,1,function(x) sum(logitCitemPredEffects*x))
  }

  if(!all(is.na(personPreds))){
    if(all(!is.na(AbilityPredEffects))) {
      for(i in 1:Nscales){
        Ability[,i] <- Ability[,i] + apply(personPreds,1,function(x) sum(AbilityPredEffects*x))
      }
    }
  }



  if(normalise){
    for(i in 1:ncol(Ability)){
      normpars <- normaliseIRT(B=B[,i],
        Ability=Ability[,i], A=A[,i])
      B[,i] = normpars$B
      Ability[,i] = normpars$Ability
      A[,i] = normpars$A
    }
  }


  C <- ctsem:::inv_logit(logitC)


  for(si in 1:Nscales){

    simdat <- data.frame(id=rep(1:Nsubs,each=Nitems),
      Item=rep( ((si-1)*Nitems+1):(si*Nitems),times=Nsubs),
      Scale=si,
      Ability=rep(Ability[,si],each=Nitems),
      A = rep(A[,si],times=Nsubs),
      B=rep(B[,si],times=Nsubs),
      C=rep(C[,si],times=Nsubs),
      pcorrect=0,score=0, stateEffect=0)

    #

    if(!all(is.na(statePreds))) {
      for(pi in 1:ncol(statePreds)){
        simdat$stateEffect <-  simdat$stateEffect + statePredEffects[si,pi] * statePreds[,pi,with=FALSE][[1]]
      }
    }

    simdat$p= C[simdat$Item-(si-1)*Nitems,si]+
      (1-C[simdat$Item-(si-1)*Nitems,si]) / (1+exp(
        -A[simdat$Item-(si-1)*Nitems,si] * #discrimination of item=
          (Ability[simdat$id,si] - #Ability
              B[simdat$Item-(si-1)*Nitems,si] + #item difficulty
              simdat$stateEffect) #state effect on ability
      ))



    simdat$score <- rbinom(n = nrow(simdat),size = 1,
      prob = simdat$p )

    if(si==1) dat <- simdat else dat <- rbind(dat,simdat)
  }

  #
  dat <- as.data.table(dat)
  if(length(statePreds) > 0) dat <- cbind(dat, statePreds)
  # browser()
  if(!all(is.na(itemPreds))) dat <- merge.data.table((dat),data.table(Item=1:Nitems,itemPreds),by=c('Item'))
  if(!all(is.na(personPreds))) dat <- merge.data.table((dat),data.table(id=1:Nsubs,personPreds),by=c('id'))

  return(list(Ability=Ability,A=A,B=B, C=C,dat=dat))
}
