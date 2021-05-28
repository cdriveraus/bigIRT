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
  personPreds=NA, AbilityPredEffects=NA,normalise=TRUE){

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
        pcorrect=0,score=0)

      simdat$p= C[simdat$Item-(si-1)*Nitems,si]+
          (1-C[simdat$Item-(si-1)*Nitems,si]) / (1+exp(
            -A[simdat$Item-(si-1)*Nitems,si] * #discrimination of item=
              (Ability[simdat$id,si] - #Ability
                  B[simdat$Item-(si-1)*Nitems,si]) #item difficulty
          ))


      simdat$score <- rbinom(n = simdat$p,size = 1,
        prob = simdat$p )

      if(si==1) dat <- simdat else dat <- rbind(dat,simdat)
    }

    #
    dat <- as.data.table(dat)
    # browser()
    if(!all(is.na(itemPreds))) dat <- merge.data.table((dat),data.table(Item=1:Nitems,itemPreds),by=c('Item'))
    if(!all(is.na(personPreds))) dat <- merge.data.table((dat),data.table(id=1:Nsubs,personPreds),by=c('id'))

    return(list(Ability=Ability,A=A,B=B, C=C,dat=dat))
  }
