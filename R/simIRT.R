IRTcurve <- function(a,b,c,theta=seq(-3,3,.01),plot=TRUE,rescale=FALSE,add=FALSE,...){
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
IRTsim <- function(Nsubs=100,Nitems=200,Nscales=3,ASD=0,AMean=1,BSD=1,BMean=0,logitCSD=1,logitCMean=-2,AbilitySD=1,AbilityMean=0,
  AB=FALSE){

  Ability <- matrix(rnorm(Nsubs*Nscales,AbilityMean,AbilitySD),Nsubs)
  A <- matrix(rnorm(Nitems*Nscales,AMean,ASD),Nitems)
  B <- matrix(rnorm(Nitems*Nscales,BMean,BSD),Nitems)
  logitC <- matrix(rnorm(Nitems*Nscales,logitCMean,logitCSD),Nitems)
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

    if(!AB) simdat$p= C[simdat$Item-(si-1)*Nitems,si]+
      (1-C[simdat$Item-(si-1)*Nitems,si]) / (1+exp(
      -A[simdat$Item-(si-1)*Nitems,si] * #discrimination of item=
        (Ability[simdat$id,si] - #Ability
        B[simdat$Item-(si-1)*Nitems,si]) #item difficulty
    ))

    if(AB)     simdat$p= C[simdat$Item-(si-1)*Nitems,si]+
      (1-C[simdat$Item-(si-1)*Nitems,si]) / (1+exp(
        B[simdat$Item-(si-1)*Nitems,si] -
      A[simdat$Item-(si-1)*Nitems,si] * #discrimination of item=
        Ability[simdat$id,si]#Ability
    ))

    if(AB) B <- B/A
simdat$score <- rbinom(n = simdat$p,size = 1,
  prob = simdat$p )

if(si==1) dat <- simdat else dat <- rbind(dat,simdat)
  }
  return(list(Ability=Ability,A=A,B=B, C=C,dat=dat))
}
