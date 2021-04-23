
# Nsubs=100;Nitems=200;Nscales=3;ASD=0;AMean=1;BSD=1;BMean=0;AbilitySD=1;AbilityMean=1
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
IRTsim <- function(Nsubs=100,Nitems=200,Nscales=3,ASD=0,AMean=1,BSD=1,BMean=0,AbilitySD=1,AbilityMean=0){

  Ability <- matrix(rnorm(Nsubs*Nscales,AbilityMean,AbilitySD),Nsubs)
  A <- matrix(rnorm(Nitems*Nscales,AMean,ASD),Nitems)
  B <- matrix(rnorm(Nitems*Nscales,BMean,BSD),Nitems)


  for(si in 1:Nscales){

    simdat <- data.frame(id=rep(1:Nsubs,each=Nitems),
      Item=rep( ((si-1)*Nitems+1):(si*Nitems),times=Nsubs),
      Scale=si,
      Ability=rep(Ability[,si],each=Nitems),
      A = rep(A[,si],times=Nsubs),B=rep(B[,si],times=Nsubs),
      pcorrect=0,score=0)

    simdat$p=1-1/(1+exp(
      A[simdat$Item-(si-1)*Nitems,si] * (#discrimination of item=
        Ability[simdat$id,si] - #ability
        B[simdat$Item-(si-1)*Nitems,si]) #item difficulty
    ))
simdat$score <- rbinom(n = simdat$p,size = 1,
  prob = simdat$p )

if(si==1) dat <- simdat else dat <- rbind(dat,simdat)
  }
  return(list(Ability=Ability,A=A,B=B, dat=dat))
}
