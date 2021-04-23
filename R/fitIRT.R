if(FALSE){
  #Generate some data
  dat <- bigIRT:::IRTsim(Nsubs = 100,Nitems = 100,Nscales = 1)

  #fit using combined approach by fixing ability sd (fixed discrimination parameters, so 1pl model)
  system.time(fitc <- bigIRT:::fitIRT(dat$dat,Adata = c(dat$A),AbilitySD=1,cores=1))
  plot(fitc$pars$B,dat$B)
  abline(0,1)
  plot(fitc$pars$A,dat$A)
  plot(c(fitc$pars$Ability),dat$Ability)


  #fit using joint approach by fixing ability sd (fixed discrimination parameters, so 1pl model)
  system.time(fit <- bigIRT:::fitIRT(dat$dat,Adata = c(dat$A),AbilitySD=1,cores=1,jml=TRUE))
  plot(fit$pars$B,dat$B)
  abline(0,1)
  plot(fit$pars$A,dat$A)
  plot(c(fit$pars$Ability),dat$Ability)
  abline(0,1)

  #comparison of combined vs step approach
  plot(fitc$pars$B,dat$B)
  points(fit$pars$B,dat$B,col='red')
  abline(a = 0,b=1)

  #error vs ability
  plot(dat$Ability,(fitc$pars$Ability-dat$Ability)^2)
  points(dat$Ability,(fit$pars$Ability-dat$Ability)^2,col='red')

  sqrt(mean((fitc$pars$Ability-dat$Ability)^2)) #rms error combined
  sqrt(mean((fit$pars$Ability-dat$Ability)^2)) #vs step

  #correlations
  cor(data.frame(True=dat$Ability,Combined=fitc$pars$Ability,Stepwise=fit$pars$Ability))



  score='score'; item='Item'; scale='Scale';Adata=c();Bdata=c();Abilitydata=c();
  AMean=1;ASD=0;BMean=0;BSD=1000; AbilityMean=0;AbilitySD=1;iter=2000;cores=6;id='id'

}

#' Title
#'
#' @param dat
#' @param score
#' @param id
#' @param item
#' @param scale
#' @param Adata
#' @param Bdata
#' @param Abilitydata
#' @param AMean
#' @param ASD
#' @param BMean
#' @param BSD
#' @param AbilityMean
#' @param AbilitySD
#' @param iter
#' @param cores
#'
#' @return
#' @export
#'
#' @examples
fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',Adata=NA,Bdata=NA,Abilitydata=NA,
  AMean=1,ASD=0.001,BMean=0,BSD=1000, AbilityMean=0,AbilitySD=1,iter=2000,cores=6,jml=FALSE){


  if(all(is.na(Adata))) Adata <- array(numeric(),dim=c(0))
  if(all(is.na(Bdata))) Bdata <- array(numeric(),dim=c(0))
  if(all(is.na(Abilitydata))) Abilitydata <- array(numeric(),dim=c(0,0))

  sdat <- list(
    Nobs=nrow(dat),
    Nsubs=length(unique(dat[[id]])),
    Nitems=length(unique(dat[[item]])),
    Nscales=length(unique(dat[[scale]])),
    id=as.integer(factor(dat[[id]])),
    dopriors=1L,
    start=1L,
    end=as.integer(nrow(dat)),
    score=array(as.integer(dat[[score]])),
    item = array(as.integer(factor(dat[[item]]))),
    scale=array(as.integer(factor(dat[[scale]]))),
    Adata=Adata,Bdata=Bdata,Abilitydata=Abilitydata,
    AMean=AMean,ASD=ASD,BMean=BMean,BSD=BSD,AbilityMean=AbilityMean,AbilitySD=AbilitySD,
    fixedA=as.integer(length(Adata > 0)),fixedB=as.integer(length(Bdata > 0)),fixedAbility=as.integer(length(Abilitydata > 0)))


  if(!jml){
    fit <- optimIRT(sm = bigIRT:::stanmodels$`2pl`,standata=sdat,Niter=iter,cores=cores)
  } else {
    #fix ability to 0 then estimate with max likelihood (no prior)
    sdat$Abilitydata = matrix(0,sdat$Nsubs,sdat$Nscales)
    sdat$fixedAbility = 1L
    fit <- optimIRT(sm = bigIRT:::stanmodels$`2pl`,standata=sdat,Niter=iter,cores=cores)
    sdat$fixedAbility = 0L
    sdat$fixedB = 1L
    sdat$Bdata <- fit$pars$B
    sdat$Abilitydata <- Abilitydata
    fit <- optimIRT(sm = bigIRT:::stanmodels$`2pl`,standata=sdat,Niter=iter,cores=cores)
    sdat$fixedB = 0L
    sdat$fixedAbility = 1L
    sdat$Bdata <- Bdata
    sdat$Abilitydata <- fit$pars$Ability
    fit <- optimIRT(sm = bigIRT:::stanmodels$`2pl`,standata=sdat,Niter=iter,cores=cores)
  }

  return(fit)

}
