if(FALSE){
dat <- bigIRT:::IRTsim(Nsubs = 1000,Nitems = 1000,Nscales = 1)
library(rstan)
#combined
fitc <- bigIRT:::fitIRT(dat$dat,Adata = rep(1,1000),cores=6)
plot(fitc$pars$B,-dat$B)
abline(0,1)
plot(fitc$pars$A,dat$A)
plot(c(fitc$pars$Ability),dat$Ability)
#fix ability first
fit <- bigIRT:::fitIRT(dat$dat,Adata = rep(1,1000),Abilitydata = matrix(0,1000,1),cores=6)
fit <- bigIRT:::fitIRT(dat$dat,Adata = rep(1,1000),Bdata = fit$pars$B,cores=6)
plot(fit$pars$B,-dat$B)
abline(0,1)
plot(fit$pars$A,dat$A)
plot(c(fit$pars$Abilitypars),dat$Ability)
abline(0,1)
#comparison of combined vs step approach
plot(fitc$pars$B,-dat$B)
points(fit$pars$B,-dat$B,col='red')
abline(a = 0,b=1)

plot(dat$Ability,(fitc$pars$Ability-dat$Ability)^2)
points(dat$Ability,(fit$pars$Ability-dat$Ability)^2,col='red')
abline(a = 0,b=1)
sqrt(mean((fitc$pars$Ability-dat$Ability)^2)) #rms error combined
sqrt(mean((fit$pars$Ability-dat$Ability)^2)) #vs step



score='score'; item='Item'; scale='Scale';Adata=c();Bdata=c();Abilitydata=c();
  AMean=1;ASD=0;BMean=0;BSD=1000; AbilityMean=0;AbilitySD=1;iter=2000;cores=6;id='id'

}

fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',Adata=NA,Bdata=NA,Abilitydata=NA,
  AMean=1,ASD=0.001,BMean=0,BSD=1000, AbilityMean=0,AbilitySD=1,iter=2000,cores=6){


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

  optimIRT(sm = bigIRT:::stanmodels$`2pl`,standata=sdat,Niter=iter,cores=cores)
}
