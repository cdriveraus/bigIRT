if(F){
#categorical covariate test

  require(data.table)
#Generate some data
Nitems=200
Nsubs=2000
itemgrade=data.table(itemgrade=rep(-2:2,each=Nitems/5))
persongrade=data.table(persongrade=rep(-2:2,each=Nsubs/5))

dat <- simIRT(Nsubs = Nsubs,Nitems = Nitems,Nscales = 1,
  logitCMean = -10,logitCSD = .03,AMean = 1,ASD = .3,
  BMean=0,BSD = 1,
  itemPreds = itemgrade,BitemPredEffects = matrix(2),
  personPreds = persongrade,AbilityPredEffects = matrix(1),
  AbilityMean = 0,AbilitySD = 1)

dat=dat$dat
dat <- dat[sample(x = 1:nrow(dat),size=ceiling(nrow(dat))),] #shuffle / select
dat$Item <- paste0('i',dat$Item)
dat$Scale <- paste0('s',dat$Scale)
dat$id <- paste0('id',dat$id)

# setnames(x = dat,old=c('score','Scale','id','Item'),c('score','scale2','id','item2'))

# drop problem people and items
dat <- bigIRT:::dropPerfectScores(dat,scoreref. = 'score',itemref. = 'Item',idref. = 'id')


plot(dat$B[!duplicated(dat$Item)])

wdat <- data.frame(dcast(data.table(dat),formula = 'id + persongrade~ Item',value.var='score'))
pl=1
#
require(TAM)
# if(pl==3) ttam <- system.time(tfit <-tam.mml.3pl(resp = wdat,est.guess = 1:ncol(wdat),
#   guess=rep(.1,ncol(wdat),control=list(msteps=20,fac.oldxsi=.6,increment.factor=1.6))))#, acceleration="Ramsay")))
if(pl==2) ttam <- system.time(tfit <-tam.mml.2pl(resp = wdat[,-1:-2],group=wdat$persongrade,est.variance = TRUE))
if(pl==1) ttam <- system.time(tfit <-tam.mml(resp = wdat[,-1:-2],group=wdat$persongrade,est.variance = TRUE))
tamAbility <- IRT.factor.scores(tfit,type = 'WLE')
#


# empirical bayes estimates for regularized final pass
fit <- fitIRT(dat,cores=6,normalise=T,ebayes=T,
  # itemPreds = 'betamean',
  pl=pl,plot=F,verbose=1,priors=T)

fiti <- fitIRT(dat,cores=6,ebayesmultiplier = 2,ebayes = T,normalise=T,
  # item = 'item2',score = 'score',id = 'id',scale = 'scale2',
  # itemDat = fiti$itemPars[1:2,],personDat = fiti$personPars[1:4,],
  itemPreds = 'itemgrade',
  personPreds = 'persongrade',
  pl=pl,plot=F,verbose=1,priors=T)

fiti$pars$Abilitybeta
fiti$pars$Bbeta

par(mfrow=c(1,1))
trueitem <- dat[!duplicated(dat$Item),]
trueitem <- trueitem[Item %in% fit$itemPars$Item]
trueitem <- trueitem[order(as.character(trueitem$Item)),]
plot(trueitem$B,fiti$itemPars$B[order(as.character(fiti$itemPars$Item))])
points(trueitem$B,tfit$item_irt$beta[order(as.character(fit$itemPars$Item))],col='red')
abline(0,1)
points(trueitem$B,fit$itemPars$B[order(as.character(fit$itemPars$Item))],col=3)
points(trueitem$B,tfit$item_irt$beta[order(as.character(fit$itemPars$Item))],col='red')
abline(0,1)
plot(fit$itemPars$B[order(as.character(fit$itemPars$Item))],fiti$itemPars$B[order(as.character(fiti$itemPars$Item))])
abline(0,1)

plot(trueitem$A,fit$itemPars$A[order(as.character(fit$itemPars$Item))])
points(trueitem$A,tfit$item_irt$alpha[order(as.character(fit$itemPars$Item))],col='red')
abline(0,1)
plot(trueitem$A,fiti$itemPars$A[order(as.character(fiti$itemPars$Item))])
points(trueitem$A,tfit$item_irt$alpha[order(as.character(fit$itemPars$Item))],col='red')
abline(0,1)

trueperson <- dat[!duplicated(dat$id),]
trueperson <- trueperson[order(as.character(trueperson$id)),]
plot(trueperson$Ability,fiti$personPars$s1[order(as.character(fiti$personPars$id))],col=rgb(1,0,0,.2),pch=16)
points(trueperson$Ability,fit$personPars$s1[order(as.character(fit$personPars$id))],col=rgb(0,1,0,.2),pch=16)
points(trueperson$Ability,tamAbility$theta[order(as.character(wdat$id))],col=rgb(0,0,1,.2),pch=16)
abline(0,1)
cor(fiti$pars$Ability,unique(dat$Ability))
cor(fit$pars$Ability,unique(dat$Ability))
cor(tamAbility$theta,unique(dat$Ability))
# plot(fit$personPars$X1[order(as.character(fit$personPars$id))],fiti$personPars$X1[order(as.character(fiti$personPars$id))])
# abline(0,1)
}
