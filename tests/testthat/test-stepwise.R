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
dat <- dat[sample(x = 1:nrow(dat),size=nrow(dat)/10),] #shuffle / select
dat$Item <- paste0('i',dat$Item)
dat$Scale <- paste0('s',dat$Scale)
dat$id <- paste0('id',dat$id)

# setnames(x = dat,old=c('score','Scale','id','Item'),c('score','scale2','id','item2'))

# drop problem people and items
dat <- bigIRT:::dropPerfectScores(dat,scoreref. = 'score',itemref. = 'Item',idref. = 'id')

itemsteps <- lapply(unique(dat$itemgrade),function(i) unique(dat[itemgrade %in%i,Item]))

fit <- bigIRT:::fitIRTstepwise(dat,itemsteps = itemsteps,cores=1,normalise=F,ebayes=F,plot=F,verbose=1,priors=T)

fitn <- fitIRT(dat[id%in% fitn$personPars$id,],cores=1,normalise=F,ebayes=F, priors=T)
fit2 <- fitIRT(dat[id%in% fitn$personPars$id,],itemDat = fit$itemPars,cores=1,normalise=F,ebayes=F, priors=T)
#
# fitn$personPars <- fitn$personPars[order(fitn$personPars$id),]
# fitn$itemPars<- fitn$itemPars[order(fitn$itemPars$Item),]

p=bigIRT::normaliseIRT(B = fit$itemPars$B,Ability = fit$personPars$s1,A = fit$itemPars$A)
p2=bigIRT::normaliseIRT(B = fit2$itemPars$B,Ability = fit2$personPars$s1,A = fit2$itemPars$A)
pn=bigIRT::normaliseIRT(B = fitn$itemPars$B,Ability = fitn$personPars$s1,A = fitn$itemPars$A)
pdat=bigIRT::normaliseIRT(B = dat[!duplicated(Item),][order(Item),][Item %in% fit$itemPars$Item,B],
  Ability = dat[!duplicated(id),][order(id),][id %in% fit$personPars$id,Ability],
  A = dat[!duplicated(Item),][order(Item),][Item %in% fit$itemPars$Item,A])


plot(p$Ability,pdat$Ability)
points(pn$Ability,pdat$Ability,col=2)
points(p2$Ability,pdat$Ability,col=3)

sqrt(mean((p$Ability-pdat$Ability)^2))
sqrt(mean((p2$Ability-pdat$Ability)^2))
sqrt(mean((pn$Ability-pdat$Ability)^2))


plot(p$B,pdat$B)
points(pn$B,pdat$B,col=2)
points(p2$B,pdat$B,col=3)

sqrt(mean((p$B-pdat$B)^2))
sqrt(mean((p2$B-pdat$B)^2))
sqrt(mean((pn$B-pdat$B)^2))
}
