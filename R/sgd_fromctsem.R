sgd <- function(init,fitfunc,whichignore=c(),plot=FALSE,
  stepbase=1e-3,gmeminit=.9,gmemmax=.98, maxparchange = .50,
  roughnessmemory=.9,groughnesstarget=.5,roughnesschangemulti = 2,
  lproughnesstarget=.1,
  gsmoothroughnesstarget=.05,
  warmuplength=20,nstore=max(100,length(init)),
  minparchange=1e-800,maxiter=50000,
  nconvergeiter=30,
  itertol=1e-3, deltatol=1e-5){

  initfull=init #including ignored params start values
  if(length(whichignore)>0) init=init[-whichignore]

  errsum = function(x) sqrt(sum(abs(x)))

  if(plot){
    parbase=par(no.readonly=TRUE)
    on.exit(do.call(par,parbase),add=TRUE)
  }
  pars=init
  delta=deltaold=rep(0,length(pars))
  bestpars = newpars=maxpars=minpars=changepars=pars
  step=rep(stepbase,length(pars))
  bestiter=1

  lpg=fitfunc(init)

  g=sign(attributes(lpg)$gradient)*(abs(lpg))^(1/8)
  gsmooth=oldgsmooth=oldg=gmid=dgsmooth=gdelta=g

  lprdif = lpdif = 0
  parscore=rep(0,length(pars))

  groughness = rep(groughnesstarget,length(g))
  gsmoothroughness = rep(gsmoothroughnesstarget,length(g))
  lproughness=oldlproughnesstarget=lproughnesstarget
  gmemory <- gmeminit
  oldgmemory  <- gmemory
  oldlpdif <- 0
  lpdif <- 0
  maxlp <- -Inf
  i=0
  lp<-c()
  oldlp <- -Inf
  converged <- FALSE
  while(!converged && i < maxiter){
    i = i + 1
    accepted <- FALSE
    lproughnesstarget2 = lproughnesstarget
    notacceptedcount <- 0

    while(!accepted){
      notacceptedcount <- notacceptedcount+1
      if(notacceptedcount > 50) {
        stop('Cannot optimize! Problematic model, or bug?')
        print(lpg)
      }

      if(i > 1){
        delta =   step * (gsmooth+dgsmooth/2)
        delta[abs(delta) > maxparchange] <- maxparchange*sign(delta[abs(delta) > maxparchange])
        delta = delta +  delta/2 - deltaold/2
        newpars = pars + delta
      }

      if(any(is.na(newpars))) browser()
      if(i==1) itertime <- Sys.time()

      fullnewpars <- initfull
      if(length(whichignore)>0) fullnewpars[-whichignore] <- newpars else fullnewpars <- newpars

      lpg= fitfunc(fullnewpars)

      if(lpg > -1e99 &&       #regular check
          class(lpg) !='try-error' &&
          !is.nan(lpg[1]) &&
          all(!is.nan(attributes(lpg)$gradient))
      ){
        accepted <- TRUE
      }
      else {
        gsmooth= gsmooth*gmemory2^2 + (1-gmemory2^2) * g #increase influence of last gradient at inflections
        step <- step * .5
        deltaold <- deltaold * 0
      }

      if( #warmup check
          i < warmuplength && i > 1 && lpg[1] < lp[i-1]-5) {
        accepted <- FALSE
        step = step * .1
        deltaold <- deltaold * 0

      }
      if(plot && !accepted) {
        print(paste0('iter ', i,' not accepted!'))
      }
    } #end acceptance loop

    #once accepted
    lp[i]=lpg[1]
    pars=newpars
    deltaold=delta
    oldg=g
    g=attributes(lpg)$gradient
    g=sign(g)*(abs(g))^(1/2)#sqrt
    gmemory2 = gmemory * min(i/warmuplength,1)^(1/8)
    roughnessmemory2 = roughnessmemory * min(i/warmuplength,1)^(1/8)
    oldgmid=gmid
    gmid = g#(oldg+g)/2
    oldgsmooth = gsmooth
    gsmooth= gsmooth*gmemory2 + (1-gmemory2) * g
    dgsmooth = gmemory2*dgsmooth +(1-gmemory2)*(gsmooth-oldgsmooth)

    if(i > 1) lproughness = lproughness * (roughnessmemory2) + (1-(roughnessmemory2)) * as.numeric(lp[i-1] > (lp[i]))#because accepted here, also see non accepted version
    groughness = groughness * (roughnessmemory2) + (1-(roughnessmemory2)) * as.numeric(sign(gmid)!=sign(oldgmid))
    gsmoothroughness = gsmoothroughness * (roughnessmemory2) + (1-(roughnessmemory2)) * as.numeric(sign(gsmooth)!=sign(oldgsmooth))

    lproughnessmod=  ( ( (1/(-lproughness-lproughnesstarget2)) / (1/-lproughnesstarget2) + .5) -1) #balanced eq for any centre / target
    gsmoothroughnessmod =  (( ( (1/(-(gsmoothroughness)-gsmoothroughnesstarget)) / (1/-gsmoothroughnesstarget) + .5) ) -1)
    groughnessmod = ( ( ( (1/(-(groughness)-groughnesstarget)) / (1/-groughnesstarget) + .5) ) -1)

    step = (step + roughnesschangemulti*(
      step* lproughnessmod
      + step*
        .8*groughnessmod
    ))

    signdif= sign(gsmooth)!=sign(gmid)

    if(i > 1 && lp[i] >= max(head(lp,length(lp)-1))) {
      if(i > warmuplength) {
        ##max/min par update extra
        parscore <- parscore * .98
        whichmax <- which(pars > maxpars | pars < minpars)
        if(length(whichmax) > 0){
          parscore[whichmax] <- parscore[whichmax]+.1*(as.numeric(pars[whichmax]>maxpars[whichmax])*2-1)
          maxpars[pars>maxpars] <-pars[pars>maxpars]
          minpars[pars<minpars] <-pars[pars<minpars]
        }
        changepars=pars
        if(length(whichmax)) changepars[-whichmax] <- NA else changepars[]<-NA
      }
      bestpars <- pars <- newpars
      bestg <- g
      bestiter <- i
    }

    if(i > 25 && i %% 20 == 0) {
      oldlpdif <- lpdif
      sublp <- tail(lp,20)
      lpdif <- diff(c(max(head(sublp,5)),max(tail(sublp,5))))
      if(oldlpdif > lpdif) gmemory <- oldgmemory
      proposal = gmemory*2-oldgmemory
      oldgmemory <- gmemory
      gmemory <- min(gmemmax, max(0, proposal + runif(1,-.025,.05)))
      if(gmemory < .95) gmemory <- gmemory + .02
    }

    if(i > 31 && i %% 30 == 0) {
      oldlprdif <- lprdif
      sublp <- tail(lp,15)
      lprdif <- diff(c(max(head(sublp,5)),max(tail(sublp,5))))
      if(oldlprdif > lprdif) lproughnesstarget <- oldlproughnesstarget
      lprproposal = lproughnesstarget*2-oldlproughnesstarget
      oldlproughnesstarget <- lproughnesstarget
      if(max(lp) > max(tail(lp,30))) lprproposal <- min(.2,.5 * lprproposal)
      lproughnesstarget <- min(.5, max(.2, lprproposal + .025 * (-1+2*rbinom(n = 1,size = 1,prob = .5))))

    }

    step[step > maxparchange] <- maxparchange
    step[step < minparchange] <- minparchange

    if(i > warmuplength && lp[i] < lp[i-1]) { #if worsening, update gradient faster
      step[signdif]=step[signdif]*.5
      if(lp[i] < lp[i-10]) gmemory <- gmemory * .995
      gsmooth[signdif]= gsmooth[signdif]*gmemory2^2 + (1-gmemory2^2) * g[signdif] #increase influence of gradient at inflections
    }

    if(plot && i %% as.numeric(plot) ==0){
      par(mfrow=c(2,3),mgp=c(2,.8,0),mar=c(2,3,1,0)+.2)
      plot(pars,col=1:length(pars))
      points(changepars,pch=17,col='red')
      plot(log(abs(step*gsmooth)+1e-50),col=1:length(pars))
      plot(tail(log(-(lp-max(lp)-1)),500),type='l')
      # plot(gamweights,col=1:length(pars))
      parsd=(apply(parstore,1,sd,na.rm=T))
      matplot(t(parstore[
        which(parsd > sort(parsd,decreasing = TRUE)[min(c(length(pars),5))]),,drop=FALSE]),
        type='l')
      if(1==1){
        plot(groughness,col='red',ylim=c(0,1))
        abline(h=mean(gsmoothroughness),col='blue',lty=2)
        abline(h=(gsmoothroughnesstarget),col='blue',lty=1,lwd=2)
        points(gsmoothroughness,ylim=c(0,1),col='blue')
        abline(h=mean(groughness),col='red',lty=2)
        abline(h=(groughnesstarget),col='red',lty=1)

        abline(h=lproughnesstarget,lty=1,col='green')
        abline(h=lproughness, col='green',lty=2)


      }
      message(paste0('Iter = ',i, '   LP = ', (lp[i]),'   grad = ', sqrt(sum(g^2)), '   gmem = ', gmemory,'  lprt = ',lproughnesstarget))
    }

    #check convergence
    if(i > 30){
      lpdiff=max(tail(lp,nconvergeiter)) - min(tail(lp,nconvergeiter))

      if(lpdiff < itertol & lpdiff > 0) converged <- TRUE
      if(i==31){ #configure progress bar
        lpdiffbase <- lpdiff
        pb <- txtProgressBar(min = 0, max = 100,style = 3, width = 10, char = "=", file = stderr())
        on.exit(add=TRUE,expr = {close(pb)})
      }
      setTxtProgressBar(pb,100*(1 - log(lpdiff/itertol) / log(lpdiffbase/itertol)))
    }
  }

  setTxtProgressBar(pb,100)
  out=list(itervalues = lp, value = max(lp),
    par=bestpars,lpstore=tail(lp,nstore))

  return(out)
}

