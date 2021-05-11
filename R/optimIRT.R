
standata_specificsubjects <- function(standata, subindices){
  standata$start <- as.integer(min(subindices))
  standata$end <- as.integer(max(subindices))
  return(standata)
}

clusterIDeval <- function(cl,commands){
  ctsem:::clusterIDexport(cl,c('commands'))
  # print(system.time(
    out <- parallel::clusterEvalQ(cl = cl,eval(parse(text=paste0(commands,collapse=';'))))
    # ))
  return(out)
}


optimIRT <- function(standata, cores=6, split=TRUE,
  verbose=0,plot=0,tol=1e-5,Niter=2000,askmore=FALSE,stochastic=FALSE,init=NA){
  # verbose=1
  # cores=8
  # plot=10
  # iterloop=2000
  # askmore=FALSE
  split=TRUE
  # standata=sdat
  standata$rowIndex <- 1:standata$Nobs



  eval(parse(text=
      'parlp <- function(parm){
      a=Sys.time()
          out <- try(rstan::log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)
      attributes(out)$time=Sys.time()-a

        if("try-error" %in% class(out)) {
          outerr <- out
          out <- -1e100
          attributes(out)$gradient <- rep(NaN, length(parm))
          attributes(out)$err <- outerr
        }
        if(is.null(attributes(out)$gradient)) attributes(out)$gradient <- rep(NaN, length(parm))
        attributes(out)$gradient[is.nan(attributes(out)$gradient)] <-
          rnorm(length(attributes(out)$gradient[is.nan(attributes(out)$gradient)]),0,100)

        return(out)
        }'))

  singletarget<-function(parm,gradnoise=TRUE) {
    a=Sys.time()
    out<- try(rstan::log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)

    if('try-error' %in% class(out) || is.nan(out)) {
      out=-1e100
      attributes(out) <- list(gradient=rep(0,length(parm)))
    }
    b=Sys.time()
    evaltime <- b-a
    if(verbose > 0) print(paste('ll=',out[1],', prob= ',exp(out/standata$Nobs),' ,    iter time = ',round(evaltime,2)),digits=14)
    return(out)
  }

  if(cores==1){
    target = singletarget #we use this for importance sampling
    smf <- ctsem:::stan_reinitsf(stanmodels$`2pl`,standata)
  }

  if(cores > 1){ #for parallelised computation after fitting, if only single subject
    splitby='rowIndex'

    stanindices <- split(unique(standata[[splitby]]),sort(unique(standata[[splitby]]) %% (cores)))
    if(!split) stanindices <- parallel::clusterCall(clms,function(x) unique(standata[[splitby]]))
    if(!split && length(stanindices) < cores){
      for(i in (length(stanindices)+1):cores){
        stanindices[[i]] <- NA
      }
    }

    # browser()
    # benv <- new.env(parent = globalenv())
    # benv$cores <- cores
    # benv$standata <- standata
    # benv$splitby <- splitby
    # benv$standata_specificsubjects <- standata_specificsubjects
    # benv$parlp <- parlp
    # environment(benv$parlp) <- benv
    # benv$stanindices <- stanindices
    # benv$clms <- parallel::makeCluster(spec = cores,type = 'PSOCK')
    # eval(parse(text="parallel::parLapply(cl = clms,X = 1:cores,function(x) assign('nodeid',x,env=globalenv()))"),envir = benv)

    parcommands <- list(
      "if(length(stanindices[[nodeid]]) < length(unique(standata[[splitby]]))) standata <- standata_specificsubjects(standata,stanindices[[nodeid]])",
      "if(!1 %in% stanindices[[nodeid]]) standata$dopriors <- 0L",
      "g = eval(parse(text=paste0('gl','obalenv()')))", #avoid spurious cran check -- assigning to global environment only on created parallel workers.
      "assign('smf',ctsem:::stan_reinitsf(bigIRT:::stanmodels$`2pl`,standata),pos = g)",
      "NULL"
    )
    cl <- ctsem:::makeClusterID(cores)
    on.exit(try({parallel::stopCluster(cl)},silent=TRUE),add=TRUE)
    environment(parlp) <- environment(standata_specificsubjects) <- globalenv()
    system.time(ctsem:::clusterIDexport(cl,c('cores','parlp','splitby','standata','stanindices','standata_specificsubjects')))
    system.time(clusterIDeval(cl,parcommands))

# parallel::clusterExport(benv$clms,c('standata','splitby','standata_specificsubjects','parlp','stanindices','commands'),envir = benv)

# tmp<-parallel::clusterEvalQ(cl = benv$clms, lapply(commands,function(x) eval(parse(text=x),envir = globalenv())))

iter <-0
storedLp <- c()

target<-function(parm,gradnoise=TRUE){
  a=Sys.time()
  ctsem:::clusterIDexport(cl,'parm')
  # parallel::clusterExport(benv$clms,varlist = 'parm',envir = environment())
  out2<- parallel::clusterEvalQ(cl,parlp(parm))

  tmp<-sapply(1:length(out2),function(x) {
    if(!is.null(attributes(out2[[x]])$err)){
      if(length(out2) > 1 && as.logical(verbose)) message('Error on core ', x,' but continuing:')
      message(attributes(out2[[x]])$err)
    }
  })

  out <- try(sum(unlist(out2)),silent=TRUE)
  # attributes(out)$gradient <- try(apply(sapply(out2,function(x) attributes(x)$gradient,simplify='matrix'),1,sum))
  for(i in seq_along(out2)){
    if(i==1) attributes(out)$gradient <- attributes(out2[[1]])$gradient
    if(i>1) attributes(out)$gradient <- attributes(out)$gradient+attributes(out2[[i]])$gradient
  }


  if('try-error' %in% class(out) || is.nan(out)) {
    out=-1e100
    attributes(out) <- list(gradient=rep(0,length(parm)))
  }

  if(plot > 0){
    if(out[1] > (-1e99)) storedLp <<- c(storedLp,out[1])
    iter <<- iter+1
    # g=log(abs(attributes(out)$gradient))*sign(attributes(out)$gradient)
    if(iter %% plot == 0){
      par(mfrow=c(1,1))
      # plot(parm,xlab='param',ylab='par value',col=1:length(parm))
      plot(tail(1:iter,500), tail(exp(storedLp/standata$Nobs),500),ylab='target',type='l') #log(1+tail(-storedLp,500)-min(tail(-storedLp,500)))
      # plot(g,type='p',col=1:length(parm),ylab='gradient',xlab='param')
    }
  }
  b=Sys.time()

  if(verbose > 0) print(paste0('ll=',out[1],', mean p= ',exp(out/standata$Nobs),' , iter time = ',round(b-a,5),
    ' , core times = ',paste(sapply(out2,function(x) round(attributes(x)$time,3)),collapse=', ')))
  return(out)
} #end target func

  }#end multicore




  if(cores==1) npars=rstan::get_num_upars(smf)
  # if(cores > 1) npars=parallel::clusterEvalQ(benv$clms, eval(rstan::get_num_upars(smf),envir = globalenv()))[[1]]
  if(cores > 1) npars=clusterIDeval(cl, 'rstan::get_num_upars(smf)')
  if(is.na(init[1])) init=rnorm(npars,0,.01)
  #target(init)

  converged <- FALSE






  continue <- TRUE

  while(continue && ! converged){

    if(!stochastic){

      mizelpg=list( #single core mize functions
        fg=function(pars){
          r=-target(pars)
          r=list(fn=r[1],gr= -attributes(r)$gradient)
          return(r)
        },
        fn=function(x) -target(x),
        gr=function(pars) -attributes(target(pars))$gradient
      )

      optimfit <- mize::mize(init, fg=mizelpg, max_iter=Niter,
        method="L-BFGS",memory=100,
        line_search='Schmidt',c1=1e-10,c2=.9,step0='schmidt',ls_max_fn=999,
        abs_tol=tol,grad_tol=0,rel_tol=0,step_tol=0,ginf_tol=0)
      init=optimfit$par
    }

    if(stochastic){
      optimfit <- ctsem:::sgd(
        init,
        fitfunc = target,
        itertol = 1e-3,
        deltatol=1e-5,
        ndatapoints=standata$ndatapoints,plot=FALSE)

      init=optimfit$par
    }

    if(askmore){
      continue <- ifelse(readline('Continue optimizing?') %in% c('y','Y','T','TRUE','yes','Yes','YES'),TRUE, FALSE)
    } else continue <- FALSE
  }
  # fit=optimfit

  try({parallel::stopCluster(clms)},silent=TRUE)
  smf <- ctsem:::stan_reinitsf(stanmodels$`2pl`,standata)
  return(list(optim=optimfit,stanfit=smf,pars=rstan::constrain_pars(object = smf, optimfit$par),dat=standata))

}
