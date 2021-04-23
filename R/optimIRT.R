optimIRT <- function(sm, standata, cores=6, split=TRUE,verbose=1,plot=10,Niter=2000,askmore=FALSE){
  # verbose=1
  # cores=8
  # plot=10
  # iterloop=2000
  # askmore=FALSE
  split=TRUE
  # standata=sdat
  standata$rowIndex <- 1:standata$Nobs


  standata_specificsubjects <- function(standata, subindices){
    standata$start <- as.integer(min(subindices))
    standata$end <- as.integer(max(subindices))
    return(standata)
  }

  msParallelStan <- function(cl, standata,splitby='rowIndex', split=TRUE){
    library(rstan)
    library(ctsem)
    cores <- length(cl)
    if(split){
      stanindices <- split(unique(standata[[splitby]]),sort(unique(standata[[splitby]][-1]) %% cores-1))
      stanindices <- list(stanindices,1)
    }
    if(!split) stanindices <- lapply(1:cores,function(x) unique(standata[[splitby]]))
    if(!split && length(stanindices) < cores){
      for(i in (length(stanindices)+1):cores){
        stanindices[[i]] <- NA
      }
    }

    parallel::clusterExport(cl,c('standata'),envir = environment())

    parallel::clusterApply(cl,stanindices,function(subindices) {
      if(length(subindices) < length(unique(standata[[splitby]]))) standata <- standata_specificsubjects(standata,subindices)
      if(!1 %in% subindices) standata$dopriors <- 0L
      if(FALSE) sm=99
      g = eval(parse(text=paste0('gl','obalenv()'))) #avoid spurious cran check -- assigning to global environment only on created parallel workers.
      assign('smf',ctsem:::stan_reinitsf(sm,standata),pos = g)

      rm(standata)
      NULL
    })
    NULL
  }





  # smf <- ctsem:::stan_reinitsf(model = sm,data = standata)

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
    out<- try(log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)

    if('try-error' %in% class(out) || is.nan(out)) {
      out=-1e100
      attributes(out) <- list(gradient=rep(0,length(parm)))
    }
    b=Sys.time()
    evaltime <- b-a
    if(verbose > 0) print(paste('prob= ',exp(out/standata$Nobs),' ,    iter time = ',round(evaltime,2)),digits=14)
    return(out)
  }

  if(cores==1){
    target = singletarget #we use this for importance sampling
    smf <- ctsem:::stan_reinitsf(sm,standata)
  }

  if(cores > 1){ #for parallelised computation after fitting, if only single subject
    clms=parallel::makeCluster(cores,useXDR=FALSE,type='PSOCK',outfile='')
    # on.exit(try({parallel::stopCluster(clms)},silent=TRUE),add=TRUE)
    parallel::clusterExport(clms,varlist = c('standata_specificsubjects','sm'),envir = environment())

    #crazy trickery to avoid parallel communication pauses
    env <- new.env(parent = globalenv(),hash = TRUE)
    environment(parlp) <- env
    iter <-0
    storedLp <- c()

    target<-function(parm,gradnoise=TRUE){
      a=Sys.time()
      out2<- parallel::clusterCall(clms,parlp,parm)

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

      if(verbose > 0) print(paste('lp= ',exp(out/standata$Nobs),' , iter time = ',round(b-a,5),
        ' , core times = ',paste(sapply(out2,function(x) round(attributes(x)$time,3)),collapse=', ')))
      return(out)
    }
  }



  mizelpg=list( #single core mize functions
    fg=function(pars){
      r=-target(pars)
      r=list(fn=r[1],gr= -attributes(r)$gradient)
      return(r)
    },
    fn=function(x) -target(x),
    gr=function(pars) -attributes(target(pars))$gradient
  )



  if(cores > 1){ #msParallelStan(cl = clms,standata = standata,splitby = 'rowIndex',split = TRUE)
    splitby='rowIndex'

    # if(split){
    #   stanindices <- split(unique(standata[[splitby]][-1]),sort(unique(standata[[splitby]][-1]) %% (cores-1)))
    #   stanindices <- c(stanindices,1)
    # }
    stanindices <- split(unique(standata[[splitby]]),sort(unique(standata[[splitby]]) %% (cores)))
    if(!split) stanindices <- lapply(1:cores,function(x) unique(standata[[splitby]]))
    if(!split && length(stanindices) < cores){
      for(i in (length(stanindices)+1):cores){
        stanindices[[i]] <- NA
      }
    }


    parallel::clusterExport(clms,c('standata','splitby'),envir = environment())

    parallel::clusterApply(clms,stanindices,function(subindices) {
      library(rstan)
      library(ctsem)
      if(length(subindices) < length(unique(standata[[splitby]]))) standata <- standata_specificsubjects(standata,subindices)
      if(!1 %in% subindices) standata$dopriors <- 0L
      if(FALSE) sm=99
      g = eval(parse(text=paste0('gl','obalenv()'))) #avoid spurious cran check -- assigning to global environment only on created parallel workers.
      assign('smf',ctsem:::stan_reinitsf(sm,standata),pos = g)

      rm(standata)
      NULL
    })

  }


  if(cores==1) npars=get_num_upars(smf) else npars=parallel::clusterCall(clms,function(x) eval(parse(text='rstan::get_num_upars(smf)')))[[1]]
  init=rnorm(npars,0,.01)
  #target(init)

  converged <- FALSE
  continue <- TRUE
  while(continue && ! converged){
    optimfit <- mize::mize(init, fg=mizelpg, max_iter=Niter,
      method="L-BFGS",memory=100,
      line_search='Schmidt',c1=1e-10,c2=.9,step0='schmidt',ls_max_fn=999,
      abs_tol=1e-7,grad_tol=0,rel_tol=0,step_tol=0,ginf_tol=0)
    init=optimfit$par
    if(askmore){
      continue <- ifelse(readline('Continue optimizing?') %in% c('y','Y','T','TRUE','yes','Yes','YES'),TRUE, FALSE)
    } else continue <- FALSE
  }
  # fit=optimfit

  try({parallel::stopCluster(clms)},silent=TRUE)
  smf <- ctsem:::stan_reinitsf(sm,standata)
  return(list(optim=optimfit,stanfit=smf,pars=rstan::constrain_pars(object = smf, optimfit$par)))

}
