#ctsem functions
stan_reinitsf <- function(model, data,fast=FALSE){
  if(fast) sf <- new(model@mk_cppmodule(model),data,0L,getcxxfun(model@dso))

  if(!fast) suppressMessages(suppressWarnings((sf<-
      rstan::sampling(model,iter=0,chains=0,init=0,data=data,check_data=FALSE,
        control=list(max_treedepth=0),save_warmup=FALSE,test_grad=FALSE))))

  return(sf)
}




standata_specificsubjects <- function(standata, subindices){
  # browser()
  include <- which(standata$id %in% subindices)
  sapply(c('id','score','item','scale','trainingLogical'),function(x) standata[[x]]<<-standata[[x]][include])
  sapply(c('itemPreds','personPreds'),function(x) standata[[x]]<<-standata[[x]][include,,drop=FALSE])
  standata$Nobs <- as.integer(length(include))
  standata$end <- standata$Nobs
  # standata$rowIndexPar <- 0L;
  # browser()
  # standata$start <- as.integer(min(subindices))
  # standata$end <- as.integer(max(subindices))
  # standata$start <- as.integer(min( (1:standata$Nobs)[standata$id %in% min(standata$id[subindices])] )) #first row of first subject in subindices
  # standata$end <- as.integer(max( (1:standata$Nobs)[standata$id %in% max(standata$id[subindices])]] )) #last row of last subject in subindices
  return(standata)
}

makeClusterID <- function(cores){
  benv <- new.env(parent=globalenv())
  benv$cores <- cores
  benv$cl <- parallel::makeCluster(spec = cores,type = "PSOCK",useXDR=FALSE,outfile='',user=NULL)
  eval(parse(text=
      "parallel::parLapply(cl = cl,X = 1:cores,function(x) assign('nodeid',x,env=globalenv()))"),envir = benv)
  return(benv$cl)
}

clusterIDexport <- function(cl, vars){
  benv <- new.env(parent=globalenv())
  benv$cl <- cl
  lookframe <- parent.frame()
  tmp<-lapply(vars,function(x) benv[[x]] <<- eval(parse(text=x),env=lookframe))
  eval(parallel::clusterExport(benv$cl,vars,benv),envir=globalenv())
}

clusterIDeval <- function(cl,commands){
  clusterIDexport(cl,c('commands'));
  rm(commands)
  out <- parallel::clusterEvalQ(cl = cl,eval(parse(text=paste0(commands,collapse=';'))))
  return(out)
}

#based on rstan function, very cut down, may fail in some cases...
#' @importFrom Rcpp cpp_object_initializer
getcxxfun <- function(object) {
  if (length(object@dso_saved) == 0){
    return(function(...) stop("this function should not be called"))
  }  else  return(object@.CXXDSOMISC$cxxfun)
}

upars_names<-function(fit){

  n<-with(fit$dat,{
    n<-c()
    if(Nitems-NfixedA) n <- c(n,paste0('a',1:(Nitems-NfixedA)))
    if(Nitems-NfixedB) n <- c(n,paste0('b',1:(Nitems-NfixedB)))
    if(Nitems-NfixedC) n <- c(n,paste0('c',1:(Nitems-NfixedC)))

    if(Nsubs*Nscales-NfixedAbility) n <- c(n,paste0('ability',1:(Nsubs*Nscales-NfixedAbility)))

    if(!fixedAMean) n <- c(n,'muA')
    if(!fixedBMean) n <- c(n,'muB')
    if(!fixedCMean) n <- c(n,'muC')
    if(!fixedAbilityMean) n <- c(n,paste0('muAbility',1:Nscales))
    return(n)
  })
  pardifflength=length(fit$optim$par)-length(n)
  if(pardifflength) n <- c(n,paste0('covpar',1:pardifflength))
  return(n)
}

scoreIRT <- function(fit, verbose=1L){

  standata=fit$dat
  standata$rowIndexPar=1L
  standata$dopriors=0L
  smf <- stan_reinitsf(stanmodels$irt,standata)
  score <- matrix(NA,standata$Nobs,rstan::get_num_upars(smf))
  for(i in 1:standata$Nobs){
    a=Sys.time()
    if(verbose && i %% 100 ==0) message(paste0(i,' / ', standata$Nobs))
    standata$start <- standata$end <- i
    # smf <- stan_reinitsf(stanmodels$irt,standata,fast = TRUE)
    score[i,] <- rstan::grad_log_prob(smf,c(fit$optim$par,i), adjust_transform = TRUE)
    # print(Sys.time()-a)
  }
  score <- score[,-ncol(score)]
  colnames(score)  <- upars_names(fit)

  return(score)
}


optimIRT <- function(standata, cores=6, mml=FALSE,split=TRUE,
  verbose=0,plot=0,tol=1e-2,Niter=2000,askmore=FALSE,stochastic=FALSE,init=NA,dohess=FALSE){
  # verbose=1
  # cores=8
  # plot=10
  # iterloop=2000
  # askmore=FALSE
  split=TRUE
  # standata=sdat
  # standata$rowIndex <- array(as.integer(1:standata$Nobs))



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

  iter <-0
  storedLp <- c()

  singletarget<-function(parm,gradnoise=TRUE) {
    iter <<- iter+1
    a=Sys.time()
    out<- try(rstan::log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)

    if('try-error' %in% class(out) || is.nan(out)) {
      out=-1e100
      attributes(out) <- list(gradient=rep(0,length(parm)))
    }
    b=Sys.time()
    evaltime <- b-a
    if(verbose > 0 && (iter %% verbose)==0) print(paste('ll=',out[1],', prob= ',exp(out/standata$Nobs),' ,    iter time = ',round(evaltime,2)),digits=14)
    return(out)
  }

  if(cores==1){
    target = singletarget #we use this for importance sampling
    if(!mml) smf <- stan_reinitsf(stanmodels$irt,standata)
    if(mml) smf <- stan_reinitsf(stanmodels$irtmml,standata)

  }

  if(cores > 1){ #for parallelised computation after fitting, if only single subject
    splitby='id'
    stanindices <- split(sort(unique(standata[[splitby]])),sort(unique(standata[[splitby]]) %% (cores)))

    parcommands <- list(
      "#if(length(stanindices[[nodeid]]) < length(unique(standata[[splitby]]))) ",
      "standata <- standata_specificsubjects(standata,stanindices[[nodeid]])",
      "if(!1 %in% stanindices[[nodeid]]) standata$dopriors <- 0L",
      "g = eval(parse(text=paste0('gl','obalenv()')))", #avoid spurious cran check -- assigning to global environment only on created parallel workers.
      paste0("assign('smf',bigIRT:::stan_reinitsf(bigIRT:::stanmodels$irt",ifelse(mml,'mml',''),",standata),pos = g)"),
      "NULL"
    )


    #first approach
    # cl <- makeClusterID(cores)
    # on.exit(try({parallel::stopCluster(cl)},silent=TRUE),add=TRUE)
    # environment(parlp) <- environment(standata_specificsubjects) <- globalenv()
    # system.time(clusterIDexport(cl,c('cores','parlp','splitby','standata','stanindices','standata_specificsubjects')))
    # system.time(clusterIDeval(cl,parcommands))

    # browser()
    #second
    benv <- new.env(parent=globalenv())
    benv$cl <- NA #placeholder for flexsapply usage
    environment(parlp) <- environment(standata_specificsubjects) <- globalenv()


    assign(x = 'cl',
      parallel::makeCluster(spec = cores,type = "PSOCK",useXDR=FALSE,outfile='',user=NULL),
      envir = benv)
    parallel::clusterExport(benv$cl,
      c('cores','parlp','splitby','standata','stanindices','standata_specificsubjects','parcommands'),envir = environment())

    benv$cores=cores

    eval(parse(text=
        "parallel::parLapply(cl = cl,X = 1:cores,function(x){
         assign('nodeid',x,envir=globalenv())
        })"),envir=benv)

    parallel::clusterEvalQ(cl = benv$cl,expr = sapply(parcommands,function(x) eval(parse(text=x),envir = globalenv())))

    on.exit(try({parallel::stopCluster(benv$cl)},silent=TRUE),add=TRUE)





    target<-function(parm,gradnoise=TRUE){
      iter <<- iter+1
      a=Sys.time()
      # clusterIDexport(cl,'parm')
      parallel::clusterExport(benv$cl,'parm',envir = environment())
      out2<- parallel::clusterEvalQ(benv$cl,parlp(parm))

      tmp<-sapply(1:length(out2),function(x) {
        if(!is.null(attributes(out2[[x]])$err)){
          if(length(out2) > 1 && as.logical(verbose)) message('Error on core ', x,' but continuing:')
          message(attributes(out2[[x]])$err)
        }
      })

      out <- try(sum(unlist(out2)),silent=TRUE)
      for(i in seq_along(out2)){
        if(i==1) attributes(out)$gradient <- attributes(out2[[1]])$gradient
        if(i>1) attributes(out)$gradient <- attributes(out)$gradient+attributes(out2[[i]])$gradient
      }


      if('try-error' %in% class(out) || is.nan(out)) {
        out=-1e100
        attributes(out) <- list(gradient=rep(0,length(parm)))
      }

      if(plot > 0){
         storedLp <<- c(storedLp,ifelse(out[1] > (-1e99),out[1],NA))
        # g=log(abs(attributes(out)$gradient))*sign(attributes(out)$gradient)
        if(iter %% plot == 0){
          par(mfrow=c(1,1))
          # plot(parm,xlab='param',ylab='par value',col=1:length(parm))
          tmp<-try(plot(tail(1:iter,500), tail(exp(storedLp/standata$Nobs),500),ylab='target',type='l')) #log(1+tail(-storedLp,500)-min(tail(-storedLp,500)))
          if('try-error' %in% class(tmp) ) browser()
          # plot(g,type='p',col=1:length(parm),ylab='gradient',xlab='param')
        }
      }
      b=Sys.time()
      if(verbose > 0  && (iter %% verbose)==0) print(paste0('ll=',out[1],', mean p= ',exp(out/standata$Nobs),' , iter time = ',round(b-a,5),
        ' , core timerange = ',paste0(range(sapply(out2,function(x) round(attributes(x)$time,3))),collapse=' : ')))
      return(out)
    } #end target func

  }#end multicore


  if(cores==1) npars=rstan::get_num_upars(smf)
  # if(cores > 1) npars=parallel::clusterEvalQ(benv$clms, eval(rstan::get_num_upars(smf),envir = globalenv()))[[1]]
  # if(cores > 1) npars=clusterIDeval(cl, 'rstan::get_num_upars(smf)')[[1]]
  if(cores > 1) npars=parallel::clusterEvalQ(benv$cl, rstan::get_num_upars(smf))[[1]]
  if(is.na(init[1])) init=rnorm(npars,0,.1)
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
      optimfit <- sgd(
        init=init,
        maxiter=Niter,
        fitfunc = target,
        itertol = tol)

      init=optimfit$par
    }

    if(askmore){
      continue <- ifelse(readline('Continue optimizing?') %in% c('y','Y','T','TRUE','yes','Yes','YES'),TRUE, FALSE)
    } else continue <- FALSE
  }

  if(dohess){
    jac <- matrix(NA,length(optimfit$par),length(optimfit$par))
    basegrad <- attributes(target(optimfit$par))$gradient
    for(i in 1:length(optimfit$par)){
      newpar=optimfit$par
      newpar[i] <- newpar[i] + .001
      jac[,i] <- (attributes(target(newpar))$gradient-basegrad)/ .001
    }
    parcov <- try(solve(-jac))
  } else parcov <- NULL

  try({parallel::stopCluster(clms)},silent=TRUE)

  standata$doGenQuant = 1L #generate extra values now
  if(!mml) smf <- stan_reinitsf(stanmodels$irt,standata)
  if(mml) smf <- stan_reinitsf(stanmodels$irtmml,standata)

  return(list(optim=optimfit,
    parcov=parcov,
    stanfit=smf,
    pars=rstan::constrain_pars(object = smf, optimfit$par),
    dat=standata))

}

