

inv_logit <- function(x) exp(x)/(1+exp(x))
logit <- function(x)  log(x)-log((1-x))

afunc <- function(x) log1p(exp(x))
afunci <- function(x) log(exp(x)-1)

pcalc <- function(score,ability,B,A=1,C=0,D=1){
  p <- C + (1.0-C) / ( 1.0 + exp( (-A * (ability - B))  ) )
}

checkP <- function(fit){ #for checking max a posteriori model parameters using R based calc instead of stan based
  p=rep(NA,fit$dat$Nobs)
  for(i in 1:length(p)){
    p[i] <- fit$itemPars$C[fit$dat$item[i]] + (1.0-fit$itemPars$C[fit$dat$item[i]]) / ( 1.0 + exp(
      (-fit$itemPars$A[fit$dat$item[i]] * (
        fit$personPars[fit$dat$id[i], 1+fit$dat$scale[i]] -
          fit$itemPars$B[fit$dat$item[i]]
      ))));

    if(fit$dat$score[i]==0) p[i]= 1.0-p[i];
  }
  return(p)
}


# birtRunGeneratedQuantities<- function(fit){
#
#   log1p_exp=function(x) log1p(exp(x))
#
#   genq <- function(){
#     browser()
#
#     A=rep(NA,Nitems) # item A values
#     B=rep(NA,Nitems) #item B values
#     C=rep(NA,Nitems) #item C values
#     #put the user supplied fixed values into the item parameter objects
#     A[fixedA] = Adata[fixedA];
#     B[fixedB] = Bdata[fixedB];
#     C[fixedC] = Cdata[fixedC];
#
#     #put the free parameters into the item parameter objects
#     A[whichnotfixedA] = invspApars;
#     B[whichnotfixedB] = Bpars;
#     C[whichnotfixedC] = logitCpars;
#
#
#     # for(i in 1:Nsubs){ #for every subject
#     #   for(j in 1:Nscales){ #and every scale
#     #     if(fixedAbilityLogical[i,j]==1){
#     #       Ability[i,j] = Abilitydata[i,j];
#     #     } else{ #if ability is user supplied, input it
#     #       Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; # or input the free parameter
#     #       if(NpersonPreds) {
#     #         predsmean=rep(0, NpersonPreds); #compute mean of person predictors
#     #         count=0;
#     #         for( ri in 1:Nobs){
#     #           if(id[i] == i){
#     #             count=count+1;
#     #             predsmean=predsmean+personPreds[i,];
#     #           }
#     #         }
#     #         predsmean= predsmean/count;
#     #         Ability[i,j] = Ability[i,j] +predsmean * Abilitybeta[j,]; #when there are person predictors, apply the effect
#     #       }
#     #     }
#     #   }
#     # }
#
#
#     for(i in 1:Nitems){ #for every item
#       count=0;
#       predsmean=rep(0,NitemPreds);
#       for( ri in 1:Nobs){
#         if(item[ri] == i){
#           count=count+1;
#           predsmean=predsmean+itemPreds[ri,];
#         }
#       }
#       predsmean= predsmean/count;
#       if(fixedAlog[i]==0){ #if free A par and item predictors, compute average item effect
#         A[i] =A[i]+ matrix(predsmean,1) %*% t(invspAbeta[ifelse(itemSpecificBetas==1,freeAref[item[i]],1),,drop=FALSE]); #when there are person predictors, apply the effect
#         A[i]=log1p_exp(A[i]);
#       }
#       if(fixedB[i]==0){ #if free B par and item predictors, compute average item effect
#         B[i] = B[i] + matrix(predsmean,1) %*% t(Bbeta[ifelse(itemSpecificBetas==1, freeBref[item[i]], 1),,drop=F]); #when there are person predictors, apply the effect
#       }
#     }
#
#
#     #
#     #       #linearised regression weights for reporting
#     #       if(doApreds){
#     #         if(size(Abeta)==1){
#     #           Abeta[1,] = ((log1p_exp(mean(invspApars)+invspAbeta[1,]*.01))-(log1p_exp(mean(invspApars)-invspAbeta[1,]*.01)))/.02;
#     #         }
#     #         if(size(Abeta)>1){
#     #           for(i in 1:size(Abeta)){
#     #             Abeta[i,] = ((log1p(exp(invspApars[i])+invspAbeta[i,]*.01))-(log1p(exp(invspApars[i])-invspAbeta[i,]*.01)))/.02;
#     #           }
#     #         }
#     #       }
#     #
#     #       if(doCpreds){
#     #         if(size(Cbeta)==1)   Cbeta[1,] = ((inv_logit(mean(logitCpars))+logitCbeta[1,]*.01)-(inv_logit(mean(logitCpars))-logitCbeta[1,]*.01))/.02;
#     #         if(size(Cbeta)>1){
#     #           for(i in 1:size(Cbeta)){
#     #             Cbeta[i,] = ((inv_logit(logitCpars[i])+logitCbeta[i,]*.01)-(inv_logit(logitCpars[i])-logitCbeta[i,]*.01))/.02;
#     #           }
#     #         }
#     #       }
#
#   } #end internal genq function
#
#   e <- list2env(c(fit$pars,fit$dat))
#   environment(genq) <- e
#   genq()
# }


#' normaliseIRT
#'
#' Normalise item response theory (IRT) parameters.
#'
#' @param B Vector of item difficulty parameters.
#' @param Ability Vector of persons' ability parameters.
#' @param A Vector of item discrimination parameters.
#' @param normbase The base from which the normalisation should be calculated. Can be 'Ability' or 'B'.
#' @param normaliseScale The scale to normalise to.
#' @param normaliseMean The mean to normalise to. The default is 0 when normbase is 'Ability' or 'B', and 1 when normbase is 'A'.
#' @param robust if TRUE, outliers (greater than 1.5x the interquartile range from the interquartile region of 25-75%)
#' are dropped before computing the mean and sd for normalisation.
#'
#' @return A list containing the normalised A, B, and Ability parameters.
#' @export
#' @examples
#' B <- rnorm(100,2,1)
#' Ability <- rnorm(500,3,.5)
#' A <- rnorm(100,1.4,.05)
#' normaliseIRT(B, Ability, A, normbase='B')
#'
#'
normaliseIRT <- function(B,Ability, A,normbase='Ability',normaliseScale=1,  normaliseMean=ifelse(normbase == 'A',1,0),robust=TRUE){

  # if(!robust){
  #   includePersons <- 1:length(Ability)
  #   includeItems <- 1:length(B)
  # }
  # if(robust){
  #   includePersons <- which(Ability %in% boxplot(Ability, plot = F)$out)
  #   includeItems <- which(B %in% boxplot(B, plot = F)$out & A %in% boxplot(A, plot = F)$out)
  # }

  if(normbase %in% c('Ability','B')){
    if(normbase =='Ability'){
      nsd <- ifelse(robust, diff(quantile(Ability,probs=c(.25,.75))), sd(Ability)) / (normaliseScale)
      nm <- ifelse(robust, median(Ability),mean(Ability))
    }

    if(normbase =='B'){
      nsd <- ifelse(robust, diff(quantile(B, probs=c(.25,.75))), sd(B)) / (normaliseScale)
      nm <- ifelse(robust, median(B), mean(B))
    }

    Ability <- (Ability -nm)/ nsd +normaliseMean
    B  <- ( B-nm) / nsd +normaliseMean
    A <-  A * nsd
  }

  # if(normbase == 'A'){
  #   logA <- log(A)
  #   nsd <- ifelse(robust,diff(quantile(logA,probs=c(.25,.75))), sd(logA)) / (normaliseScale)
  #   if(is.na(nsd)) stop('Error calculating sd of discrimination parameters -- do they vary or are any negative?')
  #   nm <- ifelse(robust, median(logA),mean(logA))
  #   normaliseMean <- log(normaliseMean)
  #
  #   Ability <- (Ability -nm)/ nsd +normaliseMean
  #   B  <- ( B-nm) / nsd +normaliseMean
  #   logA <-  (logA -nm)/nsd +normaliseMean
  # }

  return(list(A=A,B=B,Ability=Ability))
}



#' Drop subjects and items with all perfect scores
#'
#' This function drops variables/items and subjects that have all perfect scores (either all 0's or all 1's) in a data table.
#'
#' @param dat The input data table
#' @param scoreref The column name of the score variable in \code{dat}
#' @param itemref The column name of the item variable in \code{dat}
#' @param idref The column name of the id variable in \code{dat}
#' @param tol Tolerance level for checking perfect scores -- .01 would drop subjects with less than 1% correct or incorrect
#'
#' @return The input data table (\code{dat}) without variables/items and subjects with all perfect scores.
#'
#' @import data.table
#' @export
#'
#' @examples
#' dat <- data.table(id=c(1,1,1,2,2,2,3,3,3), Item=c('I1','I2','I3','I1','I2','I3','I1','I2','I3'),
#'    score=c(1,0,1,0,0,0,0,1,1))
#' print(dropPerfectScores(dat))
dropPerfectScores <- function(dat,scoreref.='score',itemref.='Item',idref.='id',tol.=.001){
  if(!'data.table' %in% class(dat)) stop('Not a data.table!')

  dropping <- TRUE
  while(dropping){
    dropping <- FALSE
    dat[,itemMean:=mean(get(scoreref.)),by=itemref.]
    if(any((abs(dat$itemMean-.5)+tol.)>= .5)){
      dropping <- TRUE
      warning('Dropping items with all 0 or 1',immediate. = TRUE)
      dat <- dat[(abs(itemMean-.5)+tol.)< .5,]
    }
    dat[,personMean:= mean(get(scoreref.)),by=idref.]
    if(any((abs(dat$personMean-.5)+tol.)>= .5)){
      warning('Dropping subjects with all 0 or 1',immediate. = TRUE)
      dropping <- TRUE
      dat <-dat[(abs(personMean-.5)+tol.)< .5,]
    }
  }
  dat[,itemMean:=NULL]
  dat[,personMean:=NULL]
  dat[1,] #weirdness required to ensure return prints properly
  return(dat)
}

# fitIRTstepwise <- function(dat,itemsteps,item='Item',id='id',normalise=FALSE,ebayes=FALSE,...){ #need to rethink...
#   .itemref <- item
#   .idref <- id
#   itemDat <- NA
#   stepseq <- c(1:length(itemsteps))#,length(itemsteps):1)
#   firststep <- TRUE
#
#
#   for(stepi in 1:length(stepseq)){
#     include <- which(dat[[.itemref]] %in% itemsteps[[stepseq[stepi]]]) #which rows to include for current item set
#     stepids <- unique(dat[include,get(.idref)]) #which subjects are relevant
#     if(stepi > 1){ #for subsequent steps,
#       itemDat <- itemDat[!get(.itemref) %in% itemsteps[[stepseq[stepi]]],] #freely estimate current item set
#       include <- unique(c(include, # and use prior step as link
#         which(dat[[.itemref]] %in% itemsteps[[stepseq[stepi-1]]] & dat[[.idref]] %in% stepids)))
#     }
#     smalldat <- dat[include,] #step specific data set
#
#     fit <- fitIRT(dat = smalldat,itemDat=itemDat,normalise=normalise,ebayes=ebayes,item=item,id=id,...)
#     itemDat <- data.table(fit$itemPars)
#
#     if(firststep){
#       itemout <- itemDat
#       personout <- data.table(fit$personPars)
#     }
#     if(!firststep){ #add new items to output
#       itemout <- rbind(itemout,itemDat[!get(.itemref) %in% itemout[[.itemref]]]) #update item output with newest estimates
#       personout <- rbind(personout, data.table(fit$personPars)[!get(.idref) %in% personout[[.idref]],]) #update item output with newest estimates
#     }
#
#     # plot(itemout[order(as.character(get(.itemref))),B],dat[!duplicated(Item) & get(.itemref) %in% itemout[[.itemref]],][order(get(.itemref)),B])
#     # rmse <- sqrt(mean((itemout[order(as.character(get(.itemref))),B]-dat[!duplicated(get(.itemref)) & Item %in% itemout[[.itemref]],][order(get(.itemref)),B])^2))
#     # message(paste('corr =',cor(cbind(itemout[order(as.character(get(.itemref))),B],dat[!duplicated(get(.itemref)) & get(.itemref) %in% itemout[[.itemref]],][order(get(.itemref)),B]))[2,1]))
#     # message(paste('RMSE =',round(rmse,3)))
#
#     firststep <- FALSE
#   }
#
#   if(FALSE){ #for testing
#     fullfit <- fitIRT(dat = dat,normalise=normalise,ebayes=ebayes)
#     #item par comparison to true
#     message(paste('corr =',cor(cbind(
#       data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% fullfit$itemPars[[.itemref]],][order(Item),B])
#     )[2,1]))
#
#     points(data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% itemout[[.itemref]],][order(Item),B],col=2)
#
#     #person par comparison to true
#     message(paste('corr =',cor(cbind( #stepwise fit
#       personout[order(as.character(get(.idref))),s1],
#       dat[!duplicated(get(.idref)) & get(.idref) %in% personout[[.idref]],][order(get(.idref)),Ability])
#     )[2,1]))
#
#     message(paste('corr =',cor(cbind( #full fit
#       data.table(fullfit$personPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% fullfit$itemPars[[.itemref]],][order(Item),B])
#     )[2,1]))
#
#     points(data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% itemout[[.itemref]],][order(Item),B],col=2)
#   }
#
#   personout <- personout[order(get(.idref)),]
#   itemout <- itemout[order(get(.itemref)),]
#
#   return(list(itemPars=itemout,personPars=personout))
# }


#' Fit a binary Item Response Theory (IRT) model
#'
#' This function fits a binary Item Response Theory (IRT) model using various parameters and options.
#'
#' @param dat A data frame containing the data to be analyzed.
#' @param score Character. The name of the column in \code{dat} representing the response. Default is 'score'.
#' @param id Character. The name of the column in \code{dat} representing the individual. Default is 'id'.
#' @param item Character. The name of the column in \code{dat} representing the item. Default is 'Item'.
#' @param scale Character. The name of the column in \code{dat} representing the scale of each item. Default is 'Scale'.
#' @param pl Integer. The number of parameters for the logistic model (1PL, 2PL, 3PL, or 4PL). Default is 1.
#' @param personDat Data frame. Any fixed ability data for persons. Default is NA.
#' @param personPreds Character vector. Names of predictors for person parameters found in data. Default is an empty character vector.
#' @param itemDat Data frame. Any fixed item data. Default is NA.
#' @param AitemPreds Character vector. Names of predictors for item discrimination parameters. Default is an empty character vector.
#' @param BitemPreds Character vector. Names of predictors for item difficulty parameters. Default is an empty character vector.
#' @param CitemPreds Character vector. Names of predictors for item guessing parameters. Default is an empty character vector.
#' @param DitemPreds Character vector. Names of predictors for item upper asymptote (slipping) parameters. Default is an empty character vector.
#' @param itemSpecificBetas Logical. Whether to allow item-specific betas for covariate effects, or simply estimate one effect per covariate. Default is FALSE.
#' @param betaScale Numeric. Scale of the prior for beta parameters. Default is 10.
#' @param invspAMeandat Numeric. Mean for the prior distribution of the raw discrimination parameters,
#' which subsequently have a 'softplus' log(1+exp(x)) applied. Default is 0.542, giving a mean for A pars of ~ 1.
#' @param invspASD Numeric. Standard deviation for the prior distribution of the raw discrimination parameters. Default is 2.
#' @param BMeandat Numeric. Mean for the prior distribution of the item difficulty parameters. Default is 0.
#' @param BSD Numeric. Standard deviation for the prior distribution of the item difficulty parameters. Default is 10.
#' @param logitCMeandat Numeric. Mean for the prior distribution of the item guessing parameters (on logit scale). Default is -4.
#' @param logitCSD Numeric. Standard deviation for the prior distribution of the item guessing parameters (on logit scale). Default is 2.
#' @param logitDMeandat Numeric. Mean for the prior distribution of the item upper asymptote parameters (on logit scale). Default is 4.
#' @param logitDSD Numeric. Standard deviation for the prior distribution of the item upper asymptote parameters (on logit scale). Default is 2.
#' @param AbilityMeandat Numeric array. Mean for the prior distribution of the ability parameters. Default is 0 for each scale.
#' @param AbilitySD Numeric array. Standard deviation for the prior distribution of the ability parameters. Default is 10 for each scale.
#' @param AbilityCorr Matrix. Correlation matrix for the ability parameters. Default is an identity matrix.
#' @param AMeanSD Numeric. Standard deviation for the prior distribution of the discrimination parameters. Default is 1.
#' @param BMeanSD Numeric. Standard deviation for the prior distribution of the difficulty parameters. Default is \code{BSD}.
#' @param logitCMeanSD Numeric. Standard deviation for the prior distribution of the guessing parameters (on logit scale). Default is \code{logitCSD}.
#' @param logitDMeanSD Numeric. Standard deviation for the prior distribution of the upper asymptote parameters (on logit scale). Default is \code{logitDSD}.
#' @param AbilityMeanSD Numeric array. Standard deviation for the prior distribution of the ability parameters. Default is 1 for each scale.
#' @param iter Integer. Maximum number of iterations for the fitting algorithm. Default is 2000.
#' @param cores Integer. Number of cores to use for parallel computation. Default is 6.
#' @param carefulfit Logical. Whether to use a slower, careful fitting procedure. Default is FALSE. Experimental.
#' @param ebayes Logical. Whether to use empirical Bayes estimation. Default is TRUE. With ebayes, the priors are adapted based on a first pass estimate.
#' @param ebayesmultiplier Numeric. Multiplier for the widths of the empirical Bayes priors. Default is 2, as this appears to work better in practice.
#' @param ebayesFromFixed Logical. Whether to initialize empirical Bayes from any specifed fixed values Default is FALSE.
#' @param ebayesiter Integer. Number of iterations for empirical Bayes estimation. Default is 1.
#' @param estMeans Character vector. Which means to estimate from 'ability', 'A', 'B', 'C', 'D'. Default is c('ability', 'B', 'C', 'D'), with
#' discrimination means fixed.
#' @param priors Logical. Whether to use prior distributions. Default is TRUE.
#' @param integrateEachAbility Logical. Whether to integrate across each ability. Default is FALSE.
#' @param integrateEachAbilityFixedSE Logical. Whether to integrate each ability with fixed standard error. Default is FALSE.
#' @param mml Logical. Experimental and not working well. Whether to use marginal maximum likelihood estimation. Default is FALSE.
#' @param NintegratePoints Integer. Number of integration points for numerical integration. Default is 5.
#' @param normalise Logical. Whether to normalize the output estimates. Default is FALSE.
#' @param normaliseScale Numeric. Scale for normalization. Default is 1.
#' @param normaliseMean Numeric. Mean for normalization. Default is 0.
#' @param dropPerfectScores Logical. Whether to drop perfect scores from each subject and item before estimation. Default is TRUE.
#' @param trainingRows Integer vector. Rows of data to use for estimation of parameters. Default is all rows in \code{dat}.
#' @param init Initial values for the fitting algorithm. Default is NA.
#' @param tol Numeric. Tolerance for convergence. Default attempts to sensibly adjust for amount of data.
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return A list containing the fitted IRT model parameters and additional information about the fit.
#'
#'
#' @return
#' @export
#'
#' @examples
#' #Generate some data (here 2pl model
#' require(data.table)
#' dat <- simIRT(Nsubs = 50,Nitems = 100,Nscales = 1,
#'   logitCMean = -10,logitCSD = 0,AMean = 1,ASD = .3,
#'   BMean=0,BSD = .5,
#'   AbilityMean = 0,AbilitySD = 1)
#'
#' #fit using bigIRT
#' fit <- fitIRT(dat$dat,cores=2,score = 'score',id = 'id',
#'   scale = 'Scale',item = 'Item', pl=2)
#'
#'   print(fit$personPars)
#'   print(fit$itemPars)
fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',pl=1,
  personDat=NA, personPreds=character(),
  itemDat=NA,
  AitemPreds=character(),
  BitemPreds=character(),
  CitemPreds=character(),
  DitemPreds=character(),
  itemSpecificBetas=FALSE,
  betaScale=10,
  invspAMeandat=.542,invspASD=2,BMeandat=0,BSD=10, logitCMeandat=-4,logitCSD=2,
  logitDMeandat=4,logitDSD=2,
  AbilityMeandat=array(0,dim=c(length(unique(dat[[scale]])))),
  AbilitySD=array(10,dim=c(length(unique(dat[[scale]])))),
  AbilityCorr=diag(1,c(length(unique(dat[[scale]])))),
  AMeanSD=1,BMeanSD=BSD,logitCMeanSD=logitCSD,logitDMeanSD=logitDSD,
  AbilityMeanSD=array(1,dim=c(length(unique(dat[[scale]])))),
  iter=2000,cores=6,carefulfit=FALSE,
  ebayes=TRUE,ebayesmultiplier=2,ebayesFromFixed=FALSE,ebayesiter=1,
  estMeans=c('ability','B','C','D'),priors=TRUE,
  integrateEachAbility=FALSE, integrateEachAbilityFixedSE=FALSE,
  mml=FALSE,NintegratePoints=5,
  normalise=FALSE,normaliseScale=1,normaliseMean=0,
  dropPerfectScores=TRUE,trainingRows=1:nrow(dat),
  init=NA,tol=1e-8 * 10^(log(nrow(dat), 10)),...){

  sdat <-list() #initialize standata object
  basetol=tol

  itemPreds <- unique(c(AitemPreds,BitemPreds,CitemPreds,DitemPreds))

  if(mml) stop('MML currently disabled -- hopefully working one day!')

  #setup unlikely names to use in data.table calls to avoid overlap from user defined names
  idref. <- id; scaleref. <- scale; itemref. <- item;
  scoreref. <- score;
  personPredsref. <- personPreds;
  itemPredsref. <- itemPreds

  if(!'data.table' %in% class(dat)){  #drop unused columns from dat and set to data.table (copy if already data table)
    dat <- as.data.table(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
      itemPredsref.,personPredsref.),with=FALSE])
  } else {
    dat <- data.table::copy(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
      itemPredsref.,personPredsref.),with=FALSE])
  }


  #drop problem people and items
  if(dropPerfectScores)    dat <- dropPerfectScores(dat,scoreref. = scoreref.,itemref. = itemref.,idref. = idref.)

  #sort data by subject
  dat <- dat[order(get(idref.)),]

  #setup indices to map user specified categories to sequential integers for stan
  itemIndex <- data.table(original=as.character(dat[[itemref.]][!duplicated(dat[[itemref.]])]))
  scaleIndex <- data.table(original=as.character(dat[[scaleref.]][!duplicated(dat[[scaleref.]])]))
  idIndex <- data.table(original=as.character(dat[[idref.]][!duplicated(dat[[idref.]])]))


  #convert categories to sequential integers
  indx <- c(idref.,itemref.,scaleref.)
  for( ci in indx) set(dat,j = ci,value = as.integer(factor(dat[[ci]])))
  for( ci in indx) set(dat,j = ci,value = as.integer((dat[[ci]])))

  #include new sequential integers in index lists
  itemIndex$new <- dat[[itemref.]][!duplicated(dat[[itemref.]])]
  itemIndex$scale <- dat[[scaleref.]][!duplicated(dat[[itemref.]])]
  scaleIndex$new <-dat[[scaleref.]][!duplicated(dat[[scaleref.]])]
  idIndex$new <- dat[[idref.]][!duplicated(dat[[idref.]])]

  #order indices by new integer

  itemIndex=itemIndex[order(new),]
  scaleIndex=scaleIndex[order(new),]
  idIndex=idIndex[order(new),]


  #checks...
  if(any(is.na(dat))) stop('Missings found in data! Probably just remove the row/s...')
  if(!is.numeric(dat[[scoreref.]])) stop('Found a non-numeric score column!')
  if(normalise && any(!is.na(c(itemDat,personDat)))) warning(
    'With fixed values provided you might want to set normalise= FALSE',immediate. = TRUE)



  Nitems <- length(unique(dat[[itemref.]]))
  Nsubs=length(unique(dat[[idref.]]))
  Nscales=length(unique(dat[[scaleref.]]))

  #if getting priors from fixed pars, do this before dropping unnecessary items from itemSetup / AbilitySetup
  if(ebayesFromFixed){
    if(length(personDat)==1 && !is.na(personDat)){
      personDat <- as.data.table(personDat)
      sdat$dopriors <- 1L

      sdat$AbilityMeandat <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,mean,na.rm=TRUE))
      sdat$AbilitySD <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,sd,na.rm=TRUE))*ebayesmultiplier+1e-5 #maybe need to better account for multiple scales here, but not that important...
      sdat$AbilityCorr <- cor(personDat[,c(scaleIndex$original),with=FALSE],use='pairwise.complete.obs')
    }
    if(length(itemDat)==1 && !is.na(itemDat)){
      itemDat <- as.data.table(itemDat)
      sdat$dopriors <- 1L

      sdat$invspAMeandat <- mean(afunci(itemDat$A),na.rm=TRUE)
      sdat$invspASD <- sd(afunci(itemDat$A),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$BMeandat <- mean(itemDat$B,na.rm=TRUE)
      sdat$BSDx <- sd(itemDat$B,na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$logitCMeandat <- mean(logit(itemDat$C+1e-8),na.rm=TRUE)
      sdat$logitCSD <- sd(logit(itemDat$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$logitDMeandat <- mean(logit(itemDat$D+1e-8),na.rm=TRUE)
      sdat$logitDSD <- sd(logit(itemDat$D+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
    }
  }

  #setup item structure to define fixed / free pars
  itemSetup <- data.table(itemIndex,A=ifelse(pl>1,as.numeric(NA),1),B=as.numeric(NA),
    C=ifelse(pl>2,as.numeric(NA),0),D=ifelse(pl>3,as.numeric(NA),1))

  if(!all(is.na(itemDat))){ #if fixed item pars
    if(!'data.table' %in% class(itemDat)) itemDat <- as.data.table(itemDat)
    itemDat <- itemDat[get(itemref.) %in% itemSetup$original,]
    setupRows <- match(itemDat[[itemref.]],itemSetup$original)
    itemSetup[setupRows,c('A','B','C'):=itemDat[,c('A','B','C')]]
    if(pl<2) itemSetup[,'A':=1]
    if(pl<3) itemSetup[,'C':=0]
    if(pl<4) itemSetup[,'D':=1]
  }
  itemSetup[,paste0(c('A','B','C','D'),'data'):= .SD, .SDcols=c('A','B','C','D')] #create data columns
  setnafill(itemSetup,fill = -99,cols = paste0(c('A','B','C','D'),'data')) #and fill with arbitrary value to avoid NA in stan

  #setup person structure to define fixed / free pars
  AbilitySetup <- data.table(idIndex)
  AbilitySetup[,c(scaleIndex$original):=as.numeric(NA)]

  if(!all(is.na(personDat))){ #if fixed person pars
    if(!'data.table' %in% class(personDat)) personDat <- as.data.table(personDat)
    personDat <- personDat[get(idref.) %in% AbilitySetup$original,]
    setupRows <- match(personDat[[idref.]],AbilitySetup$original)
    AbilitySetup[setupRows,c(scaleIndex$original):=personDat[,c(scaleIndex$original),with=FALSE]]
  }
  AbilitySetup[,paste0(c(scaleIndex$original),'data'):= .SD, .SDcols=c(scaleIndex$original)] #create data columns
  setnafill(AbilitySetup,fill = -99,cols = paste0(c(scaleIndex$original),'data')) #and fill with arbitrary value to avoid NA in stan

  #which abilities are fixed
  fixedAbilityLogical <- AbilitySetup[order(new),c(scaleIndex$original),with=FALSE]
  fixedAbilityLogical<-fixedAbilityLogical[,lapply(.SD,function(x) as.integer(!is.na(x)))]

  #which parameters do the unfixed Ability matrix slots need to refer to
  Abilityparsindex <- matrix(cumsum(1-unlist(fixedAbilityLogical)),Nsubs,Nscales)
  Abilityparsindex[fixedAbilityLogical==1] <- 0

  #which scale is each Ability par for
  Abilityparsscaleindex <- c(col(Abilityparsindex)[Abilityparsindex>0])

  # #include short predictors:
  #
  # if(length(itemPredsref.)==0){
  #   itemPreds <- array(0,dim = c(Nitems,0))
  # } else{
  #   itemPreds <- dat[!duplicated(get(itemref.)),itemPredsref.,with=FALSE]
  #   itemPreds <- itemPreds[order(unique(dat[[itemref.]])),]
  # }
  #
  # if(length(personPredsref.)==0){
  #   personPreds <- array(0,dim = c(Nsubs,0))
  # } else{
  #   personPreds <- dat[!duplicated(get(idref.)),personPredsref.,with=FALSE]
  #   personPreds <- personPreds[order(unique(dat[[idref.]])),]
  # }

  #include long predictors:

  if(length(itemPredsref.)==0){
    itemPreds <- array(0,dim = c(nrow(dat),0))
  } else{
    itemPreds <- dat[,itemPredsref.,with=FALSE]
  }

  if(length(personPredsref.)==0){
    personPreds <- array(0,dim = c(nrow(dat),0))
  } else{
    personPreds <- dat[,personPredsref.,with=FALSE]
  }

  # sdat$NstatePreds <- length(statePreds)
  # sdat$statePreds <- matrix(0, nrow(dat), sdat$NstatePreds)
  # if(sdat$NstatePreds > 0) sdat$statePreds <- as.matrix(dat[,statePredsref.,with=FALSE])

  trainingLogical=array(rep(0L,nrow(dat)))
  trainingLogical[trainingRows] <- 1L

  sdat <- c(sdat,list(
    Nobs=nrow(dat),
    Nsubs=Nsubs,
    Nitems=Nitems,
    Nscales=Nscales,
    id=array(dat[[idref.]]),
    dopriors=as.integer(priors),
    outlierfix=0L,
    outlierscale=2,
    NfixedA=as.integer(sum(!is.na(itemSetup$A))),
    NfixedB=as.integer(sum(!is.na(itemSetup$B))),
    NfixedC=as.integer(sum(!is.na(itemSetup$C))),
    NfixedD=as.integer(sum(!is.na(itemSetup$D))),
    NfixedAbility=as.integer(sum(!is.na(unlist(AbilitySetup[,scaleIndex$original,with=FALSE])))),
    whichfixedA=array(as.integer(which(!is.na(itemSetup$A)))),
    whichfixedB=array(as.integer(which(!is.na(itemSetup$B)))),
    whichfixedC=array(as.integer(which(!is.na(itemSetup$C)))),
    whichfixedD=array(as.integer(which(!is.na(itemSetup$D)))),
    fixedAlog=array(as.integer((!is.na(itemSetup$A)))),
    fixedB=array(as.integer((!is.na(itemSetup$B)))),
    fixedClogit=array(as.integer((!is.na(itemSetup$C)))),
    fixedDlogit=array(as.integer((!is.na(itemSetup$D)))),
    whichnotfixedA=array(as.integer(which(is.na(itemSetup$A)))),
    whichnotfixedB=array(as.integer(which(is.na(itemSetup$B)))),
    whichnotfixedC=array(as.integer(which(is.na(itemSetup$C)))),
    whichnotfixedD=array(as.integer(which(is.na(itemSetup$D)))),
    Abilityparsindex=array(as.integer(unlist(Abilityparsindex)),c(Nsubs,Nscales)),
    fixedAbilityLogical=array(unlist(fixedAbilityLogical),c(Nsubs,Nscales)),
    Abilityparsscaleindex=array(as.integer(Abilityparsscaleindex)),
    start=1L,
    end=as.integer(nrow(dat)),
    trainingLogical=trainingLogical,
    score=array(as.integer(dat[[scoreref.]])),
    incorrect=array(as.integer(which(dat[[scoreref.]]==0))),
    item = array(dat[[itemref.]]),
    itemMean = dat$itemMean[!duplicated(dat[[itemref.]])],
    personMean = dat$personMean[!duplicated(dat[[idref.]])],
    scale=array(dat[[scaleref.]]),
    Adata=array(itemSetup$Adata),
    Bdata=array(itemSetup$Bdata),
    Cdata=array(itemSetup$Cdata),
    Ddata=array(itemSetup$Ddata),
    Abilitydata=matrix(unlist(AbilitySetup[,paste0(c(scaleIndex$original),'data'),with=FALSE]),Nsubs,Nscales),
    NitemPreds=ncol(itemPreds),
    NAitemPreds=length(AitemPreds),
    NBitemPreds=length(BitemPreds),
    NCitemPreds=length(CitemPreds),
    NDitemPreds=length(DitemPreds),
    AitemPreds=array(as.integer(which(colnames(itemPreds) %in% AitemPreds))),
    BitemPreds=array(as.integer(which(colnames(itemPreds) %in% BitemPreds))),
    CitemPreds=array(as.integer(which(colnames(itemPreds) %in% CitemPreds))),
    DitemPreds=array(as.integer(which(colnames(itemPreds) %in% DitemPreds))),
    itemPreds=array(unlist(itemPreds),dim(itemPreds)),
    NpersonPreds=ncol(personPreds),
    personPreds=(array(unlist(personPreds),dim(personPreds))),
    itemSpecificBetas=as.integer(itemSpecificBetas),
    betaScale=betaScale,
    invspAMeandat=invspAMeandat,
    invspASD=invspASD,
    BMeandat=BMeandat,
    BSDx=BSD,
    logitCMeandat=logitCMeandat,
    logitCSD=logitCSD,
    logitDMeandat=logitDMeandat,
    logitDSD=logitDSD,
    AbilityMeandat=AbilityMeandat,
    AbilitySD=array(AbilitySD),
    AbilityCorr=AbilityCorr,
    BMeanSD=BMeanSD,
    logitCMeanSD=logitCMeanSD,
    AbilityMeanSD=array(AbilityMeanSD),
    fixedAMean=as.integer(!'A' %in% estMeans || pl < 2),
    fixedBMean=as.integer(!'B' %in% estMeans),
    fixedCMean=as.integer(!'C' %in% estMeans || pl < 3),
    fixedDMean=as.integer(!'D' %in% estMeans || pl < 4),
    fixedAbilityMean=as.integer(!'Ability' %in% estMeans & !'ability' %in% estMeans),
    rowIndexPar=0L,
    originalRow=dat$`.originalRow`,
    doGenQuant=0L,
    integrateAbility=as.integer(integrateEachAbility),
    integrateAbilityFixedSE=as.integer(integrateEachAbilityFixedSE),
    NintegratePoints=as.integer(NintegratePoints),
    integrateWeights=array(statmod::gauss.quad.prob(n=NintegratePoints,dist='normal')$weights),
    integratePoints=array(statmod::gauss.quad.prob(n=NintegratePoints,dist='normal')$nodes)
  ))

  sdat$freeAref=array(as.integer(cumsum(1-as.numeric(sdat$fixedAlog))))
  sdat$freeBref=array(as.integer(cumsum(1-as.numeric(sdat$fixedB))))
  sdat$freeCref=array(as.integer(cumsum(1-as.numeric(sdat$fixedClogit))))
  sdat$freeDref=array(as.integer(cumsum(1-as.numeric(sdat$fixedDlogit))))


  JMLfit <- function(est, sdat, ebayes=FALSE, fit=NA,narrowPriors=FALSE,...){
    skipebayes <- FALSE

    message(paste0(ifelse(narrowPriors,'Narrow priors ', ifelse(ebayes,'Empirical Bayes ','Free estimation ')),'step...'))

    if(!all(is.na(fit))){
      sdat$Adata = fit$pars$A
      sdat$Bdata = fit$pars$B
      sdat$Cdata = fit$pars$C
      sdat$Ddata = fit$pars$D
      if(!mml) sdat$Abilitydata = fit$pars$Ability
      if(!ebayes) init = fit$optim$par
    }

    if(ebayes){
      sdat$dopriors <- 1L

      if(pl > 1 &&  length(fit$pars$invspApars) > 2){
        if(sdat$fixedAMean ==0) sdat$invspAMeandat <- mean(fit$pars$invspApars[!fit$pars$invspApars %in% boxplot.stats(fit$pars$invspApars)$out] ) #mean(afunci(fit$pars$A))
        sdat$invspASD <- sd(fit$pars$invspApars[!fit$pars$invspApars %in% boxplot.stats(fit$pars$invspApars)$out])*ebayesmultiplier+1e-5 #afunci(fit$pars$A)
      }

      if(length(fit$pars$Bpars) > 2){
        if(sdat$fixedBMean ==0) sdat$BMeandat <- mean(fit$pars$Bpars[!fit$pars$Bpars %in% boxplot.stats(fit$pars$Bpars)$out])
        sdat$BSDx <- sd(fit$pars$Bpars[!fit$pars$Bpars %in% boxplot.stats(fit$pars$Bpars)$out])*ebayesmultiplier+1e-5
      }

      if(pl > 2 && length(fit$pars$logitCpars) > 2){
        if(sdat$fixedCMean ==0) sdat$logitCMeandat <- mean(fit$pars$logitCpars[!fit$pars$logitCpars %in% boxplot.stats(fit$pars$logitCpars)$out]) #mean(cfunci(fit$pars$C+1e-8))
        sdat$logitCSD <- sd(fit$pars$logitCpars[!fit$pars$logitCpars %in% boxplot.stats(fit$pars$logitCpars)$out],na.rm=TRUE) * ebayesmultiplier+1e-5 #sd(cfunci(fit$pars$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
      }

      if(pl > 3 && length(fit$pars$logitDpars) > 2){
        if(sdat$fixedDMean ==0)  sdat$logitDMeandat <- mean(fit$pars$logitDpars[!fit$pars$logitDpars %in% boxplot.stats(fit$pars$logitDpars)$out]) #mean(cfunci(fit$pars$C+1e-8))
        sdat$logitDSD <- sd(fit$pars$logitDpars[!fit$pars$logitDpars %in% boxplot.stats(fit$pars$logitDpars)$out],na.rm=TRUE) * ebayesmultiplier+1e-5 #sd(cfunci(fit$pars$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
      }

      # sdat$fixedAMean <- 1L
      # init=NA


      if(!mml && length(fit$pars$Abilitypars) > 2){

        sdat$AbilityMeandat <- array(sapply(1:Nscales,function(x){
          mean(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x][
            !fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x] %in%
              boxplot.stats(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x])$out])
        }))

        sdat$AbilitySD <- array(sapply(1:Nscales,function(x){
          sd(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x][
            !fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x] %in%
              boxplot.stats(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x])$out],na.rm=TRUE)
        })) * ebayesmultiplier + 1e-5

        sdat$AbilityCorr= cor(fit$pars$Ability) #inconsistency here -- based on overall ability, rather than conditional ability as for sd / mean.
      }
      if(any(is.na(c(sdat$BSDx,sdat$invspASD,sdat$logitCSD,sdat$AbilitySD)))){
        skipebayes <- TRUE
        warning('NA when computing item sd parameters, ebayes set to FALSE')
      }
    }

    if(narrowPriors){
      sdat$dopriors <- 1L
      sdat$ASD <- .1
      # sdat$BSDx <- 1
      sdat$logitCSD <- .01
      sdat$logitDSD <- .01
      # sdat$AbilitySD <- array(1,sdat$Nscales)
    }
    if(!skipebayes) fit <- optimIRT(standata=sdat,Niter=iter,cores=cores,init = init,mml=mml,...)

    if(!mml){
      rownames(fit$pars$Ability)[idIndex$new] <- idIndex$original
      colnames(fit$pars$Ability)[scaleIndex$new] <- scaleIndex$original
    }

    rownames(fit$pars$A)[itemIndex$new] <- itemIndex$original
    rownames(fit$pars$B)[itemIndex$new]<- itemIndex$original
    rownames(fit$pars$C)[itemIndex$new] <- itemIndex$original
    rownames(fit$pars$D)[itemIndex$new] <- itemIndex$original

    return(list(fit=fit,sdat=sdat))
  }

  rm(dat)

  JMLseq <- list()
  if(carefulfit) JMLseq[[1]] <- list(est=c('A','B','C','D','Ability'),ebayes=FALSE,narrowPriors=TRUE)
  JMLseq[[length(JMLseq)+1]] <- list(est=c('A','B','C','D','Ability'),ebayes=FALSE,narrowPriors=FALSE)
  if(ebayes) JMLseq[[length(JMLseq)+1]] <- list(est=c('A','B','C',',D','Ability'),ebayes=TRUE,narrowPriors=FALSE)

  fit <- NA
  for(i in 1:length(JMLseq)){
    if(i < length(JMLseq)) tol= basetol*ifelse(JMLseq[[i]]$narrowPriors,100,10) else tol = basetol
    # if(JMLseq[[i]]$ebayes %in% 'TRUE') fitML <- fit #store fit before ebayes step
    fit <- JMLfit(est = JMLseq[[i]]$est,sdat = sdat, ebayes=JMLseq[[i]]$ebayes,
      fit = fit,
      narrowPriors = JMLseq[[i]]$narrowPriors,
      tol=tol,...)
    sdat <- fit$sdat
    fit <- fit$fit
  }
  # if(ebayes) fit$fitML <- fitML

  if(normalise && !mml){   #normalise pars
    for(i in 1:ncol(fit$pars$Ability)){
      selector <- rownames(fit$pars$B) %in% itemSetup$original[itemSetup$scale %in% i]

      normpars <- normaliseIRT(B = fit$pars$B[selector],
        Ability = fit$pars$Ability[,i],
        A=fit$pars$A[selector],normaliseScale = normaliseScale, normaliseMean = normaliseMean)

      fit$pars$Ability[,i] <- normpars$Ability

      fit$pars$B[selector]  <- normpars$B
      fit$pars$A[selector] <-  normpars$A
    }
  }
  if(normalise && mml) warning('Cannot normalize mml currently')

  ###compute some output details
  fit$itemPars <- data.frame(item=rownames(fit$pars$B),A=fit$pars$A,B=fit$pars$B,C=fit$pars$C,D=fit$pars$D)
  colnames(fit$itemPars)[1] <- item
  if(ncol(itemPreds)>0){
    colnames(fit$pars$itemPredsMean) <- colnames(itemPreds)
    fit$itemPars <- cbind(fit$itemPars, fit$pars$itemPredsMean)
  }

  if(!mml){
    fit$personPars <- data.frame(id=rownames(fit$pars$Ability),fit$pars$Ability)
    colnames(fit$personPars)[1] = id
    if(ncol(personPreds)>0){
      colnames(fit$pars$personPredsMean) <- colnames(personPreds)
      fit$personPars <- cbind(fit$personPars, fit$pars$personPredsMean)
    }
  }

  ###Covariate effect summary
  fit$covariateEffects <- list()
  if(fit$dat$NpersonPreds > 0){
    colnames(fit$pars$Abilitybeta) <- colnames(personPreds)
    fit$covariateEffects$Ability <- fit$pars$Abilitybeta
    fit$covariateEffects$AbilityStd <- fit$pars$Abilitybeta * apply(fit$dat$personPreds,2,sd) / sd(fit$pars$Ability)
  }
  if(fit$dat$NAitemPreds > 0 && pl > 1){
    dimnames(fit$pars$Abeta)[[2]] <- (AitemPreds)
    fit$covariateEffects$A <- fit$pars$Abeta
    fit$covariateEffects$AStd <- t(t(fit$pars$Abeta) * apply(fit$dat$itemPreds[,fit$dat$AitemPreds,drop=FALSE],2,sd) / sd(fit$pars$A))
  }
  if(fit$dat$NBitemPreds > 0){
    dimnames(fit$pars$Bbeta)[[2]] <- (BitemPreds)
    fit$covariateEffects$B<- fit$pars$Bbeta
    fit$covariateEffects$BStd <- t(t(fit$pars$Bbeta) * apply(fit$dat$itemPreds[,fit$dat$BitemPreds,drop=FALSE],2,sd) / sd(fit$pars$B))
  }
  if(fit$dat$NCitemPreds > 0 && pl > 2){
    dimnames(fit$pars$Cbeta)[[2]] <- (CitemPreds)
    fit$covariateEffects$C<- fit$pars$Cbeta
    fit$covariateEffects$CStd <- t(t(fit$pars$Cbeta) * apply(fit$dat$itemPreds[,fit$dat$CitemPreds,drop=FALSE],2,sd) / sd(fit$pars$C))
  }
  if(fit$dat$NDitemPreds > 0 && pl > 3){
    dimnames(fit$pars$Dbeta)[[2]] <- (DitemPreds)
    fit$covariateEffects$D<- fit$pars$Dbeta
    fit$covariateEffects$DStd <- t(t(fit$pars$Dbeta) * apply(fit$dat$itemPreds[,fit$dat$DitemPreds,drop=FALSE],2,sd) / sd(fit$pars$D))
  }


  return(fit)
}


