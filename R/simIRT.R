inv_logit <- function(x){
  exp(x)/(1 + exp(x))
}

dfunc <- function(x){
  inv_logit(x) * 0.5 + 0.5
}

inv_log1p_exp <- function(x){
  log(exp(x)-1)
}

# Simulate a person given an IRT scale covariance cholesky, covariates, and
# covariate effects.
if(F){

# sim ---------------------------------------------------------------------


  scaleNames = c('maths','english')
  sc <- t(chol(matrix(c(2,1,1,.8),2,2,dimnames = list(scaleNames,NULL))))

  N=100

  pcovs=data.table(age=seq(-4,4,length.out=N),
    ses=rnorm(N,0,2),
    height=rnorm(N,0,.2))

  persons=simPersons(N=N,
    mu=c(0,.3),
    scaleChol=sc,
    covs=pcovs,
    beta=matrix(c(.3,.2,0, .4, .3, 0),byrow=TRUE,2,3)
  )

  cor(persons)
  cov2cor(sc)
  plot(persons$age,persons$maths)


NperScale=1000
  icovs=data.table(grade=seq(-4,4,length.out=NperScale),
    clarity=rnorm(NperScale,0,2),
    specificity=rnorm(NperScale,0,.2))

  items <- simItems(NperScale = 1000,scaleNames = colnames(sc),invspAmu = inv_log1p_exp(c(3,3)),
    invspASD = c(0,.2),Bmu = c(0,0),BSD = c(3,4),logitCmu = c(-10,-10),logitCSD = c(0,0),
    covs = icovs,
    invspAbeta = matrix(c(.01,.2,0, 0,-.2,0),byrow=TRUE,2,3),
    Bbeta = matrix(c(1,0,.1, 0,-.2,0),byrow=TRUE,2,3),
    logitCbeta = matrix(c(.1,0,.1, 0,-.2,0),byrow=TRUE,2,3)
  )

  items$C <- 0 #2pl model

  items$Item <- paste0('i_',items$Scale,'_',1:nrow(items))

  itemmat=as.matrix(items[,c('A','B','C',colnames(icovs)),with=FALSE])
  cor(itemmat)
  cor(itemmat[1:NperScale,])
  cor(itemmat[(NperScale+1):(NperScale*2),])

  plot(icovs$clarity[1:NperScale],items$A[1:NperScale])


  #simulation loops

  Nassesments <- 50
  Nperassessment <- 50
  truepersons <- copy(persons)
  persons[,(scaleNames):= 0]
  setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
  write.csv(x = persons,file = 'persons.csv')
  write.csv(x = items,file = 'items.csv')
  a=Sys.time()

  rscript <- Sys.which("Rscript")
system2(rscript, paste0(" --vanilla ", getwd(),"/algoserver.R"),wait = FALSE) #start socket server

con <- socketConnection(host = "localhost", port = 8888, #connect to socket server
  blocking = FALSE, timeout = 30)

  for(ai in 1:Nassesments){
    person <- sample(1:nrow(persons),1) #random person selected
    scale <- sample(scaleNames,1)

    adat <- data.table(Item='xxxxxxxxx') #blank placeholder
    for(i in 1:Nperassessment){

      #select an item to present
      itemcode <-  selectItem(items[Scale %in% scale & !Item %in% adat$item,],
        ability = unlist(persons[person,scale,with=FALSE]),
        targetease = 0.1,samplesize = ifelse(ai > (Nassesments/2), 1000,1))
      # item <- (algoNewItem(person=person, scale=scale, targetease=.1)

      #assessment data
      rowdat <- cbind(data.table(id=person, trueability=unlist(truepersons[person,scale,with=FALSE]), AssessmentItemCount=i,
        item=itemcode, score=as.integer(NA), ability=0.0,items[Item %in% itemcode,]),pcovs[person,])
      if(i==1) adat <- rowdat else adat <- rbind(adat,rowdat)

      #get response from student
      adat[i,score:= simResponse(items[Item %in% itemcode,],unlist(truepersons[person,scale,with=FALSE])) ]

      #update ability estimate of student
      # save(adat,file='adat.rda')

      cmd <- paste0("setwd('",getwd(),"');
      load(file='adat.rda');
      # fit <- fitIRT(dat = adat,score='score',id = 'id',item = 'item',scale = 'Scale',pl = 2,cores=1,
      #   # AbilitySD = 5,
      #   priors = TRUE,ebayes = FALSE,
      #   # personPreds = colnames(pcovs), #need fixed betas here or else unidentified
      #   itemDat = adat,normalise = FALSE,dropPerfectScores = FALSE);
      #   save(fit,file='fit.rda');
      1+3
        "
      )

      cmd <- gsub('\\n','',cmd)
      cmd <- gsub(' ','',cmd)

      system.time(
      system(command = paste0(Sys.getenv("R_HOME"),'/bin/','Rscript --vanilla ',getwd(),'/algofit.R'),
        intern = T,show.output.on.console = TRUE)
      )





      persons[person,(scale):=fit$pars$Ability]
      adat[nrow(adat),ability:=fit$pars$Ability]

      # if( (adat$trueability[i]-adat$B[i]) > 3 && adat$score[i]==0) stop('too high')
      # if( (adat$trueability[i]-adat$B[i]) < -3 && adat$score[i]==1) stop('too low')
      # print(adat[nrow(adat),])

      # readline(prompt = '')
      # setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
      # write.csv(x = persons,file = 'persons.csv')


    }
    print(ai)
    print(Sys.time()-a)
    if(ai==1) record <- data.table(AssessmentID=ai,adat) else record <- rbind(record,data.table(AssessmentID=ai,adat))

  }
print(Sys.time()-a)

  require(ggplot2)
  ggplot(record,aes(y=ability, x=AssessmentItemCount,colour=factor(AssessmentID)))+
    geom_line()+
    theme_bw()+
    geom_hline(data = record,
          aes(yintercept=trueability,colour=factor(AssessmentID)),size=1,alpha=.5,linetype=2)

record[,Random:=ifelse(AssessmentID > (Nassesments/2),TRUE,FALSE)]
record[,RMSE:=sqrt(mean((trueability-ability)^2)),by=interaction(Random,AssessmentItemCount)]

ggplot(record,aes(y=RMSE,colour=Random,x=AssessmentItemCount))+geom_line()+theme_bw()


# not sim -----------------------------------------------------------------



}

# algoNewItem<-function(person, scale, targetease){
#   persons <- fread('persons.csv')
#   items <- fread('items.csv')
#   item <- selectItem(items, ability = unlist(persons[person,scale,with=FALSE]),targetease = targetease)
# }
#
# algoAbilityEst<-function(...){
#   fitIRT(...)
#   setwd("C:/Users/Driver/Seafile/mpib/bigIRT/testing/")
#   write.csv(x = persons,file = 'persons.csv')
# }

simResponse <- function(items, ability,score=TRUE){
  D <- if("D" %in% names(items)) items$D else 1
  p=items$C +
    (D-items$C) / (1+exp(
      -items$A * #discrimination of item=
        (ability - #Ability
            items$B)))
  if(score) return(rbinom(n = length(p),size = 1,prob=p)) else return(p)

}

simPersons <- function(N, mu, scaleChol, covs=numeric(), beta=numeric){
  d=sqrt(length(scaleChol))
  y <- (matrix(rnorm(d*N),N,d) %*% t(scaleChol) + mu)
  colnames(y) <- colnames(scaleChol)
  if(length(covs) > 0)      y <- y + as.matrix(covs) %*% t(beta)
  return(data.table(y,covs))
}


simItems <- function(NperScale, scaleNames, invspAmu, invspASD, Bmu, BSD, logitCmu, logitCSD,
  logitDmu = 20, logitDSD = 0, covs=numeric(), invspAbeta, Bbeta, logitCbeta,
  logitDbeta = NULL){

  items <- lapply(1:length(scaleNames),function(i){
    invspA=rnorm(NperScale,invspAmu[i],invspASD[i])
    B = rnorm(NperScale,Bmu[i],BSD[i])
    logitC=rnorm(NperScale,logitCmu[i],logitCSD[i])
    logitD=rnorm(NperScale,logitDmu[i],logitDSD[i])

    if(length(covs) > 0){
      invspA <- c(invspA + as.matrix(covs) %*% t(invspAbeta[i,,drop=FALSE]))
      B <- c(B + as.matrix(covs) %*% t(Bbeta[i,,drop=FALSE]))
      logitC <- c(logitC + as.matrix(covs) %*% t(logitCbeta[i,,drop=FALSE]))
      if(!is.null(logitDbeta)) logitD <- c(logitD + as.matrix(covs) %*% t(logitDbeta[i,,drop=FALSE]))
    }

    o=data.table(check.names = FALSE,Scale = scaleNames[i],
      A=log1p(exp(invspA)),
      B=B,
      C=inv_logit(logitC),
      D=dfunc(logitD)
    )
    cbind(o,covs)
  })

  return(do.call(rbind,items))
}

#' Evaluate IRT Item Response Curves
#'
#' Evaluates 1D or multidimensional 4PL response curves under the `bigIRT`
#' parameterization
#' \deqn{\eta = \theta A^\top - B.}
#'
#' @param A Numeric item-by-factor loading matrix, or a numeric vector for a
#'   single-factor model.
#' @param B Numeric vector of item intercept/difficulty parameters.
#' @param C Numeric vector of lower asymptotes. Defaults to 0.
#' @param D Numeric vector of upper asymptotes. Defaults to 1.
#' @param theta Numeric matrix of latent trait values (rows are evaluation
#'   points, columns are factors), or a numeric vector for a single-factor
#'   model.
#' @param plot Logical; if `TRUE`, produce a 1D plot.
#' @param rescale Logical; if `TRUE`, standardize `theta` before evaluation.
#' @param add Logical; when plotting 1D curves, add to an existing plot.
#' @param item Optional item index/indices to evaluate.
#' @param ... Additional graphical arguments passed to plotting functions.
#'
#' @return A numeric vector for a single item or a numeric matrix with rows
#'   corresponding to `theta` points and columns to items.
#' @export
IRTcurve <- function(A, B, C = 0, D = 1, theta = seq(-3, 3, .01),
  plot = TRUE, rescale = FALSE, add = FALSE, item = NULL, ...){

  theta_in <- theta
  theta <- as.matrix(theta)
  if(is.null(dim(theta_in))) theta <- matrix(as.numeric(theta_in), ncol = 1)

  A <- as.matrix(A)
  if(is.null(dim(A))) A <- matrix(as.numeric(A), nrow = 1)
  storage.mode(A) <- "double"
  B <- as.numeric(B)
  C <- rep_len(as.numeric(C), length(B))
  D <- rep_len(as.numeric(D), length(B))

  if(!is.null(item)){
    A <- A[item, , drop = FALSE]
    B <- B[item]
    C <- C[item]
    D <- D[item]
  }

  if(ncol(theta) != ncol(A)){
    stop("theta and A must have the same number of dimensions.")
  }
  if(length(B) != nrow(A)) stop("B must have one entry per item/row in A.")

  if(rescale){
    theta <- scale(theta)
  }

  eta <- theta %*% t(A)
  eta <- sweep(eta, 2, B, "-")
  p <- sweep(matrix(inv_logit(eta), nrow = nrow(theta)), 2, D - C, "*")
  p <- sweep(p, 2, C, "+")

  if(ncol(p) == 1L){
    p <- as.numeric(p[, 1L])
  }

  if(plot){
    if(ncol(as.matrix(theta)) != 1L) stop("plot=TRUE currently requires one-dimensional theta.")
    theta_plot <- as.numeric(theta[, 1L])
    if(is.matrix(p) && ncol(p) > 1L){
      matplot(theta_plot, p, type = "l", ylim = c(0, 1), add = add, ...)
    } else {
      if(!add) {
        plot(theta_plot, p, ylim = c(0, 1), type = "l", ...)
      } else {
        points(theta_plot, p, type = "l", ...)
      }
    }
  }

  if(!plot) return(p)
}


#' Simulate IRT data
#'
#' @param Nsubs Integer. Number of subjects.
#' @param Nitems Integer. Number of items per scale.
#' @param Nscales Integer. Number of scales.
#' @param NitemsAnswered Integer (or length-`Nscales` integer vector). Number
#'   of items answered per person per scale. Values below `Nitems` generate
#'   sparse-response datasets.
#' @param ASD Numeric. SD of simulated item discrimination values (`A`)
#' @param AMean Numeric. Mean of simulated item discrimination values (`A`).
#' @param BSD Numeric. SD of simulated item difficulty values (`B`).
#' @param BMean Numeric. Mean of simulated item difficulty values (`B`).
#' @param logitCSD Numeric. SD of simulated guessing values on logit scale.
#' @param logitCMean Numeric. Mean of simulated guessing values on logit scale.
#' @param logitDSD Numeric. SD of simulated upper asymptote values on logit scale.
#' @param logitDMean Numeric. Mean of simulated upper asymptote values on logit scale.
#' @param AbilitySD Numeric. SD of simulated person abilities.
#' @param AbilityMean Numeric. Mean of simulated person abilities.
#' @param itemPreds Optional matrix/data frame of item-level predictors. Rows
#'   should align with items.
#' @param AitemPredEffects Optional numeric matrix/vector of effects of
#'   `itemPreds` on item discrimination values.
#' @param BitemPredEffects Optional numeric matrix/vector of effects of
#'   `itemPreds` on item difficulty values.
#' @param logitCitemPredEffects Optional numeric matrix/vector of effects of
#'   `itemPreds` on guessing values (logit scale).
#' @param logitDitemPredEffects Optional numeric matrix/vector of effects of
#'   `itemPreds` on upper asymptote values (logit scale).
#' @param personPreds Optional matrix/data frame of person-level predictors.
#'   Rows should align with subjects.
#' @param AbilityPredEffects Optional matrix of effects of `personPreds` on
#'   person ability by scale.
#' @param mirt Logical. If `TRUE`, simulate a multidimensional response process
#'   where each item can load on multiple latent factors. In this mode, `Nitems`
#'   is the total number of items (not items-per-scale).
#' @param loadingSparsity Numeric in `[0,1]`. Probability that a non-primary
#'   loading is active when `mirt=TRUE` and `loadings` is not supplied.
#' @param primaryScale Optional integer vector of length `Nitems` giving each
#'   item's primary scale index (`1..Nscales`) when `mirt=TRUE`.
#' @param loadings Optional numeric `Nitems x Nscales` matrix of true item
#'   loadings used directly when `mirt=TRUE`. If omitted, loadings are sampled.
#' @param crossLoadingSD Numeric. SD of sampled non-primary loading values when
#'   `mirt=TRUE` and `loadings` is not supplied.
#' @param returnRowLoadings Logical. If `TRUE` and `mirt=TRUE`, include per-row
#'   loading columns (`A_1`, ..., `A_K`) in `dat`.
#'
#' @return A list with:
#' \describe{
#'   \item{Ability}{Matrix of true person abilities (`Nsubs x Nscales`).}
#'   \item{A}{Matrix of true item discriminations (`Nitems x Nscales`).}
#'   \item{B}{Matrix/vector of true item difficulties. Matrix in legacy mode,
#'   vector in `mirt=TRUE` mode.}
#'   \item{C}{Matrix/vector of true item guessing parameters. Matrix in legacy
#'   mode, vector in `mirt=TRUE` mode.}
#'   \item{D}{Matrix/vector of true item upper asymptote parameters. Matrix in legacy
#'   mode, vector in `mirt=TRUE` mode.}
#'   \item{dat}{Long-format response data as a `data.table`.}
#' }
#' @export
#'
#' @examples
#' sim <- simIRT(Nsubs = 100, Nitems = 40, Nscales = 1, ASD = .2, BSD = .8)
#' head(sim$dat)
simIRT <- function(Nsubs=100,Nitems=200,Nscales=1, NitemsAnswered=Nitems,
  ASD=0,AMean=1,BSD=1,BMean=0,logitCSD=1,logitCMean=-2,logitDSD=0,logitDMean=20,AbilitySD=1,AbilityMean=0,
  AbilityCorr=diag(1, Nscales),
  itemPreds=NA, AitemPredEffects=NA,BitemPredEffects=NA,logitCitemPredEffects=NA,logitDitemPredEffects=NA,
  personPreds=NA, AbilityPredEffects=NA,
  mirt=FALSE, loadingSparsity=0.3, primaryScale=NA, loadings=NA,
  crossLoadingSD=0.15, returnRowLoadings=TRUE){

  if(length(NitemsAnswered) == 1){
    NitemsAnswered <- rep(NitemsAnswered,Nscales)
  }
  if(length(NitemsAnswered) != Nscales){
    stop("NitemsAnswered must have length 1 or Nscales.")
  }
  if(any(!is.finite(NitemsAnswered)) || any(NitemsAnswered < 1) ||
     any(NitemsAnswered > Nitems) || any(NitemsAnswered %% 1 != 0)){
    stop("NitemsAnswered must be integer values between 1 and Nitems.")
  }

  AbilitySD <- rep_len(as.numeric(AbilitySD), Nscales)
  AbilityMean <- rep_len(as.numeric(AbilityMean), Nscales)
  AbilityCorr <- as.matrix(AbilityCorr)
  if(!all(dim(AbilityCorr) == c(Nscales, Nscales))){
    stop("AbilityCorr must be an Nscales x Nscales matrix.")
  }
  if(any(!is.finite(AbilityCorr))) stop("AbilityCorr must contain finite values.")
  AbilityCorr <- 0.5 * (AbilityCorr + t(AbilityCorr))
  if(any(abs(diag(AbilityCorr) - 1) > 1e-8)) stop("AbilityCorr must have unit diagonal.")
  chol_corr <- try(chol(AbilityCorr), silent = TRUE)
  if(inherits(chol_corr, "try-error")) stop("AbilityCorr must be positive definite.")
  Ability <- matrix(rnorm(Nsubs * Nscales), Nsubs, Nscales) %*% chol_corr
  Ability <- sweep(Ability, 2, AbilitySD, "*")
  Ability <- sweep(Ability, 2, AbilityMean, "+")

  if(isTRUE(mirt)){
    if(Nscales < 2) stop("mirt=TRUE requires Nscales >= 2.")
    if(!is.numeric(loadingSparsity) || length(loadingSparsity) != 1 || loadingSparsity < 0 || loadingSparsity > 1){
      stop("loadingSparsity must be a scalar in [0, 1].")
    }

    if(length(primaryScale) == 1 && is.na(primaryScale[1])){
      primaryScale <- rep(seq_len(Nscales), length.out = Nitems)
    } else {
      if(length(primaryScale) != Nitems) stop("primaryScale must have length Nitems.")
      primaryScale <- as.integer(primaryScale)
      if(any(primaryScale < 1 | primaryScale > Nscales)) stop("primaryScale entries must be in 1..Nscales.")
    }

    if(length(loadings) == 1 && is.na(loadings[1])){
      A <- matrix(0, nrow = Nitems, ncol = Nscales)
      for(i in seq_len(Nitems)){
        pi <- primaryScale[i]
        A[i, pi] <- pmax(0.05, rnorm(1, AMean, ASD))
        for(si in seq_len(Nscales)){
          if(si == pi) next
          if(runif(1) <= loadingSparsity){
            A[i, si] <- rnorm(1, 0, crossLoadingSD)
          }
        }
      }
    } else {
      A <- as.matrix(loadings)
      storage.mode(A) <- "double"
      if(!all(dim(A) == c(Nitems, Nscales))){
        stop("`loadings` must be an Nitems x Nscales matrix when mirt=TRUE.")
      }
      if(any(!is.finite(A))) stop("`loadings` must be finite numeric values when provided.")
    }
    colnames(A) <- paste0("S", seq_len(Nscales))
    rownames(A) <- as.character(seq_len(Nitems))

    B <- rnorm(Nitems, BMean, BSD)
    logitC <- rnorm(Nitems, logitCMean, logitCSD)
    logitD <- rnorm(Nitems, logitDMean, logitDSD)

    if(!all(is.na(itemPreds))){
      if(nrow(as.matrix(itemPreds)) != Nitems){
        stop("When mirt=TRUE, itemPreds must have Nitems rows.")
      }
      itemPredMat <- as.matrix(itemPreds)
      if(all(!is.na(AitemPredEffects))){
        eff <- as.numeric(itemPredMat %*% as.matrix(AitemPredEffects))
        A[cbind(seq_len(Nitems), primaryScale)] <- A[cbind(seq_len(Nitems), primaryScale)] + eff
      }
      if(all(!is.na(BitemPredEffects))) B <- B + as.numeric(itemPredMat %*% as.matrix(BitemPredEffects))
      if(all(!is.na(logitCitemPredEffects))) logitC <- logitC + as.numeric(itemPredMat %*% as.matrix(logitCitemPredEffects))
      if(all(!is.na(logitDitemPredEffects))) logitD <- logitD + as.numeric(itemPredMat %*% as.matrix(logitDitemPredEffects))
    }

    if(!all(is.na(personPreds))){
      if(nrow(as.matrix(personPreds)) != Nsubs){
        stop("When mirt=TRUE, personPreds must have Nsubs rows.")
      }
      if(all(!is.na(AbilityPredEffects))){
        for(i in 1:Nscales){
          Ability[,i] <- Ability[,i] + apply(personPreds,1,function(x) sum(AbilityPredEffects[i,,drop=FALSE] %*% x))
        }
      }
    }

    C <- inv_logit(logitC)
    D <- dfunc(logitD)
    dat <- data.table(expand.grid(id = seq_len(Nsubs), Item = seq_len(Nitems)))
    dat[, Scale := primaryScale[Item]]
    dat[, Ability := Ability[cbind(id, Scale)]]
    dat[, A := A[cbind(Item, Scale)]]
    dat[, B := B[Item]]
    dat[, C := C[Item]]
    dat[, D := D[Item]]

    eta <- rowSums(A[dat$Item,,drop=FALSE] * Ability[dat$id,,drop=FALSE]) - B[dat$Item]
    dat[, p := C + (D - C) / (1 + exp(-eta))]
    dat[, pcorrect := p]
    dat[, score := rbinom(.N, size = 1, prob = p)]

    keepKeys <- rbindlist(lapply(seq_len(Nscales), function(si){
      n_ans <- NitemsAnswered[si]
      item_pool <- which(primaryScale == si)
      if(!length(item_pool)) return(NULL)
      if(n_ans >= length(item_pool)){
        data.table(id = rep(seq_len(Nsubs), each = length(item_pool)),
          Item = rep(item_pool, times = Nsubs))
      } else {
        data.table(id = rep(seq_len(Nsubs), each = n_ans),
          Item = unlist(lapply(seq_len(Nsubs), function(.i) sample(item_pool, size = n_ans))))
      }
    }), use.names = TRUE, fill = TRUE)
    dat <- dat[keepKeys, on = .(id, Item), nomatch = 0]
    dat <- dat[order(id, Item)]
    if(isTRUE(returnRowLoadings)){
      rowLoads <- as.data.table(A[dat$Item,,drop=FALSE])
      setnames(rowLoads, paste0("A_", seq_len(Nscales)))
      dat <- cbind(dat, rowLoads)
    }

    if(!all(is.na(itemPreds))){
      itemPredDt <- as.data.table(itemPreds)
      itemPredDt[, Item := seq_len(.N)]
      dat <- merge.data.table(dat, itemPredDt, by = "Item")
    }
    if(!all(is.na(personPreds))){
      personPredDt <- as.data.table(personPreds)
      personPredDt[, id := seq_len(.N)]
      dat <- merge.data.table(dat, personPredDt, by = "id")
    }
    out <- list(
      Ability = Ability,
      A = A,
      B = B,
      C = C,
      D = D,
      primaryScale = primaryScale,
      dat = as.data.table(dat)
    )
    class(out) <- c("bigIRT_simIRT", "list")
    return(out)
  }

  A <- matrix(rnorm(Nitems*Nscales,AMean,ASD),Nitems)
  B <- matrix(rnorm(Nitems*Nscales,BMean,BSD),Nitems)
  logitC <- matrix(rnorm(Nitems*Nscales,logitCMean,logitCSD),Nitems)
  logitD <- matrix(rnorm(Nitems*Nscales,logitDMean,logitDSD),Nitems)

  if(!all(is.na(itemPreds))){
    if(all(!is.na(AitemPredEffects))) A <- A + apply(itemPreds,1,function(x) sum(AitemPredEffects * x))
    if(all(!is.na(BitemPredEffects))) B <- B + apply(itemPreds,1,function(x) sum(BitemPredEffects * x))
    if(all(!is.na(logitCitemPredEffects))) logitC <- logitC + apply(itemPreds,1,function(x) sum(logitCitemPredEffects * x))
    if(all(!is.na(logitDitemPredEffects))) logitD <- logitD + apply(itemPreds,1,function(x) sum(logitDitemPredEffects * x))
  }

  if(!all(is.na(personPreds))){
    if(all(!is.na(AbilityPredEffects))) {
      for(i in 1:Nscales){
        Ability[,i] <- Ability[,i] + apply(personPreds,1,function(x) sum(AbilityPredEffects[i,,drop=FALSE] %*% x))
      }
    }
  }




  C <- inv_logit(logitC)
  D <- dfunc(logitD)


  for(si in 1:Nscales){

    simdat <- data.frame(id=rep(1:Nsubs,each=Nitems),
      Item=rep( ((si-1)*Nitems+1):(si*Nitems),times=Nsubs),
      Scale=si,
      Ability=rep(Ability[,si],each=Nitems),
      A = rep(A[,si],times=Nsubs),
      B=rep(B[,si],times=Nsubs),
      C=rep(C[,si],times=Nsubs),
      D=rep(D[,si],times=Nsubs),
      pcorrect=0,score=0)

    simdat$p= C[simdat$Item-(si-1)*Nitems,si]+
      (D[simdat$Item-(si-1)*Nitems,si]-C[simdat$Item-(si-1)*Nitems,si]) / (1+exp(
        -A[simdat$Item-(si-1)*Nitems,si] * #discrimination of item=
          (Ability[simdat$id,si] - #Ability
              B[simdat$Item-(si-1)*Nitems,si]) #item difficulty
      ))



    simdat$score <- rbinom(n = nrow(simdat),size = 1,
      prob = simdat$p )

    if(NitemsAnswered[si] < Nitems){
      simdat <- data.table(simdat)[,
        .SD[sample(.N,size=NitemsAnswered[si])],
        by=id
      ]
    }

    if(si==1) dat <- simdat else dat <- rbind(dat,simdat)
  }

  #
  dat <- as.data.table(dat)
  if(!all(is.na(itemPreds))) dat <- merge.data.table((dat),data.table(Item=1:Nitems,itemPreds),by=c('Item'))
  if(!all(is.na(personPreds))) dat <- merge.data.table((dat),data.table(id=1:Nsubs,personPreds),by=c('id'))

  out <- list(Ability=Ability,A=A,B=B, C=C,D=D,dat=dat)
  class(out) <- c("bigIRT_simIRT", "list")
  return(out)
}
