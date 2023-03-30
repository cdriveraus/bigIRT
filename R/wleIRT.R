optimBisection <- function(fn, maxiter=200,tol=.001,mn=-5, mx=5,...){

  startingPhaseCount <- 1 #start with specified range (ideally sensible but narrow), expand as needed

  while(startingPhaseCount < 10){
    fnmn <- fn(mn,...) #function values at initial minimum
    fnmx <- fn(mx,...) #function values at initial maximum

    if(sign(fnmn)!=sign(fnmx)) startingPhaseCount <- 999 #set to ensure progress beyond starting phase
    else {
      mn <- mn - (mx-mn) #expand minimum boundary
      mx <- mx + (mx-mn) #expand maximum boundary
    }
  }

  if(sign(fnmn)==sign(fnmx)){ #if same sign after all expansions there is a problem, exit function returning mid value
    return( (mx+mn)/2)
  }

  iter=0 #iteration counter
  step=  (mx-mn)/2 #initial step size

  while(abs(step) > tol && iter < maxiter){ #while step size is greater than tolerance and iteration count is below limit
    iter <- iter + 1 #update iteration counter
    fnnew <- fn(mn+step,...) #compute new function result (probability)
    if(sign(fnnew) == sign(fnmn)) mn <- mn+step else mx <- mn+step #if midpoint is same sign as min, set midpoint as new min, else set as new max
    step=  (mx-mn)/2 #next step is halfway between min and max
  } #finish iteration loop

  return(mn+step) #return mid point of final max and min
}


wleGradComplete <- function(theta, A, B, C, score){
  expRes <- exp(-(A * (theta - B)))
  expRes1 <- expRes + 1
  scoreProb <- C + (1-C) / expRes1 #probability of each observed response conditional on provided parameters
  scoreProbGrad <-  A * (1 - C) * expRes / ((1 + expRes)^2)#first derivative of probability of each observed response conditional on provided parameters
  scoreProbGrad2 <- -(expRes * A^2 * (1 - 2 * (expRes/expRes1) ) * (1 - C) / expRes1^2)#second derivative of probability of each observed response conditional on provided parameters

  I <- sum( (scoreProbGrad^2) / (scoreProb * (1-scoreProb))+1e-100)

  J <- sum( (scoreProbGrad * scoreProbGrad2 / (scoreProb * (1 - scoreProb)))+1e-100)

  loglikgrad <- sum(A * (1 - C) * (2 * score - 1) * expRes/(((1 - scoreProb) *
      (1 - score) + score * scoreProb) * (1 + expRes)^2)+1e-100)

  return(J / (2*I) + loglikgrad)
}


wleSEnumeric <- function(theta, A, B, C, score){
  sqrt(abs(1/ (
    (wleGradComplete(theta - .01, A = A, B=B, C=C, score=score) -
        wleGradComplete(theta + .01,  A = A, B=B, C=C, score=score)) / .02)))
}


#' Compute Weighted Likelihood Estimate (WLE) and Standard Error (SE)
#'
#' Computes the WLE and SE for each subject and scale in a bigIRT model.
#'
#' @param fit A bigIRT model fit object.
#'
#' @return A list containing two matrices. The first matrix contains the WLEs for each subject and scale. The second matrix contains the SEs for each subject and scale.
#'
#' @export
#'
#' @examples
#' # Fit a bigIRT model
#' #fit <- bigIRT(data, itempars)
#'
#' # Compute WLE and SE
#' #wleIRT(fit)
wleIRT <- function(fit){

  wle <- matrix(NA,nrow=fit$dat$Nsubs,ncol=fit$dat$Nscales)
  wleSE <- matrix(NA,nrow=fit$dat$Nsubs,ncol=fit$dat$Nscales)

  for(i in 1:nrow(wle)){
    for(j in 1:ncol(wle)){

      wle[i,j] <-  optimBisection(
          fn = wleGradComplete,
          A = fit$itemPars$A[fit$dat$item[fit$dat$id %in% i]],
          B = fit$itemPars$B[fit$dat$item[fit$dat$id %in% i]],
          C = fit$itemPars$C[fit$dat$item[fit$dat$id %in% i]],
          score=fit$dat$score[fit$dat$id %in% i])

      wleSE[i,j] <- wleSEnumeric(
        theta = wle[i,j],
        A = fit$itemPars$A[fit$dat$item[fit$dat$id %in% i]],
        B = fit$itemPars$B[fit$dat$item[fit$dat$id %in% i]],
        C = fit$itemPars$C[fit$dat$item[fit$dat$id %in% i]],
        score=fit$dat$score[fit$dat$id %in% i])
    }
  }
  return(list(wle=wle,wleSE=wleSE))
}



