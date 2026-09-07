#' The 'bigIRT' package.
#'
#' @description
#' Tools for fitting binary item response theory (IRT) models for large and
#' sparse response data in long format. The main entry point is [fitIRT()],
#' which estimates 1PL-4PL models by marginal maximum likelihood with a Laplace
#' approximation to the integral over person abilities
#' (`marginalApprox = "laplace"`; the `"none"` default is the older penalised
#' joint MAP estimator, retained for backwards compatibility). It supports
#' multidimensional and confirmatory loading structures with an estimated latent
#' correlation, covariate effects on ability and on each item parameter, and
#' empirical-Bayes prior adaptation.
#'
#' Post-estimation, see [itemInformation()] and [testInformation()] for
#' information and standard errors, [reliability()], [itemFit()] and
#' [empiricalICC()] for measurement diagnostics, [checkConvergence()] and
#' [plotLaplaceDiagnostics()] for the optimiser, [heldoutMetrics()], [IRTic()]
#' and [looIRT()] for model comparison, [tidyIRT()] and [reportIRT()] for
#' reporting, [as.mirt()] for handing a fit to `mirt`, and [simIRT()] to
#' generate data in the shape [fitIRT()] expects.
#'
#' @name bigIRT-package
#' @aliases bigIRT
#' @useDynLib bigIRT, .registration = TRUE
#' @import methods
#' @import Rcpp data.table mize parallel
#' @importFrom rstan sampling
#' @importFrom stats median quantile var rnorm qnorm
#' @importFrom stats nobs logLik vcov
#' @importFrom stats AIC BIC cor cov2cor dnorm rbinom runif sd setNames
#' @importFrom graphics abline matplot
#' @importFrom utils head tail write.csv
#' @importFrom graphics par hist arrows points axis mtext legend
#' @keywords internal
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.26.1. https://mc-stan.org
#' @examples
#' # Generate simple 2PL data and fit a model.
#' require(data.table)
#' dat <- simIRT(Nsubs = 500,Nitems = 50,Nscales = 1,
#'   logitCMean = -10,logitCSD = 0,AMean = 1,ASD = .2,
#'   BMean=0,BSD = .7,
#'   AbilityMean = 0,AbilitySD = 1)
#'
#' fit <- fitIRT(dat$dat,cores=1,pl=2,dropPerfectScores=FALSE,
#'   marginalApprox='laplace')
#'
#' head(fit$itemPars)
#' head(fit$personPars)
"_PACKAGE"
