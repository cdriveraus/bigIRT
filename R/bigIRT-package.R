#' The 'bigIRT' package.
#'
#' @description
#' Tools for fitting binary item response theory (IRT) models for large and
#' sparse response data. The main entry point is [fitIRT()], which supports
#' 1PL-4PL models, optional covariate effects on item and person parameters,
#' empirical-Bayes style prior adaptation, and utility functions for simulation
#' and post-estimation summaries.
#'
#' @name bigIRT-package
#' @aliases bigIRT
#' @useDynLib bigIRT, .registration = TRUE
#' @import methods
#' @import Rcpp data.table mize parallel
#' @importFrom rstan sampling
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
#' fit <- fitIRT(dat$dat,cores=1,pl=2,dropPerfectScores=FALSE)
#'
#' head(fit$itemPars)
#' head(fit$personPars)
"_PACKAGE"
