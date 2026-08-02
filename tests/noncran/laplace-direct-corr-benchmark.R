.libPaths(unique(c(
  .libPaths(),
  "C:/Users/Driver/AppData/Local/R/win-library/4.5",
  "C:/Program Files/R/R-4.5.2/library"
)))

suppressPackageStartupMessages({
  library(bigIRT)
  library(data.table)
})

fit_case <- function(dat, estimate_corr){
  started <- proc.time()[3]
  fit <- fitIRT(
    dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = TRUE,
    marginalApprox = "laplace_direct",
    estimateAbilityCorr = estimate_corr,
    laplaceOuterIter = 80,
    laplaceGradTol = 1e-2,
    laplaceDiagnostics = TRUE,
    verbose = 0,
    plot = FALSE
  )
  elapsed <- proc.time()[3] - started
  list(fit = fit, elapsed = elapsed)
}

set.seed(20260323)
Nsubs <- 700
Nitems <- 36
Nscales <- 3

sim <- simIRT(
  Nsubs = Nsubs,
  Nitems = Nitems,
  Nscales = Nscales,
  NitemsAnswered = c(10, 10, 10),
  mirt = TRUE,
  AMean = 1.0,
  ASD = 0.20,
  loadingSparsity = 0.25,
  crossLoadingSD = 0.20,
  BMean = 0,
  BSD = 0.9,
  logitCMean = -20,
  logitCSD = 0,
  AbilityCorr = matrix(c(
    1.0, 0.35, 0.15,
    0.35, 1.0, 0.25,
    0.15, 0.25, 1.0
  ), 3, 3, byrow = TRUE)
)

truth_corr <- cor(sim$Ability)
fixed_fit <- fit_case(sim$dat, FALSE)
est_fit <- fit_case(sim$dat, TRUE)

bench <- rbindlist(list(
  data.table(
    method = "laplace_direct_fixed_corr",
    elapsed_sec = fixed_fit$elapsed,
    final_grad = fixed_fit$fit$laplaceStatus$last_item_grad_norm,
    converged = isTRUE(fixed_fit$fit$laplaceStatus$converged),
    reason = as.character(fixed_fit$fit$laplaceStatus$reason),
    ability_rmse = sqrt(mean((as.matrix(fixed_fit$fit$pars$Ability) - sim$Ability)^2)),
    loading_rmse = sqrt(mean((as.matrix(fixed_fit$fit$pars$A) - sim$A)^2)),
    corr_rmse = sqrt(mean((fixed_fit$fit$abilityPrior$corr - truth_corr)^2))
  ),
  data.table(
    method = "laplace_direct_est_corr",
    elapsed_sec = est_fit$elapsed,
    final_grad = est_fit$fit$laplaceStatus$last_item_grad_norm,
    converged = isTRUE(est_fit$fit$laplaceStatus$converged),
    reason = as.character(est_fit$fit$laplaceStatus$reason),
    ability_rmse = sqrt(mean((as.matrix(est_fit$fit$pars$Ability) - sim$Ability)^2)),
    loading_rmse = sqrt(mean((as.matrix(est_fit$fit$pars$A) - sim$A)^2)),
    corr_rmse = sqrt(mean((est_fit$fit$abilityPrior$corr - truth_corr)^2))
  )
))

print(bench)
