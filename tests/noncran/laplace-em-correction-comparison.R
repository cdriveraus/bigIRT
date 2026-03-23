args <- commandArgs(trailingOnly = TRUE)

.libPaths(unique(c(
  .libPaths(),
  "C:/Users/Driver/AppData/Local/R/win-library/4.5",
  "C:/Program Files/R/R-4.5.2/library"
)))

suppressPackageStartupMessages({
  library(bigIRT)
  library(data.table)
})

rmse <- function(x, y) sqrt(mean((as.numeric(x) - as.numeric(y))^2, na.rm = TRUE))

run_case <- function(name, seed, ..., normalise = FALSE){
  set.seed(seed)
  sim <- simIRT(..., normalise = normalise)
  dat <- sim$dat

  fit_once <- function(correction){
    t0 <- proc.time()[3]
    fit <- fitIRT(
      dat,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      dropPerfectScores = FALSE,
      normalise = normalise,
      marginalApprox = "laplace_em",
      laplaceItemCorrection = correction,
      laplaceOuterIter = 12,
      noptimsteps = 10,
      laplaceDiagnostics = TRUE,
      verbose = 0,
      plot = FALSE
    )
    elapsed <- proc.time()[3] - t0
    dx <- as.data.table(fit$laplaceDiagnostics)
    data.table(
      scenario = name,
      correction = correction,
      Nobs = nrow(dat),
      outer_iter = nrow(dx),
      converged = isTRUE(fit$laplaceStatus$converged),
      reason = fit$laplaceStatus$reason,
      elapsed_sec = elapsed,
      item_step_mean_sec = mean(dx$itemStepSec),
      person_step_mean_sec = mean(dx$personStepSec),
      item_target_mean = mean(dx$itemTargetEvals),
      final_grad = tail(dx$itemGradNorm, 1),
      final_objective = tail(dx$objective, 1),
      A_rmse = rmse(fit$pars$A, sim$A),
      Ability_rmse = rmse(fit$pars$Ability, sim$Ability),
      B_rmse = rmse(as.numeric(fit$pars$B), as.numeric(sim$B))
    )
  }

  rbindlist(list(
    fit_once("current_cov"),
    fit_once("fixed_cov")
  ), use.names = TRUE)
}

bench <- rbindlist(list(
  run_case(
    "1d_sparse",
    seed = 20260321,
    Nsubs = 300,
    Nitems = 30,
    Nscales = 1,
    NitemsAnswered = 6,
    ASD = 0.2,
    BSD = 0.8
  ),
  run_case(
    "2d_sparse_mirt",
    seed = 20260322,
    Nsubs = 300,
    Nitems = 30,
    Nscales = 2,
    NitemsAnswered = c(5, 5),
    ASD = 0.2,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.2
  ),
  run_case(
    "3d_sparse_mirt",
    seed = 20260323,
    Nsubs = 250,
    Nitems = 36,
    Nscales = 3,
    NitemsAnswered = c(4, 4, 4),
    ASD = 0.15,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.15
  )
), use.names = TRUE)

bench[, `:=`(
  elapsed_ratio_to_current = elapsed_sec / elapsed_sec[correction == "current_cov"][1],
  item_time_ratio_to_current = item_step_mean_sec / item_step_mean_sec[correction == "current_cov"][1],
  A_rmse_delta = A_rmse - A_rmse[correction == "current_cov"][1],
  Ability_rmse_delta = Ability_rmse - Ability_rmse[correction == "current_cov"][1],
  B_rmse_delta = B_rmse - B_rmse[correction == "current_cov"][1]
), by = scenario]

print(bench)

if(length(args) >= 1L && nzchar(args[1])) fwrite(bench, args[1])
