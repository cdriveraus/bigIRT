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

fit_case <- function(dat, method, cores = 1L){
  started <- proc.time()[3]
  fit <- fitIRT(
    dat,
    pl = 2,
    cores = cores,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = TRUE,
    marginalApprox = method,
    laplaceOuterIter = 12,
    noptimsteps = 10,
    laplaceDiagnostics = TRUE,
    verbose = 0,
    plot = FALSE
  )
  elapsed <- proc.time()[3] - started
  dx <- as.data.table(fit$laplaceDiagnostics)
  list(fit = fit, elapsed = elapsed, diag = dx)
}

run_compare_case <- function(name, seed, cores = 1L, ...){
  set.seed(seed)
  sim <- simIRT(...)
  fast <- fit_case(sim$dat, "laplace_fast", cores = cores)
  direct <- fit_case(sim$dat, "laplace_direct", cores = cores)

  rbindlist(list(
    data.table(
      scenario = name,
      method = "laplace_fast",
      cores = cores,
      Nobs = nrow(sim$dat),
      Nsubs = fast$fit$dat$Nsubs,
      Nitems = fast$fit$dat$Nitems,
      Nscales = fast$fit$dat$Nscales,
      elapsed_sec = fast$elapsed,
      outer_iter = nrow(fast$diag),
      converged = isTRUE(fast$fit$laplaceStatus$converged),
      reason = as.character(fast$fit$laplaceStatus$reason),
      final_grad = tail(fast$diag$itemGradNorm, 1),
      final_objective = tail(fast$diag$objective, 1),
      mean_item_step_sec = mean(fast$diag$itemStepSec),
      mean_person_step_sec = mean(fast$diag$personStepSec),
      ability_rmse = sqrt(mean((as.matrix(fast$fit$pars$Ability) - sim$Ability)^2)),
      loading_rmse = sqrt(mean((as.matrix(fast$fit$pars$A) - sim$A)^2))
    ),
    data.table(
      scenario = name,
      method = "laplace_direct",
      cores = cores,
      Nobs = nrow(sim$dat),
      Nsubs = direct$fit$dat$Nsubs,
      Nitems = direct$fit$dat$Nitems,
      Nscales = direct$fit$dat$Nscales,
      elapsed_sec = direct$elapsed,
      outer_iter = if(!is.null(direct$diag) && nrow(direct$diag)) nrow(direct$diag) else NA_integer_,
      converged = isTRUE(direct$fit$laplaceStatus$converged),
      reason = as.character(direct$fit$laplaceStatus$reason),
      final_grad = if(!is.null(direct$diag) && nrow(direct$diag)) tail(direct$diag$itemGradNorm, 1) else direct$fit$laplaceStatus$last_item_grad_norm,
      final_objective = if(!is.null(direct$diag) && nrow(direct$diag)) tail(direct$diag$objective, 1) else direct$fit$optim$logLik,
      mean_item_step_sec = if(!is.null(direct$diag) && nrow(direct$diag)) mean(direct$diag$itemStepSec) else NA_real_,
      mean_person_step_sec = if(!is.null(direct$diag) && nrow(direct$diag)) mean(direct$diag$personStepSec) else NA_real_,
      ability_rmse = sqrt(mean((as.matrix(direct$fit$pars$Ability) - sim$Ability)^2)),
      loading_rmse = sqrt(mean((as.matrix(direct$fit$pars$A) - sim$A)^2))
    )
  ), use.names = TRUE)
}

bench <- rbindlist(list(
  run_compare_case(
    "1d_sparse_2pl",
    seed = 20260324,
    cores = 1L,
    Nsubs = 400,
    Nitems = 40,
    Nscales = 1,
    NitemsAnswered = 6,
    ASD = 0.2,
    BSD = 0.8
  ),
  run_compare_case(
    "2d_sparse_confirmatory",
    seed = 20260325,
    cores = 1L,
    Nsubs = 400,
    Nitems = 40,
    Nscales = 2,
    NitemsAnswered = c(5, 5),
    ASD = 0.2,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.2
  ),
  run_compare_case(
    "2d_denser_confirmatory",
    seed = 20260326,
    cores = 1L,
    Nsubs = 400,
    Nitems = 40,
    Nscales = 2,
    NitemsAnswered = c(10, 10),
    ASD = 0.2,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.35
  )
), use.names = TRUE)

print(bench)

if(length(args) >= 1L && nzchar(args[1])) fwrite(bench, args[1])
