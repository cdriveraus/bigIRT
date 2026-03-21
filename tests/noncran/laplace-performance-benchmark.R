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

run_laplace_case <- function(name, seed, ...){
  set.seed(seed)
  sim <- simIRT(...)
  dat <- sim$dat
  started <- proc.time()[3]
  fit <- fitIRT(
    dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    marginalApprox = "laplace_em",
    laplaceOuterIter = 12,
    noptimsteps = 10,
    laplaceDiagnostics = TRUE,
    verbose = 0,
    plot = FALSE
  )
  elapsed <- proc.time()[3] - started
  dx <- as.data.table(fit$laplaceDiagnostics)

  stopifnot(all(c(
    "personStepSec", "itemStepSec", "refreshStepSec", "objectiveEvalSec",
    "outerIterSec", "itemTargetEvals", "itemGradNorm", "objective"
  ) %in% names(dx)))

  data.table(
    scenario = name,
    Nobs = nrow(dat),
    Nsubs = fit$dat$Nsubs,
    Nitems = fit$dat$Nitems,
    Nscales = fit$dat$Nscales,
    outer_iter = nrow(dx),
    converged = isTRUE(fit$laplaceStatus$converged),
    reason = as.character(fit$laplaceStatus$reason),
    elapsed_sec = elapsed,
    item_step_mean_sec = mean(dx$itemStepSec),
    item_step_median_sec = median(dx$itemStepSec),
    person_step_mean_sec = mean(dx$personStepSec),
    refresh_step_mean_sec = mean(dx$refreshStepSec),
    objective_eval_mean_sec = mean(dx$objectiveEvalSec),
    item_target_mean = mean(dx$itemTargetEvals),
    item_target_max = max(dx$itemTargetEvals),
    final_grad = tail(dx$itemGradNorm, 1),
    final_objective = tail(dx$objective, 1)
  )
}

bench <- rbindlist(list(
  run_laplace_case(
    "1d_sparse_2pl",
    seed = 20260320,
    Nsubs = 400,
    Nitems = 40,
    Nscales = 1,
    NitemsAnswered = 6,
    ASD = 0.2,
    BSD = 0.8
  ),
  run_laplace_case(
    "2d_sparse_confirmatory",
    seed = 20260321,
    Nsubs = 400,
    Nitems = 40,
    Nscales = 2,
    NitemsAnswered = c(5, 5),
    ASD = 0.2,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.2
  ),
  run_laplace_case(
    "3d_sparse_confirmatory",
    seed = 20260322,
    Nsubs = 350,
    Nitems = 45,
    Nscales = 3,
    NitemsAnswered = c(4, 4, 4),
    ASD = 0.15,
    BSD = 0.8,
    mirt = TRUE,
    loadingSparsity = 0.15
  ),
  run_laplace_case(
    "2d_denser_confirmatory",
    seed = 20260323,
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
