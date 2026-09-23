# One benchmark cell: regime x seed x variant, one fit, one RDS.
#
#   Rscript harness.R <regime> <seed> <variant> <outdir>
#
# BIGIRT_LIB names the library holding the bigIRT build under test.
#
# Every objective evaluation is logged by wrapping
# bigIRT_laplace_direct_objective -- the one function every optimiser variant
# goes through -- so evaluation counts and the objective trace are measured
# identically for the package's own path and for the prototypes. The wrapper
# adds a timestamp and nothing else.
#
# Contention control: the first evaluation, at the fixed initial state, is
# repeated three times before fitting. No optimiser change can affect its
# cost, so if it moves between cells the machine moved, not the method.

args <- commandArgs(trailingOnly = TRUE)
regime <- args[1]; seed <- as.integer(args[2]); variant <- args[3]
outdir <- if(length(args) >= 4) args[4] else "results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
lib <- Sys.getenv("BIGIRT_LIB")
if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R"))
source(file.path(here, "fisher.R"))
source(file.path(here, "variants.R"))

ns <- asNamespace("bigIRT")
orig_obj <- get("bigIRT_laplace_direct_objective", ns)
TRACE <- new.env()
TRACE$rows <- list(); TRACE$t0 <- NA_real_
wrapped <- function(...){
  t1 <- proc.time()[["elapsed"]]
  res <- orig_obj(...)
  t2 <- proc.time()[["elapsed"]]
  a <- list(...)
  TRACE$rows[[length(TRACE$rows) + 1L]] <- list(
    t = t2 - TRACE$t0, sec = t2 - t1, value = res$value,
    gnorm = sqrt(sum(res$approx_grad^2)),
    keep_cov = isTRUE(a$keep_covariance),
    person_sec = res$timings$personKernelSec %||% NA_real_,
    item_sec = res$timings$itemKernelSec %||% NA_real_,
    person_iter = mean(res$posterior$niter))
  res
}
`%||%` <- function(a, b) if(is.null(a)) b else a
unlockBinding("bigIRT_laplace_direct_objective", ns)
assign("bigIRT_laplace_direct_objective", wrapped, envir = ns)
lockBinding("bigIRT_laplace_direct_objective", ns)

install_variant(variant)

dat <- bench_data(regime, seed)
fa <- bench_fit_args(regime)
fa <- modifyList(fa, variant_fit_args(variant))
fa$keepInternals <- TRUE

## Contention control, then the fit.
ctrl <- local({
  sdat_fit <- do.call(fitIRT, c(list(dat = dat), modifyList(fa, list(laplaceOuterIter = 2,
    laplacePolish = 0L, keepInternals = TRUE))))
  NULL
})
TRACE$rows <- list()
TRACE$t0 <- proc.time()[["elapsed"]]
ctrl_sec <- NA_real_
fit_err <- NULL
tt <- system.time(fit <- tryCatch(do.call(fitIRT, c(list(dat = dat), fa)),
  error = function(e){ fit_err <<- conditionMessage(e); NULL }))[["elapsed"]]
tr <- rbindlist(TRACE$rows)

## The objective the optimiser sees warm-starts each person mode from the last
## evaluation and stops the person Newton at a gradient of 1e-4, so its value
## depends slightly on the path that led to it. Score every variant on the
## same, path-free number instead: its final point, with the person modes
## cold-started and solved to 1e-10.
precise_value <- function(fit){
  it <- fit$internals
  if(is.null(it)) return(NA_real_)
  sdat <- it$sdat
  est_corr <- isTRUE(fa$estimateAbilityCorr) && sdat$Nscales > 1L
  lay <- get("bigIRT_laplace_direct_layout", ns)(sdat, estimateAbilityCorr = est_corr)
  il <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
  ctx <- get("bigIRT_laplace_item_context", ns)(sdat, layout = il)
  ctx$grain_size <- get("bigIRT_laplace_subject_grain", ns)(sdat$Nsubs, 1L)
  ctx$direct_layout <- lay
  ctx$estimateAbilityCorr <- est_corr
  ctx$corr_paramization <- fa$laplaceCorrParam %||% "stan_corsqrt"
  st <- it$state
  st$laplaceCorrParam <- ctx$corr_paramization
  th0 <- matrix(0, nrow = sdat$Nsubs, ncol = sdat$Nscales)
  pp <- get("bigIRT_laplace_prior_mats", ns)(sdat, jitter = 1e-6)$precision_array
  v <- tryCatch(orig_obj(par = fit$optim$par, state = st, sdat = sdat,
    prior_precision = pp, theta_init = th0, jitter = 1e-6, max_attempts = 8L,
    max_iter = 200L, tol = 1e-10, keep_covariance = TRUE, context = ctx)$value,
    error = function(e) NA_real_)
  v
}
ll_precise <- if(!is.null(fit)) precise_value(fit) else NA_real_
## The first evaluation of the fit is at the initial state, identical for all
## variants: its time is the contention control.
ctrl_sec <- if(nrow(tr)) tr$sec[1] else NA_real_

out <- list(regime = regime, seed = seed, variant = variant,
  host = Sys.info()[["nodename"]], error = fit_err,
  sec = tt, nobs = nrow(dat), ctrl_sec = ctrl_sec,
  evals = nrow(tr), evals_cov = if(nrow(tr)) sum(tr$keep_cov) else NA,
  logLik = if(!is.null(fit)) fit$optim$logLik else NA_real_,
  ll_precise = ll_precise,
  iter = if(!is.null(fit)) fit$optim$iter %||% NA else NA,
  terminate = if(!is.null(fit)) fit$optim$terminate$what %||% NA else NA,
  polish_steps = if(!is.null(fit)) fit$laplaceStatus$polish_steps else NA,
  polish_gain = if(!is.null(fit)) fit$laplaceStatus$polish_gain else NA,
  grad_scaled = if(!is.null(fit)) fit$laplaceStatus$last_item_grad_scaled else NA,
  converged = if(!is.null(fit)) fit$laplaceStatus$converged else NA,
  npar = if(!is.null(fit)) length(fit$optim$par) else NA,
  extra = if(exists("PROTO_EXTRA", envir = globalenv())) get("PROTO_EXTRA", envir = globalenv()) else NULL,
  par = if(!is.null(fit)) fit$optim$par else NULL,
  trace = tr)
saveRDS(out, file.path(outdir, sprintf("%s_%d_%s.rds", regime, seed, variant)))
cat(sprintf("DONE %s %d %s sec=%.2f evals=%d iter=%s ll=%.6f term=%s\n",
  regime, seed, variant, tt, nrow(tr), format(out$iter), out$logLik, format(out$terminate)))
