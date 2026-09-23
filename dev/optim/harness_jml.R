# One JML (marginalApprox = "none") benchmark cell.
#
#   Rscript harness_jml.R <regime> <seed> <variant> <outdir>
#
# The JML path optimises the joint posterior of items and persons through
# rstan::log_prob and mize, inside optimIRT. Every mize call is logged by
# wrapping mize::mize, so each stage of the JML sequence (free fit, then the
# empirical-Bayes refit) reports its own iterations and objective calls.
#
#   base       the package as installed
#   mize_unit  mize's per-iteration initial step fixed at 1
#   warm       the empirical-Bayes refit starts from the free fit's optimum
#              instead of optimIRT's random start (JMLfit only passes the
#              previous optimum on when ebayes = FALSE)
#   warm_unit  both

args <- commandArgs(trailingOnly = TRUE)
regime <- args[1]; seed <- as.integer(args[2]); variant <- args[3]
outdir <- if(length(args) >= 4) args[4] else "results_jml"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
lib <- Sys.getenv("BIGIRT_LIB")
if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R"))

MIZE_LOG <- new.env(); MIZE_LOG$calls <- list()
mns <- asNamespace("mize")
orig_mize <- get("mize", mns)
unit <- variant %in% c("mize_unit", "warm_unit")
wrapper <- function(...){
  a <- list(...)
  if(unit) a$step_next_init <- 1
  t0 <- proc.time()[["elapsed"]]
  r <- do.call(orig_mize, a)
  MIZE_LOG$calls[[length(MIZE_LOG$calls) + 1L]] <- list(sec = proc.time()[["elapsed"]] - t0,
    iter = r$iter, nf = r$nf, ng = r$ng, f = r$f, terminate = r$terminate$what,
    npar = length(r$par))
  r
}
unlockBinding("mize", mns); assign("mize", wrapper, envir = mns); lockBinding("mize", mns)

if(variant %in% c("warm", "warm_unit")){
  ## optimIRT draws init = rnorm(npars, 0, .1) whenever init is NA. Carry the
  ## last optimum across calls instead: the second call of a fitIRT run is the
  ## empirical-Bayes refit, whose optimum is near the first one.
  ns <- asNamespace("bigIRT")
  orig_opt <- get("optimIRT", ns)
  LAST <- new.env(); LAST$par <- NULL
  warm_opt <- function(standata, ..., init = NA){
    if(is.na(init[1]) && !is.null(LAST$par)) init <- LAST$par
    r <- orig_opt(standata = standata, ..., init = init)
    LAST$par <- r$optim$par
    r
  }
  environment(warm_opt) <- environment()
  unlockBinding("optimIRT", ns); assign("optimIRT", warm_opt, envir = ns); lockBinding("optimIRT", ns)
}

dat <- bench_data(regime, seed)
R <- bench_regimes[[regime]]
fa <- list(pl = R$pl, marginalApprox = "none", cores = 1, dropPerfectScores = TRUE)
if(isTRUE(R$personPreds)) fa$personPreds <- c("V1", "V2")
set.seed(seed)  # optimIRT's random start
fit_err <- NULL
tt <- system.time(fit <- tryCatch(suppressMessages(do.call(fitIRT, c(list(dat = dat), fa))),
  error = function(e){ fit_err <<- conditionMessage(e); NULL }))[["elapsed"]]
calls <- rbindlist(MIZE_LOG$calls)
out <- list(regime = regime, seed = seed, variant = variant, path = "jml",
  host = Sys.info()[["nodename"]], error = fit_err, sec = tt, nobs = nrow(dat),
  calls = calls, final_f = if(nrow(calls)) tail(calls$f, 1) else NA_real_)
saveRDS(out, file.path(outdir, sprintf("jml_%s_%d_%s.rds", regime, seed, variant)))
cat(sprintf("DONE jml %s %d %s sec=%.2f stages=%d iters=%s nf=%s f=%s\n", regime, seed, variant, tt,
  nrow(calls), paste(calls$iter, collapse = "/"), paste(calls$nf, collapse = "/"),
  paste(sprintf("%.4f", calls$f), collapse = "/")))
