# Wall-clock split of one package fit: setup before the optimiser, objective
# evaluations (and their kernels), metric builds, and what is left.
#   Rscript prof_split.R <regime> <seed> <cores>
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2]); cores <- as.integer(args[3])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT")
tm <- new.env(); tm$rows <- list()
wrap <- function(nm, label){
  orig <- get(nm, ns)
  f <- function(...){ t <- proc.time()[["elapsed"]]; r <- orig(...)
    tm$rows[[length(tm$rows) + 1L]] <- data.table(fn = label, t0 = t, sec = proc.time()[["elapsed"]] - t,
      person = if(is.list(r)) r$timings$personKernelSec %||% NA_real_ else NA_real_,
      kern = if(is.list(r)) r$timings$kernelSec %||% NA_real_ else NA_real_); r }
  unlockBinding(nm, ns); assign(nm, f, ns); lockBinding(nm, ns)
}
wrap("bigIRT_laplace_direct_objective", "eval")
wrap("bigIRT_laplace_metric_solver", "metric")
wrap("bigIRT_laplace_optimize_direct", "optimise")
dat <- bench_data(regime, seed)
fa <- modifyList(bench_fit_args(regime), list(cores = cores))
t0 <- proc.time()[["elapsed"]]
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), fa)))
tot <- proc.time()[["elapsed"]] - t0
x <- rbindlist(tm$rows)
opt <- x[fn == "optimise"]
ev <- x[fn == "eval"]; me <- x[fn == "metric"]
## evaluations made inside metric builds (finite differences) are counted once, in eval
inside <- ev[, any(t0 >= me$t0 & t0 <= me$t0 + me$sec), by = seq_len(nrow(ev))]$V1
cat(sprintf("%s: total %.1f s | before optimiser %.1f | evals %d: %.1f s (kernel %.1f, of which person %.1f; R around kernel %.1f) | metric builds %d: %.1f s (evals inside %.1f) | optimiser other %.1f | after optimiser %.1f\n",
  regime, tot, opt$t0[1] - t0, nrow(ev), sum(ev$sec), sum(ev$kern, na.rm = TRUE), sum(ev$person, na.rm = TRUE),
  sum(ev$sec) - sum(ev$kern, na.rm = TRUE), nrow(me), sum(me$sec), sum(ev$sec[inside]),
  sum(opt$sec) - sum(ev$sec[!inside]) - sum(me$sec), tot - (opt$t0[1] - t0) - sum(opt$sec)))
