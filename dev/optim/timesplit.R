# Where each variant's wall time goes: objective evaluations (person kernel,
# item kernel), metric/Fisher assembly, and everything else.
#   Rscript timesplit.R <dir> [variant regex]
args <- commandArgs(TRUE); dir <- args[1]
pat <- if(length(args) > 1) args[2] else "."
suppressPackageStartupMessages(library(data.table))
`%||%` <- function(a, b) if(is.null(a)) b else a
fs <- list.files(dir, pattern = "rds$", full.names = TRUE)
fs <- fs[!grepl("^jml_", basename(fs))]
d <- rbindlist(lapply(fs, function(f){
  r <- readRDS(f)
  if(!grepl(pat, r$variant)) return(NULL)
  data.table(regime = r$regime, seed = r$seed, v = r$variant, sec = r$sec, evals = r$evals,
    eval_sec = sum(r$trace$sec), person = sum(r$trace$person_sec), item = sum(r$trace$item_sec),
    metric = r$extra$metric_sec %||% 0, npar = r$npar, nobs = r$nobs)
}))
options(width = 200)
print(d[, .(sec = mean(sec), evals = mean(evals), eval_sec = mean(eval_sec),
  person = mean(person), item = mean(item), metric = mean(metric),
  other = mean(sec - eval_sec - metric), npar = npar[1], nobs = nobs[1]),
  by = .(regime, v)][order(regime, v)], digits = 3)
