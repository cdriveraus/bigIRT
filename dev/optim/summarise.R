# Summarise a results directory of harness.R cells.
#
#   Rscript summarise.R <dir> [tol]
#
# Scoring. The optimum for a (regime, seed) is the best PRECISE value any
# variant reached -- each fit's final point re-evaluated with the person modes
# cold-started and solved to 1e-10 (harness.R), so the score does not inherit
# the warm-start noise of the objective the optimiser saw. `gap` is how far a
# variant ended below that; more than 1e-3 nats is a failure, not a speedup.
# `to_tol` is the evaluations (and seconds) the variant needed before its
# running objective first came within `tol` nats of the optimum.
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]; tol <- if(length(args) > 1) as.numeric(args[2]) else 1e-2
suppressPackageStartupMessages(library(data.table))
fs <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
fs <- fs[!grepl("^jml_", basename(fs))]
rows <- lapply(fs, function(f){
  r <- readRDS(f)
  tr <- r$trace
  data.table(regime = r$regime, seed = r$seed, variant = r$variant,
    sec = r$sec, evals = r$evals, iter = as.numeric(r$iter),
    ll = r$logLik, ll_precise = r$ll_precise %||% NA_real_,
    polish = r$polish_steps, term = as.character(r$terminate),
    ctrl = r$ctrl_sec, err = !is.null(r$error),
    trace = list(tr))
})
`%||%` <- function(a, b) if(is.null(a)) b else a
d <- rbindlist(rows)
d[, score := ifelse(is.finite(ll_precise), ll_precise, ll)]
d[, best := max(score, na.rm = TRUE), by = .(regime, seed)]
d[, gap := best - score]
d[, c("evals_to_tol", "sec_to_tol") := {
  e <- integer(.N); s <- numeric(.N)
  for(i in seq_len(.N)){
    tr <- trace[[i]]
    hit <- which(tr$value >= best[i] - tol)
    e[i] <- if(length(hit)) hit[1] else NA_integer_
    s[i] <- if(length(hit)) tr$t[hit[1]] else NA_real_
  }
  list(e, s)
}]
d[, evals_per_iter := evals / pmax(iter, 1)]
base <- d[variant == "base", .(regime, seed, base_sec = sec, base_evals = evals)]
d <- merge(d, base, by = c("regime", "seed"), all.x = TRUE)
d[, speedup := base_sec / sec]
d[, fail := gap > 1e-3]
options(width = 200)
print(d[order(regime, seed, variant), .(regime, seed, variant, sec = round(sec, 2),
  evals, iter, epi = round(evals_per_iter, 2), polish, term,
  gap = signif(gap, 2), fail, e_tol = evals_to_tol, s_tol = round(sec_to_tol, 2),
  speedup = round(speedup, 2), ctrl = round(ctrl, 3))])
cat("\nBy variant, geometric mean over cells of base_sec / sec, and failures:\n")
print(d[, .(cells = .N, gm_speedup = round(exp(mean(log(speedup), na.rm = TRUE)), 2),
  gm_evals_ratio = round(exp(mean(log(base_evals / evals), na.rm = TRUE)), 2),
  fails = sum(fail, na.rm = TRUE), max_gap = signif(max(gap, na.rm = TRUE), 2)),
  by = variant][order(-gm_speedup)])
