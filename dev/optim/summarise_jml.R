# Summarise harness_jml.R cells.   Rscript summarise_jml.R <dir>
#
# Stage 1 (free fit) has the same objective for every variant, so its final
# value is comparable directly: gap1 is how far above the best stage-1 value a
# variant stopped (minimised objective, nats). Stage 2 (empirical Bayes) runs
# under priors computed from stage 1, so its objective differs slightly between
# variants and only its cost is compared.
args <- commandArgs(TRUE); dir <- args[1]
suppressPackageStartupMessages(library(data.table))
fs <- list.files(dir, pattern = "^jml_.*rds$", full.names = TRUE)
d <- rbindlist(lapply(fs, function(f){
  r <- readRDS(f); c <- r$calls
  if(is.null(c) || !nrow(c)) return(data.table(regime = r$regime, seed = r$seed, v = r$variant, err = TRUE))
  data.table(regime = r$regime, seed = r$seed, v = r$variant, err = !is.null(r$error),
    sec = r$sec, it1 = c$iter[1], nf1 = c$nf[1], f1 = c$f[1], term1 = c$terminate[1],
    it2 = c$iter[2], nf2 = c$nf[2], f2 = c$f[2], term2 = c$terminate[2], npar = c$npar[1])
}), fill = TRUE)
d[, gap1 := f1 - min(f1), by = .(regime, seed)]
d[, base_sec := sec[v == "base"], by = .(regime, seed)]
options(width = 200, datatable.print.nrows = 1000)
print(d[order(regime, seed, v), .(regime, seed, v, sec = round(sec, 1), npar, it1, nf1,
  gap1 = signif(gap1, 2), term1, it2, nf2, f2 = round(f2, 3), term2, speedup = round(base_sec / sec, 2))])
cat("\nGeometric mean speedup over base, and max stage-1 gap:\n")
print(d[, .(cells = .N, gm_speedup = round(exp(mean(log(base_sec / sec))), 2),
  mean_nf1 = mean(nf1), mean_nf2 = mean(nf2), max_gap1 = signif(max(gap1), 2)), by = v])
