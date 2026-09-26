# Summarise the standard-error coverage study (coverage_cell.R).
#
#   Rscript coverage_summarise.R <outdir>
#
# Per cell, arm, parameter block and standard-error method:
#   coverage   share of item x replication intervals est +- 1.96 se that hold
#              the truth, with its Monte Carlo standard error taken across
#              replications (items within a replication share persons and
#              hyperparameters, so they are not independent draws);
#   se/sd      per item, the mean reported SE over replications divided by the
#              SD of that item's estimate over replications, then the median
#              over items: whether the SE is the right size, whatever the bias;
#   bias/sd    per item, the mean error over the SD, median of its absolute
#              value over items: what shrinkage and approximation leave.
# And per cell and arm the certificate-side quantities the study records: the
# gap at the fitted point, the smallest information eigenvalue, the asymmetry
# of the differenced Hessian, and timings.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
outdir <- if(length(args)) args[1] else "coverage"
files <- list.files(outdir, pattern = "\\.rds$", full.names = TRUE)
res <- lapply(files, readRDS)
failed <- vapply(res, function(o) !is.null(o$error), logical(1))
cat(sprintf("%d result files, %d failed fits\n", length(res), sum(failed)))
if(any(failed)) print(table(vapply(res[failed], function(o) paste(o$cell, o$arm), "")))
res <- res[!failed]
if(!length(res)) quit(save = "no")
z <- qnorm(0.975)

long <- rbindlist(lapply(res, function(o){
  rbindlist(lapply(names(o$se), function(meth){
    se <- o$se[[meth]]
    rbindlist(lapply(colnames(o$est), function(b) data.table(
      cell = o$cell, arm = o$arm, rep = o$rep, method = meth, param = b,
      item = seq_len(nrow(o$est)), truth = o$truth[, b], est = o$est[, b],
      se = se[, b])))
  }))
}))
long[, covered := abs(est - truth) <= z * se]

cat("\n=== Wald coverage (nominal 0.95), items x replications ===\n")
cov_rep <- long[is.finite(se), .(cov = mean(covered)), by = .(cell, arm, method, param, rep)]
cov_tab <- cov_rep[, .(reps = .N, coverage = round(mean(cov), 3),
  mcse = round(sd(cov) / sqrt(.N), 3)), by = .(cell, arm, param, method)]
cov_tab[, method := factor(method, c("profile", "block", "blockschur", "full"))]
setorder(cov_tab, cell, arm, param, method)
print(merge(dcast(cov_tab, cell + arm + param ~ method, value.var = "coverage"),
  cov_tab[, .(reps = max(reps)), by = .(cell, arm, param)], by = c("cell", "arm", "param")))
cat("\nMonte Carlo SE of those coverages (across replications):\n")
print(dcast(cov_tab, cell + arm + param ~ method, value.var = "mcse"))

per_item <- long[is.finite(se), .(emp_sd = sd(est), mean_se = mean(se),
  bias = mean(est - truth), n = .N), by = .(cell, arm, method, param, item)]
cat("\n=== SE size: median over items of mean(se) / sd(estimate) ===\n")
se_tab <- per_item[, .(se_over_sd = round(median(mean_se / emp_sd), 3)),
  by = .(cell, arm, param, method)]
se_tab[, method := factor(method, c("profile", "block", "blockschur", "full"))]
print(dcast(se_tab, cell + arm + param ~ method, value.var = "se_over_sd"))
cat("\n=== median over items of |bias| / sd(estimate) (method-free) ===\n")
print(per_item[method == "full", .(abs_bias_over_sd = round(median(abs(bias) / emp_sd), 3),
  mean_bias = round(mean(bias), 4)), by = .(cell, arm, param)])

corr <- rbindlist(lapply(res, function(o) if(!is.null(o$corr)) data.table(cell = o$cell,
  arm = o$arm, rep = o$rep, truth = o$corr$truth, est = o$corr$est,
  se_full = o$corr$se_full, se_bs = o$corr$se_bs, se_alone = o$corr$se_alone)))
if(nrow(corr)){
  cat("\n=== latent correlation (raw scale, rho = tanh(raw)) ===\n")
  print(corr[, .(reps = .N, emp_sd = round(sd(est), 4), bias = round(mean(est - truth), 4),
    se_full = round(mean(se_full), 4), cov_full = round(mean(abs(est - truth) <= z * se_full), 3),
    se_blockschur = round(mean(se_bs), 4), cov_bs = round(mean(abs(est - truth) <= z * se_bs), 3),
    se_alone = round(mean(se_alone), 4), cov_alone = round(mean(abs(est - truth) <= z * se_alone), 3)),
    by = .(cell, arm)])
}

meta <- rbindlist(lapply(res, function(o) data.table(cell = o$cell, arm = o$arm,
  rep = o$rep, host = o$host, n = o$n, marginal = o$marginal, converged = o$converged,
  gap = o$gap, gap_bs = o$gap_blockschur, eig_min = o$eig_min, asym = o$asym,
  sec_fit = o$sec_fit, sec_hess = o$sec_hess)))
cat("\n=== fits: gap at the fitted point (nats), curvature, timings ===\n")
print(meta[, .(reps = .N, hosts = paste(unique(host), collapse = ","),
  marginal = paste(unique(marginal), collapse = ","),
  converged = sum(converged), gap_median = signif(median(gap), 3),
  gap_max = signif(max(gap), 3), gap_bs_max = signif(max(gap_bs), 3),
  eig_min_min = signif(min(eig_min), 3), asym_max = signif(max(asym), 3),
  fit_s = round(median(sec_fit), 1), hess_s = round(median(sec_hess), 1)),
  by = .(cell, arm)])
fwrite(cov_tab, file.path(outdir, "summary_coverage.csv"))
fwrite(se_tab, file.path(outdir, "summary_se_ratio.csv"))
fwrite(meta, file.path(outdir, "summary_meta.csv"))
cat("SUMMARY DONE\n")
