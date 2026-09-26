# Summarise the certificate-cost run (cert_cost.R): Rscript cert_cost_summarise.R <outdir>
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
res <- lapply(list.files(args[1], pattern = "\\.rds$", full.names = TRUE), readRDS)
cat(sprintf("%d result files\n", length(res)))
st <- Filter(function(o) identical(o$kind, "study"), res)
if(length(st)){
  s <- rbindlist(lapply(st, function(o){
    r <- if(!is.null(o$fast$se) && !is.null(o$exact$se)) as.numeric(o$fast$se / o$exact$se) else NA_real_
    data.table(cell = o$cell, arm = o$arm, rep = o$rep, host = o$host, npar = o$npar,
      fit = o$fit_sec, fast = o$fast$sec, fast_evals = o$fast$evaluations,
      exact = o$exact$sec, exact_evals = o$exact$evaluations,
      fast_status = o$fast$status, exact_status = o$exact$status,
      gap_fast = o$fast$gap, gap_exact = o$exact$gap,
      se_min = min(r), se_med = median(r), se_max = max(r))
  }))
  cat("\n=== study fits: fast against exact, same fitted point ===\n")
  print(s[, .(reps = .N, host = paste(unique(host), collapse = ","), npar = npar[1],
    fit_s = round(median(fit), 1), fast_s = round(median(fast), 2),
    fast_over_fit = round(median(fast / fit), 3), fast_evals = median(fast_evals),
    exact_s = round(median(exact), 1), exact_over_fit = round(median(exact / fit), 2),
    same_verdict = sum(fast_status == exact_status),
    verdicts = paste(unique(paste(fast_status, exact_status, sep = "/")), collapse = " "),
    gap_ratio_med = signif(median(gap_fast / gap_exact), 2),
    gap_ratio_range = paste(signif(range(gap_fast / gap_exact), 2), collapse = "-"),
    se_ratio_med = round(median(se_med), 3),
    se_ratio_range = paste(round(c(min(se_min), max(se_max)), 3), collapse = "-")),
    by = .(cell, arm)])
}
sc <- Filter(function(o) identical(o$kind, "scale"), res)
if(length(sc)){
  x <- rbindlist(lapply(sc, function(o) data.table(persons = o$persons, items = o$items,
    pl = o$pl, cores = o$cores, host = o$host, rows = o$rows, npar = o$npar,
    marginal = o$marginal, fit_s = round(o$fit_sec, 1), fit_evals = o$fit_evals,
    eval_s = round(o$fast$eval_sec, 3), fast_s = round(o$fast$sec, 2),
    fast_evals = o$fast$evaluations, fast_over_fit = round(o$fast$sec / o$fit_sec, 3),
    fast_in_evals = round(o$fast$sec / o$fast$eval_sec, 1), status = o$fast$status,
    gap = signif(o$fast$gap, 2))))
  setorder(x, pl, rows)
  cat("\n=== sparse design, 25 items a person, fast route ===\n")
  print(x)
}
cat("SUMMARY DONE\n")
