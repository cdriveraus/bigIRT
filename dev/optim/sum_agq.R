# Summarise the refinement grid (ragq/) against the prototype AGQ optima (agq/).
suppressPackageStartupMessages(library(data.table))
rows <- list()
for(f in Sys.glob(file.path(Sys.getenv("D", "ragq"), "*.rds"))){
  r <- readRDS(f)
  proto <- NULL
  for(pf in c(sprintf("agq/agq_%s_%d_%s_%d_sm_ref.rds", r$regime, r$seed, r$regime, r$seed),
              sprintf("agq/agq_%s_%d.rds", r$regime, r$seed)))
    if(file.exists(pf)){ proto <- readRDS(pf); break }
  dmax <- NA_real_
  if(!is.null(proto) && !isTRUE(grepl("_eb$", r$variant)) && length(r$par) == length(proto$par_agq)){
    d <- (r$par - proto$par_agq) * proto$ps
    dmax <- max(abs(d))
  }
  rows[[length(rows) + 1L]] <- data.table(regime = r$regime, seed = r$seed, variant = r$variant,
    sec = r$sec, evals = r$evals, marginal = r$marginal, logLik = r$ll_precise,
    refine_evals = r$refine$evals %||% NA_integer_, max_abs_dpar_vs_agq_opt = dmax,
    hyperCSD = if(length(r$hyper) >= 3) unname(r$hyper[3]) else NA_real_)
}
`%||%` <- function(a, b) if(is.null(a)) b else a
tab <- rbindlist(rows)[order(regime, seed, variant)]
print(tab, digits = 6)
cat("\nmean time and evaluations by regime and variant:\n")
print(tab[, .(sec = mean(sec), evals = mean(evals)), by = .(regime, variant)][order(regime, variant)])
