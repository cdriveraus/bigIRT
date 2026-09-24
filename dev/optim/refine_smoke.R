# Package-path check of the quadrature refinement against the prototype.
#   Rscript refine_smoke.R <regime> <seed> <agq_prototype.rds>
# Fits with laplaceRefine = "none" and "auto"; the refined end point must match
# the prototype's AGQ optimum (dev/optim/laplace_vs_agq.R, 21 nodes) to a small
# fraction of a standard error, and the unrefined one must not.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
dat <- bench_data(regime, seed)
proto <- readRDS(args[3])
fa <- bench_fit_args(regime)
run <- function(refine){
  t <- system.time(f <- do.call(fitIRT, c(list(dat = dat), fa, list(laplaceRefine = refine))))[["elapsed"]]
  list(fit = f, sec = t)
}
r0 <- run("none"); r1 <- run("auto")
for(nm in c("none", "auto")){
  r <- if(nm == "none") r0 else r1
  st <- r$fit$laplaceStatus
  dz <- (r$fit$optim$par - proto$par_agq) / proto$se
  cat(sprintf("%-5s %6.1f s  marginal %-7s evals %4d  refine iter %s evals %s  logLik %.4f  |dz| max %.3f mean %.3f  term %s\n",
    nm, r$sec, st$marginal, r$fit$optim$target_evals,
    format(st$refine$iter %||% NA), format(st$refine$evals %||% NA), r$fit$optim$logLik,
    max(abs(dz)), mean(abs(dz)), st$optimizer_terminate))
}
cat(sprintf("refinement: Laplace value %.4f, AGQ start %.4f, AGQ end %.4f (gain %.4f); prototype AGQ optimum %.4f\n",
  r1$fit$laplaceStatus$refine$laplace_value, r1$fit$laplaceStatus$refine$agq_start,
  r1$fit$laplaceStatus$refine$agq_end,
  r1$fit$laplaceStatus$refine$agq_end - r1$fit$laplaceStatus$refine$agq_start, NA))
cat("SMOKE_DONE\n")
