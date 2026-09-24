# Cost of each piece of the scale metrics at the initial state.
#   Rscript sm_cost.R <regime> <seed>
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table); library(Matrix)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
source(file.path(here, "scale_metric.R"))
ns <- asNamespace("bigIRT")
for(f in c("sm_ops", "sm_coarse", "sm_precond", "sm_pcg", "sm_minv", "tabulate_sum", "sm_row_q", "sm_block_inverse")){
  g <- get(f); environment(g) <- ns; assign(f, g) }
G <- function(nm) get(nm, ns)
tm <- function(label, expr){ t0 <- proc.time()[["elapsed"]]; v <- force(expr)
  cat(sprintf("  %-28s %7.2f s\n", label, proc.time()[["elapsed"]] - t0)); invisible(v) }
dat <- bench_data(regime, seed)
fa <- c(list(dat = dat), bench_fit_args(regime), list(keepInternals = TRUE, laplaceOuterIter = 2,
  laplaceOptimizer = "mize", laplacePolish = 0L))
fit <- suppressWarnings(do.call(fitIRT, fa))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
cat(sprintf("%s: n = %d, rows = %d\n", regime, length(d$par), nrow(dat)))
tm("objective evaluation", G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
  theta_init = d$eval$posterior$theta_mode, context = d$context))
D <- tm("row derivatives", G("bigIRT_laplace_row_derivatives")(d$eval, d$sdat, d$context, d$layout))
ops <- tm("sm_ops total", sm_ops(d$eval, d$sdat, d$context, d$layout))
cat(sprintf("  nnz V %d, C %d, Bd %d\n", ops$nnz_V, ops$nnz_C, ops$nnz_Bd))
v <- rnorm(ops$n)
tm("one F v product", ops$Fv(v))
tm("one Bd solve", ops$bd(v))
for(m in c(1, 100)){
  co <- tm(sprintf("coarse space m=%d", m), sm_coarse(ops, d$layout, d$context, d$eval$posterior$theta_mode, m))
  tm(sprintf("one Q apply m=%d", m), co$Q(v))
  M <- sm_precond(ops, co, "bal")
  tm(sprintf("one bal apply m=%d", m), M(v))
  pc <- tm(sprintf("PCG bal m=%d to 1e-3", m), sm_pcg(ops$Fv, as.numeric(d$eval$approx_grad), M, rtol = 1e-3))
  cat(sprintf("    iterations %d, eig range [%.3f, %.3f]\n", pc$iter, pc$eig[1], pc$eig[2]))
}
cat("COST_DONE\n")
