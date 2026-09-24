# Cost of the package's block-diagonal metric build, piece by piece, at the
# initial state of one regime, with Rprof's top functions.
#   Rscript pkg_cost.R <regime> <seed>     (BIGIRT_LIB -> a build with the metric)
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table); library(Matrix)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
tm <- function(label, expr){ t0 <- proc.time()[["elapsed"]]; v <- force(expr)
  cat(sprintf("  %-34s %7.2f s\n", label, proc.time()[["elapsed"]] - t0)); invisible(v) }
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceOptimizer = "mize", laplacePolish = 0L))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
cat(sprintf("%s: n = %d, rows = %d\n", regime, length(d$par), nrow(dat)))
tm("objective evaluation", G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
  theta_init = d$eval$posterior$theta_mode, context = d$context))
tm("row_effective (R)", G("bigIRT_laplace_row_effective")(state = d$eval$state, sdat = d$sdat,
  thetaBase = d$eval$posterior$theta_mode, rows = d$context$train_rows,
  context = d$context$row_context, include_raw = TRUE))
tm("row derivatives", G("bigIRT_laplace_row_derivatives")(d$eval, d$sdat, d$context, d$layout))
P <- tm("fisher parts", G("bigIRT_laplace_fisher_parts")(d$eval, d$sdat, d$context, d$layout))
tm("row q", G("bigIRT_laplace_row_q")(P$D$W, P$N, P$K, P$P0, as.integer(d$context$row_context$ids)))
tm("one F v", P$Fv(rnorm(P$n)))
Rprof("pkg_cost.prof")
m <- tm("blockdiag solver total", G("bigIRT_laplace_blockdiag_solver")(d$eval, d$par,
  -d$eval$approx_grad, d$sdat, d$context, d$layout, gfun = function(p) stop("no FD")))
Rprof(NULL)
tm("one solve", m(rnorm(P$n)))
s <- summaryRprof("pkg_cost.prof")$by.total
print(head(s[order(-s$total.time), ], 20))
cat("COST_DONE\n")
