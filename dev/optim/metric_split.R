# Where a block-diagonal metric build spends its time.
#   Rscript metric_split.R <regime> <seed> <cores>
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2]); cores <- as.integer(args[3])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), modifyList(bench_fit_args(regime),
  list(cores = cores, keepInternals = TRUE, laplaceRefine = "none")))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE, cores = cores)
RcppParallel::setThreadOptions(numThreads = cores)
ev <- d$eval; L <- d$layout; n <- length(d$par); N <- d$sdat$Nsubs
ps <- attr(L, "par_scale")
tm <- function(expr) { t <- proc.time()[["elapsed"]]; force(expr); proc.time()[["elapsed"]] - t }
P0 <- as.matrix(ev$prior_mats$precision)
t_pr <- tm(Pr <- G("bigIRT_laplace_prior_curvature")(d$sdat, L, n, N, P0))
t_call <- tm(out <- G("bigIRT_fisher_kernel_call")("blocks", ev, d$sdat, d$context, L, ps,
  as.numeric(Matrix::diag(Pr)) * ps^2))
t_all <- tm(m <- G("bigIRT_laplace_blockdiag_solver")(ev, d$par, -ev$approx_grad, d$sdat, d$context, L,
  gfun = function(p) stop("none")))
v <- rnorm(n)
t_apply <- tm(for(i in 1:10) m(v)) / 10
cat(sprintf("%s: n %d | prior curvature %.3f s | C++ kernel %.3f s | whole solver %.3f s | apply %.4f s\n",
  regime, n, t_pr, t_call, t_all, t_apply))
