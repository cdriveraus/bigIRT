# Where the Fisher metric's cost goes, at the initial state of one regime.
#   Rscript probe_fisher_cost.R <regime> <seed>     (BIGIRT_LIB -> build with laplaceOptimizer.R)
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table); library(Matrix)})
source(file.path(Sys.getenv("BENCH_DIR", "."), "models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fa <- c(list(dat = dat), bench_fit_args(regime), list(keepInternals = TRUE, laplaceOuterIter = 2))
fit <- suppressWarnings(do.call(fitIRT, fa))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
tm <- function(expr){ t0 <- proc.time()[["elapsed"]]; v <- force(expr); list(v = v, s = proc.time()[["elapsed"]] - t0) }
t_eval <- tm(G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state, sdat = d$sdat,
  prior_precision = fit$internals$priorPrecision, theta_init = d$eval$posterior$theta_mode,
  context = d$context))$s
r1 <- tm(G("bigIRT_laplace_row_derivatives")(d$eval, d$sdat, d$context, d$layout)); D <- r1$v
N <- D$N; K <- D$K; P0 <- D$P0
r2 <- tm(Matrix::crossprod(D$W) + Matrix::kronecker(Matrix::Diagonal(N), Matrix::Matrix(P0, sparse = TRUE))); Ipp <- r2$v
r3 <- tm(Matrix::crossprod(D$W, D$V)); Ipi <- r3$v
r4 <- tm(Matrix::solve(Matrix::forceSymmetric(Ipp), Ipi)); X <- r4$v
r5 <- tm(Matrix::crossprod(Ipi, X)); S <- r5$v
r6 <- tm(Matrix::crossprod(D$V)); Vv <- r6$v
r7 <- tm(G("bigIRT_laplace_fisher_info")(d$eval, d$sdat, d$context, d$layout)); H <- r7$v
r8 <- tm(Matrix::Cholesky(Matrix::forceSymmetric(H), perm = TRUE, LDL = FALSE))
r9 <- tm(chol(as.matrix(H)))
cat(sprintf("regime %s: n = %d, nnz(H) = %d (density %.2f), one objective evaluation %.2f s\n",
  regime, D$n, Matrix::nnzero(H), Matrix::nnzero(H) / D$n^2, t_eval))
cat(sprintf("  row derivatives %.2f | W'W %.2f | W'V %.2f | solve(Ipp) %.2f | Ipi'X %.2f | V'V %.2f | whole fisher_info %.2f\n",
  r1$s, r2$s, r3$s, r4$s, r5$s, r6$s, r7$s))
cat(sprintf("  CHOLMOD Cholesky %.2f s | dense LAPACK chol %.2f s\n", r8$s, r9$s))
cat("PROBE_DONE\n")
