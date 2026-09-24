# Two end points of one 3PL dataset that differ by ~10 nats: a second local
# optimum, or a point the optimiser stopped at early?
#   Rscript local_optima.R <regime> <seed> <dirA/variantA.rds> <dirB/variantB.rds>
# At each end point: the precise objective, the gradient norm, the predicted
# gain in the block-diagonal and dense metrics, and which parameter blocks
# differ between the two. Then L-BFGS (block-diagonal metric) is continued
# from the worse point to a predicted gain of 1e-8, to see whether it leaves.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table); library(Matrix)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceOptimizer = "mize", laplacePolish = 0L))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
A <- readRDS(args[3]); B <- readRDS(args[4])
th0 <- 0 * d$eval$posterior$theta_mode
ev <- function(par, tol = 1e-10) G("bigIRT_laplace_direct_objective")(par = par, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision, theta_init = th0,
  max_iter = 200L, tol = tol, context = d$context)
report <- function(lbl, par){
  e <- ev(par)
  g <- -e$approx_grad
  mb <- G("bigIRT_laplace_blockdiag_solver")(e, par, g, d$sdat, d$context, d$layout,
    gfun = function(p) -ev(p)$approx_grad)
  gain_bd <- 0.5 * sum(g * mb(g))
  cat(sprintf("%-10s value %.6f  |g| %.3e  predicted gain (Bd^-1) %.3e\n", lbl, e$value,
    sqrt(sum(g^2)), gain_bd))
  e
}
eA <- report(A$variant, A$par); eB <- report(B$variant, B$par)
L <- d$layout; ps <- attr(L, "par_scale")
for(b in c("B", "A", "C", "B_mean", "A_mean", "C_mean")){
  ix <- L[[b]]; if(!length(ix)) next
  dx <- (A$par[ix] - B$par[ix]) * ps[ix]
  cat(sprintf("  block %-7s max |diff| %.3f  rms %.3f  (n %d)\n", b, max(abs(dx)), sqrt(mean(dx^2)), length(ix)))
}
worse <- if(eA$value < eB$value) A else B
cat("continuing from", worse$variant, "\n")
cache <- new.env()
fg <- function(par){ e <- ev(par, 1e-8); list(f = -e$value, g = -e$approx_grad, ev = e, x = par) }
e0 <- fg(worse$par)
m <- G("bigIRT_laplace_blockdiag_solver")(e0$ev, worse$par, e0$g, d$sdat, d$context, d$layout,
  gfun = function(p) -ev(p)$approx_grad)
r <- G("bigIRT_laplace_lbfgs")(worse$par, fg, minv = m, maxit = 300L, gain_tol = 1e-8)
cat(sprintf("continued: %d iterations, terminate %s, value %.6f (moved %.3e nats)\n",
  r$iter, r$terminate$what, -r$f, -r$f - max(eA$value, eB$value) + abs(eA$value - eB$value)))
cat(sprintf("final value %.6f vs better end point %.6f\n", -r$f, max(eA$value, eB$value)))
cat("LOCAL_DONE\n")
