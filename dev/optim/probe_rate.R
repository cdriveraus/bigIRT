# Two probes at a fitted point, before designing further.
#
#   Rscript probe_rate.R <regime> <seed>
#
# 1. Objective noise. The Laplace objective solves every person mode by Newton
#    to a gradient tolerance (1e-4 by default) from a warm start, so its value
#    depends on where the previous evaluation left the modes. Evaluate one
#    point from several starts and tolerances and report the spread, and what
#    a tighter tolerance costs.
# 2. Newton rate. Fisher scoring converges at rate max|1 - lambda| over the
#    eigenvalues lambda of F^-1 H, H the true curvature. Compute H by central
#    differences (modes re-solved cold to 1e-10), and report the spectrum and
#    which parameter blocks the worst directions load on.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "fisher.R"))
ns <- asNamespace("bigIRT"); environment(proto_fisher_hessian) <- ns
G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fa <- c(list(dat = dat), bench_fit_args(regime), list(keepInternals = TRUE))
fit <- suppressWarnings(do.call(fitIRT, fa))
it <- fit$internals; sdat <- it$sdat
est_corr <- isTRUE(fa$estimateAbilityCorr)
lay <- G("bigIRT_laplace_direct_layout")(sdat, estimateAbilityCorr = est_corr)
il <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
ctx <- G("bigIRT_laplace_item_context")(sdat, layout = il)
ctx$grain_size <- G("bigIRT_laplace_subject_grain")(sdat$Nsubs, 1L)
ctx$direct_layout <- lay; ctx$estimateAbilityCorr <- est_corr; ctx$corr_paramization <- "stan_corsqrt"
st <- it$state; st$laplaceCorrParam <- "stan_corsqrt"
pp <- G("bigIRT_laplace_prior_mats")(sdat, jitter = 1e-6)$precision_array
ev <- function(par, th, tol) G("bigIRT_laplace_direct_objective")(par = par, state = st,
  sdat = sdat, prior_precision = pp, theta_init = th, jitter = 1e-6, max_attempts = 8L,
  max_iter = 200L, tol = tol, keep_covariance = TRUE, context = ctx)
x <- fit$optim$par
th_mode <- it$state$AbilityBase
th_zero <- matrix(0, sdat$Nsubs, sdat$Nscales)
set.seed(1); th_jit <- th_mode + matrix(rnorm(length(th_mode), 0, .05), nrow(th_mode))
cat("== objective noise at the fitted point\n")
for(tol in c(1e-4, 1e-6, 1e-8, 1e-10)){
  vals <- c(); secs <- c(); its <- c()
  for(th in list(th_mode, th_jit, th_zero)){
    t0 <- proc.time()[["elapsed"]]; e <- ev(x, th, tol)
    secs <- c(secs, proc.time()[["elapsed"]] - t0); vals <- c(vals, e$value)
    its <- c(its, mean(e$posterior$niter))
  }
  cat(sprintf("  tol=%.0e  value spread over 3 starts %.2e  (warm-start value - tol1e-10 cold: see below)  sec %s  mean person iters %s\n",
    tol, diff(range(vals)), paste(sprintf("%.3f", secs), collapse = "/"), paste(sprintf("%.2f", its), collapse = "/")))
  if(tol == 1e-4) v4 <- vals
}
vref <- ev(x, th_zero, 1e-12)$value
cat(sprintf("  tol=1e-4 values minus exact: %s\n", paste(sprintf("%.2e", v4 - vref), collapse = ", ")))

cat("== Newton rate: spectrum of F^-1 H at the fitted point\n")
e <- ev(x, th_zero, 1e-10)
Fm <- proto_fisher_hessian(e, sdat, ctx, lay)
n <- length(x); H <- matrix(0, n, n); h <- 1e-4
t0 <- proc.time()[["elapsed"]]
for(j in seq_len(n)){
  xp <- x; xm <- x; xp[j] <- x[j] + h; xm[j] <- x[j] - h
  H[, j] <- -(ev(xp, e$posterior$theta_mode, 1e-10)$approx_grad -
    ev(xm, e$posterior$theta_mode, 1e-10)$approx_grad) / (2 * h)
}
H <- (H + t(H)) / 2
cat(sprintf("  FD Hessian: %d parameters, %.1f s\n", n, proc.time()[["elapsed"]] - t0))
und <- attr(Fm, "undescribed"); if(length(und)){ Fm[, und] <- H[, und]; Fm[und, ] <- H[und, ] }
## ch' ch = F; M = ch^-T H ch^-1 is similar to F^-1 H and symmetric.
ch <- chol(Fm); M <- t(backsolve(ch, t(backsolve(ch, H, transpose = TRUE)), transpose = TRUE))
lam <- eigen((M + t(M)) / 2, symmetric = TRUE)
cat(sprintf("  eigenvalues of F^-1 H: min %.3f  max %.3f  -> Newton rate %.3f\n",
  min(lam$values), max(lam$values), max(abs(1 - lam$values))))
cat("  smallest five:", paste(sprintf("%.3f", tail(lam$values, 5)), collapse = " "), "\n")
cat("  largest five: ", paste(sprintf("%.3f", head(lam$values, 5)), collapse = " "), "\n")
blocks <- lay[vapply(lay, length, integer(1)) > 0L]
worst <- c(which.min(lam$values), which.max(lam$values))
for(w in worst){
  v <- backsolve(ch, lam$vectors[, w]); v <- v / sqrt(sum(v^2))
  share <- vapply(blocks, function(ix) sum(v[ix]^2), numeric(1))
  cat(sprintf("  direction lambda=%.3f loads on: %s\n", lam$values[w],
    paste(sprintf("%s %.2f", names(share), share)[share > .01], collapse = ", ")))
}
## The same for the block-diagonal item information the polish uses.
re <- G("bigIRT_laplace_row_effective")(state = e$state, sdat = sdat,
  thetaBase = e$posterior$theta_mode, rows = ctx$train_rows, context = ctx$row_context)
bl <- G("bigIRT_item_info_blocks")(state = e$state, sdat = sdat, context = ctx,
  row_effective = re, posterior = e$posterior, thetaBase = e$posterior$theta_mode)
ps <- attr(lay, "par_scale")
Bd <- diag(diag(H))
for(j in seq_len(bl$ni)){
  idx <- vapply(bl$active, function(b) lay[[b]][j], integer(1))
  Bd[idx, idx] <- (matrix(bl$info[, , j], bl$P, bl$P) + diag(bl$prior_precision[, j], bl$P)) * outer(ps[idx], ps[idx])
}
chb <- chol(Bd); Mb <- t(backsolve(chb, t(backsolve(chb, H, transpose = TRUE)), transpose = TRUE))
lb <- eigen((Mb + t(Mb)) / 2, symmetric = TRUE, only.values = TRUE)$values
cat(sprintf("  block-diagonal item info (polish metric): eigen range of B^-1 H [%.3f, %.3f] -> rate %.3f, condition %.1f\n",
  min(lb), max(lb), max(abs(1 - lb)), max(lb) / min(lb)))
cat(sprintf("  Fisher: condition of F^-1 H %.2f\n", max(lam$values) / min(lam$values)))
cat("PROBE_DONE\n")
