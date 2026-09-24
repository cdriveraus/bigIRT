# Objective noise and evaluation cost against the person-mode tolerance.
#
#   Rscript noise_probe.R <regime> <seed>
#
# Fits once (package default), then at the fitted point evaluates the
# objective from three starting points for the person modes -- the fitted
# modes, the modes jittered by N(0, 0.05), and zero -- at each tolerance.
# The spread across starts is the noise an optimiser sees; each is compared
# with a cold evaluation at 1e-12. Each timing is the minimum of 3 repeats.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fa <- c(list(dat = dat), bench_fit_args(regime), list(keepInternals = TRUE))
fit <- suppressWarnings(do.call(fitIRT, fa))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
ev <- function(th, tol) G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision, theta_init = th,
  max_iter = 200L, tol = tol, context = d$context)
th_mode <- d$eval$posterior$theta_mode
set.seed(1); th_jit <- th_mode + matrix(rnorm(length(th_mode), 0, .05), nrow(th_mode))
th_zero <- 0 * th_mode
exact <- ev(th_zero, 1e-12)$value
rows <- list()
for(tol in c(1e-4, 1e-6, 1e-8)){
  for(nm in c("fitted", "jittered", "zero")){
    th <- switch(nm, fitted = th_mode, jittered = th_jit, zero = th_zero)
    secs <- numeric(3)
    for(k in 1:3){ t0 <- proc.time()[["elapsed"]]; e <- ev(th, tol); secs[k] <- proc.time()[["elapsed"]] - t0 }
    rows[[length(rows) + 1L]] <- data.table(regime = regime, seed = seed, tol = tol, start = nm,
      err = e$value - exact, sec = min(secs), person_iter = mean(e$posterior$niter),
      nobs = nrow(dat))
  }
}
r <- rbindlist(rows)
saveRDS(r, sprintf("noise_%s_%d.rds", regime, seed))
print(r[, .(spread = diff(range(err)), max_abs_err = max(abs(err)), sec_jit = sec[start == "jittered"],
  iters_jit = person_iter[start == "jittered"]), by = .(regime, seed, tol)], digits = 3)
cat("NOISE_DONE\n")
