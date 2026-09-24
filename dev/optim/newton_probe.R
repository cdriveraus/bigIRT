# Person-mode Newton: iterations, value and time for one build.
#   BIGIRT_LIB=<lib> Rscript newton_probe.R <regime> <seed> <endpoint.rds>
# Evaluates the Laplace objective at the end point from a cold start, then at
# a small perturbation warm-started from there (what every optimiser step
# does), at the default person tolerance 1e-6.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceRefine = "none"))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
par <- readRDS(args[3])$par
ev <- function(p, warm, tol = 1e-6) G("bigIRT_laplace_direct_objective")(par = p, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision, theta_init = warm,
  max_iter = 200L, tol = tol, keep_covariance = FALSE, context = d$context)
th0 <- 0 * d$eval$posterior$theta_mode
set.seed(7); dp <- rnorm(length(par), 0, 0.01)
out <- NULL
for(rep in 1:3){
  t1 <- system.time(e1 <- ev(par, th0))[["elapsed"]]
  t2 <- system.time(e2 <- ev(par + dp, e1$posterior$theta_mode))[["elapsed"]]
  out <- rbind(out, data.frame(cold_sec = t1, cold_iter = mean(e1$posterior$niter),
    cold_value = e1$value, warm_sec = t2, warm_iter = mean(e2$posterior$niter),
    warm_value = e2$value, person_sec = e2$timings$personKernelSec,
    status_bad = sum(e1$posterior$status_code != 0) + sum(e2$posterior$status_code != 0)))
}
tight <- ev(par, th0, 1e-12)$value
cat(sprintf("%s %d lib=%s  cold: %.3f s, %.2f iter, value %.6f (vs tight %.2e) | warm: %.3f s, %.2f iter, person kernel %.3f s | bad status %d\n",
  regime, seed, basename(lib), median(out$cold_sec), out$cold_iter[1], out$cold_value[1],
  out$cold_value[1] - tight, median(out$warm_sec), out$warm_iter[1], median(out$person_sec), out$status_bad[1]))
