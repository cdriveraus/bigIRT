# Try the item-wise basin search from an end point.
#   Rscript basin_run.R <regime> <seed> <start.rds> [ref.rds]
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
source(file.path(here, "item_basin.R"))
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceOptimizer = "mize", laplacePolish = 0L))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
sdat <- d$sdat; ctx <- d$context; L <- d$layout; ps <- attr(L, "par_scale")
S0 <- readRDS(args[3]); par0 <- S0$par
ref <- if(length(args) >= 4) readRDS(args[4]) else NULL
th0 <- 0 * d$eval$posterior$theta_mode
ev <- function(par, tol = 1e-10, warm = th0) G("bigIRT_laplace_direct_objective")(par = par, state = d$state,
  sdat = sdat, prior_precision = fit$internals$priorPrecision, theta_init = warm,
  max_iter = 200L, tol = tol, keep_covariance = TRUE, context = ctx)
e0 <- ev(par0)
cat(sprintf("start value %.4f%s\n", e0$value,
  if(!is.null(ref)) sprintf("   reference %.4f (gap %.4f)", ev(ref$par)$value, ev(ref$par)$value - e0$value) else ""))

## Finite-difference check of the cavity gradient on a few items.
cr <- cavity_rows(e0, sdat, ctx)
stopifnot(all(cr$item == as.integer(ctx$row_context$B_ref)))
gh <- statmod::gauss.quad(9, "hermite"); xq <- sqrt(2) * gh$nodes; wq <- gh$weights / sqrt(pi)
for(j in c(1L, 17L, 400L)){
  r <- which(cr$item == j)
  psi <- c(0.3, -0.2, -1, 2)
  a <- item_cavity_fg(psi, cr$mu[r], cr$sd[r], cr$y[r], xq, wq)$g
  fd <- vapply(1:4, function(k){ h <- 1e-5; pp <- psi; pm <- psi; pp[k] <- pp[k] + h; pm[k] <- pm[k] - h
    (item_cavity_fg(pp, cr$mu[r], cr$sd[r], cr$y[r], xq, wq)$f - item_cavity_fg(pm, cr$mu[r], cr$sd[r], cr$y[r], xq, wq)$f) / (2 * h) }, 1)
  cat(sprintf("FD check item %d: max rel err %.2e\n", j, max(abs(a - fd) / pmax(abs(fd), 1e-8))))
}

t0 <- proc.time()[["elapsed"]]
srch <- item_basin_search(e0, sdat, ctx, L)
cat(sprintf("search time %.1f s\n", proc.time()[["elapsed"]] - t0))
cand <- srch$table[gain > 0.05 & dist > 0.1][order(-gain)]

## Does the cavity gain predict the full-objective change of a single switch?
for(k in seq_len(min(5L, nrow(cand)))){
  j <- cand$j[k]
  pj <- apply_switches(par0, srch, L, ps, j)
  ej <- ev(pj, warm = e0$posterior$theta_mode)
  cat(sprintf("item %d alone: cavity gain %.4f, full objective change %.4f\n", j, cand$gain[k], ej$value - e0$value))
}

## Switch every candidate at once, then re-optimise with the package optimiser.
if(nrow(cand)){
  p1 <- apply_switches(par0, srch, L, ps, cand$j)
  e1 <- ev(p1, warm = e0$posterior$theta_mode)
  cat(sprintf("all %d switched: full objective change %.4f\n", nrow(cand), e1$value - e0$value))
  if(e1$value <= e0$value) p1 <- par0
  warm <- e1$posterior$theta_mode
  fg <- function(par){ e <- ev(par, 1e-6, warm); warm <<- e$posterior$theta_mode
    list(f = -e$value, g = -e$approx_grad, ev = e, x = par) }
  gfun <- function(p) -ev(p, 1e-6, warm)$approx_grad
  x0 <- fg(p1)
  m <- G("bigIRT_laplace_metric_solver")(x0, sdat, ctx, L, gfun)
  solver <- function(e) G("bigIRT_laplace_metric_solver")(e, sdat, ctx, L, gfun)
  r <- G("bigIRT_laplace_lbfgs")(p1, fg, minv = m, maxit = 300L, gain_tol = 1e-4,
    refresh = solver, refresh_every = 10L)
  ef <- ev(r$par)
  cat(sprintf("re-optimised: %d iterations (%s), value %.4f, change from start %.4f%s\n",
    r$iter, r$terminate$what, ef$value, ef$value - e0$value,
    if(!is.null(ref)) sprintf(", vs reference %.4f", ef$value - ev(ref$par)$value) else ""))
  ## A second search from the new point: does anything else want to move?
  s2 <- item_basin_search(ev(r$par), sdat, ctx, L)
}
cat("BASIN_RUN_DONE\n")
