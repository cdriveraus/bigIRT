# Would progressive batching over persons pay? The ctsem growth test, measured.
#
#   Rscript batch_probe.R <regime> <seed>
#
# A batch of m of the N persons gives the scaled gradient
#   G_b = (N/m) * (likelihood gradient over the batch) + prior gradient,
# and a step on it predicts a gain of G_b' M^-1 G_b / 2. The batch is useful
# only while the sampling noise in that predicted gain, tr(M^-1 Cov G_b) / 2,
# is small against the gain itself: ctsem grows the batch as soon as
# noise > theta * gain, theta = 0.25.
#
# Here both are measured directly, at three points on a real fit's path (the
# start, after 3 iterations, after 10), by splitting the persons into N/m
# disjoint random batches and comparing each batch gradient with the full
# one, in the block-diagonal metric M. With M close to the inverse Hessian the
# noise is about n (N/m - 1) / 2 for n parameters, which for an IRT model --
# every item parameter estimated from its own few hundred responses -- is
# large; this checks that against the measured gain.
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
dat <- bench_data(regime, seed)
point <- function(iters){
  fa <- c(list(dat = dat), bench_fit_args(regime), list(keepInternals = TRUE,
    laplaceOuterIter = max(iters, 2), laplaceOptimizer = "mize", laplacePolish = 0L,
    laplacePersonTol = 1e-6))
  f <- suppressWarnings(do.call(fitIRT, fa))
  d <- G("bigIRT_direct_eval_from_fit")(f, keep_covariance = FALSE)
  if(iters == 0){
    st0 <- G("bigIRT_laplace_initial_state")(d$sdat, eps = 1e-6, corr_paramization = "stan_corsqrt")
    d$state <- st0; d$par <- G("bigIRT_laplace_pack_direct_state")(st0, d$sdat, layout = d$layout)
  }
  list(d = d, f = f)
}
out <- list()
for(it in c(0, 3, 10)){
  P <- point(it); d <- P$d; f <- P$f
  sdat <- d$sdat; lay <- d$layout
  il <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
  ps <- attr(lay, "par_scale")
  evb <- function(rows, th){
    rc <- G("bigIRT_laplace_row_context")(sdat, rows = rows)
    ctx <- G("bigIRT_laplace_item_context")(sdat, layout = il, row_context = rc)
    ctx$grain_size <- d$context$grain_size; ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- FALSE; ctx$corr_paramization <- d$context$corr_paramization
    e <- G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state, sdat = sdat,
      prior_precision = f$internals$priorPrecision, theta_init = th, tol = 1e-6, context = ctx)
    pg <- numeric(length(d$par)); pg[seq_along(e$prior$grad)] <- e$prior$grad * ps[seq_along(e$prior$grad)]
    list(lik = e$approx_grad - pg, prior = pg, ev = e)
  }
  th <- matrix(0, sdat$Nsubs, sdat$Nscales)
  full <- evb(d$context$train_rows, th)
  gF <- full$lik + full$prior
  ops <- sm_ops(full$ev, sdat, d$context, lay)
  Mi <- ops$bd
  ## gamma: scale the metric so its predicted step is a Newton step along g
  ## (s'y/y'My is not available at a single point); use the ratio g'M g / g'F g.
  Mg <- Mi(gF); gam <- sum(gF * Mg) / sum(Mg * ops$Fv(Mg))
  gain <- 0.5 * gam * sum(gF * Mg)
  ids <- sdat$id[d$context$train_rows]
  persons <- unique(ids); Np <- length(persons)
  for(frac in c(1/8, 1/4, 1/2)){
    set.seed(1)
    grp <- sample(rep_len(seq_len(round(1 / frac)), Np))
    names(grp) <- persons
    noise <- c()
    for(k in seq_len(round(1 / frac))){
      rows <- d$context$train_rows[grp[as.character(ids)] == k]
      b <- evb(rows, th)
      m <- sum(grp == k)
      Gb <- (Np / m) * b$lik + b$prior
      dG <- Gb - gF
      noise <- c(noise, 0.5 * gam * sum(dG * Mi(dG)))
    }
    out[[length(out) + 1L]] <- data.table(regime = regime, seed = seed, iter = it, frac = frac,
      n = length(d$par), N = Np, gain = gain, noise = mean(noise),
      ratio = mean(noise) / gain, predicted_noise = 0.5 * length(d$par) * (1 / frac - 1))
  }
}
r <- rbindlist(out)
saveRDS(r, sprintf("batch_%s_%d.rds", regime, seed))
print(r, digits = 3)
cat("BATCH_DONE\n")
