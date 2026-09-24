# Many dimensions with cross loadings: cost of the Laplace fit, and how wrong
# Laplace is per person, against quadrature rules that scale with K.
#   Rscript mdim_study.R <K> <N> <items_per_dim> <pl> <cores> [seed]
# Simulates a correlated K-dimensional model in which each item has a primary
# loading and, with probability 1 - sparsity, cross loadings; fits it with the
# loading pattern (free where the generating loading is nonzero, zero
# elsewhere) and estimated correlations; then at the Laplace optimum scores
# every person's log marginal under Laplace and under antithetic QMC rules of
# increasing size, against a 16,384-point reference.
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5]); seed <- if(length(args) >= 6) as.integer(args[6]) else 1L
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
set.seed(seed)
R <- matrix(0.3, K, K); diag(R) <- 1
## Loadings are positive in bigIRT (softplus), so the generating ones are too:
## a primary loading per item, and cross loadings with probability 0.2 on each
## other dimension.
ni <- K * J
prim <- rep(seq_len(K), each = J)
A0 <- matrix(0, ni, K)
A0[cbind(seq_len(ni), prim)] <- exp(rnorm(ni, log(1.2), 0.25))
cross <- matrix(runif(ni * K) < 0.2, ni, K); cross[cbind(seq_len(ni), prim)] <- FALSE
A0[cross] <- runif(sum(cross), 0.2, 0.5)
sim <- simIRT(Nsubs = N, Nitems = ni, Nscales = K, NitemsAnswered = ni, mirt = TRUE,
  loadings = A0, primaryScale = prim, BSD = 1, AbilityCorr = R,
  logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) 0.5 else 0)
A <- sim$A
L <- ifelse(abs(A) > 0, NA_real_, 0)
dimnames(L) <- list(rownames(A), sort(unique(as.character(sim$dat$Scale))))
cat(sprintf("K %d, N %d, items %d, rows %d, free loadings %d (cross %d)\n", K, N, nrow(A),
  nrow(sim$dat), sum(is.na(L)), sum(is.na(L)) - nrow(A)))
t0 <- proc.time()[["elapsed"]]
fit <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
  marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE,
  laplaceRefine = "none", keepInternals = TRUE, dropPerfectScores = FALSE))
t_fit <- proc.time()[["elapsed"]] - t0
st <- fit$laplaceStatus
cat(sprintf("Laplace fit: %.1f s, %d evaluations, %s iterations, terminate %s, metric %s, npar %d\n",
  t_fit, fit$optim$target_evals, format(fit$optim$iter), st$optimizer_terminate,
  st$optimizer_metric, length(fit$optim$par)))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = TRUE, cores = cores)
lap <- d$eval
ev <- function(rule){
  ctx <- d$context; ctx$agq <- rule; ctx$agq_centre <- lap$posterior
  t <- system.time(e <- G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state,
    sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
    theta_init = lap$posterior$theta_mode, keep_covariance = FALSE, context = ctx))[["elapsed"]]
  list(e = e, sec = t, by = e$item_fg$value_by_subj)
}
one <- list(Z = lapply(seq_len(K), function(dd) matrix(0, 1, dd)),
  log_weights = lapply(seq_len(K), function(dd) 0), desc = "laplace")
ref <- ev(G("bigIRT_agq_rules")(K, points = 16384L, inflate = 1.3))
ref1 <- ev(G("bigIRT_agq_rules")(K, points = 16384L, inflate = 1))
cat(sprintf("two references (16384 points, s = 1.3 and 1): totals differ by %.3f, per-person rmse %.2e\n",
  sum(ref$by - ref1$by), sqrt(mean((ref$by - ref1$by)^2))))
l1 <- ev(one)
cat(sprintf("Laplace (1 node) vs package Laplace: %.2e\n", l1$e$value - lap$value))
out <- data.table(rule = "laplace", M = 1L, sec = l1$sec, total_err = sum(l1$by - ref$by),
  rmse = sqrt(mean((l1$by - ref$by)^2)), max_abs = max(abs(l1$by - ref$by)))
for(M in c(64L, 256L, 1024L, 4096L)) for(s in c(1, 1.15, 1.3, 1.5)){
  r <- ev(G("bigIRT_agq_rules")(K, points = M, inflate = s))
  out <- rbind(out, data.table(rule = sprintf("qmc x%.2f", s), M = M, sec = r$sec, total_err = sum(r$by - ref$by),
    rmse = sqrt(mean((r$by - ref$by)^2)), max_abs = max(abs(r$by - ref$by))))
}
cat(sprintf("reference (16384 points) %.1f s; errors per person against it:\n", ref$sec))
print(out, digits = 3)
saveRDS(list(fit = fit[c("optim", "laplaceStatus")], out = out, A = A, L = L),
  sprintf("mdim_K%d_N%d_pl%d.rds", K, N, pl))
cat("MDIM_DONE\n")
