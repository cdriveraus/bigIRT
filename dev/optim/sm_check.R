# Correctness of the scale metrics against the dense Fisher information
# (bigIRT_laplace_fisher_info, itself checked against the numerical Hessian in
# test-laplace-optimizer.R), before anything built on them is timed.
#   * F v (matrix-free) against dense F %*% v, for random v;
#   * Bd against the item-diagonal blocks of dense F (exact by construction);
#   * each preconditioner is SPD: v' M^-1 v > 0, and symmetric: u' M^-1 v = v' M^-1 u;
#   * PCG iterations and Lanczos eigen range of M^-1 F, per preconditioner.
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
source(file.path(here, "scale_metric.R"))
ns <- asNamespace("bigIRT")
for(f in c("sm_ops", "sm_coarse", "sm_precond", "sm_pcg", "sm_minv", "tabulate_sum", "sm_row_q", "sm_block_inverse")){
  g <- get(f); environment(g) <- ns; assign(f, g) }
G <- function(nm) get(nm, ns)
cases <- list(
  list(name = "2PL+cov", pl = 2, preds = TRUE, est = c("A", "B", "C", "D")),
  list(name = "2PL abilitymean", pl = 2, preds = FALSE, est = c("A", "ability")),
  list(name = "3PL", pl = 3, preds = FALSE, est = c("A", "B", "C", "D")),
  list(name = "4PL", pl = 4, preds = FALSE, est = c("A", "B", "C", "D")),
  list(name = "2PL 2D+cov", pl = 2, preds = TRUE, est = c("A", "B", "C", "D"), K = 2))
for(cs in cases){
  set.seed(3)
  args <- list(Nsubs = 1500, Nitems = if(isTRUE(cs$K == 2)) 30 else 60, Nscales = if(isTRUE(cs$K == 2)) 2 else 1,
    NitemsAnswered = if(isTRUE(cs$K == 2)) 10 else 20, AMean = 1.2, ASD = .3, BSD = 1,
    logitCMean = if(cs$pl >= 3) -1.5 else -20, logitCSD = if(cs$pl >= 3) .5 else 0)
  if(cs$pl == 4){ args$logitDMean <- 2.5; args$logitDSD <- .5 }
  if(cs$preds){ args$personPreds <- matrix(rnorm(1500 * 2), ncol = 2); args$AbilityPredEffects <- matrix(c(.5, -.4), args$Nscales, 2, byrow = TRUE) }
  s <- do.call(simIRT, args)
  fa <- list(dat = s$dat, pl = cs$pl, marginalApprox = "laplace", cores = 1, ebayes = FALSE,
    keepInternals = TRUE, estMeans = cs$est, laplaceOuterIter = 5)
  if(cs$preds) fa$personPreds <- c("V1", "V2")
  f <- suppressWarnings(do.call(fitIRT, fa))
  d <- G("bigIRT_direct_eval_from_fit")(f, keep_covariance = TRUE)
  Fd <- as.matrix(G("bigIRT_laplace_fisher_info")(d$eval, d$sdat, d$context, d$layout))
  ops <- sm_ops(d$eval, d$sdat, d$context, d$layout)
  n <- ops$n
  set.seed(1); v <- rnorm(n)
  e1 <- max(abs(ops$Fv(v) - Fd %*% v)) / max(abs(Fd %*% v))
  ## item-diagonal blocks
  L <- d$layout; items <- as.integer(d$sdat$Nitems); worst <- 0
  Bdm <- as.matrix(ops$Bd)
  for(j in seq_len(items)){
    idx <- unlist(lapply(c("B", "A", "C", "D"), function(b) if(length(L[[b]]) == items) L[[b]][j]))
    worst <- max(worst, max(abs(Bdm[idx, idx] - Fd[idx, idx])) / max(abs(Fd[idx, idx])))
  }
  cat(sprintf("\n== %s (n = %d): Fv vs dense rel err %.2e; item-diagonal blocks Bd vs dense F max rel %.2e\n",
    cs$name, n, e1, worst))
  g <- as.numeric(d$eval$approx_grad)
  for(tp in c("bd", "add", "bal")){
    for(m in if(tp == "bd") 1 else c(1, 10, 30)){
      co <- sm_coarse(ops, d$layout, d$context, d$eval$posterior$theta_mode, m)
      M <- sm_precond(ops, co, tp)
      u <- rnorm(n); w <- rnorm(n)
      sym <- abs(sum(u * M(w)) - sum(w * M(u))) / abs(sum(u * M(u)))
      pos <- sum(u * M(u)) > 0
      pc <- sm_pcg(ops$Fv, g, M, rtol = 1e-8)
      ## exact condition of M^-1 F, from the dense matrices
      Mi <- sapply(seq_len(n), function(k) M(diag(n)[, k]))
      lam <- Re(eigen(Mi %*% Fd, only.values = TRUE)$values)
      cat(sprintf("  %-3s m=%-3d symmetric %.1e  SPD %s  PCG iters to 1e-8: %3d  cond(M^-1 F) %.2f  [%.3f, %.3f]\n",
        tp, m, sym, pos, pc$iter, max(lam) / min(lam), min(lam), max(lam)))
    }
  }
  pc <- sm_pcg(ops$Fv, g, function(v) v, rtol = 1e-8, maxit = 2000)
  cat(sprintf("  none      PCG iters to 1e-8: %d\n", pc$iter))
}
cat("SMCHECK_DONE\n")
