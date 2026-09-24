# Deterministic alternatives to quasi-Monte Carlo at many dimensions, scored
# per person against a 65,536-point reference (two shifts, to show its own
# error). At the Laplace optimum of the mdim_study design.
#   Rscript mdim_rules.R <K> <N> <items_per_dim> <pl> <cores> <n_persons_scored>
#
# Candidates, for a person with mode m and posterior covariance S = V:
#   laplace      lp(m) + d/2 log 2pi + 1/2 log|S|
#   additive(Q)  Laplace + sum_k [ log E exp(delta(z_k e_k)) ] over whitened
#                axes, delta the departure of lp from its quadratic at m,
#                each a Q-node Gauss-Hermite line integral; axes from
#                (chol) the Cholesky factor, (eig) the eigenvectors of S,
#                (data) the eigenvectors of the whitened data curvature.
#   laplace2     second-order Laplace: + sum_r l4_r c_rr^2 / 8
#                + sum_rs l3_r l3_s (3 c_rr c_ss c_rs + 2 c_rs^3) / 24,
#                c_rs = a_r' S a_s, l3/l4 the row derivatives in eta.
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5]); nsc <- as.integer(args[6])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
set.seed(1)
R <- matrix(0.3, K, K); diag(R) <- 1
ni <- K * J; prim <- rep(seq_len(K), each = J)
A0 <- matrix(0, ni, K); A0[cbind(seq_len(ni), prim)] <- exp(rnorm(ni, log(1.2), 0.25))
cross <- matrix(runif(ni * K) < 0.2, ni, K); cross[cbind(seq_len(ni), prim)] <- FALSE
A0[cross] <- runif(sum(cross), 0.2, 0.5)
sim <- simIRT(Nsubs = N, Nitems = ni, Nscales = K, NitemsAnswered = ni, mirt = TRUE,
  loadings = A0, primaryScale = prim, BSD = 1, AbilityCorr = R,
  logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) 0.5 else 0)
L <- ifelse(abs(sim$A) > 0, NA_real_, 0)
dimnames(L) <- list(rownames(sim$A), sort(unique(as.character(sim$dat$Scale))))
fit <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
  marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE,
  laplaceRefine = "none", keepInternals = TRUE, dropPerfectScores = FALSE))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = TRUE, cores = cores)
lap <- d$eval
evr <- function(rule){
  ctx <- d$context; ctx$agq <- rule; ctx$agq_centre <- lap$posterior
  G("bigIRT_laplace_direct_objective")(par = d$par, state = d$state, sdat = d$sdat,
    prior_precision = fit$internals$priorPrecision, theta_init = lap$posterior$theta_mode,
    keep_covariance = FALSE, context = ctx)$item_fg$value_by_subj
}
ref <- evr(G("bigIRT_agq_rules")(K, points = 65536L, inflate = 1.15))
ref2 <- evr(G("bigIRT_agq_rules")(K, points = 65536L, inflate = 1.3))
cat(sprintf("reference: two 65,536-point rules differ by per-person rmse %.2e\n", sqrt(mean((ref - ref2)^2))))

## Per-row pieces at the mode, for the persons scored.
re <- G("bigIRT_laplace_row_effective")(state = lap$state, sdat = d$sdat,
  thetaBase = lap$posterior$theta_mode, rows = d$context$train_rows, context = d$context$row_context)
ids <- as.integer(d$context$row_context$ids); y <- d$sdat$score[d$context$train_rows]
lo <- matrix(as.numeric(re$loadings), ncol = K); eta0 <- as.numeric(re$eta_row); cc <- as.numeric(re$c_row); dd <- as.numeric(re$d_row)
P0 <- as.matrix(lap$prior_mats$precision); mu <- as.numeric(lap$state$AbilityMean)
Hp <- array(lap$posterior$precision, c(K, K, d$sdat$Nsubs))
llr <- function(e, yy, c0, d0){ p <- pmin(pmax(c0 + (d0 - c0) * plogis(e), 1e-300), 1 - 1e-16)
  yy * log(p) + (1 - yy) * log1p(-p) }
## eta derivatives of a row's log-likelihood by central differences.
dl <- function(e, yy, c0, d0, k){ h <- 1e-3
  f <- function(x) llr(x, yy, c0, d0)
  switch(k, (f(e + h) - f(e - h)) / (2 * h),
    (f(e + h) - 2 * f(e) + f(e - h)) / h^2,
    (f(e + 2 * h) - 2 * f(e + h) + 2 * f(e - h) - f(e - 2 * h)) / (2 * h^3),
    (f(e + 2 * h) - 4 * f(e + h) + 6 * f(e) - 4 * f(e - h) + f(e - 2 * h)) / h^4) }
gh <- G("bigIRT_gh_rule")
m_all <- lap$posterior$theta_mode
scored <- sort(sample(d$sdat$Nsubs, nsc))
res <- rbindlist(lapply(scored, function(i){
  r <- which(ids == i); m <- m_all[i, ]; H <- Hp[, , i]; S <- solve(H)
  a <- lo[r, , drop = FALSE]; e0 <- eta0[r]
  lp <- function(th){ sum(llr(as.numeric(e0 + a %*% (th - m)), y[r], cc[r], dd[r])) -
    0.5 * sum((th - mu) * (P0 %*% (th - mu))) }
  lp0 <- lp(m)
  lapv <- lp0 + K / 2 * log(2 * pi) + 0.5 * as.numeric(determinant(S)$modulus)
  axes <- list(chol = solve(chol(H)),
    eig = { e <- eigen(S, symmetric = TRUE); e$vectors %*% diag(sqrt(pmax(e$values, 0)), K) },
    data = { B <- t(chol(S)); Dz <- t(B) %*% (H - P0) %*% B; ee <- eigen((Dz + t(Dz)) / 2, symmetric = TRUE)
      B %*% ee$vectors })
  out <- list(laplace = lapv)
  for(Q in c(5L, 9L)){
    g <- gh(Q); w <- exp(g$log_weights)
    for(an in names(axes)){
      Ax <- axes[[an]]; corr <- 0
      for(k in seq_len(K)){
        v <- vapply(g$nodes, function(z) lp(m + Ax[, k] * z) - lp0 + 0.5 * z^2, 1)
        corr <- corr + log(sum(w * exp(v)))
      }
      out[[sprintf("add%d_%s", Q, an)]] <- lapv + corr
    }
  }
  l3 <- dl(e0, y[r], cc[r], dd[r], 3); l4 <- dl(e0, y[r], cc[r], dd[r], 4)
  C <- a %*% S %*% t(a); cd <- diag(C)
  out$laplace2 <- lapv + sum(l4 * cd^2) / 8 +
    sum(outer(l3, l3) * (3 * outer(cd, cd) * C + 2 * C^3)) / 24
  as.data.table(c(list(i = i, ref = ref[i]), out))
}))
meth <- setdiff(names(res), c("i", "ref"))
cat(sprintf("%d persons scored; per-person error against the reference:\n", nrow(res)))
print(rbindlist(lapply(meth, function(mm) data.table(method = mm,
  mean = mean(res[[mm]] - res$ref), rmse = sqrt(mean((res[[mm]] - res$ref)^2)),
  max_abs = max(abs(res[[mm]] - res$ref))))), digits = 3)
cat("RULES_DONE\n")
