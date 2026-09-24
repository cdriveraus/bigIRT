# Two candidates for 3PL/4PL refinement at K >= 4, scored per person against a
# 65,536-point reference -- not only at the Laplace optimum but at the points
# where the refinements tried so far went wrong. A candidate that is accurate
# at the Laplace optimum and wrong at a runaway end point is what failed
# before; accuracy at the runaway points is the test that matters.
#   Rscript mdim_pairs.R <K> <N> <items_per_dim> <pl> <cores> <n_persons_scored>
# Same simulation as mdim_rules.R / mdim_refine.R. Fits are cached per variant
# (pairs_fit_K*_pl*_<variant>.rds, successes only).
#
# Points scored: the optima of Laplace, laplace2 (known to run away), 256-point
# QMC (known to run away) and 16,384-point QMC (the reference optimum).
#
# In whitened coordinates z (theta = m + B z, S = B B'), delta(z) is the
# departure of the log posterior from its Laplace quadratic, and the ratio of
# the marginal to Laplace is E_N exp(delta). Candidates for log E_N exp(delta):
#   pairQ_axes  sum_{k<l} log I_kl - (K - 2) sum_k log I_k, I_k and I_kl the
#               1-D and 2-D Q-node Gauss-Hermite integrals of exp(delta) on
#               the axes' coordinate lines and planes: the many-body
#               (anchored ANOVA) expansion truncated at pairs. Exact for the
#               E T4 term, misses the three-way part of E T3^2.
#   cvM_add     QMC with the second-order Taylor polynomial as a control
#               variate: sum_j w_j (exp delta_j - h_j) + 1 + c2, with
#               h = 1 + T3 + T4 + T3^2 / 2 and E_N h = 1 + c2 exactly (c2 the
#               laplace2 correction).
#   cvM_ratio   log(1 + c2) + log sum_j w_j exp delta_j - log sum_j w_j h_j.
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
sim <- do.call(simIRT, c(list(Nsubs = N, Nitems = ni, Nscales = K, NitemsAnswered = ni, mirt = TRUE,
  loadings = A0, primaryScale = prim, BSD = 1, AbilityCorr = R,
  logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) 0.5 else 0),
  if(pl >= 4) list(logitDMean = 2, logitDSD = 0.3)))
L <- ifelse(abs(sim$A) > 0, NA_real_, 0)
dimnames(L) <- list(rownames(sim$A), sort(unique(as.character(sim$dat$Scale))))
options(bigIRT.agq_default_inflate = 1.15, bigIRT.laplace2_floor = 0)
fitv <- function(refine, points = NULL) suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores,
  loadings = L, marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE,
  laplaceRefine = refine, agqPoints = points, keepInternals = TRUE, dropPerfectScores = FALSE))
lapfit <- fitv("none")
cached_par <- function(tag, refine, points = NULL){
  f <- sprintf("pairs_fit_K%d_pl%d_%s.rds", K, pl, tag)
  if(file.exists(f)) return(readRDS(f))
  t <- system.time(ft <- fitv(refine, points))[["elapsed"]]
  ## A reverted refinement's end point is the point of interest here.
  rp <- ft$laplaceStatus$refine$rejected_par
  out <- list(par = if(!is.null(rp)) rp else ft$optim$par, sec = t, refine = ft$laplaceStatus$refine)
  saveRDS(out, f); out
}
pts <- list(laplace = list(par = lapfit$optim$par),
  laplace2_opt = cached_par("laplace2", "laplace2"),
  qmc256_opt = cached_par("qmc256", "agq", 256L),
  ref_opt = cached_par("qmc16384", "agq", 16384L))
d <- G("bigIRT_direct_eval_from_fit")(lapfit, keep_covariance = TRUE, cores = cores)
Ly <- d$layout
m0 <- G("bigIRT_laplace_metric_solver")(list(ev = d$eval, x = d$par, g = -d$eval$approx_grad),
  d$sdat, d$context, d$layout, gfun = function(p) stop("no differences expected"))
n <- length(d$par)
se <- sqrt(pmax(vapply(seq_len(n), function(k){ e <- numeric(n); e[k] <- 1; m0(e)[k] }, 1), 0))
evalp <- function(p, rule = NULL, centre = NULL){
  ctx <- d$context; ctx$agq <- rule; ctx$agq_centre <- centre
  G("bigIRT_laplace_direct_objective")(par = p, state = d$state, sdat = d$sdat,
    prior_precision = lapfit$internals$priorPrecision, theta_init = d$eval$posterior$theta_mode,
    keep_covariance = TRUE, context = ctx, tol = 1e-10, max_iter = 4000L)
}
llr <- function(e, yy, c0, d0){ p <- pmin(pmax(c0 + (d0 - c0) * plogis(e), 1e-300), 1 - 1e-16)
  yy * log(p) + (1 - yy) * log1p(-p) }
dl <- function(e, yy, c0, d0, k){ h <- 1e-3
  f <- function(x) llr(x, yy, c0, d0)
  switch(k, (f(e + h) - f(e - h)) / (2 * h),
    (f(e + h) - 2 * f(e) + f(e - h)) / h^2,
    (f(e + 2 * h) - 2 * f(e + h) + 2 * f(e - h) - f(e - 2 * h)) / (2 * h^3),
    (f(e + 2 * h) - 4 * f(e + h) + 6 * f(e) - 4 * f(e - h) + f(e - 2 * h)) / h^4) }
gh <- G("bigIRT_gh_rule")
lse <- function(v) { mx <- max(v); mx + log(sum(exp(v - mx))) }
qrules <- lapply(c(q256 = 256L, q1024 = 1024L), function(M) {
  r <- G("bigIRT_agq_rules")(K, points = M, inflate = 1.15); list(Z = t(r$Z[[K]]), W = r$log_weights[[K]]) })
set.seed(7); scored <- sort(sample(d$sdat$Nsubs, nsc))

score_point <- function(pname, p){
  lap <- evalp(p)
  ref <- evalp(p, G("bigIRT_agq_rules")(K, points = 65536L, inflate = 1.15), lap$posterior)$item_fg$value_by_subj
  ref2 <- evalp(p, G("bigIRT_agq_rules")(K, points = 65536L, inflate = 1.3), lap$posterior)$item_fg$value_by_subj
  re <- G("bigIRT_laplace_row_effective")(state = lap$state, sdat = d$sdat,
    thetaBase = lap$posterior$theta_mode, rows = d$context$train_rows, context = d$context$row_context)
  ids <- as.integer(d$context$row_context$ids); y <- as.numeric(d$sdat$score[d$context$train_rows])
  lo <- matrix(as.numeric(re$loadings), ncol = K); eta0 <- as.numeric(re$eta_row)
  cc <- as.numeric(re$c_row); dd <- as.numeric(re$d_row)
  P0 <- as.matrix(lap$prior_mats$precision); mu <- as.numeric(lap$state$AbilityMean)
  Hp <- array(lap$posterior$precision, c(K, K, d$sdat$Nsubs))
  m_all <- lap$posterior$theta_mode
  res <- rbindlist(lapply(scored, function(i){
    r <- which(ids == i); m <- m_all[i, ]; H <- Hp[, , i]; S <- solve(H)
    a <- lo[r, , drop = FALSE]; e0 <- eta0[r]; yy <- y[r]; c0 <- cc[r]; d0 <- dd[r]
    lpM <- function(TH){ E <- e0 + a %*% (TH - m); D <- TH - mu
      colSums(llr(E, yy, c0, d0)) - 0.5 * colSums(D * (P0 %*% D)) }
    lp0 <- lpM(matrix(m))
    lapv <- lp0 + K / 2 * log(2 * pi) + 0.5 * as.numeric(determinant(S)$modulus)
    delta <- function(B, Z) lpM(m + B %*% Z) - lp0 + 0.5 * colSums(Z^2)
    axes <- list(chol = solve(chol(H)),
      eig = { e <- eigen(S, symmetric = TRUE); e$vectors %*% diag(sqrt(pmax(e$values, 0)), K) },
      data = { B <- t(chol(S)); Dz <- t(B) %*% (H - P0) %*% B; ee <- eigen((Dz + t(Dz)) / 2, symmetric = TRUE)
        B %*% ee$vectors })
    out <- list(laplace = lapv)
    for(Q in c(5L, 7L)){
      g <- gh(Q); z1 <- matrix(g$nodes, 1); w1 <- g$log_weights
      gg <- as.matrix(expand.grid(seq_len(Q), seq_len(Q)))
      z2 <- rbind(g$nodes[gg[, 1]], g$nodes[gg[, 2]]); w2 <- w1[gg[, 1]] + w1[gg[, 2]]
      for(an in if(Q == 5L) names(axes) else "chol"){
        B <- axes[[an]]
        I1 <- vapply(seq_len(K), function(k) lse(w1 + delta(B[, k, drop = FALSE], z1)), 1)
        pr <- utils::combn(K, 2)
        I2 <- vapply(seq_len(ncol(pr)), function(j) lse(w2 + delta(B[, pr[, j]], z2)), 1)
        out[[sprintf("add%d_%s", Q, an)]] <- lapv + sum(I1)
        out[[sprintf("pair%d_%s", Q, an)]] <- lapv + sum(I2) - (K - 2) * sum(I1)
      }
    }
    ## Order three, Cholesky axes: sum I_klm - (K - 3) sum I_kl
    ## + C(K - 2, 2) sum I_k. And the combination p7t3: the 7-node pairwise
    ## value plus the 3-node three-way correction
    ## sum I_klm - (K - 2) sum I_kl + C(K - 1, 2) sum I_k, whose coefficients
    ## sum to zero (it vanishes where three-way interactions do).
    B <- axes$chol; tr <- utils::combn(K, 3); pr <- utils::combn(K, 2)
    for(Q in c(3L, 5L)){
      g <- gh(Q); w1 <- g$log_weights
      g2 <- as.matrix(expand.grid(seq_len(Q), seq_len(Q)))
      g3 <- as.matrix(expand.grid(seq_len(Q), seq_len(Q), seq_len(Q)))
      z2 <- t(matrix(g$nodes[g2], ncol = 2)); w2 <- rowSums(matrix(w1[g2], ncol = 2))
      z3 <- t(matrix(g$nodes[g3], ncol = 3)); w3 <- rowSums(matrix(w1[g3], ncol = 3))
      I1 <- vapply(seq_len(K), function(k) lse(w1 + delta(B[, k, drop = FALSE], matrix(g$nodes, 1))), 1)
      I2 <- vapply(seq_len(ncol(pr)), function(j) lse(w2 + delta(B[, pr[, j]], z2)), 1)
      I3 <- vapply(seq_len(ncol(tr)), function(j) lse(w3 + delta(B[, tr[, j]], z3)), 1)
      out[[sprintf("trip%d_chol", Q)]] <- lapv + sum(I3) - (K - 3) * sum(I2) + choose(K - 2, 2) * sum(I1)
      if(Q == 3L) out$p7t3_chol <- out$pair7_chol + sum(I3) - (K - 2) * sum(I2) + choose(K - 1, 2) * sum(I1)
    }
    l3 <- dl(e0, yy, c0, d0, 3); l4 <- dl(e0, yy, c0, d0, 4)
    Cm <- a %*% S %*% t(a); cd <- diag(Cm)
    c2 <- sum(l4 * cd^2) / 8 + sum(outer(l3, l3) * (3 * outer(cd, cd) * Cm + 2 * Cm^3)) / 24
    out$laplace2 <- lapv + c2
    B <- t(chol(S))
    for(qn in names(qrules)){
      Z <- qrules[[qn]]$Z; W <- qrules[[qn]]$W
      dv <- delta(B, Z); U <- a %*% B %*% Z
      T3 <- colSums(l3 * U^3) / 6; T4 <- colSums(l4 * U^4) / 24
      h <- 1 + T3 + T4 + T3^2 / 2
      out[[sub("q", "qmc", qn)]] <- lapv + lse(W + dv)
      Radd <- sum(exp(W) * (exp(dv) - h)) + 1 + c2
      out[[sub("q", "cv", paste0(qn, "_add"))]] <- lapv + log(max(Radd, 1e-300))
      Sh <- sum(exp(W) * h)
      out[[sub("q", "cv", paste0(qn, "_ratio"))]] <- if(1 + c2 > 0 && Sh > 0)
        lapv + log(1 + c2) + lse(W + dv) - log(Sh) else NA_real_
    }
    as.data.table(c(list(i = i, ref = ref[i]), out))
  }))
  meth <- setdiff(names(res), c("i", "ref"))
  cat(sprintf("\n== %s: %d persons; reference self-check (65,536 points, s = 1.15 vs 1.3) rmse %.2e\n",
    pname, nrow(res), sqrt(mean((ref[scored] - ref2[scored])^2))))
  if(pname != "laplace"){
    dz <- (p - pts$laplace$par) / se
    cat(sprintf("   distance from the Laplace optimum (SE): A max %.1f, B max %.1f\n",
      max(abs(dz[Ly$A])), max(abs(dz[Ly$B]))))
  }
  print(rbindlist(lapply(meth, function(mm) { e <- res[[mm]] - res$ref
    data.table(method = mm, mean = mean(e, na.rm = TRUE), rmse = sqrt(mean(e^2, na.rm = TRUE)),
      max_abs = max(abs(e), na.rm = TRUE), n_na = sum(is.na(e))) })), digits = 3)
}
for(pn in names(pts)) score_point(pn, pts[[pn]]$par)
cat("PAIRS_DONE\n")
