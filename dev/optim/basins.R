# Two end points of one fit that differ by several nats: which items differ,
# and does the Laplace objective rank them the way the exact marginal does?
#
#   Rscript basins.R <regime> <seed> <A.rds> <B.rds> [Q]
#
# Both points are re-scored with person modes cold-started and solved to 1e-10.
# The exact marginal is computed per person by adaptive Gauss-Hermite
# quadrature centred at the mode with the package's own posterior SD as scale,
# at Q and at 2Q - 1 nodes, so its own error is visible. One dimension only.
#
# Laplace is checked against the package: the sum of the per-person Laplace
# terms computed here must equal the package's value minus its item prior, or
# the comparison is between two different objectives.
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
Q <- if(length(args) >= 5) as.integer(args[5]) else 21L
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceOptimizer = "mize", laplacePolish = 0L))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
sdat <- d$sdat; ctx <- d$context; L <- d$layout; ps <- attr(L, "par_scale")
stopifnot(sdat$Nscales == 1L)
A <- readRDS(args[3]); B <- readRDS(args[4])
th0 <- 0 * d$eval$posterior$theta_mode
ev <- function(par) G("bigIRT_laplace_direct_objective")(par = par, state = d$state,
  sdat = sdat, prior_precision = fit$internals$priorPrecision, theta_init = th0,
  max_iter = 200L, tol = 1e-10, keep_covariance = TRUE, context = ctx)

gh <- statmod::gauss.quad(Q, "hermite"); gh2 <- statmod::gauss.quad(2L * Q - 1L, "hermite")
rows <- ctx$train_rows
ids <- as.integer(ctx$row_context$ids); y <- sdat$score[rows]; N <- sdat$Nsubs

## Per-person log marginal: Laplace (as the package defines it) and AGQ.
person_terms <- function(e){
  re <- G("bigIRT_laplace_row_effective")(state = e$state, sdat = sdat,
    thetaBase = e$posterior$theta_mode, rows = rows, context = ctx$row_context)
  lo <- re$loadings[, 1]; int <- re$eta_row - lo * re$row_ability[, 1]
  cc <- re$c_row; dd <- re$d_row
  m <- as.numeric(e$posterior$theta_mode[, 1])
  s <- sqrt(as.numeric(e$posterior$covariance)[seq(1, by = 1, length.out = N)])
  mu <- as.numeric(e$state$AbilityMean)[1]
  prec <- as.numeric(e$prior_mats$precision)[1]
  ll_at <- function(th){
    p <- cc + (dd - cc) * plogis(lo * th[ids] + int)
    p <- pmin(pmax(p, 1e-300), 1 - 1e-16)
    v <- numeric(N); r <- rowsum(y * log(p) + (1 - y) * log1p(-p), ids)
    v[as.integer(rownames(r))] <- r[, 1]
    v + dnorm(th, mu, 1 / sqrt(prec), log = TRUE)
  }
  lap <- ll_at(m) + 0.5 * log(2 * pi) + log(s)
  agq <- function(g){
    M <- vapply(seq_along(g$nodes), function(q)
      ll_at(m + sqrt(2) * s * g$nodes[q]) + log(g$weights[q]) + g$nodes[q]^2, numeric(N))
    mx <- apply(M, 1, max)
    mx + log(rowSums(exp(M - mx))) + log(sqrt(2) * s)
  }
  list(lap = lap, agq = agq(gh), agq2 = agq(gh2), m = m, s = s, re = re)
}

eA <- ev(A$par); eB <- ev(B$par)
pA <- person_terms(eA); pB <- person_terms(eB)
cat(sprintf("package value      A %.4f   B %.4f   B - A %.4f\n", eA$value, eB$value, eB$value - eA$value))
for(nm in c("A", "B")){
  e <- get(paste0("e", nm)); p <- get(paste0("p", nm))
  cat(sprintf("%s: package Laplace (value - item prior) %.6f, recomputed %.6f, diff %.2e\n",
    nm, e$value - e$prior$value, sum(p$lap), e$value - e$prior$value - sum(p$lap)))
  cat(sprintf("%s: AGQ Q=%d %.6f, Q=%d %.6f (quadrature error %.2e); Laplace error %.4f nats (%.2e per person)\n",
    nm, Q, sum(p$agq), 2L * Q - 1L, sum(p$agq2), sum(p$agq) - sum(p$agq2),
    sum(p$lap) - sum(p$agq2), (sum(p$lap) - sum(p$agq2)) / N))
}
dLap <- (eB$value - eA$value)
dExact <- (sum(pB$agq2) + eB$prior$value) - (sum(pA$agq2) + eA$prior$value)
cat(sprintf("B - A:  Laplace %.4f   exact marginal %.4f\n", dLap, dExact))

## Which items differ, and how.
blk <- function(par, b) if(length(L[[b]])) par[L[[b]]] * ps[L[[b]]] else numeric(0)
it <- data.table(j = seq_along(L$B),
  bA = blk(A$par, "B"), bB = blk(B$par, "B"),
  aA = blk(A$par, "A"), aB = blk(B$par, "A"))
if(length(L$C)){ it[, cA := blk(A$par, "C")]; it[, cB := blk(B$par, "C")] }
for(b in c("B_mean", "A_mean", "C_mean")) if(length(L[[b]]))
  cat(sprintf("  %-7s A %.4f  B %.4f\n", b, blk(A$par, b), blk(B$par, b)))
## Item of each row, and the ability of its takers at A's modes.
item_row <- as.integer(ctx$row_context$B_ref)
tk <- data.table(j = item_row, th = pA$m[ids], y = y)[, .(n = .N, pc = mean(y),
  th_mean = mean(th), th_min = min(th), th_q10 = quantile(th, .1)), by = j]
it <- merge(it, tk, by = "j", all.x = TRUE)
if(length(L$C)){
  it[, dc := cB - cA]
  cat(sprintf("items with |d logit c| > 0.5: %d, > 1: %d, > 2: %d of %d\n",
    sum(abs(it$dc) > 0.5), sum(abs(it$dc) > 1), sum(abs(it$dc) > 2), nrow(it)))
  cat("quantiles of d logit c:\n"); print(quantile(it$dc, c(0, .01, .05, .25, .5, .75, .95, .99, 1)))
  cat("the 15 largest |d logit c|:\n")
  print(it[order(-abs(dc))][1:15, .(j, n, pc = round(pc, 2), th_mean = round(th_mean, 2),
    th_q10 = round(th_q10, 2), aA = round(log1p(exp(aA)), 2), aB = round(log1p(exp(aB)), 2),
    bA = round(bA, 2), bB = round(bB, 2), cA = round(plogis(cA) / 2, 3), cB = round(plogis(cB) / 2, 3))])
  cat("correlation of |d logit c| with n, th_mean:\n")
  print(round(cor(abs(it$dc), it[, .(n, th_mean, pc)]), 3))
}
## Per-person decomposition of the ranking: where do Laplace and exact disagree?
dl <- pB$lap - pA$lap; dx <- pB$agq2 - pA$agq2
cat(sprintf("persons: sum dLaplace %.3f, sum dExact %.3f, sum |dLap - dExact| %.3f\n",
  sum(dl), sum(dx), sum(abs(dl - dx))))
saveRDS(list(items = it, pA = pA[c("lap", "agq", "agq2", "m", "s")], pB = pB[c("lap", "agq", "agq2", "m", "s")],
  priorA = eA$prior$value, priorB = eB$prior$value, vA = eA$value, vB = eB$value),
  sprintf("basins_%s_%d.rds", regime, seed))
cat("BASINS_DONE\n")
