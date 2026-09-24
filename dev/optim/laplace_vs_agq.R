# How far is the Laplace optimum from the optimum of the (essentially) exact
# marginal? One dimension, fixed priors.
#   Rscript laplace_vs_agq.R <regime> <seed> <laplace_endpoint.rds> [Q]
#
# The exact marginal is adaptive Gauss-Hermite quadrature per person, centred at
# the Laplace mode with the Laplace posterior SD, Q nodes (default 21). Its
# gradient is the Fisher identity, d/dpsi log p(y) = E_post[d/dpsi log p(y|theta)],
# with the posterior represented by the normalised node weights. The centring
# moves with psi but the integral does not depend on it, so value and gradient
# agree to the quadrature error; that is checked against central differences.
#
# The AGQ objective is maximised from the Laplace optimum by the package's own
# L-BFGS and metric, and the move is reported per parameter block in units of
# the standard error (sqrt of the diagonal of the inverse Fisher metric).
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2])
Q <- if(length(args) >= 4) as.integer(args[4]) else 21L
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
## The gradient below covers item parameters and the item prior; person-side
## blocks would need their own posterior expectations.
stopifnot(!length(L$ability_mean), !length(L$ability_beta), !length(L$corr))
par_lap <- readRDS(args[3])$par
warm <- 0 * d$eval$posterior$theta_mode
ev <- function(par, tol = 1e-10) {
  e <- G("bigIRT_laplace_direct_objective")(par = par, state = d$state,
    sdat = sdat, prior_precision = fit$internals$priorPrecision, theta_init = warm,
    max_iter = 200L, tol = tol, keep_covariance = TRUE, context = ctx)
  warm <<- e$posterior$theta_mode
  e
}
gh <- statmod::gauss.quad(Q, "hermite")
rows <- ctx$train_rows; rc <- ctx$row_context
ids <- as.integer(rc$ids); y <- sdat$score[rows]; N <- sdat$Nsubs
n <- length(par_lap)
refs <- list(A = rc$A_ref[, 1], B = rc$B_ref, C = rc$C_ref, D = rc$D_ref)

agq <- function(par){
  e <- ev(par)
  st <- e$state
  re <- G("bigIRT_laplace_row_effective")(state = st, sdat = sdat,
    thetaBase = e$posterior$theta_mode, rows = rows, context = rc, include_raw = TRUE)
  lo <- re$loadings[, 1]; int <- re$eta_row - lo * re$row_ability[, 1]
  cc <- re$c_row; dd <- re$d_row; alin <- re$a_linear[, 1]
  m <- as.numeric(e$posterior$theta_mode[, 1])
  s <- sqrt(as.numeric(e$posterior$covariance)[seq_len(N)])
  mu <- as.numeric(st$AbilityMean)[1]; prec <- as.numeric(e$prior_mats$precision)[1]
  ## log integrand at each node, persons x Q
  LW <- matrix(0, N, Q); TH <- matrix(0, N, Q)
  PR <- list()
  for(q in seq_len(Q)){
    th <- m + sqrt(2) * s * gh$nodes[q]
    sg <- plogis(lo * th[ids] + int)
    p <- pmin(pmax(cc + (dd - cc) * sg, 1e-300), 1 - 1e-16)
    v <- numeric(N); r <- rowsum(y * log(p) + (1 - y) * log1p(-p), ids)
    v[as.integer(rownames(r))] <- r[, 1]
    LW[, q] <- v + dnorm(th, mu, 1 / sqrt(prec), log = TRUE) + log(gh$weights[q]) + gh$nodes[q]^2
    TH[, q] <- th
    PR[[q]] <- list(sg = sg, p = p, th = th[ids])
  }
  mx <- apply(LW, 1, max)
  lse <- mx + log(rowSums(exp(LW - mx)))
  val <- sum(lse + log(sqrt(2) * s)) + e$prior$value
  W <- exp(LW - lse)                       # posterior node weights, rows sum to 1
  g <- numeric(n)
  for(q in seq_len(Q)){
    z <- PR[[q]]; wq <- W[ids, q]
    dl <- (y / z$p - (1 - y) / (1 - z$p)) * wq
    ds <- (dd - cc) * z$sg * (1 - z$sg)
    parts <- list(
      A = dl * ds * z$th * plogis(alin),
      B = -dl * ds,
      C = dl * (1 - z$sg) * cc * (1 - 2 * cc),
      D = dl * z$sg * (2 * dd - 1) * (1 - dd))
    for(b in names(parts)){
      idx <- L[[b]]; ref <- refs[[b]]
      if(!length(idx)) next
      k <- ref > 0L
      a <- rowsum(parts[[b]][k], ref[k])
      g[idx[as.integer(rownames(a))]] <- g[idx[as.integer(rownames(a))]] + a[, 1]
    }
  }
  g <- g + e$prior$grad[seq_len(n)] * (seq_len(n) <= length(e$prior$grad))
  list(value = val, grad = g * ps, lap = e$value, lap_grad = e$approx_grad, ev = e)
}

a0 <- agq(par_lap)
## Central-difference check of the AGQ gradient in a few coordinates.
set.seed(1); chk <- sort(sample.int(n, min(n, 6L)))
fd <- vapply(chk, function(k){ h <- 1e-4; pp <- par_lap; pm <- par_lap
  pp[k] <- pp[k] + h; pm[k] <- pm[k] - h; (agq(pp)$value - agq(pm)$value) / (2 * h) }, 1)
cat(sprintf("AGQ gradient vs central differences: max abs diff %.2e (max |g| %.2e)\n",
  max(abs(fd - a0$grad[chk])), max(abs(a0$grad[chk]))))
cat(sprintf("at the Laplace optimum: Laplace %.4f, AGQ %.4f (diff %.4f); |g_lap| %.2e, |g_agq| %.2e\n",
  a0$lap, a0$value, a0$value - a0$lap, sqrt(sum(a0$lap_grad^2)), sqrt(sum(a0$grad^2))))

fg <- function(par){ a <- agq(par); list(f = -a$value, g = -a$grad, ev = a$ev, x = par) }
gfun <- function(p) -agq(p)$grad
x0 <- fg(par_lap)
x0$ev$approx_grad <- a0$lap_grad
solver <- function(e) G("bigIRT_laplace_metric_solver")(e, sdat, ctx, L, gfun)
m0 <- solver(x0)
gain0 <- 0.5 * sum(a0$grad * m0(a0$grad))
cat(sprintf("predicted AGQ gain from the Laplace optimum: %.4f nats\n", gain0))
t0 <- proc.time()[["elapsed"]]
r <- G("bigIRT_laplace_lbfgs")(par_lap, fg, minv = m0, maxit = 200L, gain_tol = 1e-6)
a1 <- agq(r$par)
cat(sprintf("AGQ optimum: %d iterations (%s) in %.1f s, AGQ %.4f (+%.4f), Laplace there %.4f (%+.4f)\n",
  r$iter, r$terminate$what, proc.time()[["elapsed"]] - t0, a1$value, a1$value - a0$value, a1$lap, a1$lap - a0$lap))

## Standard errors from the inverse Fisher metric at the Laplace optimum.
if(n <= 1500){
  Finv <- vapply(seq_len(n), function(k){ e1 <- numeric(n); e1[k] <- 1; m0(e1)[k] }, 1)
  se <- sqrt(pmax(Finv, 0))
  dz <- (r$par - par_lap) / se
  for(b in c("A", "B", "C", "D", "A_mean", "B_mean", "C_mean", "D_mean")){
    ix <- L[[b]]; if(!length(ix)) next
    cat(sprintf("  %-6s move in SE units: mean %+.3f  sd %.3f  max |.| %.3f  (n %d)\n",
      b, mean(dz[ix]), if(length(ix) > 1) sd(dz[ix]) else 0, max(abs(dz[ix])), length(ix)))
  }
  ## Discrimination on the natural scale.
  aL <- log1p(exp(par_lap[L$A] * ps[L$A])); aQ <- log1p(exp(r$par[L$A] * ps[L$A]))
  cat(sprintf("  loading: Laplace mean %.4f, AGQ mean %.4f, mean ratio AGQ/Laplace %.4f\n",
    mean(aL), mean(aQ), mean(aQ / aL)))
  saveRDS(list(dz = dz, par_lap = par_lap, par_agq = r$par, layout = L, ps = ps, se = se),
    sprintf("agq_%s_%d.rds", regime, seed))
}
if(n > 1500) saveRDS(list(par_lap = par_lap, par_agq = r$par, layout = L, ps = ps, value = a1$value),
  sprintf("agq_%s_%d_%s", regime, seed, basename(args[3])))
cat("AGQ_DONE\n")
