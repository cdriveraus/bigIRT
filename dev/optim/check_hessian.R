# Check curvature code against central differences of the analytic gradient,
# before anything built on it is benchmarked.
#
#   Rscript check_hessian.R
#
# For small 2PL, 3PL and 4PL fits (and 2D, person-covariate cases) it compares,
# at the fitted point and in the optimiser's coordinates:
#   * proto_fisher_hessian (fisher.R), the person-coupled Fisher information,
#   * the package's block-diagonal item information (bigIRT_item_info_blocks),
#     which the Newton polish and the standard errors use,
# against H_fd, the central difference of the gradient with every person mode
# re-solved from a cold start to 1e-10.
#
# Expected: for the 2PL, Fisher = observed information, so the Fisher matrix
# should match H_fd up to the second derivative of the log-determinant terms
# (small); for 3PL/4PL it differs by the usual Fisher-observed gap, which
# vanishes in expectation. The item-diagonal blocks of the Fisher matrix and of
# the package's info blocks are the same construction and should agree with
# each other exactly -- any block where they do not points at one of the two.
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "fisher.R"))
ns <- asNamespace("bigIRT")
environment(proto_fisher_hessian) <- ns
G <- function(nm) get(nm, ns)

setup <- function(fit, est_corr = FALSE){
  it <- fit$internals; sdat <- it$sdat
  lay <- G("bigIRT_laplace_direct_layout")(sdat, estimateAbilityCorr = est_corr)
  il <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
  ctx <- G("bigIRT_laplace_item_context")(sdat, layout = il)
  ctx$grain_size <- G("bigIRT_laplace_subject_grain")(sdat$Nsubs, 1L)
  ctx$direct_layout <- lay; ctx$estimateAbilityCorr <- est_corr
  ctx$corr_paramization <- "stan_corsqrt"
  st <- it$state; st$laplaceCorrParam <- "stan_corsqrt"
  pp <- G("bigIRT_laplace_prior_mats")(sdat, jitter = 1e-6)$precision_array
  th0 <- matrix(0, sdat$Nsubs, sdat$Nscales)
  ev <- function(par) G("bigIRT_laplace_direct_objective")(par = par, state = st,
    sdat = sdat, prior_precision = pp, theta_init = th0, jitter = 1e-6,
    max_attempts = 8L, max_iter = 200L, tol = 1e-10, keep_covariance = TRUE, context = ctx)
  list(sdat = sdat, lay = lay, ctx = ctx, ev = ev, par = fit$optim$par)
}

fd_hessian <- function(S, h = 1e-4){
  x <- S$par; n <- length(x)
  H <- matrix(0, n, n)
  for(j in seq_len(n)){
    xp <- x; xm <- x; xp[j] <- x[j] + h; xm[j] <- x[j] - h
    H[, j] <- -(S$ev(xp)$approx_grad - S$ev(xm)$approx_grad) / (2 * h)
  }
  (H + t(H)) / 2
}

pkg_blocks <- function(S, e){
  re <- G("bigIRT_laplace_row_effective")(state = e$state, sdat = S$sdat,
    thetaBase = e$posterior$theta_mode, rows = S$ctx$train_rows, context = S$ctx$row_context)
  bl <- G("bigIRT_item_info_blocks")(state = e$state, sdat = S$sdat, context = S$ctx,
    row_effective = re, posterior = e$posterior, thetaBase = e$posterior$theta_mode)
  n <- length(S$par); ps <- attr(S$lay, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  Hb <- matrix(NA_real_, n, n)
  for(j in seq_len(bl$ni)){
    idx <- vapply(bl$active, function(b) S$lay[[b]][j], integer(1))
    Hb[idx, idx] <- (matrix(bl$info[, , j], bl$P, bl$P) + diag(bl$prior_precision[, j], bl$P)) *
      outer(ps[idx], ps[idx])
  }
  Hb
}

relerr <- function(A, B, idx){
  a <- A[idx, idx, drop = FALSE]; b <- B[idx, idx, drop = FALSE]
  ok <- is.finite(a) & is.finite(b)
  sqrt(sum((a[ok] - b[ok])^2)) / max(sqrt(sum(b[ok]^2)), 1e-300)
}
diag_ratio <- function(A, B, idx) signif(range(diag(A)[idx] / diag(B)[idx]), 3)

cases <- list(
  list(name = "2PL", pl = 2, sim = list(Nsubs = 400, Nitems = 12)),
  list(name = "3PL", pl = 3, sim = list(Nsubs = 400, Nitems = 12, logitCMean = -1.5, logitCSD = .5)),
  list(name = "4PL", pl = 4, sim = list(Nsubs = 400, Nitems = 12, logitCMean = -2, logitCSD = .5,
    logitDMean = 2.5, logitDSD = .5)),
  list(name = "2PL+cov", pl = 2, sim = list(Nsubs = 400, Nitems = 12), preds = TRUE),
  list(name = "2PL 2D", pl = 2, sim = list(Nsubs = 400, Nitems = 8, Nscales = 2))
)
for(cs in cases){
  set.seed(11)
  args <- modifyList(list(AMean = 1.2, ASD = .3, BSD = 1, logitCMean = -20, logitCSD = 0), cs$sim)
  if(isTRUE(cs$preds)){
    args$personPreds <- matrix(rnorm(args$Nsubs * 2), ncol = 2)
    args$AbilityPredEffects <- matrix(c(.4, -.3), 1)
  }
  dat <- do.call(simIRT, args)$dat
  fa <- list(dat = dat, pl = cs$pl, marginalApprox = "laplace", ebayes = FALSE, cores = 1,
    keepInternals = TRUE)
  if(isTRUE(cs$preds)) fa$personPreds <- c("V1", "V2")
  fit <- suppressWarnings(do.call(fitIRT, fa))
  S <- setup(fit)
  e <- S$ev(S$par)
  Hfd <- fd_hessian(S)
  Hgn <- proto_fisher_hessian(e, S$sdat, S$ctx, S$lay)
  Hpk <- pkg_blocks(S, e)
  L <- S$lay
  cat(sprintf("\n== %s  (n = %d parameters)\n", cs$name, length(S$par)))
  cat(sprintf("  Fisher vs FD, whole matrix: rel Frobenius error %.3g\n", relerr(Hgn, Hfd, seq_along(S$par))))
  for(b in c("B", "A", "C", "D", "B_mean", "A_mean", "ability_beta", "ability_mean")){
    idx <- L[[b]]; if(!length(idx)) next
    cat(sprintf("  %-12s Fisher/FD diag ratio [%s]   pkg-block/FD diag ratio [%s]   pkg vs Fisher item-diag rel err %.2g\n",
      b, paste(diag_ratio(Hgn, Hfd, idx), collapse = ", "),
      if(all(is.na(diag(Hpk)[idx]))) "-" else paste(diag_ratio(Hpk, Hfd, idx), collapse = ", "),
      if(all(is.na(diag(Hpk)[idx]))) NA else max(abs(diag(Hpk)[idx] / diag(Hgn)[idx] - 1), na.rm = TRUE)))
  }
  ## Newton step agreement: the direction each curvature would take from a
  ## perturbed point, against the FD Hessian's.
  g <- -e$approx_grad
  set.seed(3); dx <- rnorm(length(S$par), 0, .05)
  st_fd <- solve(Hfd, Hfd %*% dx); st_gn <- solve(Hgn, Hfd %*% dx)
  cat(sprintf("  Newton step from a 0.05 perturbation: |gn - fd| / |fd| = %.3g\n",
    sqrt(sum((st_gn - st_fd)^2)) / sqrt(sum(st_fd^2))))
}
cat("\nCHECK_DONE\n")
