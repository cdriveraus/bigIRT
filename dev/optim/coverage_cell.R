# One cell of the refined-fit standard-error coverage study: one replication,
# one fit, one RDS.
#
#   Rscript coverage_cell.R <cell> <rep> <arm> <outdir>
#
#   cell  K1 (3PL, one dimension, 40 items) or K2 (3PL, two dimensions, 20
#         items each, latent correlation 0.5 estimated); 2,000 persons, every
#         person answers every item.
#   arm   "refined" (package defaults, so laplaceRefine = "auto", which is
#         adaptive quadrature for a 3PL, and empirical Bayes on),
#         "refined_noeb" (the same with the default priors held fixed,
#         ebayes = FALSE, so less of each estimate is the prior's), or
#         "laplace" (laplaceRefine = "none", empirical Bayes on).
#
# BIGIRT_LIB names the library holding the build under test.
#
# The item bank is drawn once per cell from a fixed seed and held fixed across
# replications; each replication redraws the persons and the responses. So the
# spread of an item's estimate across replications is exactly the sampling
# variability its standard error claims to describe, and coverage is measured
# against a known truth rather than averaged over a redrawn item population.
#
# Responses are simulated here, in fitIRT's own parameterisation
# (p = c + (1 - c) logistic(a theta - b), c = logistic(x) / 2), rather than by
# simIRT, whose B is a difficulty on its legacy path and which redraws the
# items on every call.
#
# Standard errors compared, all on the free-parameter scale (the softplus
# pre-image of a, the intercept b, the logit x of the guessing link), where
# Wald intervals are formed and where no delta-method factor can intervene:
#
#   profile     itemInformation(method = "profile"): the closed-form weight at
#               the fitted point, built on the Laplace person curvature. What
#               tidyIRT() and summary() report.
#   block       each item's block of the exact Hessian of the fitted objective,
#               inverted alone: other items and the shared parameters held at
#               their estimates. What itemInformation(method = "hessian")
#               reports.
#   blockschur  the exact Hessian with the item-item coupling through shared
#               persons dropped, and the item blocks coupled to the shared
#               block (means, correlation) through its Schur complement.
#   full        the full inverse of the exact Hessian: nothing dropped.
#
# The exact Hessian is central differences (step 1e-4) of the analytic
# gradient in every coordinate. On a quadrature-refined fit the nodes are held
# at the fitted centre, where the Fisher-identity gradient is the exact
# gradient of a smooth function and no person mode is re-solved; on a Laplace
# fit the person modes are re-solved to 1e-10 at every perturbed point.

args <- commandArgs(trailingOnly = TRUE)
cell <- args[1]; rep <- as.integer(args[2]); arm <- args[3]
outdir <- if(length(args) >= 4) args[4] else "coverage"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
lib <- Sys.getenv("BIGIRT_LIB")
if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT")
cat("LOADED", cell, rep, arm, "\n")

NPERS <- 2000L
bank <- local({
  set.seed(switch(cell, K1 = 7101L, K2 = 7202L))
  ni <- 40L
  a <- pmax(stats::rnorm(ni, 1.2, 0.3), 0.4)
  b <- stats::rnorm(ni, 0, 1)
  x <- stats::rnorm(ni, stats::qlogis(0.4), 0.4)      # c = plogis(x) / 2, about 0.2
  scale <- if(identical(cell, "K2")) rep(1:2, each = ni / 2L) else rep(1L, ni)
  list(a = a, b = b, x = x, c = stats::plogis(x) / 2, scale = scale, ni = ni,
       K = max(scale), rho = if(identical(cell, "K2")) 0.5 else NA_real_)
})

set.seed(100000L * match(cell, c("K1", "K2")) + rep)
K <- bank$K
R <- if(K == 2L) matrix(c(1, bank$rho, bank$rho, 1), 2) else diag(1)
theta <- matrix(stats::rnorm(NPERS * K), NPERS, K) %*% chol(R)
dat <- data.table(id = rep(seq_len(NPERS), each = bank$ni),
                  Item = rep(seq_len(bank$ni), times = NPERS))
dat[, Scale := bank$scale[Item]]
th_row <- theta[cbind(dat$id, dat$Scale)]
p <- bank$c[dat$Item] + (1 - bank$c[dat$Item]) *
  stats::plogis(bank$a[dat$Item] * th_row - bank$b[dat$Item])
dat[, score := stats::rbinom(.N, 1, p)]

fit_args <- list(dat = dat, score = "score", id = "id", item = "Item",
  scale = "Scale", pl = 3L, marginalApprox = "laplace", cores = 1L,
  keepInternals = TRUE, dropPerfectScores = FALSE, verbose = 0L,
  estimateAbilityCorr = K > 1L,
  ebayes = !identical(arm, "refined_noeb"),
  laplaceRefine = if(identical(arm, "laplace")) "none" else "auto")
if(!arm %in% c("refined", "refined_noeb", "laplace")) stop("unknown arm ", arm)
t_fit <- system.time(fit <- tryCatch(suppressWarnings(do.call(fitIRT, fit_args)),
  error = function(e) conditionMessage(e)))[["elapsed"]]
if(is.character(fit)) {
  saveRDS(list(cell = cell, rep = rep, arm = arm, error = fit),
    file.path(outdir, sprintf("%s_%s_%03d.rds", cell, arm, rep)))
  cat("FAILED", fit, "\n"); quit(save = "no", status = 0)
}
if(!identical(as.character(fit$itemPars$Item), as.character(seq_len(bank$ni))))
  stop("itemPars are not in item order; the truth pairing would be wrong.")

d <- ns$bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE, cores = 1L)
lay <- d$layout; ps <- attr(lay, "par_scale"); n <- length(d$par)
ctx <- d$context
agq <- !is.null(ctx$agq)
if(agq) ctx$agq_centre <- d$eval$posterior
ev <- function(par) ns$bigIRT_laplace_direct_objective(par = par, state = d$state,
  sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
  theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
  context = ctx, tol = 1e-10, max_iter = 500L)
g0 <- ev(d$par)$approx_grad
eps <- 1e-4
t_hess <- system.time({
  H <- matrix(NA_real_, n, n)
  for(k in seq_len(n)){
    up <- d$par; up[k] <- up[k] + eps; dn <- d$par; dn[k] <- dn[k] - eps
    H[, k] <- (ev(up)$approx_grad - ev(dn)$approx_grad) / (2 * eps)
  }
})[["elapsed"]]
asym <- sqrt(sum(((H - t(H)) / 2)^2)) / sqrt(sum(H^2))
I <- -(H + t(H)) / 2                    # information, optimiser coordinates
eig <- eigen(I, symmetric = TRUE, only.values = TRUE)$values
Ssc <- diag(ps, n)                       # u -> natural coordinates
Vfull <- tryCatch(Ssc %*% solve(I) %*% Ssc, error = function(e) NULL)
gap <- tryCatch(0.5 * sum(g0 * solve(I, g0)), error = function(e) NA_real_)

item_blocks <- intersect(c("B", "A", "C"), names(lay))
item_idx <- lapply(seq_len(bank$ni), function(j)
  vapply(item_blocks, function(b) lay[[b]][j], integer(1)))
shared <- setdiff(seq_len(n), unlist(item_idx))

## blockschur: the item-item entries of I set to zero, nothing else changed.
Ibs <- I
for(j in seq_len(bank$ni)) for(k in seq_len(bank$ni)) if(j != k)
  Ibs[item_idx[[j]], item_idx[[k]]] <- 0
Vbs <- tryCatch(Ssc %*% solve(Ibs) %*% Ssc, error = function(e) NULL)
gap_bs <- tryCatch(0.5 * sum(g0 * solve(Ibs, g0)), error = function(e) NA_real_)

se_block <- matrix(NA_real_, bank$ni, length(item_blocks), dimnames = list(NULL, item_blocks))
se_full <- se_bs <- se_block
for(j in seq_len(bank$ni)){
  ix <- item_idx[[j]]
  Vj <- tryCatch(solve(I[ix, ix, drop = FALSE]), error = function(e) NULL)
  if(!is.null(Vj)) se_block[j, ] <- sqrt(pmax(diag(Vj), 0)) * ps[ix]
  if(!is.null(Vfull)) se_full[j, ] <- sqrt(pmax(diag(Vfull)[ix], 0))
  if(!is.null(Vbs)) se_bs[j, ] <- sqrt(pmax(diag(Vbs)[ix], 0))
}
ip <- tryCatch(itemInformation(fit, method = "profile"), error = function(e) NULL)
se_profile <- if(!is.null(ip)) ip$se[, item_blocks, drop = FALSE] else se_block * NA

st <- fit$internals$state
est <- cbind(B = as.numeric(st$Bpars), A = as.numeric(st$invspApars),
             C = as.numeric(st$logitCpars))[, item_blocks, drop = FALSE]
truth <- cbind(B = bank$b, A = log(expm1(bank$a)), C = bank$x)[, item_blocks, drop = FALSE]

corr <- NULL
if(length(lay$corr)){
  ci <- lay$corr
  corr <- list(truth = atanh(bank$rho), est = as.numeric(st$AbilityCorrPars),
    se_full = if(!is.null(Vfull)) sqrt(diag(Vfull)[ci]) else NA_real_,
    se_bs = if(!is.null(Vbs)) sqrt(diag(Vbs)[ci]) else NA_real_,
    se_alone = sqrt(1 / I[ci, ci]) * ps[ci],
    sample_rho = stats::cor(theta[, 1], theta[, 2]))
}

out <- list(cell = cell, rep = rep, arm = arm, host = Sys.info()[["nodename"]],
  n = n, marginal = fit$laplaceStatus$marginal, converged = fit$laplaceStatus$converged,
  reason = fit$laplaceStatus$reason, terminate = fit$laplaceStatus$optimizer_terminate,
  logLik = fit$optim$logLik, sec_fit = t_fit, sec_hess = t_hess,
  hyper = with(fit$internals$sdat, c(BSD = BSDx[1], ASD = invspASD[1], CSD = logitCSD[1])),
  gap = gap, gap_blockschur = gap_bs, asym = asym,
  eig_min = min(eig), eig_max = max(eig), max_abs_grad = max(abs(g0)),
  truth = truth, est = est,
  se = list(profile = se_profile, block = se_block, blockschur = se_bs, full = se_full),
  corr = corr)
saveRDS(out, file.path(outdir, sprintf("%s_%s_%03d.rds", cell, arm, rep)))
cat(sprintf("DONE %s %s %d fit=%.1fs hess=%.1fs n=%d gap=%.2e eig=[%.3g, %.3g] asym=%.1e %s\n",
  cell, arm, rep, t_fit, t_hess, n, gap, min(eig), max(eig), asym, out$marginal))
