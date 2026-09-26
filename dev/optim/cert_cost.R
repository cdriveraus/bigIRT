# What the convergence certificate costs, and whether the fast route agrees
# with the exact one. One fit per process, one RDS.
#
#   Rscript cert_cost.R study <K1|K2> <rep> <refined|laplace> <outdir>
#   Rscript cert_cost.R scale <persons> <items> <pl> <outdir> [cores]
#
# study: the coverage study's cells (coverage_cell.R: a fixed 40-item 3PL bank,
#   2,000 persons answering every item, K = 2 with the correlation estimated),
#   fitted with package defaults (empirical Bayes on) at cores = 1. Both routes
#   certify the same fitted object afterwards, so their verdicts, gaps and
#   standard errors are compared on identical points.
# scale: a sparse design of Mindsteps' shape -- many persons, each answering
#   25 of many items drawn at random -- fitted with empirical Bayes off (the
#   shortest fit, so the largest overhead fraction), fast route only.
#
# Timings are wall clock in one process; the certificate's is divided by the
# fit's in the same process, so machine load cancels to first order. One
# evaluation of the fitted objective is also timed, to state the certificate
# in evaluations.
args <- commandArgs(trailingOnly = TRUE)
kind <- args[1]
lib <- Sys.getenv("BIGIRT_LIB")
if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT")
cat("LOADED", paste(args, collapse = " "), "\n")

certify_post <- function(fit, route, cores){
  d <- ns$bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE, cores = cores)
  t_eval <- system.time(ns$bigIRT_laplace_direct_objective(par = d$par, state = d$state,
    sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
    theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
    context = d$context))[["elapsed"]]
  sec <- system.time(ce <- ns$bigIRT_certify(final = d$eval, par = d$par, context = d$context,
    state = fit$internals$state, sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
    layout = d$layout, route = route, tolerance = 1e-3))[["elapsed"]]
  se <- if(!is.null(ce$covariance$items)) t(sqrt(apply(ce$covariance$items, 3, diag))) else NULL
  list(status = ce$status, gap = ce$gap, route = ce$route, evaluations = ce$evaluations,
    ntrusted = ce$ntrusted, nnegative = ce$nnegative, nflat = ce$nflat,
    saturated = ce$saturated, sec = sec, eval_sec = t_eval, se = se)
}

if(identical(kind, "study")){
  cell <- args[2]; rep <- as.integer(args[3]); arm <- args[4]; outdir <- args[5]
  bank <- local({
    set.seed(switch(cell, K1 = 7101L, K2 = 7202L))
    ni <- 40L
    a <- pmax(stats::rnorm(ni, 1.2, 0.3), 0.4)
    b <- stats::rnorm(ni, 0, 1)
    x <- stats::rnorm(ni, stats::qlogis(0.4), 0.4)
    scale <- if(identical(cell, "K2")) rep(1:2, each = ni / 2L) else rep(1L, ni)
    list(a = a, b = b, c = stats::plogis(x) / 2, scale = scale, ni = ni, K = max(scale))
  })
  set.seed(100000L * match(cell, c("K1", "K2")) + rep)
  K <- bank$K
  R <- if(K == 2L) matrix(c(1, .5, .5, 1), 2) else diag(1)
  theta <- matrix(stats::rnorm(2000L * K), 2000L, K) %*% chol(R)
  dat <- data.table(id = rep(seq_len(2000L), each = bank$ni), Item = rep(seq_len(bank$ni), times = 2000L))
  dat[, Scale := bank$scale[Item]]
  p <- bank$c[dat$Item] + (1 - bank$c[dat$Item]) *
    stats::plogis(bank$a[dat$Item] * theta[cbind(dat$id, dat$Scale)] - bank$b[dat$Item])
  dat[, score := stats::rbinom(.N, 1, p)]
  cores <- 1L
  fit_sec <- system.time(fit <- suppressWarnings(fitIRT(dat, pl = 3L, marginalApprox = "laplace",
    cores = cores, keepInternals = TRUE, dropPerfectScores = FALSE, verbose = 0L,
    estimateAbilityCorr = K > 1L, laplaceRefine = if(identical(arm, "laplace")) "none" else "auto")))[["elapsed"]]
  fast <- certify_post(fit, "fast", cores)
  exact <- certify_post(fit, "exact", cores)
  out <- list(kind = kind, cell = cell, rep = rep, arm = arm, host = Sys.info()[["nodename"]],
    rows = nrow(dat), npar = length(fit$optim$par), marginal = fit$laplaceStatus$marginal,
    fit_sec = fit_sec, fast = fast, exact = exact)
  f <- file.path(outdir, sprintf("study_%s_%s_%03d.rds", cell, arm, rep))
} else {
  N <- as.integer(args[2]); ni <- as.integer(args[3]); pl <- as.integer(args[4])
  outdir <- args[5]; cores <- if(length(args) >= 6) as.integer(args[6]) else 4L
  J <- 25L
  set.seed(9000L + N %/% 1000L + ni)
  a <- pmax(stats::rnorm(ni, 1.2, 0.3), 0.4); b <- stats::rnorm(ni, 0, 1)
  cc <- if(pl >= 3L) stats::plogis(stats::rnorm(ni, stats::qlogis(0.4), 0.4)) / 2 else rep(0, ni)
  theta <- stats::rnorm(N)
  item <- as.vector(vapply(seq_len(N), function(i) sample.int(ni, J), integer(J)))
  id <- rep(seq_len(N), each = J)
  p <- cc[item] + (1 - cc[item]) * stats::plogis(a[item] * theta[id] - b[item])
  dat <- data.table(id = id, Item = item, Scale = 1L, score = stats::rbinom(length(p), 1, p))
  fit_sec <- system.time(fit <- suppressWarnings(fitIRT(dat, pl = pl, marginalApprox = "laplace",
    cores = cores, keepInternals = TRUE, ebayes = FALSE, verbose = 0L)))[["elapsed"]]
  fast <- certify_post(fit, "fast", cores)
  fast$se <- NULL
  out <- list(kind = kind, persons = N, items = ni, pl = pl, cores = cores,
    host = Sys.info()[["nodename"]], rows = nrow(dat), npar = length(fit$optim$par),
    marginal = fit$laplaceStatus$marginal, fit_sec = fit_sec,
    fit_evals = fit$optim$target_evals, fast = fast)
  f <- file.path(outdir, sprintf("scale_%d_%d_%dPL.rds", N, ni, pl))
}
saveRDS(out, f)
cat(sprintf("DONE %s fit=%.1fs fast=%.1fs (%d evals, %s)%s\n", basename(f), out$fit_sec,
  out$fast$sec, out$fast$evaluations, out$fast$status,
  if(!is.null(out$exact)) sprintf(" exact=%.1fs (%s)", out$exact$sec, out$exact$status) else ""))
