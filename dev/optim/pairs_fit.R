# Full refinement with the pairwise rule (options(bigIRT.agq_pairs = TRUE)),
# against the 16,384-point QMC optimum cached by mdim_pairs.R in the same
# directory (or PAIRS_REF points, below). Pointwise accuracy is not the test -- every approximation tried
# at K >= 4 was accurate pointwise and still exploited; this is.
#   Rscript pairs_fit.R <K> <N> <items_per_dim> <pl> <cores> <nodes...>
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5]); nodes <- as.integer(args[-(1:5)])
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
## PAIRS_N3 (environment): nodes of the three-way correction, 0 for none.
options(bigIRT.agq_default_inflate = 1.15, bigIRT.laplace2_floor = 0,
  bigIRT.agq_nodes3 = as.integer(Sys.getenv("PAIRS_N3", "3")))
fitv <- function(refine, nodes = NULL, points = NULL){
  t <- system.time(f <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
    marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE,
    laplaceRefine = refine, agqNodes = nodes, agqPoints = points, keepInternals = TRUE,
    dropPerfectScores = FALSE)))[["elapsed"]]
  f$sec <- t; f
}
## PAIRS_REF=<M> (environment): measure against an M-point QMC optimum,
## fitted and cached here if missing, and report the 16,384-point optimum's
## own distance from it.
refM <- as.integer(Sys.getenv("PAIRS_REF", "16384"))
ref_file <- function(M) sprintf("pairs_fit_K%d_pl%d_qmc%d.rds", K, pl, M)
if(!file.exists(ref_file(refM))){
  rf0 <- fitv("agq", points = refM)
  saveRDS(list(par = rf0$optim$par, sec = rf0$sec, refine = rf0$laplaceStatus$refine), ref_file(refM))
}
ref <- readRDS(ref_file(refM))
lap <- fitv("none")
d <- G("bigIRT_direct_eval_from_fit")(lap, keep_covariance = TRUE, cores = cores)
m <- G("bigIRT_laplace_metric_solver")(list(ev = d$eval, x = d$par, g = -d$eval$approx_grad),
  d$sdat, d$context, d$layout, gfun = function(p) stop("no differences expected"))
n <- length(d$par)
se <- sqrt(pmax(vapply(seq_len(n), function(k){ e <- numeric(n); e[k] <- 1; m(e)[k] }, 1), 0))
Ly <- d$layout
report <- function(tag, f, p = f$optim$par){
  dz <- (p - ref$par) / se
  blk <- vapply(c("A", "B", "C", "D", "corr"), function(b) if(length(Ly[[b]]))
    sprintf("%s mean %+.2f max %.2f", b, mean(dz[Ly[[b]]]), max(abs(dz[Ly[[b]]]))) else "", "")
  rf <- f$laplaceStatus$refine
  cat(sprintf("K %d pl %d %-10s %7.1f s  refine evals %4s  reverted %-5s | vs %d-point optimum (SE): %s\n",
    K, pl, tag, f$sec, format(rf$evals %||% NA), isTRUE(rf$reverted), refM,
    paste(blk[nzchar(blk)], collapse = "; ")))
  if(isTRUE(rf$reverted) && !is.null(rf$rejected_par)) {
    dz <- (rf$rejected_par - ref$par) / se
    cat(sprintf("    rejected end point: A max %.1f, B max %.1f SE\n", max(abs(dz[Ly$A])), max(abs(dz[Ly$B]))))
  }
}
report("laplace", lap)
if(refM != 16384L && file.exists(ref_file(16384L)))
  report("qmc16384", list(sec = NA), readRDS(ref_file(16384L))$par)
options(bigIRT.agq_pairs = TRUE)
for(q in nodes){
  f <- fitv("agq", q)
  stopifnot(isTRUE(f$laplaceStatus$agq_rule$pairs))
  report(sprintf("pairs%d+t%s", q, Sys.getenv("PAIRS_N3", "3")), f)
}
cat(sprintf("reference fit %.1f s\n", ref$sec))
cat("PAIRSFIT_DONE\n")
