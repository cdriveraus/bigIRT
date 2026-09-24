# How far are Laplace estimates from the exact-marginal ones at many
# dimensions, and how many quasi-Monte Carlo points does the refinement need?
#   Rscript mdim_refine.R <K> <N> <items_per_dim> <pl> <cores> [seed]
# Same simulation as mdim_study.R. Fits Laplace only, then refined fits with
# 256, 1024 and 4096 points (inflation 1.15), and reports each parameter
# block's distance from the 4096-point optimum in standard errors (from the
# inverse Fisher metric at the Laplace optimum), with time and evaluations.
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5]); seed <- if(length(args) >= 6) as.integer(args[6]) else 1L
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
set.seed(seed)
R <- matrix(0.3, K, K); diag(R) <- 1
ni <- K * J
prim <- rep(seq_len(K), each = J)
A0 <- matrix(0, ni, K)
A0[cbind(seq_len(ni), prim)] <- exp(rnorm(ni, log(1.2), 0.25))
cross <- matrix(runif(ni * K) < 0.2, ni, K); cross[cbind(seq_len(ni), prim)] <- FALSE
A0[cross] <- runif(sum(cross), 0.2, 0.5)
sim <- simIRT(Nsubs = N, Nitems = ni, Nscales = K, NitemsAnswered = ni, mirt = TRUE,
  loadings = A0, primaryScale = prim, BSD = 1, AbilityCorr = R,
  logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) 0.5 else 0)
L <- ifelse(abs(sim$A) > 0, NA_real_, 0)
dimnames(L) <- list(rownames(sim$A), sort(unique(as.character(sim$dat$Scale))))
fitv <- function(refine, points = NULL){
  t <- system.time(f <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
    marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE,
    laplaceRefine = refine, agqPoints = points, keepInternals = TRUE,
    dropPerfectScores = FALSE)))[["elapsed"]]
  f$sec <- t; f
}
options(bigIRT.agq_default_inflate = 1.15)
fits <- list(laplace = fitv("none"))
for(M in c(256L, 1024L, 4096L)) fits[[paste0("qmc", M)]] <- fitv("agq", M)
d <- G("bigIRT_direct_eval_from_fit")(fits$laplace, keep_covariance = TRUE, cores = cores)
m <- G("bigIRT_laplace_metric_solver")(list(ev = d$eval, x = d$par, g = -d$eval$approx_grad),
  d$sdat, d$context, d$layout, gfun = function(p) stop("no differences expected"))
n <- length(d$par)
se <- sqrt(pmax(vapply(seq_len(n), function(k){ e <- numeric(n); e[k] <- 1; m(e)[k] }, 1), 0))
ref <- fits$qmc4096$optim$par
Ly <- d$layout
for(nm in names(fits)){
  f <- fits[[nm]]; dz <- (f$optim$par - ref) / se
  blk <- vapply(c("A", "B", "C", "corr"), function(b) if(length(Ly[[b]]))
    sprintf("%s mean %+.2f max %.2f", b, mean(dz[Ly[[b]]]), max(abs(dz[Ly[[b]]]))) else "", "")
  cat(sprintf("%-8s %6.1f s  evals %4d  refine evals %s  | vs 4096-point optimum (SE units): %s\n",
    nm, f$sec, f$optim$target_evals, format(f$laplaceStatus$refine$evals %||% NA),
    paste(blk[nzchar(blk)], collapse = "; ")))
}
aL <- log1p(exp(fits$laplace$optim$par[Ly$A] * attr(Ly, "par_scale")[Ly$A]))
aQ <- log1p(exp(ref[Ly$A] * attr(Ly, "par_scale")[Ly$A]))
cat(sprintf("loadings: mean ratio 4096-point / Laplace %.4f\n", mean(aQ / aL)))
cat("MREF_DONE\n")
