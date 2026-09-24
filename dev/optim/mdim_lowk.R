# Low dimensions: which refinement, and how many nodes? Against the 9^K
# product-rule optimum, in SE units.
#   Rscript mdim_lowk.R <K> <N> <items_per_dim> <pl> <cores>
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5])
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
fitv <- function(refine, nodes = NULL){
  t <- system.time(f <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
    marginalApprox = "laplace", estimateAbilityCorr = K > 1, ebayes = FALSE,
    laplaceRefine = refine, agqNodes = nodes, keepInternals = TRUE, dropPerfectScores = FALSE)))[["elapsed"]]
  f$sec <- t; f
}
fits <- list(laplace = fitv("none"), laplace2 = fitv("laplace2"))
for(q in c(3L, 5L, 7L)) if(q^K <= 400) fits[[paste0("gh", q)]] <- fitv("agq", q)
fits$ref <- fitv("agq", 9L)
d <- G("bigIRT_direct_eval_from_fit")(fits$laplace, keep_covariance = TRUE, cores = cores)
m <- G("bigIRT_laplace_metric_solver")(list(ev = d$eval, x = d$par, g = -d$eval$approx_grad),
  d$sdat, d$context, d$layout, gfun = function(p) stop("no differences expected"))
n <- length(d$par)
se <- sqrt(pmax(vapply(seq_len(n), function(k){ e <- numeric(n); e[k] <- 1; m(e)[k] }, 1), 0))
ref <- fits$ref$optim$par; Ly <- d$layout
for(nm in names(fits)){
  f <- fits[[nm]]; dz <- (f$optim$par - ref) / se
  blk <- vapply(c("A", "B", "C", "corr"), function(b) if(length(Ly[[b]]))
    sprintf("%s mean %+.2f max %.2f", b, mean(dz[Ly[[b]]]), max(abs(dz[Ly[[b]]]))) else "", "")
  cat(sprintf("K %d pl %d %-8s %6.1f s  evals %4d  refine evals %s  | vs 9^K optimum (SE): %s\n",
    K, pl, nm, f$sec, f$optim$target_evals, format(f$laplaceStatus$refine$evals %||% NA),
    paste(blk[nzchar(blk)], collapse = "; ")))
}
cat("LOWK_DONE\n")
