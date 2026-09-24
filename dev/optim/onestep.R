# Can a few Newton steps from the Laplace optimum, taken on a better
# marginal with the Laplace metric held fixed, remove the Laplace bias
# without giving the optimiser room to exploit that marginal? A full
# optimisation of second-order Laplace or small-sample QMC runs away (3PL,
# K >= 2); a step or two cannot go far, and the bias is first order.
#   Rscript onestep.R <K> <N> <items_per_dim> <pl> <cores> [seed]
# Same simulation as mdim_lowk.R / mdim_refine.R. Reference: the 9^K product
# rule optimum (K <= 3) or the 16,384-point QMC optimum, cached in
# onestep_ref_K<K>_pl<pl>_s<seed>.rds. Distances in SE units (inverse
# Fisher metric at the Laplace optimum).
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5]); seed <- if(length(args) >= 6) as.integer(args[6]) else 1L
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
set.seed(seed)
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
fitv <- function(refine, nodes = NULL, points = NULL){
  t <- system.time(f <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
    marginalApprox = "laplace", estimateAbilityCorr = K > 1, ebayes = FALSE,
    laplaceRefine = refine, agqNodes = nodes, agqPoints = points, keepInternals = TRUE,
    dropPerfectScores = FALSE)))[["elapsed"]]
  f$sec <- t; f
}
options(bigIRT.agq_default_inflate = 1.15, bigIRT.laplace2_floor = 0)
lap <- fitv("none")
rf <- sprintf("onestep_ref_K%d_pl%d_s%d.rds", K, pl, seed)
if(file.exists(rf)) ref <- readRDS(rf) else {
  rfit <- if(K <= 3) fitv("agq", nodes = 9L) else fitv("agq", points = 16384L)
  ref <- list(par = rfit$optim$par, sec = rfit$sec); saveRDS(ref, rf)
}
d <- G("bigIRT_direct_eval_from_fit")(lap, keep_covariance = TRUE, cores = cores)
stopifnot(max(abs(d$par - lap$optim$par)) < 1e-8)
m <- G("bigIRT_laplace_metric_solver")(list(ev = d$eval, x = d$par, g = -d$eval$approx_grad),
  d$sdat, d$context, d$layout, gfun = function(p) stop("no differences expected"))
n <- length(d$par)
se <- sqrt(pmax(vapply(seq_len(n), function(k){ e <- numeric(n); e[k] <- 1; m(e)[k] }, 1), 0))
Ly <- d$layout
report <- function(tag, p, extra = ""){
  dz <- (p - ref$par) / se
  blk <- vapply(c("A", "B", "C", "corr"), function(b) if(length(Ly[[b]]))
    sprintf("%s mean %+.2f max %.2f", b, mean(dz[Ly[[b]]]), max(abs(dz[Ly[[b]]]))) else "", "")
  cat(sprintf("K %d pl %d %-16s %s| %s\n", K, pl, tag, extra, paste(blk[nzchar(blk)], collapse = "; ")))
}
evalv <- function(ctx, p, th) G("bigIRT_laplace_direct_objective")(par = p, state = d$state,
  sdat = d$sdat, prior_precision = lap$internals$priorPrecision, theta_init = th,
  jitter = 1e-6, keep_covariance = TRUE, context = ctx)
report("laplace", d$par)
marg <- list(laplace2 = function(cx){ cx$laplace2 <- TRUE; cx })
if(K <= 3) for(q in c(5L, 7L)) marg[[paste0("gh", q)]] <- local({ qq <- q
  function(cx){ cx$agq <- G("bigIRT_agq_rules")(K, nodes = qq); cx } })
if(K >= 2) for(M in c(1024L, 4096L)) marg[[paste0("qmc", M)]] <- local({ MM <- M
  function(cx){ cx$agq <- G("bigIRT_agq_rules")(K, points = MM, inflate = 1.15); cx } })
for(nm in names(marg)){
  cx <- marg[[nm]](d$context); p <- d$par; th <- d$eval$posterior$theta_mode
  t0 <- proc.time()[["elapsed"]]
  for(s in 1:3){
    e <- evalv(cx, p, th)
    stp <- m(e$approx_grad)
    ## the value is the refined objective at the point the step starts from
    p <- p + stp; th <- e$posterior$theta_mode
    report(sprintf("%s step %d", nm, s), p,
      sprintf("value %.3f  pred %.3f  %5.1f s ", e$value, 0.5 * sum(e$approx_grad * stp),
        proc.time()[["elapsed"]] - t0))
  }
}
cat(sprintf("laplace fit %.1f s, reference fit %.1f s\n", lap$sec, ref$sec))
cat("ONESTEP_DONE\n")
