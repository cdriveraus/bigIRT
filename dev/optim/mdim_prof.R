# Where a many-dimensional Laplace fit spends its time.
#   Rscript mdim_prof.R <K> <N> <items_per_dim> <pl> <cores>
args <- commandArgs(TRUE)
K <- as.integer(args[1]); N <- as.integer(args[2]); J <- as.integer(args[3])
pl <- as.integer(args[4]); cores <- as.integer(args[5])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT")
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
tm <- new.env(); tm$rows <- list()
wrapf <- function(nm){
  orig <- get(nm, ns)
  f <- function(...){ t <- proc.time()[["elapsed"]]; r <- orig(...)
    tm$rows[[length(tm$rows) + 1L]] <- data.table(fn = nm, sec = proc.time()[["elapsed"]] - t,
      person = r$timings$personKernelSec %||% NA_real_, item = r$timings$itemKernelSec %||% NA_real_); r }
  unlockBinding(nm, ns); assign(nm, f, ns); lockBinding(nm, ns)
}
wrapf("bigIRT_laplace_direct_objective")
for(nm in c("bigIRT_laplace_metric_solver")){
  orig <- get(nm, ns)
  f <- function(...){ t <- proc.time()[["elapsed"]]; r <- orig(...)
    tm$rows[[length(tm$rows) + 1L]] <- data.table(fn = "metric", sec = proc.time()[["elapsed"]] - t,
      person = NA_real_, item = NA_real_); r }
  unlockBinding(nm, ns); assign(nm, f, ns); lockBinding(nm, ns)
}
for(refine in c("none", "agq")){
  tm$rows <- list()
  t <- system.time(fit <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = cores, loadings = L,
    marginalApprox = "laplace", estimateAbilityCorr = TRUE, ebayes = FALSE, laplaceRefine = refine,
    dropPerfectScores = FALSE)))[["elapsed"]]
  x <- rbindlist(tm$rows)
  ## metric time includes the objective evaluations it makes; separate them.
  cat(sprintf("refine %-4s total %.1f s | objective evals %d, %.1f s (person %.1f, item/agq %.1f) | metric builds %d, %.1f s incl. evals | other %.1f s\n",
    refine, t, sum(x$fn != "metric"), sum(x$sec[x$fn != "metric"]), sum(x$person, na.rm = TRUE),
    sum(x$item, na.rm = TRUE), sum(x$fn == "metric"), sum(x$sec[x$fn == "metric"]),
    t - sum(x$sec[x$fn != "metric"]) - sum(x$sec[x$fn == "metric"])))
}
