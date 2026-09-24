# Is the better optimum reachable by one item's switch, or only collectively?
# Score hybrids of the two end points A (worse) and B (better) on the full
# objective, and compare with what the cavity objective says for the item.
#   Rscript basin_hybrid.R <regime> <seed> <A.rds> <B.rds> <item>
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2]); jj <- as.integer(args[5])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
ns <- asNamespace("bigIRT"); G <- function(nm) get(nm, ns)
source(file.path(here, "item_basin.R"))
dat <- bench_data(regime, seed)
fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), bench_fit_args(regime),
  list(keepInternals = TRUE, laplaceOuterIter = 2, laplaceOptimizer = "mize", laplacePolish = 0L))))
d <- G("bigIRT_direct_eval_from_fit")(fit, keep_covariance = FALSE)
sdat <- d$sdat; ctx <- d$context; L <- d$layout; ps <- attr(L, "par_scale")
A <- readRDS(args[3])$par; B <- readRDS(args[4])$par
th0 <- 0 * d$eval$posterior$theta_mode
ev <- function(par) G("bigIRT_laplace_direct_objective")(par = par, state = d$state,
  sdat = sdat, prior_precision = fit$internals$priorPrecision, theta_init = th0,
  max_iter = 200L, tol = 1e-10, keep_covariance = TRUE, context = ctx)
eA <- ev(A); eB <- ev(B)
item_idx <- c(L$A[jj], L$B[jj], L$C[jj])
cblock <- c(L$C, L$C_mean)
hy <- list(
  "A" = A,
  "A + item from B" = replace(A, item_idx, B[item_idx]),
  "A + C block and C mean from B, item from A" = replace(replace(A, cblock, B[cblock]), item_idx, A[item_idx]),
  "A + C block and C mean from B, incl. item" = replace(A, cblock, B[cblock]),
  "A + C block, C mean and item from B" = replace(replace(A, cblock, B[cblock]), item_idx, B[item_idx]),
  "B + item from A" = replace(B, item_idx, A[item_idx]),
  "B" = B)
for(nm in names(hy)){
  e <- ev(hy[[nm]])
  cat(sprintf("%-45s value %.4f  (vs A %+.4f)  item prior %.4f\n", nm, e$value, e$value - eA$value, e$prior$value))
}
## The cavity objective for this item at its A and B parameters, under A's and
## under B's cavities.
gh <- statmod::gauss.quad(9, "hermite"); xq <- sqrt(2) * gh$nodes; wq <- gh$weights / sqrt(pi)
for(src in c("A", "B")){
  e <- if(src == "A") eA else eB
  cr <- cavity_rows(e, sdat, ctx); r <- which(cr$item == jj)
  for(pt in c("A", "B")){
    p <- if(pt == "A") A else B
    psi <- c(p[L$A[jj]] * ps[L$A[jj]], p[L$B[jj]] * ps[L$B[jj]], p[L$C[jj]] * ps[L$C[jj]], 30)
    f <- item_cavity_fg(psi, cr$mu[r], cr$sd[r], cr$y[r], xq, wq)$f
    cm <- e$state$logitCMean
    cat(sprintf("cavity(%s) at item params of %s: loglik %.4f, c prior %.4f (C mean %.3f), cavity sd median %.3f\n",
      src, pt, f, -0.5 * ((psi[3] - cm) / sdat$logitCSD)^2, cm, median(cr$sd[r])))
  }
}
cat("HYBRID_DONE\n")
