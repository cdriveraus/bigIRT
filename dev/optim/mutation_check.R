# Can test-laplace-optimizer.R's second test fail? Run its core with the
# Fisher assembly deliberately broken in two ways and report the errors it
# measures; each should exceed the test's 0.1 bar.
#   Rscript mutation_check.R      (BIGIRT_LIB -> a build with laplaceOptimizer.R)
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
ns <- asNamespace("bigIRT")
orig <- get("bigIRT_laplace_fisher_info", ns)
src <- paste(deparse(orig), collapse = "\n")
mutants <- list(
  no_coupling = sub("Matrix::crossprod(D$V) - Matrix::crossprod(Ipi,", "Matrix::crossprod(D$V) - 0 * Matrix::crossprod(Ipi,", src, fixed = TRUE),
  mean_sign = sub("rep(-P0[, kk], times = N)", "rep(P0[, kk], times = N)", src, fixed = TRUE)
)
run_core <- function(estMeans, preds){
  set.seed(5)
  args <- list(Nsubs = 800, Nitems = 10, AMean = 1.2, ASD = .3, BSD = 1, logitCMean = -20, logitCSD = 0)
  if(preds){ args$personPreds <- matrix(rnorm(800 * 2), ncol = 2); args$AbilityPredEffects <- matrix(c(.5, -.4), 1) }
  s <- do.call(simIRT, args)
  fa <- list(dat = s$dat, pl = 2, marginalApprox = "laplace", cores = 1, ebayes = FALSE,
    keepInternals = TRUE, estMeans = estMeans)
  if(preds) fa$personPreds <- c("V1", "V2")
  f <- suppressWarnings(do.call(fitIRT, fa))
  d <- ns$bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
  gr <- function(p) -ns$bigIRT_laplace_direct_objective(par = p, state = d$state, sdat = d$sdat,
    prior_precision = f$internals$priorPrecision, theta_init = d$eval$posterior$theta_mode,
    keep_covariance = TRUE, context = d$context, tol = 1e-10, max_iter = 200L)$approx_grad
  n <- length(d$par); H <- matrix(0, n, n)
  for(j in seq_len(n)){ xp <- d$par; xm <- d$par; xp[j] <- xp[j] + 1e-4; xm[j] <- xm[j] - 1e-4
    H[, j] <- (gr(xp) - gr(xm)) / 2e-4 }
  H <- (H + t(H)) / 2
  list(d = d, H = H)
}
cases <- list(list(estMeans = c("A", "B", "C", "D"), preds = TRUE), list(estMeans = c("A", "ability"), preds = FALSE))
for(cs in cases){
  R <- run_core(cs$estMeans, cs$preds)
  for(nm in c("original", names(mutants))){
    fn <- if(nm == "original") orig else { f <- eval(parse(text = mutants[[nm]])); environment(f) <- ns; f }
    if(nm != "original" && identical(mutants[[nm]], src)) { cat(nm, ": MUTATION DID NOT APPLY\n"); next }
    Fm <- as.matrix(fn(R$d$eval, R$d$sdat, R$d$context, R$d$layout))
    tot <- sqrt(sum((Fm - R$H)^2)) / sqrt(sum(R$H^2))
    blk <- sapply(c("ability_mean", "ability_beta"), function(b){ ix <- R$d$layout[[b]]
      if(!length(ix)) NA else max(abs(Fm[ix, ] - R$H[ix, ])) / max(abs(R$H[ix, ])) })
    cat(sprintf("%-22s %-12s whole %.3f   ability_mean %.3f   ability_beta %.3f\n",
      paste(cs$estMeans, collapse = ""), nm, tot, blk[1], blk[2]))
  }
}
cat("MUTATION_DONE\n")
