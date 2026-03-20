pkgload::load_all(path = ".", quiet = TRUE)
library(data.table)

set.seed(42)
sim <- simIRT(
  Nsubs = 500,
  Nitems = 60,
  Nscales = 3,
  NitemsAnswered = c(8, 8, 8),
  mirt = TRUE,
  loadingSparsity = 0.25,
  crossLoadingSD = 0.2,
  logitCMean = -20,
  logitCSD = 0
)

Atrue <- sim$A
loading_mask <- matrix(0, nrow = nrow(Atrue), ncol = ncol(Atrue))
loading_mask[abs(Atrue) > 1e-8] <- NA_real_
rownames(loading_mask) <- as.character(seq_len(nrow(loading_mask)))
colnames(loading_mask) <- as.character(seq_len(ncol(loading_mask)))

prof <- tempfile(fileext = ".out")
Rprof(prof, interval = 0.001)
t0 <- proc.time()[["elapsed"]]
fit <- fitIRT(
  sim$dat,
  pl = 2,
  cores = 1,
  priors = TRUE,
  ebayes = FALSE,
  loadings = loading_mask,
  sampledAbilityStep = TRUE,
  sampledAbilityOuterIter = 8,
  noptimsteps = 1,
  sampledAbilityDiagnostics = TRUE,
  dropPerfectScores = FALSE,
  normalise = FALSE,
  verbose = 0,
  plot = FALSE
)
telapsed <- proc.time()[["elapsed"]] - t0
Rprof(NULL)

s <- summaryRprof(prof)
cat("TOTAL_ELAPSED_SEC\n")
print(telapsed)

cat("\nSAMPLED_TIMING_TOTALS\n")
print(colSums(fit$sampledAbilityTiming[
  fit$sampledAbilityTiming$stage == "outer",
  c("person_opt_sec", "posterior_sec", "item_opt_sec", "person_target_evals", "item_target_evals", "person_logprob_evals", "item_logprob_evals", "total_sec")
]))

cat("\nSAMPLED_TIMING_MEAN_PER_OUTER\n")
print(colMeans(fit$sampledAbilityTiming[
  fit$sampledAbilityTiming$stage == "outer",
  c("person_opt_sec", "posterior_sec", "item_opt_sec", "person_target_evals", "item_target_evals", "person_logprob_evals", "item_logprob_evals", "total_sec")
]))
cat("\nSAMPLED_TIMING_BY_OUTER\n")
print(fit$sampledAbilityTiming[fit$sampledAbilityTiming$stage == "outer", ])

cat("\nSAMPLED_STATUS\n")
print(fit$sampledAbilityStatus)

cat("\nSAMPLED_ACCEPT_REJECT_COUNTS\n")
print(fit$sampledAbilityTiming[
  fit$sampledAbilityTiming$stage == "outer",
  c("accepted", "rejected", "reject_reason", "sigmaScaleUsed", "combinedGradNorm")
])

cat("\nTOP_BY_TOTAL\n")
print(head(s$by.total, 25))

cat("\nTOP_BY_SELF\n")
print(head(s$by.self, 25))
