devtools::load_all()

set.seed(20260319)
Nsubs <- 700
Nitems <- 36
Nscales <- 3

sim <- bigIRT::simIRT(
  Nsubs = Nsubs,
  Nitems = Nitems,
  Nscales = Nscales,
  NitemsAnswered = c(10, 10, 10),
  mirt = TRUE,
  AMean = 1.0,
  ASD = 0.20,
  loadingSparsity = 0.25,
  crossLoadingSD = 0.20,
  BMean = 0,
  BSD = 0.9,
  logitCMean = -20,
  logitCSD = 0
)

loading_mask <- matrix(0, nrow = nrow(sim$A), ncol = ncol(sim$A))
loading_mask[abs(sim$A) > 1e-8] <- NA_real_
rownames(loading_mask) <- as.character(seq_len(nrow(loading_mask)))
colnames(loading_mask) <- as.character(seq_len(ncol(loading_mask)))

fit <- bigIRT::fitIRT(
  sim$dat,
  pl = 2,
  cores = 1,
  priors = TRUE,
  ebayes = FALSE,
  loadings = loading_mask,
  sampledAbilityStep = TRUE,
  sampledAbilityOuterIter = 1,
  noptimsteps = 1,
  sampledAbilityDiagnostics = FALSE,
  dropPerfectScores = FALSE,
  normalise = FALSE,
  verbose = 0,
  plot = FALSE
)

sdat <- fit$dat
layout <- bigIRT:::bigIRT_param_layout(sdat)
priorSD <- pmax(as.numeric(sdat$AbilitySD), 1e-6)
priorCov <- diag(priorSD, length(priorSD)) %*% sdat$AbilityCorr %*% diag(priorSD, length(priorSD))
priorPrec <- solve(priorCov + diag(1e-6, nrow(priorCov)))

f <- function() {
  bigIRT:::bigIRT_person_posterior_and_sigma(
    fit = fit,
    sdat = sdat,
    layout = layout,
    jitter = 1e-6,
    sigmaScale = 0.5,
    priorPrec = priorPrec
  )
}

for(i in 1:3) f()

elapsed <- system.time(for(i in 1:20) f())
print("Elapsed for 20 calls (sec)")
print(elapsed)

pfile <- tempfile(fileext = ".out")
Rprof(pfile, interval = 0.001)
for(i in 1:20) f()
Rprof(NULL)
s <- summaryRprof(pfile)

print("Top by total")
print(head(s$by.total, 20))
print("Top by self")
print(head(s$by.self, 20))

rowLoadings <- fit$pars$row_loadings
print("rowLoadingsToSparse x20 (sec)")
print(system.time(for(i in 1:20) bigIRT::rowLoadingsToSparse(rowLoadings)))

sparseLoadings <- bigIRT::rowLoadingsToSparse(rowLoadings)
print("bigIRT_person_sigma_points_cpp dense loadings x20 (sec)")
print(system.time(for(i in 1:20){
  bigIRT:::bigIRT_person_sigma_points_cpp(
    id = sdat$id,
    theta_mean = fit$pars$Ability,
    b = fit$pars$b_row,
    c = fit$pars$c_row,
    d = fit$pars$d_row,
    loadings = rowLoadings,
    prior_precision = priorPrec,
    jitter = 1e-6,
    sigma_scale = 0.5
  )
}))

print("bigIRT_person_sigma_points_cpp sparse loadings x20 (sec)")
print(system.time(for(i in 1:20){
  bigIRT:::bigIRT_person_sigma_points_cpp(
    id = sdat$id,
    theta_mean = fit$pars$Ability,
    b = fit$pars$b_row,
    c = fit$pars$c_row,
    d = fit$pars$d_row,
    loadings = sparseLoadings,
    prior_precision = priorPrec,
    jitter = 1e-6,
    sigma_scale = 0.5
  )
}))
