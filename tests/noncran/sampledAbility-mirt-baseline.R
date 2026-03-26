if(identical(Sys.getenv("NOT_CRAN"), "true") && .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(data.table)

  set.seed(20260320)

  extract_ordered_A <- function(fit, Nitems, Nscales){
    A <- as.matrix(fit$pars$A)
    idx <- as.integer(rownames(A))
    if(any(is.na(idx))) idx <- seq_len(nrow(A))
    A[idx, seq_len(Nscales), drop = FALSE]
  }

  extract_ordered_theta <- function(fit, Nsubs, Nscales){
    th <- as.matrix(fit$pars$Ability)
    idx <- as.integer(rownames(th))
    if(any(is.na(idx))) idx <- seq_len(nrow(th))
    th[idx, seq_len(Nscales), drop = FALSE]
  }

  align_mirt_solution <- function(theta_est, A_est, theta_true, eps = 1e-8){
    mu_est <- colMeans(theta_est)
    mu_true <- colMeans(theta_true)
    th_est_c <- sweep(theta_est, 2, mu_est, "-")
    th_true_c <- sweep(theta_true, 2, mu_true, "-")

    sd_est <- pmax(apply(th_est_c, 2, sd), eps)
    sd_true <- pmax(apply(th_true_c, 2, sd), eps)

    z_est <- th_est_c %*% diag(1 / sd_est, ncol(theta_est))
    z_true <- th_true_c %*% diag(1 / sd_true, ncol(theta_true))

    s <- svd(t(z_est) %*% z_true)
    qrot <- s$u %*% t(s$v)
    tmat <- diag(1 / sd_est, ncol(theta_est)) %*% qrot %*% diag(sd_true, ncol(theta_true))

    list(
      theta = th_est_c %*% tmat + matrix(rep(mu_true, each = nrow(theta_est)), nrow(theta_est)),
      A = A_est %*% solve(tmat)
    )
  }

  rmse <- function(x, y) sqrt(mean((x - y)^2))

  Nsubs <- 450
  Nitems <- 24
  Nscales <- 2
  sim <- simIRT(
    Nsubs = Nsubs,
    Nitems = Nitems,
    Nscales = Nscales,
    NitemsAnswered = c(8, 8),
    mirt = TRUE,
    AMean = 1.0,
    ASD = 0.20,
    loadingSparsity = 0.35,
    crossLoadingSD = 0.15,
    BMean = 0,
    BSD = 0.9,
    logitCMean = -20,
    logitCSD = 0
  )

  loading_mask <- matrix(0, nrow = nrow(sim$A), ncol = ncol(sim$A))
  loading_mask[abs(sim$A) > 1e-8] <- NA_real_
  rownames(loading_mask) <- as.character(seq_len(nrow(loading_mask)))
  colnames(loading_mask) <- as.character(seq_len(ncol(loading_mask)))
  free_idx <- which(is.na(loading_mask), arr.ind = TRUE)

  fit_jml <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    loadings = loading_mask,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    verbose = 0,
    plot = FALSE
  )

  fit_samp <- fitIRT(
    sim$dat,
    pl = 2,
    cores = 1,
    priors = TRUE,
    ebayes = FALSE,
    loadings = loading_mask,
    marginalApprox = "laplace_em",
    laplaceOuterIter = 30,
    noptimsteps = 5,
    laplaceDiagnostics = TRUE,
    laplacePlot = FALSE,
    dropPerfectScores = FALSE,
    normalise = FALSE,
    verbose = 0,
    plot = FALSE
  )

  A_jml <- extract_ordered_A(fit_jml, Nitems, Nscales)
  A_samp <- extract_ordered_A(fit_samp, Nitems, Nscales)
  th_jml <- extract_ordered_theta(fit_jml, Nsubs, Nscales)
  th_samp <- extract_ordered_theta(fit_samp, Nsubs, Nscales)

  align_jml <- align_mirt_solution(th_jml, A_jml, sim$Ability)
  align_samp <- align_mirt_solution(th_samp, A_samp, sim$Ability)

  ability_rmse_jml <- rmse(align_jml$theta, sim$Ability)
  ability_rmse_samp <- rmse(align_samp$theta, sim$Ability)
  loading_rmse_jml <- rmse(align_jml$A[free_idx], sim$A[free_idx])
  loading_rmse_samp <- rmse(align_samp$A[free_idx], sim$A[free_idx])

  timing_outer <- as.data.table(fit_samp$sampledAbilityTiming)[stage == "outer"]
  stopifnot(nrow(timing_outer) > 0)
  stopifnot(sum(timing_outer$rejected %in% TRUE) <= 10)

  accepted_outer <- timing_outer[accepted %in% TRUE]
  stopifnot(nrow(accepted_outer) >= 3)

  accepted_grad <- accepted_outer$combinedGradNorm
  if(length(accepted_grad) >= 4){
    grad_diff <- diff(accepted_grad)
    stopifnot(!any(stats::filter(as.numeric(grad_diff > 0), rep(1, 4), sides = 1)[4:length(grad_diff)] >= 4, na.rm = TRUE))
  }

  if(all(c("meanPosteriorSD_ratio", "maxPosteriorSD_ratio") %in% names(timing_outer))){
    stopifnot(!any(timing_outer$meanPosteriorSD_ratio > 1.2 & timing_outer$maxPosteriorSD_ratio > 1.35, na.rm = TRUE))
  }

  stopifnot(ability_rmse_samp <= ability_rmse_jml * 1.15)
  stopifnot(loading_rmse_samp <= loading_rmse_jml * 1.15)

  print(data.table(
    method = c("JML", "Sampled"),
    ability_rmse = c(ability_rmse_jml, ability_rmse_samp),
    loading_rmse = c(loading_rmse_jml, loading_rmse_samp)
  ))
  print(timing_outer[, .(
    accepted = sum(accepted %in% TRUE),
    rejected = sum(rejected %in% TRUE),
    min_grad = min(combinedGradNorm, na.rm = TRUE),
    final_grad = tail(combinedGradNorm, 1)
  )])
}
