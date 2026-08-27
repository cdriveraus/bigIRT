
bigIRT_laplace_item_objective_cpp_impl <- function(id, score, row_ability,
  b, c, d, loadings, prior_precision, jitter, max_attempts, grain_size = 64L){
  .Call(
    `_bigIRT_laplace_item_objective_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(row_ability),
    as.numeric(b),
    as.numeric(c),
    as.numeric(d),
    as.matrix(loadings),
    prior_precision,
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(grain_size)
  )
}

## Pass an array through when it is already in the form .Call needs.
##
## The item context stores A_ref and A_beta_row as integer matrices of exactly
## the right shape, but the wrapper below rebuilt them with
## matrix(as.integer(x), nrow, ncol) on every call: as.integer() drops the dims
## and allocates, then matrix() allocates again. Two full copies of an
## Nobs-by-K integer array, per objective evaluation, for no change in value.
## On a fit with 6.5 million training rows and two dimensions that is around
## 200 MB of copying per evaluation, single-threaded, in front of the parallel
## kernel -- which is why the parallel share looked poor while .Call itself was
## a steady two thirds of the profile.
bigirt_int_matrix <- function(x, nr, nc){
  if(is.integer(x) && is.matrix(x) && nrow(x) == nr && ncol(x) == nc) return(x)
  matrix(as.integer(x), nrow = nr, ncol = nc)
}

## Convert the per-response data once and keep it behind an external pointer.
##
## Everything here is data rather than parameters, so it is identical across the
## eleven or so objective evaluations the item line search makes per outer
## iteration. Converting it per call was the largest single piece of the R-level
## share of the profile.
bigIRT_laplace_item_prepare <- function(id, score, row_ability,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  prior_precision, jitter, max_attempts){
  nr <- nrow(row_ability); nc <- ncol(row_ability)
  .Call(`_bigIRT_laplace_item_prepare_cpp_impl`,
    as.integer(id), as.integer(score), as.matrix(row_ability),
    bigirt_int_matrix(A_ref, nr, nc), as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nr, nc), as.matrix(A_pred),
    as.integer(B_ref), as.numeric(B_fixed_value), as.integer(B_beta_row), as.matrix(B_pred),
    as.integer(C_ref), as.numeric(C_fixed_value), as.integer(C_beta_row), as.matrix(C_pred),
    as.integer(D_ref), as.numeric(D_fixed_value), as.integer(D_beta_row), as.matrix(D_pred),
    prior_precision, as.numeric(jitter), as.integer(max_attempts))
}

## Evaluate against prepared data. Only the item parameters cross the boundary.
bigIRT_laplace_item_eval <- function(prepared, invspApars, invspAbeta, Bpars, Bbeta,
  logitCpars, logitCbeta, logitDpars, logitDbeta, grain_size = 64L,
  adjoint_scale = getOption("bigIRT.adjoint_scale", 1),
  logdet_scale = getOption("bigIRT.logdet_scale", 1)){
  .Call(`_bigIRT_laplace_item_eval_cpp_impl`, prepared,
    as.numeric(invspApars), as.matrix(invspAbeta),
    as.numeric(Bpars), as.matrix(Bbeta),
    as.numeric(logitCpars), as.matrix(logitCbeta),
    as.numeric(logitDpars), as.matrix(logitDbeta),
    as.numeric(adjoint_scale), as.numeric(logdet_scale), as.integer(grain_size))
}

bigIRT_laplace_item_block_objective_cpp_impl <- function(id, score, row_ability,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_precision, jitter, max_attempts, grain_size = 64L,
  adjoint_scale = getOption("bigIRT.adjoint_scale", 1),
  logdet_scale = getOption("bigIRT.logdet_scale", 1)){
  .Call(
    `_bigIRT_laplace_item_block_objective_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(row_ability),
    bigirt_int_matrix(A_ref, nrow(row_ability), ncol(row_ability)),
    as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nrow(row_ability), ncol(row_ability)),
    as.matrix(A_pred),
    as.integer(B_ref),
    as.numeric(B_fixed_value),
    as.integer(B_beta_row),
    as.matrix(B_pred),
    as.integer(C_ref),
    as.numeric(C_fixed_value),
    as.integer(C_beta_row),
    as.matrix(C_pred),
    as.integer(D_ref),
    as.numeric(D_fixed_value),
    as.integer(D_beta_row),
    as.matrix(D_pred),
    as.numeric(invspApars),
    as.matrix(invspAbeta),
    as.numeric(Bpars),
    as.matrix(Bbeta),
    as.numeric(logitCpars),
    as.matrix(logitCbeta),
    as.numeric(logitDpars),
    as.matrix(logitDbeta),
    prior_precision,
    as.numeric(jitter),
    as.integer(max_attempts),
    as.numeric(adjoint_scale),
    as.numeric(logdet_scale),
    as.integer(grain_size)
  )
}


bigIRT_laplace_direct_block_fg_cpp_impl <- function(id, score, theta_init,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask,
  jitter, max_attempts, max_iter, tol, keep_covariance = FALSE, grain_size = 64L){
  .Call(
    `_bigIRT_laplace_direct_block_fg_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(theta_init),
    as.matrix(person_pred),
    matrix(as.integer(fixed_ability), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(fixed_ability_value),
    matrix(as.integer(A_ref), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(A_fixed_value),
    matrix(as.integer(A_beta_row), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(A_pred),
    as.integer(B_ref),
    as.numeric(B_fixed_value),
    as.integer(B_beta_row),
    as.matrix(B_pred),
    as.integer(C_ref),
    as.numeric(C_fixed_value),
    as.integer(C_beta_row),
    as.matrix(C_pred),
    as.integer(D_ref),
    as.numeric(D_fixed_value),
    as.integer(D_beta_row),
    as.matrix(D_pred),
    as.matrix(Abilitybeta),
    as.numeric(invspApars),
    as.matrix(invspAbeta),
    as.numeric(Bpars),
    as.matrix(Bbeta),
    as.numeric(logitCpars),
    as.matrix(logitCbeta),
    as.numeric(logitDpars),
    as.matrix(logitDbeta),
    as.matrix(prior_mean),
    prior_precision,
    matrix(as.integer(free_mask), nrow = nrow(theta_init), ncol = ncol(theta_init)),
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(max_iter),
    as.numeric(tol),
    as.logical(keep_covariance),
    as.integer(grain_size)
  )
}

bigIRT_laplace_as_matrix <- function(x, nrow, ncol){
  if(is.null(x) || length(x) == 0) return(matrix(0, nrow = nrow, ncol = ncol))
  if(is.matrix(x)) return(matrix(as.numeric(x), nrow = nrow, ncol = ncol))
  matrix(as.numeric(x), nrow = nrow, ncol = ncol, byrow = TRUE)
}



bigIRT_laplace_prior_precision_array <- function(sdat, jitter = 1e-8){
  priorSD <- pmax(as.numeric(sdat$AbilitySD), jitter)
  priorCov <- diag(priorSD, length(priorSD)) %*% sdat$AbilityCorr %*% diag(priorSD, length(priorSD))
  priorPrec <- solve(priorCov + diag(jitter, nrow(priorCov)))
  bigIRT_prior_precision_array(priorPrec, Nsubs = sdat$Nsubs, K = sdat$Nscales)
}

## Build a dense prior covariance/precision pair from fixed marginal SDs and a
## candidate correlation matrix. Inputs: standata plus optional corr override.
## Returns the subject-replicated precision array and the shared KxK matrices;
## mutates nothing.
bigIRT_laplace_prior_mats <- function(sdat, AbilityCorr = sdat$AbilityCorr, jitter = 1e-8){
  priorSD <- pmax(as.numeric(sdat$AbilitySD), jitter)
  scaleMat <- diag(priorSD, length(priorSD))
  corr <- as.matrix(AbilityCorr)
  cov <- scaleMat %*% corr %*% scaleMat
  prec <- solve(cov + diag(jitter, nrow(cov)))
  list(
    corr = corr,
    covariance = cov,
    precision = prec,
    precision_array = bigIRT_prior_precision_array(prec, Nsubs = sdat$Nsubs, K = sdat$Nscales)
  )
}

bigIRT_laplace_corr_grad_cpp_impl <- function(theta_mode, covariance, precision,
  prior_mean, ability_sd, corr_par, logdet_slope, corr_paramization = 0L, jitter = 1e-8){
  .Call(
    `_bigIRT_laplace_corr_grad_cpp_impl`,
    as.matrix(theta_mode),
    covariance,
    precision,
    as.numeric(prior_mean),
    as.numeric(ability_sd),
    as.numeric(corr_par),
    as.matrix(logdet_slope),
    as.integer(corr_paramization),
    as.numeric(jitter)
  )
}

bigIRT_laplace_clamp <- function(x, lo = 1e-6, hi = 1 - 1e-6){
  pmin(pmax(x, lo), hi)
}

bigIRT_laplace_corr_param_code <- function(paramization = c("normalized_chol", "stan_corsqrt")){
  switch(match.arg(paramization), normalized_chol = 0L, stan_corsqrt = 1L)
}

## Convert an unconstrained packed lower-triangle vector into a valid SPD
## correlation matrix using a normalized Cholesky factor with unit diagonal.
## Inputs: packed free parameters and target dimension K. Returns a KxK
## correlation matrix; mutates nothing.
bigIRT_laplace_corr_from_packed <- function(par, K,
  paramization = c("normalized_chol", "stan_corsqrt")){
  paramization <- match.arg(paramization)
  if(K <= 1L) return(diag(1, K))
  if(identical(paramization, "stan_corsqrt")){
    raw <- tanh(as.numeric(par))
    L <- diag(0, K)
    L[1, 1] <- 1
    idx <- 1L
    for(i in 2:K){
      row_raw <- raw[idx:(idx + i - 2L)]
      prod_term <- 1
      for(j in 1:(i - 1L)){
        if(j > 1L) prod_term <- prod_term * sqrt(pmax(1 - row_raw[j - 1L]^2, 1e-12))
        L[i, j] <- row_raw[j] * prod_term
      }
      L[i, i] <- prod(sqrt(pmax(1 - row_raw^2, 1e-12)))
      idx <- idx + i - 1L
    }
    return(tcrossprod(L))
  }
  L <- diag(1, K)
  idx <- 1L
  for(i in 2:K){
    for(j in 1:(i - 1L)){
      L[i, j] <- par[idx]
      idx <- idx + 1L
    }
  }
  S <- tcrossprod(L)
  s <- sqrt(pmax(diag(S), 1e-12))
  sweep(sweep(S, 1, s, "/"), 2, s, "/")
}

## Pack the free lower-triangle of a correlation-generating Cholesky factor.
## Inputs: a KxK correlation matrix. Returns the unconstrained vector used by
## `bigIRT_laplace_corr_from_packed()`.
bigIRT_laplace_pack_corr <- function(corr,
  paramization = c("normalized_chol", "stan_corsqrt")){
  paramization <- match.arg(paramization)
  K <- nrow(corr)
  if(K <= 1L) return(numeric())
  L <- t(chol(corr))
  if(identical(paramization, "stan_corsqrt")){
    out <- numeric(K * (K - 1L) / 2L)
    idx <- 1L
    for(i in 2:K){
      prefix_prod <- 1
      for(j in 1:(i - 1L)){
        raw_ij <- L[i, j] / pmax(prefix_prod, 1e-12)
        raw_ij <- bigIRT_laplace_clamp(raw_ij, lo = -0.999999, hi = 0.999999)
        out[idx] <- atanh(raw_ij)
        prefix_prod <- prefix_prod * sqrt(pmax(1 - raw_ij^2, 1e-12))
        idx <- idx + 1L
      }
    }
    return(out)
  }
  for(i in seq_len(K)){
    L[i,] <- L[i,] / pmax(L[i, i], 1e-12)
  }
  out <- numeric(K * (K - 1L) / 2L)
  idx <- 1L
  for(i in 2:K){
    for(j in 1:(i - 1L)){
      out[idx] <- L[i, j]
      idx <- idx + 1L
    }
  }
  out
}

bigIRT_laplace_subject_grain <- function(nsubs, cores, chunks_per_core = 8L){
  cores <- max(1L, as.integer(cores)[1])
  nsubs <- max(1L, as.integer(nsubs)[1])
  target_chunks <- max(1L, cores * as.integer(chunks_per_core)[1])
  min(nsubs, max(1L, ceiling(nsubs / target_chunks)))
}

## Build a direct, prior-anchored initial state for the Laplace path.
## Inputs: standata only; no JML fit required.
## Returns: a complete state list matching the Laplace backend contract;
## mutates nothing.
bigIRT_laplace_initial_state <- function(sdat, eps = 1e-6,
  corr_paramization = c("normalized_chol", "stan_corsqrt")){
  corr_paramization <- match.arg(corr_paramization)
  abilityBase <- matrix(as.numeric(sdat$Abilitydata), nrow = sdat$Nsubs, ncol = sdat$Nscales)
  abilityMean <- rep(0, sdat$Nscales)

  freeAbility <- which(sdat$fixedAbilityLogical == 0L)
  if(length(freeAbility)) abilityBase[freeAbility] <- 0

  freeA <- max(0L, sdat$NitemScales - sdat$NfixedA)
  freeB <- max(0L, sdat$Nitems - sdat$NfixedB)
  freeC <- max(0L, sdat$Nitems - sdat$NfixedC)
  freeD <- max(0L, sdat$Nitems - sdat$NfixedD)
  A_beta_rows <- if(sdat$itemSpecificBetas == 1L) max(1L, freeA) else 1L
  item_beta_rows <- function(freeN) if(sdat$itemSpecificBetas == 1L) max(1L, freeN) else 1L

  state <- list(
    AbilityBase = abilityBase,
    AbilityMean = if(sdat$fixedAbilityMean == 0L) abilityMean else as.numeric(sdat$AbilityMeandat),
    AbilityCorr = as.matrix(sdat$AbilityCorr),
    laplaceCorrParam = corr_paramization,
    AbilityCorrPars = if(sdat$Nscales > 1L) bigIRT_laplace_pack_corr(as.matrix(sdat$AbilityCorr), paramization = corr_paramization) else numeric(),
    Abilitybeta = matrix(0, nrow = sdat$Nscales, ncol = sdat$NpersonPreds),
    Bpars = rep(0, freeB),
    BMean = if(sdat$fixedBMean == 0L) 0 else as.numeric(sdat$BMeandat)[1],
    Bbeta = matrix(0, nrow = item_beta_rows(freeB), ncol = sdat$NBitemPreds),
    invspApars = rep(0, freeA),
    invspAMean = if(sdat$fixedAMean == 0L) 0 else as.numeric(sdat$invspAMeandat)[1],
    invspAbeta = matrix(0, nrow = A_beta_rows, ncol = sdat$NAitemPreds),
    logitCpars = rep(0, freeC),
    logitCMean = if(sdat$fixedCMean == 0L) 0 else as.numeric(sdat$logitCMeandat)[1],
    logitCbeta = matrix(0, nrow = item_beta_rows(freeC), ncol = sdat$NCitemPreds),
    logitDpars = rep(0, freeD),
    logitDMean = if(sdat$fixedDMean == 0L) 0 else as.numeric(sdat$logitDMeandat)[1],
    logitDbeta = matrix(0, nrow = item_beta_rows(freeD), ncol = sdat$NDitemPreds)
  )

  state
}

bigIRT_laplace_training_rows <- function(sdat){
  which(as.integer(sdat$trainingLogical) == 1L)
}

## Precompute row-level indexing and fixed pieces used repeatedly by the
## Laplace backend. Inputs: `sdat`, optional row subset. Returns immutable
## lookup structures for vectorized row-effective parameter construction.
bigIRT_laplace_row_context <- function(sdat, rows = bigIRT_laplace_training_rows(sdat)){
  rows <- as.integer(rows)
  nrows <- length(rows)
  K <- sdat$Nscales
  ids <- sdat$id[rows]
  items <- sdat$item[rows]
  item_offsets <- (items - 1L) * K

  fixed_ability <- matrix(FALSE, nrow = nrows, ncol = K)
  fixed_ability_value <- matrix(0, nrow = nrows, ncol = K)
  fixed_A <- matrix(FALSE, nrow = nrows, ncol = K)
  fixed_A_value <- matrix(0, nrow = nrows, ncol = K)
  A_ref <- matrix(0L, nrow = nrows, ncol = K)
  A_beta_row <- matrix(0L, nrow = nrows, ncol = K)

  for(si in seq_len(K)){
    fixed_ability[, si] <- sdat$fixedAbilityLogical[ids, si] == 1L
    fixed_ability_value[, si] <- sdat$Abilitydata[ids, si]

    aidx <- item_offsets + si
    fixed_A[, si] <- sdat$fixedAlog[aidx] == 1L
    fixed_A_value[, si] <- sdat$Adata[aidx]
    ref <- sdat$freeAref[aidx]
    ref[fixed_A[, si]] <- 0L
    A_ref[, si] <- ref
    if(sdat$NAitemPreds > 0){
      A_beta_row[, si] <- ifelse(sdat$itemSpecificBetas == 1L, ref, ifelse(ref > 0L, 1L, 0L))
    }
  }

  B_ref <- sdat$freeBref[items]
  B_ref[sdat$fixedB[items] == 1L] <- 0L
  C_ref <- sdat$freeCref[items]
  C_ref[sdat$fixedClogit[items] == 1L] <- 0L
  D_ref <- sdat$freeDref[items]
  D_ref[sdat$fixedDlogit[items] == 1L] <- 0L

  list(
    rows = rows,
    ids = ids,
    items = items,
    fixed_ability = fixed_ability,
    fixed_ability_value = fixed_ability_value,
    fixed_A = fixed_A,
    fixed_A_value = fixed_A_value,
    A_ref = A_ref,
    A_beta_row = A_beta_row,
    B_ref = B_ref,
    B_fixed = sdat$fixedB[items] == 1L,
    B_fixed_value = sdat$Bdata[items],
    B_beta_row = if(sdat$NBitemPreds > 0) ifelse(sdat$itemSpecificBetas == 1L, B_ref, ifelse(B_ref > 0L, 1L, 0L)) else integer(nrows),
    C_ref = C_ref,
    C_fixed = sdat$fixedClogit[items] == 1L,
    C_fixed_value = sdat$Cdata[items],
    C_beta_row = if(sdat$NCitemPreds > 0) ifelse(sdat$itemSpecificBetas == 1L, C_ref, ifelse(C_ref > 0L, 1L, 0L)) else integer(nrows),
    D_ref = D_ref,
    D_fixed = sdat$fixedDlogit[items] == 1L,
    D_fixed_value = sdat$Ddata[items],
    D_beta_row = if(sdat$NDitemPreds > 0) ifelse(sdat$itemSpecificBetas == 1L, D_ref, ifelse(D_ref > 0L, 1L, 0L)) else integer(nrows),
    person_pred = if(sdat$NpersonPreds > 0) as.matrix(sdat$personPreds[rows,, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    # Both predictor matrices are long-row matrices in standata.  Index them
    # with `rows`, never with the item id: item ids are not row positions.
    A_pred = if(sdat$NAitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$AitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    B_pred = if(sdat$NBitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$BitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    C_pred = if(sdat$NCitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$CitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    D_pred = if(sdat$NDitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$DitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0)
  )
}

bigIRT_laplace_person_means_by_id <- function(sdat){
  if(sdat$NpersonPreds == 0) return(matrix(0, nrow = sdat$Nsubs, ncol = 0))
  out <- matrix(0, nrow = sdat$Nsubs, ncol = sdat$NpersonPreds)
  counts <- tabulate(sdat$id, nbins = sdat$Nsubs)
  for(i in seq_len(sdat$Nobs)) out[sdat$id[i],] <- out[sdat$id[i],] + sdat$personPreds[i,,drop = FALSE]
  out / pmax(counts, 1)
}

bigIRT_laplace_item_means <- function(sdat){
  if(sdat$NitemPreds == 0) return(matrix(0, nrow = sdat$Nitems, ncol = 0))
  out <- matrix(0, nrow = sdat$Nitems, ncol = sdat$NitemPreds)
  counts <- tabulate(sdat$item, nbins = sdat$Nitems)
  for(i in seq_len(sdat$Nobs)) out[sdat$item[i],] <- out[sdat$item[i],] + sdat$itemPreds[i,,drop = FALSE]
  out / pmax(counts, 1)
}

bigIRT_laplace_row_effective <- function(state, sdat, thetaBase = state$AbilityBase,
  rows = seq_len(sdat$Nobs), include_raw = FALSE, context = NULL){
  context <- if(is.null(context)) bigIRT_laplace_row_context(sdat, rows = rows) else context
  rows <- context$rows
  nrows <- length(rows)
  K <- sdat$Nscales

  ability_offset <- if(sdat$NpersonPreds > 0 && length(state$Abilitybeta)) {
    context$person_pred %*% t(state$Abilitybeta)
  } else {
    matrix(0, nrow = nrows, ncol = K)
  }
  row_ability <- thetaBase[context$ids,, drop = FALSE] + ability_offset
  row_ability[context$fixed_ability] <- context$fixed_ability_value[context$fixed_ability]
  ability_offset[context$fixed_ability] <- 0

  loadings <- matrix(0, nrow = nrows, ncol = K)
  a_linear <- if(include_raw) matrix(NA_real_, nrow = nrows, ncol = K) else NULL
  for(si in seq_len(K)){
    ref <- context$A_ref[, si]
    fixed <- context$fixed_A[, si]
    raw <- numeric(nrows)
    if(any(ref > 0L)) raw[ref > 0L] <- state$invspApars[ref[ref > 0L]]
    if(sdat$NAitemPreds > 0){
      beta_row <- context$A_beta_row[, si]
      keep <- beta_row > 0L
      if(any(keep)){
        raw[keep] <- raw[keep] + rowSums(context$A_pred[keep,, drop = FALSE] * state$invspAbeta[beta_row[keep],, drop = FALSE])
      }
    }
    loadings[, si] <- context$fixed_A_value[, si]
    if(any(!fixed)) loadings[!fixed, si] <- afunc(raw[!fixed])
    if(include_raw) a_linear[, si] <- raw
  }

  b_row <- rep(0, nrows)
  b_row[context$B_fixed] <- context$B_fixed_value[context$B_fixed]
  keepB <- !context$B_fixed
  if(any(keepB)){
    b_row[keepB] <- as.numeric(state$Bpars)[as.integer(context$B_ref[keepB])]
    if(sdat$NBitemPreds > 0){
      brow <- context$B_beta_row[keepB]
      bpred <- context$B_pred[keepB,, drop = FALSE]
      beta_effect <- if(sdat$itemSpecificBetas == 0L) {
        as.numeric(bpred %*% as.numeric(state$Bbeta[1L,, drop = TRUE]))
      } else rowSums(bpred * state$Bbeta[brow, , drop = FALSE])
      b_row[keepB] <- b_row[keepB] + beta_effect
    }
    if(any(!is.finite(b_row[keepB])))
      stop("Non-finite effective difficulties in the Laplace item-predictor path.", call. = FALSE)
  }

  c_linear <- if(include_raw) numeric(nrows) else NULL
  c_row <- context$C_fixed_value
  keepC <- !context$C_fixed
  if(any(keepC)){
    rawC <- state$logitCpars[context$C_ref[keepC]]
    if(sdat$NCitemPreds > 0){
      crow <- context$C_beta_row[keepC]
      rawC <- rawC + rowSums(context$C_pred[keepC,, drop = FALSE] * state$logitCbeta[crow,, drop = FALSE])
    }
    c_row[keepC] <- cfunc(rawC)
    if(include_raw) c_linear[keepC] <- rawC
  }

  d_linear <- if(include_raw) numeric(nrows) else NULL
  d_row <- context$D_fixed_value
  keepD <- !context$D_fixed
  if(any(keepD)){
    rawD <- state$logitDpars[context$D_ref[keepD]]
    if(sdat$NDitemPreds > 0){
      drow <- context$D_beta_row[keepD]
      rawD <- rawD + rowSums(context$D_pred[keepD,, drop = FALSE] * state$logitDbeta[drow,, drop = FALSE])
    }
    d_row[keepD] <- dfunc(rawD)
    if(include_raw) d_linear[keepD] <- rawD
  }

  out <- list(
    rows = rows,
    loadings = loadings,
    ability_offset = ability_offset,
    row_ability = row_ability,
    b_row = b_row,
    c_row = c_row,
    d_row = d_row,
    eta_row = rowSums(loadings * row_ability) - b_row
  )
  if(include_raw){
    out$a_linear <- a_linear
    out$c_linear <- c_linear
    out$d_linear <- d_linear
  }
  out
}

bigIRT_laplace_fixed_row_ability <- function(state, sdat, thetaBase = state$AbilityBase,
  rows = seq_len(sdat$Nobs), context = NULL){
  context <- if(is.null(context)) bigIRT_laplace_row_context(sdat, rows = rows) else context
  nrows <- length(context$rows)
  K <- sdat$Nscales
  ability_offset <- if(sdat$NpersonPreds > 0 && length(state$Abilitybeta)) {
    context$person_pred %*% t(state$Abilitybeta)
  } else {
    matrix(0, nrow = nrows, ncol = K)
  }
  row_ability <- thetaBase[context$ids,, drop = FALSE] + ability_offset
  row_ability[context$fixed_ability] <- context$fixed_ability_value[context$fixed_ability]
  row_ability
}

bigIRT_laplace_person_step <- function(state, sdat, prior_precision, jitter = 1e-6,
  max_attempts = 8L, max_iter = 50L, tol = 1e-4, keep_covariance = FALSE,
  cores = 1L, row_context = NULL, train_rows = NULL){
  ## The row context depends only on the data, but this function rebuilt it on
  ## every call, and the outer loop calls it twice per iteration. Profiling on a
  ## 20,000-person fit put that rebuild at roughly 80 per cent of the posterior
  ## refresh phase -- serial R work inside a phase whose time reads as parallel
  ## solving, and the reason that phase showed a speedup of 1.07x on 16 cores.
  ## Callers that fit repeatedly should build it once and pass it in.
  if(is.null(train_rows)) train_rows <- bigIRT_laplace_training_rows(sdat)
  if(is.null(row_context)) row_context <- bigIRT_laplace_row_context(sdat, rows = train_rows)
  prior_mean <- matrix(rep(state$AbilityMean, each = sdat$Nsubs), nrow = sdat$Nsubs)
  free_mask <- 1L - sdat$fixedAbilityLogical
  ## row_effective is no longer needed here: the block kernel derives the same
  ## per-response values internally from the compact parameters.
  ## Two kernels compute this. bigIRT_laplace_person_step_cpp_impl runs the
  ## worker as worker(0, Nsubs) -- serial, ignoring the grain_size it is
  ## handed. The block kernel does the same arithmetic under parallelFor and
  ## takes item parameters plus the row-to-item index instead of per-response
  ## expansions, so it also avoids an O(Nobs) materialisation.
  ##
  ## The block kernel had no callers, and untested code is untested: it read
  ## the beta-row index per response, when that vector holds a single element
  ## unless effects are item-specific, so it segfaulted whenever item
  ## covariates were present. Fixed by guarding the index the way the item
  ## kernel always has.
  out <- bigIRT_laplace_person_step_block_cpp_impl(
    id = sdat$id[train_rows], score = sdat$score[train_rows],
    theta_init = state$AbilityBase,
    person_pred = row_context$person_pred,
    fixed_ability = row_context$fixed_ability,
    fixed_ability_value = row_context$fixed_ability_value,
    A_ref = row_context$A_ref, A_fixed_value = row_context$fixed_A_value,
    A_beta_row = row_context$A_beta_row, A_pred = row_context$A_pred,
    B_ref = row_context$B_ref, B_fixed_value = row_context$B_fixed_value,
    B_beta_row = row_context$B_beta_row, B_pred = row_context$B_pred,
    C_ref = row_context$C_ref, C_fixed_value = row_context$C_fixed_value,
    C_beta_row = row_context$C_beta_row, C_pred = row_context$C_pred,
    D_ref = row_context$D_ref, D_fixed_value = row_context$D_fixed_value,
    D_beta_row = row_context$D_beta_row, D_pred = row_context$D_pred,
    Abilitybeta = state$Abilitybeta,
    invspApars = state$invspApars, invspAbeta = state$invspAbeta,
    Bpars = state$Bpars, Bbeta = state$Bbeta,
    logitCpars = state$logitCpars, logitCbeta = state$logitCbeta,
    logitDpars = state$logitDpars, logitDbeta = state$logitDbeta,
    prior_mean = prior_mean, prior_precision = prior_precision,
    free_mask = free_mask,
    jitter = jitter, max_attempts = max_attempts, max_iter = max_iter,
    tol = tol, keep_covariance = keep_covariance,
    grain_size = bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  )
  # Native status codes are authoritative: 0 is a resolved mode; 1/2 are
  # ordinary non-convergence; higher values are numerical failures.
  out$converged <- as.integer(out$status_code) == 0L
  hard <- which(as.integer(out$status_code) >= 3L)
  if(length(hard)){
    stop(sprintf("Laplace person kernel numerical failure (person IDs: %s; codes: %s).",
      paste(hard, collapse = ","), paste(out$status_code[hard], collapse = ",")), call. = FALSE)
  }
  state$AbilityBase <- out$theta_mode
  if(sdat$fixedAbilityMean == 0L){
    for(si in seq_len(sdat$Nscales)){
      free_idx <- which(sdat$fixedAbilityLogical[,si] == 0L)
      if(length(free_idx)) state$AbilityMean[si] <- mean(state$AbilityBase[free_idx, si])
    }
  }
  list(state = state, posterior = out)
}


bigIRT_laplace_item_layout <- function(sdat){
  itemBetaCount <- function(freeN, predN){
    if(freeN <= 0 || predN <= 0) return(0L)
    as.integer((if(sdat$itemSpecificBetas == 1L) freeN else 1L) * predN)
  }
  freeA <- sdat$NitemScales - sdat$NfixedA
  freeB <- sdat$Nitems - sdat$NfixedB
  freeC <- sdat$Nitems - sdat$NfixedC
  freeD <- sdat$Nitems - sdat$NfixedD
  cursor <- 1L
  take <- function(n){
    if(n <= 0) return(integer())
    idx <- seq.int(cursor, length.out = n)
    cursor <<- cursor + n
    idx
  }
  list(
    B = take(freeB),
    B_mean = take(if(sdat$fixedBMean == 0L) 1L else 0L),
    B_beta = take(itemBetaCount(freeB, sdat$NBitemPreds)),
    A = take(freeA),
    A_mean = take(if(sdat$fixedAMean == 0L) 1L else 0L),
    A_beta = take(itemBetaCount(freeA, sdat$NAitemPreds)),
    C = take(freeC),
    C_mean = take(if(sdat$fixedCMean == 0L) 1L else 0L),
    C_beta = take(itemBetaCount(freeC, sdat$NCitemPreds)),
    D = take(freeD),
    D_mean = take(if(sdat$fixedDMean == 0L) 1L else 0L),
    D_beta = take(itemBetaCount(freeD, sdat$NDitemPreds))
  )
}

bigIRT_laplace_item_context <- function(sdat, layout = bigIRT_laplace_item_layout(sdat), row_context = NULL){
  ## Same rebuild as in the person step: data-only, and repeated on every
  ## construction of the item context.
  if(is.null(row_context))
    row_context <- bigIRT_laplace_row_context(sdat, rows = bigIRT_laplace_training_rows(sdat))
  list(
    layout = layout,
    train_rows = row_context$rows,
    grain_size = 64L,
    items = row_context$items,
    A_ref = row_context$A_ref,
    A_beta_row = row_context$A_beta_row,
    A_pred = row_context$A_pred,
    B_ref = row_context$B_ref,
    B_beta_row = row_context$B_beta_row,
    B_pred = row_context$B_pred,
    C_ref = row_context$C_ref,
    C_beta_row = row_context$C_beta_row,
    C_pred = row_context$C_pred,
    D_ref = row_context$D_ref,
    D_beta_row = row_context$D_beta_row,
    D_pred = row_context$D_pred,
    row_context = row_context
  )
}



bigIRT_laplace_pack_item_state <- function(state, sdat, layout = bigIRT_laplace_item_layout(sdat)){
  out <- numeric(max(unlist(layout), 0L))
  if(length(layout$B)) out[layout$B] <- state$Bpars
  if(length(layout$B_mean)) out[layout$B_mean] <- state$BMean
  if(length(layout$B_beta)) out[layout$B_beta] <- as.numeric(state$Bbeta)
  if(length(layout$A)) out[layout$A] <- state$invspApars
  if(length(layout$A_mean)) out[layout$A_mean] <- state$invspAMean
  if(length(layout$A_beta)) out[layout$A_beta] <- as.numeric(state$invspAbeta)
  if(length(layout$C)) out[layout$C] <- state$logitCpars
  if(length(layout$C_mean)) out[layout$C_mean] <- state$logitCMean
  if(length(layout$C_beta)) out[layout$C_beta] <- as.numeric(state$logitCbeta)
  if(length(layout$D)) out[layout$D] <- state$logitDpars
  if(length(layout$D_mean)) out[layout$D_mean] <- state$logitDMean
  if(length(layout$D_beta)) out[layout$D_beta] <- as.numeric(state$logitDbeta)
  out
}

bigIRT_laplace_unpack_item_state <- function(par, state, sdat, layout = bigIRT_laplace_item_layout(sdat)){
  out <- state
  if(length(layout$B)) out$Bpars <- as.numeric(par[layout$B])
  if(length(layout$B_mean)) out$BMean <- as.numeric(par[layout$B_mean])[1]
  if(length(layout$B_beta)) out$Bbeta <- matrix(as.numeric(par[layout$B_beta]), nrow = nrow(state$Bbeta))
  if(length(layout$A)) out$invspApars <- as.numeric(par[layout$A])
  if(length(layout$A_mean)) out$invspAMean <- as.numeric(par[layout$A_mean])[1]
  if(length(layout$A_beta)) out$invspAbeta <- matrix(as.numeric(par[layout$A_beta]), nrow = nrow(state$invspAbeta))
  if(length(layout$C)) out$logitCpars <- as.numeric(par[layout$C])
  if(length(layout$C_mean)) out$logitCMean <- as.numeric(par[layout$C_mean])[1]
  if(length(layout$C_beta)) out$logitCbeta <- matrix(as.numeric(par[layout$C_beta]), nrow = nrow(state$logitCbeta))
  if(length(layout$D)) out$logitDpars <- as.numeric(par[layout$D])
  if(length(layout$D_mean)) out$logitDMean <- as.numeric(par[layout$D_mean])[1]
  if(length(layout$D_beta)) out$logitDbeta <- matrix(as.numeric(par[layout$D_beta]), nrow = nrow(state$logitDbeta))
  out
}

## Extend the packed item/global layout with optional AbilityCorr parameters
## for the direct Laplace backend. Inputs: standata and a boolean toggle.
## Returns item-layout indices plus `corr` when enabled; mutates nothing.
bigIRT_laplace_direct_layout <- function(sdat, estimateAbilityCorr = FALSE){
  item_layout <- bigIRT_laplace_item_layout(sdat)
  cursor <- max(unlist(item_layout), 0L) + 1L
  take <- function(n){
    if(n <= 0L) return(integer())
    idx <- seq.int(cursor, length.out = n)
    cursor <<- cursor + n
    idx
  }
  item_layout$ability_beta <- take(sdat$Nscales * sdat$NpersonPreds)
  item_layout$ability_mean <- take(if(sdat$fixedAbilityMean == 0L) sdat$Nscales else 0L)
  n_corr <- if(isTRUE(estimateAbilityCorr) && sdat$Nscales > 1L) sdat$Nscales * (sdat$Nscales - 1L) / 2L else 0L
  item_layout$corr <- take(n_corr)
  item_layout
}

## Pack the direct Laplace parameter vector, optionally including the latent
## correlation parameters. Inputs: current state, standata, and a direct layout.
bigIRT_laplace_pack_direct_state <- function(state, sdat,
  layout = bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)){
  out <- numeric(max(unlist(layout), 0L))
  item_slots <- layout[intersect(names(layout), names(bigIRT_laplace_item_layout(sdat)))]
  if(length(unlist(item_slots))){
    out[seq_len(max(unlist(item_slots), 0L))] <- bigIRT_laplace_pack_item_state(state, sdat, layout = item_slots)
  }
  if(length(layout$ability_beta)){
    out[layout$ability_beta] <- as.numeric(state$Abilitybeta)
  }
  if(length(layout$ability_mean)){
    out[layout$ability_mean] <- as.numeric(state$AbilityMean)
  }
  if(length(layout$corr)){
    out[layout$corr] <- as.numeric(state$AbilityCorrPars)
  }
  out
}

## Unpack the direct Laplace parameter vector, including latent correlation
## parameters when present. Returns an updated state list; mutates nothing.
bigIRT_laplace_unpack_direct_state <- function(par, state, sdat,
  layout = bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE),
  corr_paramization = c("normalized_chol", "stan_corsqrt")){
  corr_paramization <- match.arg(corr_paramization)
  out <- state
  item_slots <- layout[intersect(names(layout), names(bigIRT_laplace_item_layout(sdat)))]
  out <- bigIRT_laplace_unpack_item_state(par, out, sdat, layout = item_slots)
  if(length(layout$ability_beta)){
    out$Abilitybeta <- matrix(
      as.numeric(par[layout$ability_beta]),
      nrow = nrow(state$Abilitybeta),
      ncol = ncol(state$Abilitybeta)
    )
  }
  if(length(layout$ability_mean)){
    out$AbilityMean <- as.numeric(par[layout$ability_mean])
  }
  if(length(layout$corr)){
    out$AbilityCorrPars <- as.numeric(par[layout$corr])
    out$AbilityCorr <- bigIRT_laplace_corr_from_packed(out$AbilityCorrPars, sdat$Nscales, paramization = corr_paramization)
  } else if(is.null(out$AbilityCorr)) {
    out$AbilityCorr <- as.matrix(sdat$AbilityCorr)
  }
  out
}

## Approximate the direct Laplace gradient with respect to the packed
## correlation parameters while holding the resolved person summaries fixed.
## Inputs: current state, standata, direct-layout, and resolved posterior.
## Returns the gradient contribution for `layout$corr`; mutates nothing.
bigIRT_laplace_corr_grad <- function(state, sdat, layout, posterior, prior_precision_matrix, logdet_slope, jitter = 1e-8){
  out <- numeric(length(layout$corr))
  if(length(layout$corr) == 0L) return(out)
  if(is.null(posterior$covariance)) stop("laplace_direct correlation gradients require posterior covariances.")
  if(missing(logdet_slope) || is.null(logdet_slope)) stop("laplace_direct correlation gradients require the log-determinant slope.")
  corr_paramization <- if(!is.null(state$laplaceCorrParam)) state$laplaceCorrParam else "normalized_chol"
  as.numeric(bigIRT_laplace_corr_grad_cpp_impl(
    theta_mode = posterior$theta_mode,
    covariance = posterior$covariance,
    precision = posterior$precision,
    prior_mean = state$AbilityMean,
    ability_sd = sdat$AbilitySD,
    corr_par = state$AbilityCorrPars,
    logdet_slope = logdet_slope,
    corr_paramization = bigIRT_laplace_corr_param_code(corr_paramization),
    jitter = jitter
  ))
}

bigIRT_laplace_item_prior <- function(state, sdat, layout = bigIRT_laplace_item_layout(sdat)){
  if(!isTRUE(as.logical(sdat$dopriors))) return(list(value = 0, grad = numeric(max(unlist(layout), 0L))))
  grad <- numeric(max(unlist(layout), 0L))
  value <- 0

  if(length(state$invspApars)){
    value <- value + sum(dnorm(state$invspApars, mean = state$invspAMean, sd = sdat$invspASD, log = TRUE))
    if(length(layout$A)) grad[layout$A] <- grad[layout$A] - (state$invspApars - state$invspAMean) / (sdat$invspASD^2)
    if(length(layout$A_mean)) grad[layout$A_mean] <- sum((state$invspApars - state$invspAMean) / (sdat$invspASD^2))
  }
  if(length(layout$A_mean)){
    value <- value + stats::dnorm(
      state$invspAMean,
      mean = as.numeric(sdat$invspAMeandat)[1],
      sd = pmax(as.numeric(sdat$AMeanSD)[1], 1e-8),
      log = TRUE
    )
    grad[layout$A_mean] <- grad[layout$A_mean] -
      (state$invspAMean - as.numeric(sdat$invspAMeandat)[1]) / (pmax(as.numeric(sdat$AMeanSD)[1], 1e-8)^2)
  }
  if(length(state$Bpars)){
    value <- value + sum(dnorm(state$Bpars, mean = state$BMean, sd = sdat$BSDx, log = TRUE))
    if(length(layout$B)) grad[layout$B] <- grad[layout$B] - (state$Bpars - state$BMean) / (sdat$BSDx^2)
    if(length(layout$B_mean)) grad[layout$B_mean] <- sum((state$Bpars - state$BMean) / (sdat$BSDx^2))
  }
  if(length(layout$B_mean)){
    value <- value + stats::dnorm(
      state$BMean,
      mean = as.numeric(sdat$BMeandat)[1],
      sd = pmax(as.numeric(sdat$BMeanSD)[1], 1e-8),
      log = TRUE
    )
    grad[layout$B_mean] <- grad[layout$B_mean] -
      (state$BMean - as.numeric(sdat$BMeandat)[1]) / (pmax(as.numeric(sdat$BMeanSD)[1], 1e-8)^2)
  }
  if(length(state$logitCpars)){
    value <- value + sum(dnorm(state$logitCpars, mean = state$logitCMean, sd = sdat$logitCSD, log = TRUE))
    if(length(layout$C)) grad[layout$C] <- grad[layout$C] - (state$logitCpars - state$logitCMean) / (sdat$logitCSD^2)
    if(length(layout$C_mean)) grad[layout$C_mean] <- sum((state$logitCpars - state$logitCMean) / (sdat$logitCSD^2))
  }
  if(length(layout$C_mean)){
    value <- value + stats::dnorm(
      state$logitCMean,
      mean = as.numeric(sdat$logitCMeandat)[1],
      sd = pmax(as.numeric(sdat$logitCMeanSD)[1], 1e-8),
      log = TRUE
    )
    grad[layout$C_mean] <- grad[layout$C_mean] -
      (state$logitCMean - as.numeric(sdat$logitCMeandat)[1]) / (pmax(as.numeric(sdat$logitCMeanSD)[1], 1e-8)^2)
  }
  if(length(state$logitDpars)){
    value <- value + sum(dnorm(state$logitDpars, mean = state$logitDMean, sd = sdat$logitDSD, log = TRUE))
    if(length(layout$D)) grad[layout$D] <- grad[layout$D] - (state$logitDpars - state$logitDMean) / (sdat$logitDSD^2)
    if(length(layout$D_mean)) grad[layout$D_mean] <- sum((state$logitDpars - state$logitDMean) / (sdat$logitDSD^2))
  }
  if(length(layout$D_mean)){
    value <- value + stats::dnorm(
      state$logitDMean,
      mean = as.numeric(sdat$logitDMeandat)[1],
      sd = pmax(as.numeric(sdat$logitDMeanSD)[1], 1e-8),
      log = TRUE
    )
    grad[layout$D_mean] <- grad[layout$D_mean] -
      (state$logitDMean - as.numeric(sdat$logitDMeandat)[1]) / (pmax(as.numeric(sdat$logitDMeanSD)[1], 1e-8)^2)
  }

  betaScale <- as.numeric(sdat$betaScale)
  if(betaScale > 0){
    if(length(layout$ability_beta)){
      value <- value + sum(dnorm(as.numeric(state$Abilitybeta), 0, betaScale, log = TRUE))
      grad[layout$ability_beta] <- grad[layout$ability_beta] - as.numeric(state$Abilitybeta) / (betaScale^2)
    }
    if(length(layout$A_beta)){
      value <- value + sum(dnorm(as.numeric(state$invspAbeta), 0, betaScale, log = TRUE))
      grad[layout$A_beta] <- grad[layout$A_beta] - as.numeric(state$invspAbeta) / (betaScale^2)
    }
    if(length(layout$B_beta)){
      value <- value + sum(dnorm(as.numeric(state$Bbeta), 0, betaScale, log = TRUE))
      grad[layout$B_beta] <- grad[layout$B_beta] - as.numeric(state$Bbeta) / (betaScale^2)
    }
    if(length(layout$C_beta)){
      value <- value + sum(dnorm(as.numeric(state$logitCbeta), 0, betaScale, log = TRUE))
      grad[layout$C_beta] <- grad[layout$C_beta] - as.numeric(state$logitCbeta) / (betaScale^2)
    }
    if(length(layout$D_beta)){
      value <- value + sum(dnorm(as.numeric(state$logitDbeta), 0, betaScale, log = TRUE))
      grad[layout$D_beta] <- grad[layout$D_beta] - as.numeric(state$logitDbeta) / (betaScale^2)
    }
  }

  list(value = value, grad = grad)
}

bigIRT_laplace_ability_mean_contribution <- function(state, sdat, posterior, prior_precision_matrix, layout, person_terms = NULL){
  out <- list(value = 0, grad = numeric(length(layout$ability_mean)))
  if(length(layout$ability_mean) == 0L) return(out)
  theta_mode <- as.matrix(posterior$theta_mode)
  mu <- as.numeric(state$AbilityMean)
  Q <- as.matrix(prior_precision_matrix)
  centered <- sweep(theta_mode, 2, mu, "-")
  out$grad <- colSums(centered %*% Q)
  ## Adjoint. The prior mean moves the solved mode, dtheta_i/dmu = H_i^-1 Q, and
  ## the log determinant is not part of the mode condition, so dL/dtheta_i is
  ## -g_i/2 rather than zero. This is the same term the ability regression
  ## coefficients need -- mu is the intercept those coefficients are measured
  ## against -- and leaving it out ran about two per cent short against finite
  ## differences.
  if(!is.null(person_terms) && !is.null(posterior$covariance)){
    Sig <- posterior$covariance
    K <- ncol(theta_mode)
    N <- nrow(theta_mode)
    gacc <- person_terms$slope
    QS <- array(0, c(K, K, N))
    for(k in seq_len(K)) for(l in seq_len(K)) for(m in seq_len(K))
      QS[k, l, ] <- QS[k, l, ] + Q[k, m] * Sig[m, l, ]
    adj <- numeric(K)
    for(k in seq_len(K)) for(l in seq_len(K))
      adj[k] <- adj[k] + sum(QS[k, l, ] * gacc[, l])
    out$grad <- out$grad - 0.5 * adj
  }
  if(isTRUE(as.logical(sdat$dopriors))){
    mu0 <- rep_len(as.numeric(sdat$AbilityMeandat), length(mu))
    mu_sd <- pmax(rep_len(as.numeric(sdat$AbilityMeanSD), length(mu)), 1e-8)
    out$value <- out$value + sum(stats::dnorm(mu, mean = mu0, sd = mu_sd, log = TRUE))
    out$grad <- out$grad - (mu - mu0) / (mu_sd^2)
  }
  out
}

## Evaluate the frozen-mode Laplace item objective and analytic gradient.
## Inputs: packed item/global parameter vector, current state, standata, person
## modes `thetaBase`, and a cached item context. Returns objective, gradient,
## unpacked state, and row-effective quantities for diagnostics.
bigIRT_laplace_item_objective <- function(par, state, sdat, thetaBase, prior_precision, jitter = 1e-6, context = NULL, cores = 1L, prepared = NULL, row_context = NULL){
  if(is.null(context)){
    context <- bigIRT_laplace_item_context(sdat, row_context = row_context)
    context$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores, chunks_per_core = 1L)
  }
  layout <- context$layout
  curState <- bigIRT_laplace_unpack_item_state(par, state, sdat, layout = layout)
  if(is.null(context$row_ability)){
    context$row_ability <- bigIRT_laplace_fixed_row_ability(
      state = state,
      sdat = sdat,
      thetaBase = thetaBase,
      rows = context$train_rows,
      context = context$row_context
    )
  }

  ## With prepared data the per-response arrays stay in C++ and only the item
  ## parameters cross the boundary; without it, fall back to the original path.
  cpp <- if(!is.null(prepared)){
    bigIRT_laplace_item_eval(prepared,
      invspApars = curState$invspApars, invspAbeta = curState$invspAbeta,
      Bpars = curState$Bpars, Bbeta = curState$Bbeta,
      logitCpars = curState$logitCpars, logitCbeta = curState$logitCbeta,
      logitDpars = curState$logitDpars, logitDbeta = curState$logitDbeta,
      grain_size = context$grain_size)
  } else bigIRT_laplace_item_block_objective_cpp_impl(
    id = sdat$id[context$train_rows], score = sdat$score[context$train_rows], row_ability = context$row_ability,
    A_ref = context$A_ref, A_fixed_value = context$row_context$fixed_A_value,
    A_beta_row = context$A_beta_row, A_pred = context$A_pred,
    B_ref = context$B_ref, B_fixed_value = context$row_context$B_fixed_value,
    B_beta_row = context$B_beta_row, B_pred = context$B_pred,
    C_ref = context$C_ref, C_fixed_value = context$row_context$C_fixed_value,
    C_beta_row = context$C_beta_row, C_pred = context$C_pred,
    D_ref = context$D_ref, D_fixed_value = context$row_context$D_fixed_value,
    D_beta_row = context$D_beta_row, D_pred = context$D_pred,
    invspApars = curState$invspApars, invspAbeta = curState$invspAbeta,
    Bpars = curState$Bpars, Bbeta = curState$Bbeta,
    logitCpars = curState$logitCpars, logitCbeta = curState$logitCbeta,
    logitDpars = curState$logitDpars, logitDbeta = curState$logitDbeta,
    prior_precision = prior_precision, jitter = jitter, max_attempts = 8L,
    grain_size = context$grain_size)

  grad <- numeric(length(par))
  if(length(layout$A)){
    grad[layout$A] <- grad[layout$A] + cpp$grad_A
    if(sdat$NAitemPreds > 0 && length(layout$A_beta)) grad[layout$A_beta] <- grad[layout$A_beta] + as.numeric(cpp$grad_A_beta)
  }

  if(length(layout$B)){
    grad[layout$B] <- grad[layout$B] + cpp$grad_B
    if(sdat$NBitemPreds > 0 && length(layout$B_beta)) grad[layout$B_beta] <- grad[layout$B_beta] + as.numeric(cpp$grad_B_beta)
  }

  if(length(layout$C)){
    grad[layout$C] <- grad[layout$C] + cpp$grad_C
    if(sdat$NCitemPreds > 0 && length(layout$C_beta)) grad[layout$C_beta] <- grad[layout$C_beta] + as.numeric(cpp$grad_C_beta)
  }

  if(length(layout$D)){
    grad[layout$D] <- grad[layout$D] + cpp$grad_D
    if(sdat$NDitemPreds > 0 && length(layout$D_beta)) grad[layout$D_beta] <- grad[layout$D_beta] + as.numeric(cpp$grad_D_beta)
  }

  prior <- bigIRT_laplace_item_prior(curState, sdat)
  ## The C++ objective accumulates the data log-likelihood at the modes, the
  ## Gaussian constant and the log-determinant, but not the ability prior
  ## evaluated at those modes. The Laplace approximation to the marginal is
  ##   l(theta_hat) + log p(theta_hat) + K/2 log 2pi - 1/2 log|H|,
  ## so the quadratic -1/2 (theta_hat - mu)' Q (theta_hat - mu) belongs in it.
  ##
  ## Omitting it is not a constant offset: theta_hat moves with the item
  ## parameters. Worse, it is the term that makes df/dtheta vanish at the mode,
  ## so without it the envelope theorem does not apply to the reported
  ## objective, and the analytic gradient -- which is correct for the complete
  ## objective -- disagrees with finite differences of the incomplete one by a
  ## factor that looks like a broken adjoint.
  ability_quad <- 0
  if(!is.null(thetaBase) && !is.null(prior_precision)){
    th <- as.matrix(thetaBase)
    mu <- as.numeric(curState$AbilityMean)
    if(length(mu) == ncol(th)){
      dev <- sweep(th, 2L, mu, "-")
      if(!is.null(sdat$fixedAbilityLogical)) dev[sdat$fixedAbilityLogical != 0L] <- 0
      ## Contract over the K x K x Nsubs precision array with K^2 vector
      ## operations rather than one small matrix product per subject: the loop
      ## version cost more than the rest of the objective at realistic sizes.
      K <- ncol(th)
      if(K == 1L){
        ability_quad <- sum(dev[, 1L]^2 * as.numeric(prior_precision))
      } else {
        ## Index the K x K x Nsubs array in place. Re-wrapping it with array()
        ## copies the whole thing on every objective evaluation, and the item
        ## line search calls this many times per outer iteration.
        tmp <- matrix(0, nrow(th), K)
        for(r in seq_len(K)) for(cc in seq_len(K))
          tmp[, r] <- tmp[, r] + prior_precision[r, cc, ] * dev[, cc]
        ability_quad <- sum(dev * tmp)
      }
    }
  }

  list(
    value = cpp$objective + prior$value - 0.5 * ability_quad,
    grad = grad + prior$grad,
    state = curState,
    rowEffective = NULL,
    cpp = cpp
  )
}

bigIRT_laplace_person_step_block_cpp_impl <- function(id, score, theta_init,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask,
  jitter, max_attempts, max_iter, tol, keep_covariance = FALSE, grain_size = 64L){
  .Call(
    `_bigIRT_laplace_person_step_block_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(theta_init),
    as.matrix(person_pred),
    matrix(as.integer(fixed_ability), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(fixed_ability_value),
    matrix(as.integer(A_ref), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(A_fixed_value),
    matrix(as.integer(A_beta_row), nrow = nrow(person_pred), ncol = ncol(theta_init)),
    as.matrix(A_pred),
    as.integer(B_ref),
    as.numeric(B_fixed_value),
    as.integer(B_beta_row),
    as.matrix(B_pred),
    as.integer(C_ref),
    as.numeric(C_fixed_value),
    as.integer(C_beta_row),
    as.matrix(C_pred),
    as.integer(D_ref),
    as.numeric(D_fixed_value),
    as.integer(D_beta_row),
    as.matrix(D_pred),
    as.matrix(Abilitybeta),
    as.numeric(invspApars),
    as.matrix(invspAbeta),
    as.numeric(Bpars),
    as.matrix(Bbeta),
    as.numeric(logitCpars),
    as.matrix(logitCbeta),
    as.numeric(logitDpars),
    as.matrix(logitDbeta),
    as.matrix(prior_mean),
    prior_precision,
    matrix(as.integer(free_mask), nrow = nrow(theta_init), ncol = ncol(theta_init)),
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(max_iter),
    as.numeric(tol),
    as.logical(keep_covariance),
    as.integer(grain_size)
  )
}


## Evaluate the direct Laplace objective by solving all person modes inside the
## objective call. This is the experimental single-stage backend used by
## `marginalApprox = "laplace_direct"`.
bigIRT_laplace_direct_objective <- function(par, state, sdat, prior_precision,
  theta_init = state$AbilityBase, jitter = 1e-6, max_attempts = 8L,
  max_iter = 50L, tol = 1e-4, keep_covariance = FALSE, context = NULL){
  wall_time_sec <- function() as.numeric(proc.time()[["elapsed"]])
  t_total0 <- wall_time_sec()
  estimateAbilityCorr <- isTRUE(context$estimateAbilityCorr)
  t_setup0 <- wall_time_sec()
  if(is.null(context)) {
    direct_layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = estimateAbilityCorr)
    item_layout <- direct_layout[setdiff(names(direct_layout), c("corr", "ability_mean", "ability_beta"))]
    context <- bigIRT_laplace_item_context(sdat, layout = item_layout)
    context$direct_layout <- direct_layout
    context$estimateAbilityCorr <- estimateAbilityCorr
    context$corr_paramization <- if(!is.null(state$laplaceCorrParam)) state$laplaceCorrParam else "normalized_chol"
  }
  curState <- bigIRT_laplace_unpack_direct_state(
    par, state, sdat,
    layout = context$direct_layout,
    corr_paramization = context$corr_paramization
  )
  row_context <- context$row_context

  # Some C++ kernels have brittle handling for 0-column Eigen matrices.
  # When there are no person predictors, replace 0-column predictor inputs
  # with 1-column all-zero matrices (math unchanged).
  person_pred <- row_context$person_pred
  ability_beta <- curState$Abilitybeta
  if(ncol(person_pred) == 0L){
    person_pred <- matrix(0, nrow = nrow(person_pred), ncol = 1L)
    ability_beta <- matrix(0, nrow = nrow(ability_beta), ncol = 1L)
  }

  prior_mean <- matrix(rep(curState$AbilityMean, each = sdat$Nsubs), nrow = sdat$Nsubs)
  prior_mats <- if(isTRUE(context$estimateAbilityCorr)) {
    bigIRT_laplace_prior_mats(sdat, AbilityCorr = curState$AbilityCorr, jitter = jitter)
  } else {
    list(
      corr = as.matrix(sdat$AbilityCorr),
      precision = if(length(dim(prior_precision)) == 3L) prior_precision[,,1] else prior_precision,
      precision_array = prior_precision
    )
  }
  setup_sec <- wall_time_sec() - t_setup0
  t_kernel0 <- wall_time_sec()
  if(isTRUE(getOption("bigIRT.debug.direct", FALSE))) message("bigIRT debug: entering direct_fg kernel")
  direct_fg <- bigIRT_laplace_direct_block_fg_cpp_impl(
    id = sdat$id[context$train_rows],
    score = sdat$score[context$train_rows],
    theta_init = theta_init,
    person_pred = person_pred,
    fixed_ability = row_context$fixed_ability,
    fixed_ability_value = row_context$fixed_ability_value,
    A_ref = row_context$A_ref,
    A_fixed_value = row_context$fixed_A_value,
    A_beta_row = row_context$A_beta_row,
    A_pred = row_context$A_pred,
    B_ref = row_context$B_ref,
    B_fixed_value = row_context$B_fixed_value,
    B_beta_row = row_context$B_beta_row,
    B_pred = row_context$B_pred,
    C_ref = row_context$C_ref,
    C_fixed_value = row_context$C_fixed_value,
    C_beta_row = row_context$C_beta_row,
    C_pred = row_context$C_pred,
    D_ref = row_context$D_ref,
    D_fixed_value = row_context$D_fixed_value,
    D_beta_row = row_context$D_beta_row,
    D_pred = row_context$D_pred,
    Abilitybeta = ability_beta,
    invspApars = curState$invspApars,
    invspAbeta = curState$invspAbeta,
    Bpars = curState$Bpars,
    Bbeta = curState$Bbeta,
    logitCpars = curState$logitCpars,
    logitCbeta = curState$logitCbeta,
    logitDpars = curState$logitDpars,
    logitDbeta = curState$logitDbeta,
    prior_mean = prior_mean,
    prior_precision = prior_mats$precision_array,
    free_mask = 1L - sdat$fixedAbilityLogical,
    jitter = jitter,
    max_attempts = max_attempts,
    max_iter = max_iter,
    tol = tol,
    keep_covariance = keep_covariance,
    grain_size = context$grain_size
  )
  if(isTRUE(getOption("bigIRT.debug.direct", FALSE))) message("bigIRT debug: direct_fg kernel returned")
  kernel_sec <- wall_time_sec() - t_kernel0
  kernel_timings <- direct_fg$timings
  posterior <- direct_fg$posterior
  laplace_value <- direct_fg$value
  t_prior0 <- wall_time_sec()
  prior_norm_value <- if(isTRUE(context$estimateAbilityCorr)) {
    0.5 * sdat$Nsubs * as.numeric(determinant(prior_mats$precision, logarithm = TRUE)$modulus)
  } else 0
  prior <- bigIRT_laplace_item_prior(curState, sdat, layout = context$direct_layout)
  prior_sec <- wall_time_sec() - t_prior0
  t_grad0 <- wall_time_sec()
  approx_grad <- numeric(length(par))
  item_fg <- direct_fg$item_fg
  if(length(context$layout$A)){
    approx_grad[context$layout$A] <- approx_grad[context$layout$A] + as.numeric(item_fg$grad_A)
    if(sdat$NAitemPreds > 0 && length(context$layout$A_beta)) approx_grad[context$layout$A_beta] <- approx_grad[context$layout$A_beta] + as.numeric(item_fg$grad_A_beta)
  }
  if(length(context$layout$B)){
    approx_grad[context$layout$B] <- approx_grad[context$layout$B] + as.numeric(item_fg$grad_B)
    if(sdat$NBitemPreds > 0 && length(context$layout$B_beta)) approx_grad[context$layout$B_beta] <- approx_grad[context$layout$B_beta] + as.numeric(item_fg$grad_B_beta)
  }
  if(length(context$layout$C)){
    approx_grad[context$layout$C] <- approx_grad[context$layout$C] + as.numeric(item_fg$grad_C)
    if(sdat$NCitemPreds > 0 && length(context$layout$C_beta)) approx_grad[context$layout$C_beta] <- approx_grad[context$layout$C_beta] + as.numeric(item_fg$grad_C_beta)
  }
  if(length(context$layout$D)){
    approx_grad[context$layout$D] <- approx_grad[context$layout$D] + as.numeric(item_fg$grad_D)
    if(sdat$NDitemPreds > 0 && length(context$layout$D_beta)) approx_grad[context$layout$D_beta] <- approx_grad[context$layout$D_beta] + as.numeric(item_fg$grad_D_beta)
  }
  if(length(prior$grad)) approx_grad[seq_along(prior$grad)] <- approx_grad[seq_along(prior$grad)] + prior$grad
  item_grad_sec <- wall_time_sec() - t_grad0
  ## The ability-predictor and latent-correlation gradients both need the
  ## per-person score and log-determinant slope, so build the response rows and
  ## those sums once and share them.
  need_person_terms <- length(context$direct_layout$ability_beta) > 0L ||
    (isTRUE(context$estimateAbilityCorr) && length(context$direct_layout$corr) > 0L) ||
    length(context$direct_layout$ability_mean) > 0L
  person_terms <- NULL
  if(need_person_terms){
    row_effective <- bigIRT_laplace_row_effective(
      state = curState,
      sdat = sdat,
      thetaBase = posterior$theta_mode,
      rows = context$train_rows,
      context = row_context
    )
    person_terms <- bigIRT_laplace_person_row_terms(
      sdat = sdat, posterior = posterior,
      row_effective = row_effective, row_context = row_context
    )
  }
  if(length(context$direct_layout$ability_beta)){
    ## Accumulate. The item prior above already wrote this slot's beta prior
    ## gradient, and assigning over it dropped that term; every item beta slot
    ## accumulates for the same reason. Against finite differences the missing
    ## piece scaled exactly as 1/betaScale^2 -- invisible at a loose prior,
    ## worth .08 per cent at betaScale 5 and 8 per cent at betaScale 1.
    approx_grad[context$direct_layout$ability_beta] <- approx_grad[context$direct_layout$ability_beta] + as.numeric(
      bigIRT_laplace_ability_beta_contribution(
        state = curState,
        sdat = sdat,
        posterior = posterior,
        row_context = row_context,
        layout = context$direct_layout,
        prior_precision = prior_precision,
        person_terms = person_terms
      )
    )
  }
  ability_mean_sec <- 0
  if(length(context$direct_layout$ability_mean)){
    t_mean0 <- wall_time_sec()
    ability_mean_contrib <- bigIRT_laplace_ability_mean_contribution(
      state = curState,
      sdat = sdat,
      posterior = posterior,
      prior_precision_matrix = prior_mats$precision,
      layout = context$direct_layout,
      person_terms = person_terms
    )
    approx_grad[context$direct_layout$ability_mean] <- ability_mean_contrib$grad
    prior$value <- prior$value + ability_mean_contrib$value
    ability_mean_sec <- wall_time_sec() - t_mean0
  }
  corr_grad_sec <- 0
  if(isTRUE(context$estimateAbilityCorr) && length(context$direct_layout$corr)){
    t_corr0 <- wall_time_sec()
    approx_grad[context$direct_layout$corr] <- approx_grad[context$direct_layout$corr] + bigIRT_laplace_corr_grad(
      state = curState,
      sdat = sdat,
      layout = context$direct_layout,
      posterior = posterior,
      prior_precision_matrix = prior_mats$precision,
      logdet_slope = person_terms$slope,
      jitter = jitter
    )
    corr_grad_sec <- wall_time_sec() - t_corr0
  }
  total_sec <- wall_time_sec() - t_total0
  list(
    value = laplace_value + prior_norm_value + prior$value,
    approx_grad = approx_grad,
    state = curState,
    prior_mats = prior_mats,
    posterior = posterior,
    prior = prior,
    rowEffective = NULL,
    item_fg = item_fg,
    timings = list(
      totalSec = total_sec,
      setupSec = setup_sec,
      kernelSec = if(!is.null(kernel_timings$kernelSec)) kernel_timings$kernelSec else kernel_sec,
      personKernelSec = if(!is.null(kernel_timings$personKernelSec)) kernel_timings$personKernelSec else NA_real_,
      rowAssemblySec = if(!is.null(kernel_timings$rowAssemblySec)) kernel_timings$rowAssemblySec else NA_real_,
      itemKernelSec = if(!is.null(kernel_timings$itemKernelSec)) kernel_timings$itemKernelSec else NA_real_,
      priorSec = prior_sec,
      itemGradSec = item_grad_sec,
      abilityMeanSec = ability_mean_sec,
      corrGradSec = corr_grad_sec,
      postKernelSec = if(!is.null(kernel_timings$postKernelSec)) kernel_timings$postKernelSec else (total_sec - kernel_sec)
    )
  )
}

bigIRT_laplace_ability_beta_mstep_cpp_impl <- function(theta_residual, person_pred,
  ability_beta, free_mask, beta_scale, jitter){
  .Call(`_bigIRT_laplace_ability_beta_mstep_cpp_impl`, as.matrix(theta_residual),
    as.matrix(person_pred), as.matrix(ability_beta), matrix(as.integer(free_mask),
      nrow=nrow(theta_residual), ncol=ncol(theta_residual)), as.numeric(beta_scale), as.numeric(jitter))
}



## Optimize the direct Laplace objective with person modes solved inside each
## objective evaluation. The current implementation uses the exact direct
## objective value together with the frozen-mode Laplace gradient as a proxy
## gradient for efficient experimentation.
bigIRT_laplace_optimize_direct <- function(state, sdat, prior_precision,
  niter = 50L, tol = 1e-4, jitter = 1e-6, person_tol = 1e-4,
  keep_covariance = FALSE, cores = 1L, estimateAbilityCorr = FALSE,
  corr_paramization = c("normalized_chol", "stan_corsqrt"),
  collect_history = FALSE, plot_callback = NULL, plot_every = 1L,
  verbose = 0L, trace_fn = NULL, stochastic = FALSE){
  corr_paramization <- match.arg(corr_paramization)
  direct_layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = estimateAbilityCorr)
  item_layout <- direct_layout[setdiff(names(direct_layout), c("corr", "ability_mean", "ability_beta"))]
  context <- bigIRT_laplace_item_context(sdat, layout = item_layout)
  context$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  context$direct_layout <- direct_layout
  context$estimateAbilityCorr <- isTRUE(estimateAbilityCorr)
  context$corr_paramization <- corr_paramization
  state$laplaceCorrParam <- corr_paramization
  init <- bigIRT_laplace_pack_direct_state(state, sdat, layout = direct_layout)
  if(length(init) == 0L){
    eval0 <- bigIRT_laplace_direct_objective(
      par = init, state = state, sdat = sdat, prior_precision = prior_precision,
      theta_init = state$AbilityBase, jitter = jitter, max_iter = max(20L, as.integer(niter)),
      tol = person_tol, keep_covariance = keep_covariance, context = context
    )
    return(list(state = eval0$state, optim = list(par = init, value = eval0$value, masked_grad_norm = 0, target_evals = 1L, iter = 0L, terminate = list(what = "no_free_item_parameters", val = NA_real_)), eval = eval0))
  }

  eval_count <- 0L
  theta_warm <- state$AbilityBase
  cache_par <- NULL
  cache_eval <- NULL
  history <- list()
  last_value <- NULL
  last_par <- NULL
  last_theta <- NULL
  get_eval <- function(par){
    if(!is.null(cache_par) && identical(as.numeric(par), cache_par)) return(cache_eval)
    t_eval <- as.numeric(proc.time()[["elapsed"]])
    eval_count <<- eval_count + 1L
    res <- bigIRT_laplace_direct_objective(
      par = par,
      state = state,
      sdat = sdat,
      prior_precision = prior_precision,
      theta_init = theta_warm,
      jitter = jitter,
      max_attempts = 8L,
      max_iter = max(20L, as.integer(niter * 2L)),
      tol = person_tol,
      keep_covariance = keep_covariance,
      context = context
    )
    eval_sec <- as.numeric(proc.time()[["elapsed"]]) - t_eval
    # Keep the last solved modes as a warm start for a *new* candidate, but
    # never rerun a candidate merely because mize requests its fn/gr pair.
    theta_warm <<- res$posterior$theta_mode
    cache_par <<- as.numeric(par)
    cache_eval <<- res
    if(as.integer(verbose) >= 2L){
      tt <- res$timings
      trace_msg <- sprintf(
        paste0(
          "Direct Laplace eval %d: total=%.3fs | person=%.3fs | row=%.3fs | item=%.3fs | ",
          "post=%.3fs | setup=%.3fs | prior=%.3fs | item_grad=%.3fs | mean_grad=%.3fs | corr_grad=%.3fs | ",
          "obj=%.6f | grad=%.3g"
        ),
        eval_count,
        if(!is.null(tt$totalSec)) tt$totalSec else eval_sec,
        if(!is.null(tt$personKernelSec)) tt$personKernelSec else NA_real_,
        if(!is.null(tt$rowAssemblySec)) tt$rowAssemblySec else NA_real_,
        if(!is.null(tt$itemKernelSec)) tt$itemKernelSec else if(!is.null(tt$kernelSec)) tt$kernelSec else NA_real_,
        if(!is.null(tt$postKernelSec)) tt$postKernelSec else NA_real_,
        if(!is.null(tt$setupSec)) tt$setupSec else NA_real_,
        if(!is.null(tt$priorSec)) tt$priorSec else NA_real_,
        if(!is.null(tt$itemGradSec)) tt$itemGradSec else NA_real_,
        if(!is.null(tt$abilityMeanSec)) tt$abilityMeanSec else 0,
        if(!is.null(tt$corrGradSec)) tt$corrGradSec else 0,
        res$value,
        sqrt(sum(res$approx_grad^2))
      )
      if(is.function(trace_fn)) trace_fn(trace_msg) else message(trace_msg)
    }
    if(isTRUE(collect_history)){
      cur_theta <- res$posterior$theta_mode
      cov_arr <- res$posterior$covariance
      if(!is.null(cov_arr)){
        post_sd <- unlist(lapply(seq_len(dim(cov_arr)[3]), function(ii) sqrt(pmax(diag(cov_arr[,,ii]), 0))))
        post_sd <- post_sd[is.finite(post_sd)]
        if(length(post_sd)){
          mean_post_sd <- mean(post_sd, na.rm = TRUE)
          max_post_sd <- max(post_sd, na.rm = TRUE)
        } else {
          mean_post_sd <- NA_real_
          max_post_sd <- NA_real_
        }
      } else {
        mean_post_sd <- NA_real_
        max_post_sd <- NA_real_
      }
      history[[length(history) + 1L]] <<- list(
        outerIter = eval_count,
        objective = res$value,
        relativeImprove = if(is.null(last_value) || !is.finite(last_value) || abs(last_value) < .Machine$double.eps) NA_real_ else abs((res$value - last_value) / last_value),
        itemStepRms = if(is.null(last_par)) NA_real_ else sqrt(mean((par - last_par)^2)),
        personStepRms = if(is.null(last_theta)) NA_real_ else sqrt(mean((as.numeric(cur_theta) - as.numeric(last_theta))^2)),
        itemGradNorm = sqrt(sum(res$approx_grad^2)),
        meanPosteriorSD = mean_post_sd,
        maxPosteriorSD = max_post_sd,
        personConverged = all(res$posterior$converged),
        personStepSec = NA_real_,
        itemStepSec = eval_sec,
        refreshStepSec = NA_real_,
        objectiveEvalSec = eval_sec,
        outerIterSec = eval_sec,
        personKernelSec = if(!is.null(res$timings$personKernelSec)) res$timings$personKernelSec else NA_real_,
        rowAssemblySec = if(!is.null(res$timings$rowAssemblySec)) res$timings$rowAssemblySec else NA_real_,
        itemKernelSec = if(!is.null(res$timings$itemKernelSec)) res$timings$itemKernelSec else NA_real_,
        kernelSec = if(!is.null(res$timings$kernelSec)) res$timings$kernelSec else NA_real_,
        setupSec = if(!is.null(res$timings$setupSec)) res$timings$setupSec else NA_real_,
        priorSec = if(!is.null(res$timings$priorSec)) res$timings$priorSec else NA_real_,
        itemGradSec = if(!is.null(res$timings$itemGradSec)) res$timings$itemGradSec else NA_real_,
        abilityMeanSec = if(!is.null(res$timings$abilityMeanSec)) res$timings$abilityMeanSec else NA_real_,
        corrGradSec = if(!is.null(res$timings$corrGradSec)) res$timings$corrGradSec else NA_real_,
        itemTargetEvals = eval_count,
        itemMaskedGradNorm = sqrt(sum(res$approx_grad^2)),
        optimizerIter = NA_integer_,
        optimizerTerminate = NA_character_,
        optimizerTerminateValue = NA_real_,
        personMeanNiter = mean(res$posterior$niter, na.rm = TRUE),
        personMaxNiter = max(res$posterior$niter, na.rm = TRUE),
        strictCriterion = FALSE,
        stabilityCriterion = FALSE,
        recentObjectiveRange = NA_real_,
        recentGradRelChange = NA_real_,
        recentItemStepMean = NA_real_,
        recentPersonStepMean = NA_real_,
        strictStreak = 0L,
        stabilityStreak = 0L
      )
      last_value <<- res$value
      last_par <<- par
      last_theta <<- cur_theta
      if(!is.null(plot_callback) && (eval_count %% max(1L, as.integer(plot_every)) == 0L)){
        try(plot_callback(history), silent = TRUE)
      }
    }
    cache_eval
  }
  target_fg <- function(par){
    res <- get_eval(par)
    list(fn = -res$value, gr = -res$approx_grad)
  }
  target_fn <- function(par){
    target_fg(par)$fn
  }
  target_gr <- function(par){
    target_fg(par)$gr
  }
  if(isTRUE(stochastic)){
    target <- function(par){
      res <- get_eval(par)
      out <- res$value
      attributes(out)$gradient <- res$approx_grad
      out
    }
    fit <- sgd(
      init = init,
      maxiter = niter,
      fitfunc = target,
      itertol = tol
    )
    fit$iter <- length(fit$itervalues)
    fit$terminate <- list(
      what = if(length(fit$itervalues) < as.integer(niter)) "sgd_converged" else "sgd_max_iter",
      val = if(length(fit$itervalues)) tail(fit$itervalues, 1L) else NA_real_
    )
  } else {
    fit <- mize::mize(
      init,
      fg = list(fg = target_fg, fn = target_fn, gr = target_gr),
      max_iter = niter,
      method = "L-BFGS",
      memory = 30,
      line_search = "Schmidt",
      # c1 = 1e-10,
      # c2 = 0.9,
      step0 = "schmidt",
      ls_max_fn = 20L,
      abs_tol = tol,
      grad_tol = tol,
      rel_tol = 0,
      step_tol = 0,
      ginf_tol = 0
    )
  }
  final <- get_eval(fit$par)
  fit$masked_grad_norm <- sqrt(sum(final$approx_grad^2))
  fit$target_evals <- eval_count
  fit$logLik <- final$value
  fit$timings <- final$timings
  if(length(history)){
    history[[length(history)]][["optimizerIter"]] <- if(!is.null(fit$iter)) fit$iter else NA_integer_
    history[[length(history)]][["optimizerTerminate"]] <- if(!is.null(fit$terminate$what)) as.character(fit$terminate$what) else NA_character_
    history[[length(history)]][["optimizerTerminateValue"]] <- if(!is.null(fit$terminate$val)) fit$terminate$val else NA_real_
  }
  list(state = final$state, optim = fit, eval = final, history = history)
}

bigIRT_laplace_transformed_betas <- function(state, sdat){
  out <- list(
    Abilitybeta = state$Abilitybeta,
    Bbeta = state$Bbeta,
    Abeta = state$invspAbeta,
    Cbeta = state$logitCbeta,
    Dbeta = state$logitDbeta
  )

  if(length(state$invspAbeta)){
    if(nrow(state$invspAbeta) == 1L){
      a_mult <- mean(inv_logit(state$invspApars), na.rm = TRUE)
      if(!is.finite(a_mult)) a_mult <- 0
      out$Abeta <- state$invspAbeta * a_mult
    } else {
      out$Abeta <- state$invspAbeta * matrix(
        inv_logit(state$invspApars),
        nrow = nrow(state$invspAbeta),
        ncol = ncol(state$invspAbeta)
      )
    }
  }

  if(length(state$logitCbeta)){
    c_mult_vec <- 0.5 * inv_logit(state$logitCpars) * (1 - inv_logit(state$logitCpars))
    if(nrow(state$logitCbeta) == 1L){
      c_mult <- mean(c_mult_vec, na.rm = TRUE)
      if(!is.finite(c_mult)) c_mult <- 0
      out$Cbeta <- state$logitCbeta * c_mult
    } else {
      out$Cbeta <- state$logitCbeta * matrix(c_mult_vec, nrow = nrow(state$logitCbeta), ncol = ncol(state$logitCbeta))
    }
  }

  if(length(state$logitDbeta)){
    d_sig <- inv_logit(state$logitDpars)
    d_mult_vec <- 0.5 * d_sig * (1 - d_sig)
    if(nrow(state$logitDbeta) == 1L){
      d_mult <- mean(d_mult_vec, na.rm = TRUE)
      if(!is.finite(d_mult)) d_mult <- 0
      out$Dbeta <- state$logitDbeta * d_mult
    } else {
      out$Dbeta <- state$logitDbeta * matrix(d_mult_vec, nrow = nrow(state$logitDbeta), ncol = ncol(state$logitDbeta))
    }
  }

  out
}

## Per-person score and log-determinant slope, summed over a person's responses.
##
##   score_i = dl/dtheta_i
##   slope_i = dlog|H_i|/dtheta_i
##
## Both the ability-predictor and the latent-correlation gradients need the
## slope, and it is the same quantity, so it is built once per objective
## evaluation and shared. Vectorised over responses on purpose: the obvious
## per-response loop cost more than the rest of the fit put together.
bigIRT_laplace_person_row_terms <- function(sdat, posterior, row_effective, row_context){
  if(is.null(posterior$covariance)) stop("bigIRT_laplace_person_row_terms needs posterior covariances; the caller must set keep_covariance.")
  K <- as.integer(sdat$Nscales)
  N <- as.integer(sdat$Nsubs)
  ids <- as.integer(row_context$ids)
  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)
  y <- as.numeric(sdat$score[row_context$rows])

  eta <- as.numeric(row_effective$eta_row)
  c_row <- as.numeric(row_effective$c_row)
  d_row <- as.numeric(row_effective$d_row)
  gg <- inv_logit(eta)
  qq <- gg * (1 - gg)
  u <- d_row - c_row
  p <- pmin(pmax(c_row + u * gg, 1e-12), 1 - 1e-12)
  s <- u * qq
  r <- pmax(p * (1 - p), 1e-12)
  grad_eta <- ((y - p) / r) * s
  dq_deta <- qq * (1 - 2 * gg)
  ds_deta <- u * dq_deta
  dr_deta <- s * (1 - 2 * p)
  dw_deta <- (2 * s * ds_deta * r - s^2 * dr_deta) / (r^2)

  ## aSa = a' Sigma a per response, assembled from the K^2 posterior blocks.
  Sig <- posterior$covariance
  aSa <- numeric(length(ids))
  for(k in seq_len(K)) for(l in seq_len(K))
    aSa <- aSa + lo[, k] * lo[, l] * Sig[k, l, ids]

  ## rowsum(reorder = FALSE) returns groups in the order they are encountered,
  ## not in level order, so the result has to be scattered back by label rather
  ## than assumed aligned. Response data are usually ordered by person, which
  ## makes the two coincide here and hid the difference; they do not coincide
  ## when the grouping is by item.
  acc_raw <- rowsum(cbind(grad_eta * lo, (dw_deta * aSa) * lo), group = ids)
  acc <- matrix(0, N, 2L * K)
  acc[as.integer(rownames(acc_raw)), ] <- acc_raw
  list(score = acc[, seq_len(K), drop = FALSE],
       slope = acc[, K + seq_len(K), drop = FALSE])
}

## Gradient of the direct Laplace objective with respect to the ability
## regression coefficients.
##
## Ability enters as theta_i = base_i + beta x_i, with the prior on base_i, so
## beta moves the likelihood and the log determinant directly and moves the
## solved mode with them. Writing s_i for the score, H_i for the person
## precision, Q for the prior precision and g_i = dlog|H_i|/dtheta, the mode
## condition s_i - Q base_i = 0 makes dL/dbase_i equal -g_i/2 rather than zero,
## because the log determinant is not part of that condition. Differentiating
## the mode condition gives dbase_i/dbeta_km = x_im H_i^-1 J e_k with
## J = Q - H_i, and the J terms cancel down to
##
##   dL/dbeta = sum_i [ s_i - (1/2) Q H_i^-1 g_i ] x_i'
##
## An earlier version accumulated s_i - g_i/2, dropping the Q H_i^-1 that maps
## the log-determinant slope through the posterior. That factor varies from
## person to person, which is why its error against finite differences moved
## around instead of being a constant. H_i^-1 is the stored posterior
## covariance, which is why this needs keep_covariance.
bigIRT_laplace_ability_beta_contribution <- function(state, sdat, posterior, row_context, layout, prior_precision, person_terms){
  out <- matrix(0, nrow = sdat$Nscales, ncol = sdat$NpersonPreds)
  if(length(layout$ability_beta) == 0L || sdat$NpersonPreds == 0L) return(out)
  if(is.null(posterior$covariance)) stop("laplace_direct Abilitybeta gradients require posterior covariances.")
  if(missing(prior_precision) || is.null(prior_precision)) stop("laplace_direct Abilitybeta gradients require the prior precision.")

  K <- as.integer(sdat$Nscales)
  N <- as.integer(sdat$Nsubs)
  P <- as.integer(sdat$NpersonPreds)
  ids <- as.integer(row_context$ids)
  Sig <- posterior$covariance
  sacc <- person_terms$score
  gacc <- person_terms$slope

  ## One covariate row and one fixed-ability mask per person; both are constant
  ## within person, so writing every response in turn leaves the right value.
  xmat <- matrix(0, N, P)
  xmat[ids, ] <- as.matrix(row_context$person_pred)
  fixedmask <- matrix(FALSE, N, K)
  fixedmask[ids, ] <- as.logical(row_context$fixed_ability)

  ## contrib_i = s_i - (1/2) Q_i Sigma_i g_i, built blockwise across persons.
  prior_is_array <- length(dim(prior_precision)) == 3L
  QS <- array(0, c(K, K, N))
  for(k in seq_len(K)) for(l in seq_len(K)) for(m in seq_len(K)){
    qkm <- if(prior_is_array) prior_precision[k, m, ] else prior_precision[k, m]
    QS[k, l, ] <- QS[k, l, ] + qkm * Sig[m, l, ]
  }
  contrib <- sacc
  for(k in seq_len(K)) for(l in seq_len(K))
    contrib[, k] <- contrib[, k] - 0.5 * QS[k, l, ] * gacc[, l]
  contrib[fixedmask] <- 0

  out <- t(contrib) %*% xmat
  dimnames(out) <- NULL
  out
}


bigIRT_laplace_constrained_pars <- function(state, sdat, posterior = NULL){
  rows <- seq_len(sdat$Nobs)
  row_context <- bigIRT_laplace_row_context(sdat, rows = rows)
  # Final reporting intentionally materializes the response rows in R.  This
  # keeps the long-row predictor contract explicit and avoids an Rcpp return
  # boundary after a native fit.  The likelihood itself remains native.
  effective <- bigIRT_laplace_row_effective(state, sdat, thetaBase = state$AbilityBase,
    rows = rows, context = row_context)
  personPredsMean <- bigIRT_laplace_person_means_by_id(sdat)
  itemPredsMean <- bigIRT_laplace_item_means(sdat)
  Ability <- state$AbilityBase
  if(sdat$NpersonPreds > 0L) Ability <- Ability + personPredsMean %*% t(state$Abilitybeta)
  for(k in seq_len(sdat$Nscales)){
    fixed <- sdat$fixedAbilityLogical[,k] == 1L
    Ability[fixed,k] <- sdat$Abilitydata[fixed,k]
  }
  first_item_row <- match(seq_len(sdat$Nitems), row_context$items)
  materialized <- list(
    A = effective$loadings[first_item_row,,drop = FALSE],
    B = effective$b_row[first_item_row], C = effective$c_row[first_item_row],
    D = effective$d_row[first_item_row], Ability = Ability,
    itemPredsMean = itemPredsMean, personPredsMean = personPredsMean,
    b_row = effective$b_row, c_row = effective$c_row, d_row = effective$d_row,
    eta_row = effective$eta_row, row_loadings = effective$loadings,
    row_ability = effective$row_ability,
    pcorrect = cfunc(effective$eta_row) + (dfunc(effective$eta_row) - cfunc(effective$eta_row)) * 0,
    p = numeric(length(rows))
  )
  materialized$pcorrect <- effective$c_row + (effective$d_row - effective$c_row) * stats::plogis(effective$eta_row)
  materialized$p <- ifelse(sdat$score[rows] == 1L, materialized$pcorrect, 1 - materialized$pcorrect)

  out <- list(
    A = materialized$A,
    B = matrix(materialized$B, ncol = 1),
    C = matrix(materialized$C, ncol = 1),
    D = matrix(materialized$D, ncol = 1),
    Ability = materialized$Ability,
    Abilitypars = as.numeric(state$AbilityBase[sdat$Abilityparsindex > 0]),
    AbilityMeanpar = if(sdat$fixedAbilityMean == 0L) as.numeric(state$AbilityMean) else numeric(),
    Abilitybeta = state$Abilitybeta,
    Bpars = as.numeric(state$Bpars),
    BMeanpar = if(sdat$fixedBMean == 0L) state$BMean else numeric(),
    Bbeta = state$Bbeta,
    invspApars = as.numeric(state$invspApars),
    invspAMeanpar = if(sdat$fixedAMean == 0L) state$invspAMean else numeric(),
    invspAbeta = state$invspAbeta,
    logitCpars = as.numeric(state$logitCpars),
    logitCMeanpar = if(sdat$fixedCMean == 0L) state$logitCMean else numeric(),
    logitCbeta = state$logitCbeta,
    logitDpars = as.numeric(state$logitDpars),
    logitDMeanpar = if(sdat$fixedDMean == 0L) state$logitDMean else numeric(),
    logitDbeta = state$logitDbeta,
    itemPredsMean = materialized$itemPredsMean,
    personPredsMean = materialized$personPredsMean,
    b_row = materialized$b_row,
    c_row = materialized$c_row,
    d_row = materialized$d_row,
    eta_row = materialized$eta_row,
    row_loadings = materialized$row_loadings,
    row_ability = materialized$row_ability,
    p = materialized$p,
    pcorrect = materialized$pcorrect
  )
  beta_alias <- bigIRT_laplace_transformed_betas(state, sdat)
  out$Abeta <- beta_alias$Abeta
  out$Bbeta <- beta_alias$Bbeta
  out$Cbeta <- beta_alias$Cbeta
  out$Dbeta <- beta_alias$Dbeta
  if(!is.null(posterior) && !is.null(posterior$covariance)){
    sAbilitySD <- matrix(0, nrow = sdat$Nsubs, ncol = sdat$Nscales)
    for(i in seq_len(sdat$Nsubs)) sAbilitySD[i,] <- sqrt(pmax(diag(as.matrix(posterior$covariance[,,i])), 0))
    out$sAbilitySD <- sAbilitySD
  }
  out
}

bigIRT_laplace_can_rescale <- function(sdat){
  sdat$NitemPreds == 0 &&
    sdat$NpersonPreds == 0 &&
    !any(sdat$fixedB == 1L) &&
    !any(sdat$fixedAbilityLogical == 1L) &&
    !any(sdat$fixedAlog == 1L & abs(sdat$Adata) > 1e-10)
}


## Empirical Bayes hyperparameter update for the Laplace backend.
##
## This replaces, rather than ports, the JML version, whose logic has three
## problems. It set each prior SD to sd(estimates) * ebayesmultiplier, but the
## spread of estimates is the spread of the truth plus estimation noise, so that
## overstates the dispersion; the default multiplier of 2 then inflates the
## variance fourfold, which is the wrong direction for something whose purpose
## is regularisation. It trimmed with boxplot.stats()$out first, which pulls the
## SD back down by an amount nobody controls, so two ad hoc corrections fight
## each other. And it ran once rather than to a fixed point.
##
## The update here is the EM M-step for a normal-normal hierarchy. Item
## parameters are treated as theta_j ~ N(mu, tau^2) with the fit returning
## posterior modes, so
##
##   tau^2_new = mean( (theta_j - mu)^2 + Var(theta_j | data) )
##
## with Var(theta_j | data) = 1 / (I_j + 1/tau^2_old). The information I_j is
## accumulated per item from the response rows: for difficulty it is the same
## s^2/r weight the Hessian uses, and the asymptotes and discrimination carry
## their chain-rule factors. Adding the posterior variance is what the ability
## correlation M-step already did, and for the same reason: modes alone
## understate the second moment.
##
## Note what this does when a parameter is unidentified. I_j is then near zero,
## Var(theta_j | data) tends to tau^2_old, and the estimates sit at mu, so the
## update returns roughly what it started with. That is the correct answer
## given no information -- it is not a way of rescuing a prior that started in
## the wrong place.
bigIRT_laplace_eb_update <- function(state, sdat, context, row_effective, row_context,
                                     thetaBase, multiplier = 1, min_sd = 0.05){
  eta <- as.numeric(row_effective$eta_row)
  cr <- as.numeric(row_effective$c_row)
  dr <- as.numeric(row_effective$d_row)
  gg <- inv_logit(eta)
  qq <- gg * (1 - gg)
  u <- dr - cr
  p <- pmin(pmax(cr + u * gg, 1e-12), 1 - 1e-12)
  s <- u * qq
  r <- pmax(p * (1 - p), 1e-12)
  w <- s^2 / r                       # information in eta

  sum_by <- function(x, ref, n){
    ref <- as.integer(ref)
    keep <- ref > 0L
    if(!any(keep)) return(rep(0, n))
    agg <- rowsum(x[keep], ref[keep])
    out <- numeric(n)
    out[as.integer(rownames(agg))] <- as.numeric(agg)
    out
  }
  ## tau^2 = mean((est-mu)^2) - mean(1/I_j), floored at min_sd.
  ##
  ## This is deliberately the conservative estimator rather than the textbook
  ## one, and it is worth being explicit about why.
  ##
  ## The EM M-step for a normal-normal hierarchy is
  ## mean((est-mu)^2 + Var(theta_j|data)), adding the posterior variance back
  ## because posterior modes are shrunk. That is the right algebra when the
  ## estimates really are posterior means of a correctly specified hierarchy.
  ## Here they are joint MAP estimates from a fit in which the 3PL/4PL
  ## asymptotes are barely identified, the assumption fails, and the update
  ## walks upward: a loose prior lets the estimates spread, the spread feeds
  ## back as a looser prior. Measured on a 4PL with 20 responses an item, the
  ## discrimination SD went 1.0 -> 1.37 -> 1.72 -> 2.03 -> 2.26 -> 2.48 against
  ## a true spread of 0.33, and capping per-round growth at 1.5 only slowed it.
  ##
  ## Subtracting the sampling variance instead cannot inflate. Its cost is that
  ## it double-counts shrinkage and floors when the data cannot support
  ## item-level variation -- which on these designs is the honest reading:
  ## mean(1/I) for a per-item asymptote runs about fifteen times its true
  ## variance, and the fitted asymptotes correlate with the truth at about .07.
  ## Flooring pools the items towards a common asymptote, and pooling is what
  ## recovers difficulty on those cells. Erring towards too much pooling costs
  ## a little bias; erring the other way produced difficulty RMSE above 3.
  em_sd <- function(est, info, tau_old){
    est <- as.numeric(est)
    if(length(est) < 3L || !all(is.finite(est))) return(NULL)
    mu <- mean(est)
    noise <- mean(1 / pmax(info, 1e-8))
    tau2 <- mean((est - mu)^2) - noise
    list(mean = mu, sd = max(sqrt(max(tau2, 0)) * multiplier, min_sd))
  }
  out <- list(sdat = sdat, changed = character(0))
  set_block <- function(sd_name, mean_name, est, info, fixed_mean){
    tau_old <- as.numeric(out$sdat[[sd_name]])[1]
    upd <- em_sd(est, info, tau_old)
    if(is.null(upd)) return(invisible(NULL))
    out$sdat[[sd_name]] <<- upd$sd
    if(identical(as.integer(fixed_mean), 0L) && !is.null(mean_name))
      out$sdat[[mean_name]] <<- upd$mean
    out$changed <<- c(out$changed, sd_name)
  }

  ni <- as.integer(sdat$Nitems)
  if(length(state$Bpars) >= 3L)
    set_block("BSDx", "BMeandat", state$Bpars, sum_by(w, context$B_ref, ni), sdat$fixedBMean)

  ## Free-parameter length is the reliable signal: a 2PL leaves logitCpars
  ## empty rather than recording a model order in standata.
  if(length(state$logitCpars) >= 3L){
    ic <- sum_by(((1 - gg) * cr * (1 - cr))^2 / r, context$C_ref, ni)
    set_block("logitCSD", "logitCMeandat", state$logitCpars, ic, sdat$fixedCMean)
  }
  if(length(state$logitDpars) >= 3L){
    id <- sum_by((gg * dr * (1 - dr))^2 / r, context$D_ref, ni)
    set_block("logitDSD", "logitDMeandat", state$logitDpars, id, sdat$fixedDMean)
  }
  ## Discrimination enters through a*theta, so its information carries theta^2
  ## and the softplus derivative. Only the unidimensional case is handled; with
  ## several scales the mapping from rows to per-scale loadings is not a single
  ## index and the prior is left alone rather than updated approximately.
  if(as.integer(sdat$Nscales) == 1L && length(state$invspApars) >= 3L){
    th <- as.numeric(thetaBase)[as.integer(row_context$ids)]
    dsp <- inv_logit(as.numeric(state$invspApars))
    aref <- as.integer(context$A_ref[, 1])
    dspr <- rep(1, length(th)); keep <- aref > 0L
    dspr[keep] <- dsp[aref[keep]]
    ia <- sum_by(w * (th * dspr)^2, aref, ni)
    set_block("invspASD", "invspAMeandat", state$invspApars, ia, sdat$fixedAMean)
  }
  out
}

## Prior hyperparameters by Laplace approximation over the item parameters.
##
## The moment rule in bigIRT_laplace_eb_update has no Occam term, so it either
## walks upward or floors depending on which way you write it. Approximating
## the integral over the item block instead gives a real objective in tau:
##
##   log L(tau) ~ sum_b [ -sum_j (psi_jb - mu_b)^2 / (2 tau_b^2) - n log tau_b ]
##                - 0.5 * log|H(tau)|
##
## and the -n log tau term is what keeps it bounded.
##
## Structure makes this cheap. A response belongs to exactly one item, so the
## item block of the joint Hessian is block diagonal by item; items couple only
## through the persons, via the Schur correction. Writing v for dp/dpsi, s for
## dp/deta and Sigma for the person posterior covariance, the correction
## collapses to a per-response scalar and the whole block is
##
##   S_j[b1,b2] = sum_{r in j} v_b1 v_b2 ( 1/r - s^2 (a' Sigma a) / r^2 )
##
## with a' Sigma a already computed for the gradients. Only the prior precision
## on the diagonal depends on tau, so S_j is built once per fit and the
## optimisation over tau costs one small determinant per item per evaluation.
##
## The discarded off-diagonal -- item i to item j through shared persons -- was
## implemented and measured. On 80 items at both 150 and 600 responses an item
## it changed difficulty RMSE in the fourth decimal (.3763 against .3764) for
## about 35 per cent more time, so it is not built here.
bigIRT_laplace_hyper_pieces <- function(state, sdat, context, row_effective,
                                        row_context, posterior, thetaBase){
  if(is.null(posterior$covariance))
    stop("Laplace hyperparameter estimation needs posterior covariances; set keep_covariance.")
  K <- as.integer(sdat$Nscales); ni <- as.integer(sdat$Nitems)
  ids <- as.integer(row_context$ids)
  eta <- as.numeric(row_effective$eta_row)
  cr <- as.numeric(row_effective$c_row); dr <- as.numeric(row_effective$d_row)
  gg <- inv_logit(eta); qq <- gg * (1 - gg); u <- dr - cr
  p <- pmin(pmax(cr + u * gg, 1e-12), 1 - 1e-12)
  s <- u * qq; r <- pmax(p * (1 - p), 1e-12)
  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)
  Sig <- posterior$covariance
  aSa <- numeric(length(ids))
  for(k in seq_len(K)) for(l in seq_len(K))
    aSa <- aSa + lo[, k] * lo[, l] * Sig[k, l, ids]
  wcorr <- 1 / r - (s^2 * aSa) / r^2      # profile weight after removing theta

  sig <- inv_logit(as.numeric(state$invspApars))
  aref <- as.integer(context$A_ref[, 1])
  sigr <- rep(1, length(ids)); keep <- aref > 0L; sigr[keep] <- sig[aref[keep]]
  th1 <- if(K == 1L) as.numeric(thetaBase)[ids] else as.matrix(thetaBase)[ids, 1]
  V <- list(B = -s, A = s * th1 * sigr,
            C = (1 - gg) * cr * (1 - cr), D = gg * dr * (1 - dr))
  est <- list(B = as.numeric(state$Bpars), A = as.numeric(state$invspApars),
              C = as.numeric(state$logitCpars), D = as.numeric(state$logitDpars))
  ## Only blocks with enough free parameters to have a dispersion are updated.
  ## Discrimination is skipped beyond one dimension: its information carries a
  ## per-scale theta and the row-to-loading map is not a single index there.
  active <- names(est)[vapply(est, function(z) length(z) >= 3L, logical(1))]
  if(K > 1L) active <- setdiff(active, "A")
  if(!length(active)) return(NULL)
  V <- V[active]; est <- est[active]; P <- length(active)
  item <- as.integer(context$B_ref)
  scat <- function(x){
    agg <- rowsum(x, item); out <- numeric(ni)
    out[as.integer(rownames(agg))] <- as.numeric(agg); out
  }
  Sbase <- array(0, c(P, P, ni))
  for(b1 in seq_len(P)) for(b2 in b1:P){
    v <- scat(V[[b1]] * V[[b2]] * wcorr)
    Sbase[b1, b2, ] <- v
    if(b2 > b1) Sbase[b2, b1, ] <- v
  }
  list(Sbase = Sbase, est = est, active = active, P = P, ni = ni)
}

## Negative profile log marginal at prior SDs exp(logtau).
bigIRT_laplace_hyper_objective <- function(logtau, pieces, fixed_tau = NULL){
  ## fixed_tau holds blocks that have been frozen; only the free entries vary.
  tau <- if(is.null(fixed_tau)) exp(logtau) else {
    z <- fixed_tau; z[is.na(z)] <- exp(logtau); z
  }
  if(any(!is.finite(tau)) || any(tau <= 0)) return(1e10)
  P <- pieces$P
  pen <- 0
  for(b in seq_len(P)){
    e <- pieces$est[[b]]; mu <- mean(e)
    pen <- pen - sum((e - mu)^2) / (2 * tau[b]^2) - length(e) * log(tau[b])
  }
  add <- diag(1 / tau^2, P)
  ld <- 0
  for(j in seq_len(pieces$ni)){
    Sj <- pieces$Sbase[, , j] + add
    d <- suppressWarnings(determinant(Sj, logarithm = TRUE))
    if(!is.finite(d$modulus) || d$sign <= 0) return(1e10)
    ld <- ld + as.numeric(d$modulus)
  }
  -(pen - 0.5 * ld)
}

## Update sdat's prior SDs (and free means) from one fit.
bigIRT_laplace_hyper_update <- function(state, sdat, context, row_effective, row_context,
                                        posterior, thetaBase, min_sd = 1e-3, max_sd = 1e3,
                                        fixed = character(0)){
  pieces <- bigIRT_laplace_hyper_pieces(state, sdat, context, row_effective,
                                        row_context, posterior, thetaBase)
  if(is.null(pieces)) return(list(sdat = sdat, tau = NULL))
  sd_names <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")
  mean_names <- c(B = "BMeandat", A = "invspAMeandat", C = "logitCMeandat", D = "logitDMeandat")
  fixed_mean <- c(B = "fixedBMean", A = "fixedAMean", C = "fixedCMean", D = "fixedDMean")
  start <- vapply(pieces$active, function(nm)
    max(as.numeric(sdat[[sd_names[[nm]]]])[1], min_sd), numeric(1))
  frozen <- pieces$active %in% fixed
  if(all(frozen)) return(list(sdat = sdat, tau = setNames(start, pieces$active)))
  fixed_tau <- ifelse(frozen, start, NA_real_)
  free_start <- log(start[!frozen])
  opt <- if(length(free_start) == 1L)
    stats::optim(free_start, bigIRT_laplace_hyper_objective, pieces = pieces,
                 fixed_tau = fixed_tau, method = "Brent",
                 lower = log(min_sd), upper = log(max_sd))
  else
    stats::optim(free_start, bigIRT_laplace_hyper_objective, pieces = pieces,
                 fixed_tau = fixed_tau, method = "Nelder-Mead",
                 control = list(maxit = 200, reltol = 1e-8))
  tau <- start
  tau[!frozen] <- pmin(pmax(exp(opt$par), min_sd), max_sd)
  names(tau) <- pieces$active
  for(b in seq_along(pieces$active)){
    nm <- pieces$active[b]
    if(frozen[b]) next
    sdat[[sd_names[[nm]]]] <- tau[b]
    if(identical(as.integer(sdat[[fixed_mean[[nm]]]]), 0L))
      sdat[[mean_names[[nm]]]] <- mean(pieces$est[[b]])
  }
  list(sdat = sdat, tau = tau)
}
