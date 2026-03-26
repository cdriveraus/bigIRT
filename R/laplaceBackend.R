bigIRT_laplace_person_step_cpp_impl <- function(id, score, theta_init, ability_offset,
  b, c, d, loadings, prior_mean, prior_precision, free_mask,
  jitter, max_attempts, max_iter, tol, keep_covariance = FALSE, grain_size = 64L){
  .Call(
    `_bigIRT_laplace_person_step_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(theta_init),
    as.matrix(ability_offset),
    as.numeric(b),
    as.numeric(c),
    as.numeric(d),
    as.matrix(loadings),
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

bigIRT_laplace_item_block_objective_cpp_impl <- function(id, score, row_ability,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_precision, jitter, max_attempts, grain_size = 64L){
  .Call(
    `_bigIRT_laplace_item_block_objective_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(row_ability),
    matrix(as.integer(A_ref), nrow = nrow(row_ability), ncol = ncol(row_ability)),
    as.matrix(A_fixed_value),
    matrix(as.integer(A_beta_row), nrow = nrow(row_ability), ncol = ncol(row_ability)),
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
    as.integer(grain_size)
  )
}

bigIRT_laplace_direct_objective_cpp_impl <- function(id, score, theta_init,
  ability_offset, b, c, d, loadings, prior_mean, prior_precision, free_mask,
  jitter, max_attempts, max_iter, tol, keep_covariance = FALSE){
  .Call(
    `_bigIRT_laplace_direct_objective_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(theta_init),
    as.matrix(ability_offset),
    as.numeric(b),
    as.numeric(c),
    as.numeric(d),
    as.matrix(loadings),
    as.matrix(prior_mean),
    prior_precision,
    matrix(as.integer(free_mask), nrow = nrow(theta_init), ncol = ncol(theta_init)),
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(max_iter),
    as.numeric(tol),
    as.logical(keep_covariance)
  )
}

bigIRT_laplace_as_matrix <- function(x, nrow, ncol){
  if(is.null(x) || length(x) == 0) return(matrix(0, nrow = nrow, ncol = ncol))
  if(is.matrix(x)) return(matrix(as.numeric(x), nrow = nrow, ncol = ncol))
  matrix(as.numeric(x), nrow = nrow, ncol = ncol, byrow = TRUE)
}

bigIRT_laplace_extract_state <- function(fit, sdat){
  freeAbility <- which(sdat$Abilityparsindex > 0)
  abilityBase <- matrix(sdat$Abilitydata, nrow = sdat$Nsubs, ncol = sdat$Nscales)
  if(length(freeAbility)){
    abilityBase[freeAbility] <- as.numeric(fit$pars$Abilitypars)
  }

  abilityMean <- if(sdat$fixedAbilityMean == 0L && "AbilityMeanpar" %in% names(fit$pars)){
    as.numeric(fit$pars$AbilityMeanpar)
  } else {
    as.numeric(sdat$AbilityMeandat)
  }
  if(length(abilityMean) == 0) abilityMean <- as.numeric(sdat$AbilityMeandat)

  list(
    AbilityBase = abilityBase,
    AbilityMean = as.numeric(abilityMean),
    Abilitybeta = bigIRT_laplace_as_matrix(fit$pars$Abilitybeta, sdat$Nscales, sdat$NpersonPreds),
    Bpars = if("Bpars" %in% names(fit$pars)) as.numeric(fit$pars$Bpars) else numeric(),
    BMean = if(sdat$fixedBMean == 0L && "BMeanpar" %in% names(fit$pars)) as.numeric(fit$pars$BMeanpar)[1] else as.numeric(sdat$BMeandat),
    Bbeta = bigIRT_laplace_as_matrix(fit$pars$Bbeta,
      if(sdat$itemSpecificBetas == 1L) max(1L, sdat$Nitems - sdat$NfixedB) else 1L,
      sdat$NBitemPreds),
    invspApars = if("invspApars" %in% names(fit$pars)) as.numeric(fit$pars$invspApars) else numeric(),
    invspAMean = if(sdat$fixedAMean == 0L && "invspAMeanpar" %in% names(fit$pars)) as.numeric(fit$pars$invspAMeanpar)[1] else as.numeric(sdat$invspAMeandat),
    invspAbeta = bigIRT_laplace_as_matrix(fit$pars$invspAbeta,
      if(sdat$itemSpecificBetas == 1L) max(1L, sdat$NitemScales - sdat$NfixedA) else 1L,
      sdat$NAitemPreds),
    logitCpars = if("logitCpars" %in% names(fit$pars)) as.numeric(fit$pars$logitCpars) else numeric(),
    logitCMean = if(sdat$fixedCMean == 0L && "logitCMeanpar" %in% names(fit$pars)) as.numeric(fit$pars$logitCMeanpar)[1] else as.numeric(sdat$logitCMeandat),
    logitCbeta = bigIRT_laplace_as_matrix(fit$pars$logitCbeta,
      if(sdat$itemSpecificBetas == 1L) max(1L, sdat$Nitems - sdat$NfixedC) else 1L,
      sdat$NCitemPreds),
    logitDpars = if("logitDpars" %in% names(fit$pars)) as.numeric(fit$pars$logitDpars) else numeric(),
    logitDMean = if(sdat$fixedDMean == 0L && "logitDMeanpar" %in% names(fit$pars)) as.numeric(fit$pars$logitDMeanpar)[1] else as.numeric(sdat$logitDMeandat),
    logitDbeta = bigIRT_laplace_as_matrix(fit$pars$logitDbeta,
      if(sdat$itemSpecificBetas == 1L) max(1L, sdat$Nitems - sdat$NfixedD) else 1L,
      sdat$NDitemPreds)
  )
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
  prior_mean, ability_sd, corr_par, corr_paramization = 0L, jitter = 1e-8){
  .Call(
    `_bigIRT_laplace_corr_grad_cpp_impl`,
    as.matrix(theta_mode),
    covariance,
    precision,
    as.numeric(prior_mean),
    as.numeric(ability_sd),
    as.numeric(corr_par),
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

bigIRT_laplace_subject_grain <- function(nsubs, cores){
  cores <- max(1L, as.integer(cores)[1])
  nsubs <- max(1L, as.integer(nsubs)[1])
  max(1L, ceiling(nsubs / cores))
}

## Build a direct, prior-anchored initial state for the Laplace path.
## Inputs: standata only; no JML fit required.
## Returns: a complete state list matching the Laplace backend contract;
## mutates nothing.
bigIRT_laplace_initial_state <- function(sdat, eps = 1e-6,
  corr_paramization = c("normalized_chol", "stan_corsqrt")){
  corr_paramization <- match.arg(corr_paramization)
  abilityBase <- matrix(as.numeric(sdat$Abilitydata), nrow = sdat$Nsubs, ncol = sdat$Nscales)
  abilityMean <- as.numeric(sdat$AbilityMeandat)
  if(length(abilityMean) == 0) abilityMean <- rep(0, sdat$Nscales)
  if(length(abilityMean) == 1L && sdat$Nscales > 1L) abilityMean <- rep(abilityMean, sdat$Nscales)

  freeAbility <- which(sdat$fixedAbilityLogical == 0L)
  if(length(freeAbility)) abilityBase[freeAbility] <- abilityMean[col(abilityBase)[freeAbility]]

  item_p <- rep(0.5, sdat$Nitems)
  train_rows <- bigIRT_laplace_training_rows(sdat)
  if(length(train_rows)){
    item_sum <- tapply(sdat$score[train_rows], sdat$item[train_rows], sum)
    item_n <- tapply(sdat$score[train_rows], sdat$item[train_rows], length)
    item_mean <- item_sum / pmax(item_n, 1)
    item_p[as.integer(names(item_mean))] <- as.numeric(item_mean)
  }
  item_p <- bigIRT_laplace_clamp(item_p, 1e-4, 1 - 1e-4)

  A_default_raw <- if(is.finite(as.numeric(sdat$invspAMeandat)[1])) as.numeric(sdat$invspAMeandat)[1] else afunci(1)
  C_default_raw <- if(is.finite(as.numeric(sdat$logitCMeandat)[1])) as.numeric(sdat$logitCMeandat)[1] else cfunci(0.05)
  D_default_raw <- if(is.finite(as.numeric(sdat$logitDMeandat)[1])) as.numeric(sdat$logitDMeandat)[1] else dfunci(0.95)
  C_default <- if(sdat$NfixedC < sdat$Nitems) cfunc(C_default_raw) else 0
  D_default <- if(sdat$NfixedD < sdat$Nitems) dfunc(D_default_raw) else 1
  width_default <- pmax(D_default - C_default, 1e-4)
  target_mid <- bigIRT_laplace_clamp((item_p - C_default) / width_default, 1e-4, 1 - 1e-4)
  B_default <- -qlogis(target_mid)

  freeA <- max(0L, sdat$NitemScales - sdat$NfixedA)
  freeB <- max(0L, sdat$Nitems - sdat$NfixedB)
  freeC <- max(0L, sdat$Nitems - sdat$NfixedC)
  freeD <- max(0L, sdat$Nitems - sdat$NfixedD)
  A_beta_rows <- if(sdat$itemSpecificBetas == 1L) max(1L, freeA) else 1L
  item_beta_rows <- function(freeN) if(sdat$itemSpecificBetas == 1L) max(1L, freeN) else 1L

  state <- list(
    AbilityBase = abilityBase,
    AbilityMean = abilityMean,
    AbilityCorr = as.matrix(sdat$AbilityCorr),
    laplaceCorrParam = corr_paramization,
    AbilityCorrPars = if(sdat$Nscales > 1L) bigIRT_laplace_pack_corr(as.matrix(sdat$AbilityCorr), paramization = corr_paramization) else numeric(),
    Abilitybeta = matrix(0, nrow = sdat$Nscales, ncol = sdat$NpersonPreds),
    Bpars = rep(0, freeB),
    BMean = if(sdat$fixedBMean == 0L) as.numeric(sdat$BMeandat)[1] else as.numeric(sdat$BMeandat)[1],
    Bbeta = matrix(0, nrow = item_beta_rows(freeB), ncol = sdat$NBitemPreds),
    invspApars = rep(A_default_raw, freeA),
    invspAMean = if(sdat$fixedAMean == 0L) as.numeric(sdat$invspAMeandat)[1] else as.numeric(sdat$invspAMeandat)[1],
    invspAbeta = matrix(0, nrow = A_beta_rows, ncol = sdat$NAitemPreds),
    logitCpars = rep(C_default_raw, freeC),
    logitCMean = if(sdat$fixedCMean == 0L) as.numeric(sdat$logitCMeandat)[1] else as.numeric(sdat$logitCMeandat)[1],
    logitCbeta = matrix(0, nrow = item_beta_rows(freeC), ncol = sdat$NCitemPreds),
    logitDpars = rep(D_default_raw, freeD),
    logitDMean = if(sdat$fixedDMean == 0L) as.numeric(sdat$logitDMeandat)[1] else as.numeric(sdat$logitDMeandat)[1],
    logitDbeta = matrix(0, nrow = item_beta_rows(freeD), ncol = sdat$NDitemPreds)
  )

  if(length(state$Bpars)){
    seen <- rep(FALSE, length(state$Bpars))
    for(itemi in seq_len(sdat$Nitems)){
      if(sdat$fixedB[itemi] == 0L){
        ref <- sdat$freeBref[itemi]
        if(ref > 0L && !seen[ref]){
          state$Bpars[ref] <- B_default[itemi]
          seen[ref] <- TRUE
        }
      }
    }
  }
  if(length(state$logitCpars)){
    for(itemi in seq_len(sdat$Nitems)){
      if(sdat$fixedClogit[itemi] == 0L){
        ref <- sdat$freeCref[itemi]
        if(ref > 0L) state$logitCpars[ref] <- C_default_raw
      }
    }
  }
  if(length(state$logitDpars)){
    for(itemi in seq_len(sdat$Nitems)){
      if(sdat$fixedDlogit[itemi] == 0L){
        ref <- sdat$freeDref[itemi]
        if(ref > 0L) state$logitDpars[ref] <- D_default_raw
      }
    }
  }

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

  b_row <- context$B_fixed_value
  keepB <- !context$B_fixed
  if(any(keepB)){
    b_row[keepB] <- state$Bpars[context$B_ref[keepB]]
    if(sdat$NBitemPreds > 0){
      brow <- context$B_beta_row[keepB]
      b_row[keepB] <- b_row[keepB] + rowSums(context$B_pred[keepB,, drop = FALSE] * state$Bbeta[brow,, drop = FALSE])
    }
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
  cores = 1L){
  train_rows <- bigIRT_laplace_training_rows(sdat)
  row_context <- bigIRT_laplace_row_context(sdat, rows = train_rows)
  prior_mean <- matrix(rep(state$AbilityMean, each = sdat$Nsubs), nrow = sdat$Nsubs)
  free_mask <- 1L - sdat$fixedAbilityLogical
  out <- bigIRT_laplace_person_step_block_cpp_impl(
    id = sdat$id[train_rows],
    score = sdat$score[train_rows],
    theta_init = state$AbilityBase,
    person_pred = row_context$person_pred,
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
    Abilitybeta = state$Abilitybeta,
    invspApars = state$invspApars,
    invspAbeta = state$invspAbeta,
    Bpars = state$Bpars,
    Bbeta = state$Bbeta,
    logitCpars = state$logitCpars,
    logitCbeta = state$logitCbeta,
    logitDpars = state$logitDpars,
    logitDbeta = state$logitDbeta,
    prior_mean = prior_mean,
    prior_precision = prior_precision,
    free_mask = free_mask,
    jitter = jitter,
    max_attempts = max_attempts,
    max_iter = max_iter,
    tol = tol,
    keep_covariance = keep_covariance,
    grain_size = bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  )
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

bigIRT_laplace_item_context <- function(sdat, layout = bigIRT_laplace_item_layout(sdat)){
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

## Accumulate row-wise contributions onto free-parameter references.
## Inputs: per-row values and 1-based reference ids. Returns a dense gradient
## vector of length `nout`.
bigIRT_laplace_accum_index <- function(values, refs, nout){
  out <- numeric(nout)
  if(nout <= 0) return(out)
  keep <- refs > 0L & is.finite(values)
  if(!any(keep)) return(out)
  agg <- rowsum(matrix(values[keep], ncol = 1L), group = refs[keep], reorder = FALSE)[,1]
  out[as.integer(names(agg))] <- as.numeric(agg)
  out
}

## Accumulate row-wise predictor contributions onto stacked beta parameters.
## Inputs: per-row values, the beta-row selected by each row, predictor matrix,
## and the packed layout indices for that beta block.
bigIRT_laplace_accum_beta <- function(values, beta_row, pred, nrow_beta, layout_idx){
  out <- numeric(length(layout_idx))
  if(length(layout_idx) == 0 || nrow_beta <= 0 || is.null(pred) || ncol(pred) == 0) return(out)
  keep <- beta_row > 0L & is.finite(values)
  if(!any(keep)) return(out)
  contrib <- pred[keep,,drop = FALSE] * values[keep]
  agg <- rowsum(contrib, group = beta_row[keep], reorder = FALSE)
  beta_idx <- matrix(layout_idx, nrow = nrow_beta, byrow = TRUE)
  agg_rows <- as.integer(rownames(agg))
  for(ii in seq_along(agg_rows)){
    out[beta_idx[agg_rows[ii],]] <- out[beta_idx[agg_rows[ii],]] + agg[ii,]
  }
  out
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
  n_corr <- if(isTRUE(estimateAbilityCorr) && sdat$Nscales > 1L) sdat$Nscales * (sdat$Nscales - 1L) / 2L else 0L
  item_layout$corr <- take(n_corr)
  item_layout
}

## Pack the direct Laplace parameter vector, optionally including the latent
## correlation parameters. Inputs: current state, standata, and a direct layout.
bigIRT_laplace_pack_direct_state <- function(state, sdat,
  layout = bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)){
  out <- numeric(max(unlist(layout), 0L))
  item_slots <- layout[setdiff(names(layout), "corr")]
  if(length(unlist(item_slots))){
    out[seq_len(max(unlist(item_slots), 0L))] <- bigIRT_laplace_pack_item_state(state, sdat, layout = item_slots)
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
  item_slots <- layout[setdiff(names(layout), "corr")]
  out <- bigIRT_laplace_unpack_item_state(par, out, sdat, layout = item_slots)
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
bigIRT_laplace_corr_grad <- function(state, sdat, layout, posterior, prior_precision_matrix, jitter = 1e-8){
  out <- numeric(length(layout$corr))
  if(length(layout$corr) == 0L) return(out)
  if(is.null(posterior$covariance)) stop("laplace_direct correlation gradients require posterior covariances.")
  corr_paramization <- if(!is.null(state$laplaceCorrParam)) state$laplaceCorrParam else "normalized_chol"
  as.numeric(bigIRT_laplace_corr_grad_cpp_impl(
    theta_mode = posterior$theta_mode,
    covariance = posterior$covariance,
    precision = posterior$precision,
    prior_mean = state$AbilityMean,
    ability_sd = sdat$AbilitySD,
    corr_par = state$AbilityCorrPars,
    corr_paramization = bigIRT_laplace_corr_param_code(corr_paramization),
    jitter = jitter
  ))
}

bigIRT_laplace_item_prior <- function(state, sdat){
  if(!isTRUE(as.logical(sdat$dopriors))) return(list(value = 0, grad = bigIRT_laplace_pack_item_state(state, sdat) * 0))
  layout <- bigIRT_laplace_item_layout(sdat)
  grad <- numeric(max(unlist(layout), 0L))
  value <- 0

  if(length(state$invspApars)){
    value <- value + sum(dnorm(state$invspApars, mean = state$invspAMean, sd = sdat$invspASD, log = TRUE))
    if(length(layout$A)) grad[layout$A] <- grad[layout$A] - (state$invspApars - state$invspAMean) / (sdat$invspASD^2)
    if(length(layout$A_mean)) grad[layout$A_mean] <- sum((state$invspApars - state$invspAMean) / (sdat$invspASD^2))
  }
  if(length(state$Bpars)){
    value <- value + sum(dnorm(state$Bpars, mean = state$BMean, sd = sdat$BSDx, log = TRUE))
    if(length(layout$B)) grad[layout$B] <- grad[layout$B] - (state$Bpars - state$BMean) / (sdat$BSDx^2)
    if(length(layout$B_mean)) grad[layout$B_mean] <- sum((state$Bpars - state$BMean) / (sdat$BSDx^2))
  }
  if(length(state$logitCpars)){
    value <- value + sum(dnorm(state$logitCpars, mean = state$logitCMean, sd = sdat$logitCSD, log = TRUE))
    if(length(layout$C)) grad[layout$C] <- grad[layout$C] - (state$logitCpars - state$logitCMean) / (sdat$logitCSD^2)
    if(length(layout$C_mean)) grad[layout$C_mean] <- sum((state$logitCpars - state$logitCMean) / (sdat$logitCSD^2))
  }
  if(length(state$logitDpars)){
    value <- value + sum(dnorm(state$logitDpars, mean = state$logitDMean, sd = sdat$logitDSD, log = TRUE))
    if(length(layout$D)) grad[layout$D] <- grad[layout$D] - (state$logitDpars - state$logitDMean) / (sdat$logitDSD^2)
    if(length(layout$D_mean)) grad[layout$D_mean] <- sum((state$logitDpars - state$logitDMean) / (sdat$logitDSD^2))
  }

  betaScale <- as.numeric(sdat$betaScale)
  if(betaScale > 0){
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

## Evaluate the frozen-mode Laplace item objective and analytic gradient.
## Inputs: packed item/global parameter vector, current state, standata, person
## modes `thetaBase`, and a cached item context. Returns objective, gradient,
## unpacked state, and row-effective quantities for diagnostics.
bigIRT_laplace_item_objective <- function(par, state, sdat, thetaBase, prior_precision, jitter = 1e-6, context = NULL){
  if(is.null(context)) context <- bigIRT_laplace_item_context(sdat)
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

  cpp <- bigIRT_laplace_item_block_objective_cpp_impl(
    id = sdat$id[context$train_rows],
    score = sdat$score[context$train_rows],
    row_ability = context$row_ability,
    A_ref = context$A_ref,
    A_fixed_value = context$row_context$fixed_A_value,
    A_beta_row = context$A_beta_row,
    A_pred = context$A_pred,
    B_ref = context$B_ref,
    B_fixed_value = context$row_context$B_fixed_value,
    B_beta_row = context$B_beta_row,
    B_pred = context$B_pred,
    C_ref = context$C_ref,
    C_fixed_value = context$row_context$C_fixed_value,
    C_beta_row = context$C_beta_row,
    C_pred = context$C_pred,
    D_ref = context$D_ref,
    D_fixed_value = context$row_context$D_fixed_value,
    D_beta_row = context$D_beta_row,
    D_pred = context$D_pred,
    invspApars = curState$invspApars,
    invspAbeta = curState$invspAbeta,
    Bpars = curState$Bpars,
    Bbeta = curState$Bbeta,
    logitCpars = curState$logitCpars,
    logitCbeta = curState$logitCbeta,
    logitDpars = curState$logitDpars,
    logitDbeta = curState$logitDbeta,
    prior_precision = prior_precision,
    jitter = jitter,
    max_attempts = 8L,
    grain_size = context$grain_size
  )

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
  list(
    value = cpp$objective + prior$value,
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

bigIRT_laplace_materialize_block_cpp_impl <- function(item, id, score, theta_base,
  person_pred, item_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta,
  logitCpars, logitCbeta, logitDpars, logitDbeta) {
  Nobs <- length(item)
  K <- ncol(theta_base)
  .Call(
    `_bigIRT_laplace_materialize_block_cpp_impl`,
    as.integer(item),
    as.integer(id),
    as.integer(score),
    as.matrix(theta_base),
    as.matrix(person_pred),
    as.matrix(item_pred),
    matrix(as.integer(fixed_ability), nrow = Nobs, ncol = K),
    as.matrix(fixed_ability_value),
    matrix(as.integer(A_ref), nrow = Nobs, ncol = K),
    as.matrix(A_fixed_value),
    matrix(as.integer(A_beta_row), nrow = Nobs, ncol = K),
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
    as.matrix(logitDbeta)
  )
}

## Evaluate the direct Laplace objective by solving all person modes inside the
## objective call. This is the experimental single-stage backend used by
## `marginalApprox = "laplace_direct"`.
bigIRT_laplace_direct_objective <- function(par, state, sdat, prior_precision,
  theta_init = state$AbilityBase, jitter = 1e-6, max_attempts = 8L,
  max_iter = 50L, tol = 1e-4, keep_covariance = FALSE, context = NULL){
  estimateAbilityCorr <- isTRUE(context$estimateAbilityCorr)
  if(is.null(context)) {
    direct_layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = estimateAbilityCorr)
    item_layout <- direct_layout[setdiff(names(direct_layout), "corr")]
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
  posterior <- bigIRT_laplace_person_step_block_cpp_impl(
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
  laplace_value <- sum(
    posterior$objective +
      0.5 * sdat$Nscales * log(2 * pi) -
      0.5 * posterior$logdet_precision
  )
  prior_norm_value <- if(isTRUE(context$estimateAbilityCorr)) {
    0.5 * sdat$Nsubs * as.numeric(determinant(prior_mats$precision, logarithm = TRUE)$modulus)
  } else 0
  prior <- bigIRT_laplace_item_prior(curState, sdat)
  surrogate <- bigIRT_laplace_item_objective(
    par = par[seq_len(max(unlist(context$layout), 0L))],
    state = state,
    sdat = sdat,
    thetaBase = posterior$theta_mode,
    prior_precision = prior_mats$precision_array,
    jitter = jitter,
    context = context
  )
  approx_grad <- numeric(length(par))
  if(length(surrogate$grad)) approx_grad[seq_along(surrogate$grad)] <- surrogate$grad
  if(isTRUE(context$estimateAbilityCorr) && length(context$direct_layout$corr)){
    approx_grad[context$direct_layout$corr] <- bigIRT_laplace_corr_grad(
      state = curState,
      sdat = sdat,
      layout = context$direct_layout,
      posterior = posterior,
      prior_precision_matrix = prior_mats$precision,
      jitter = jitter
    )
  }
  list(
    value = laplace_value + prior_norm_value + prior$value,
    approx_grad = approx_grad,
    state = curState,
    prior_mats = prior_mats,
    posterior = posterior,
    prior = prior,
    rowEffective = NULL,
    surrogate = surrogate
  )
}

## Optimize the item/global block with a cached row context and memoized target
## evaluations so line-search queries do not rebuild the whole Laplace object
## repeatedly at identical parameter values.
bigIRT_laplace_optimize_item <- function(state, sdat, thetaBase, prior_precision,
  niter = 50L, tol = 1e-4, jitter = 1e-6, cores = 1L){
  layout <- bigIRT_laplace_item_layout(sdat)
  context <- bigIRT_laplace_item_context(sdat, layout = layout)
  context$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  context$row_ability <- bigIRT_laplace_fixed_row_ability(
    state = state,
    sdat = sdat,
    thetaBase = thetaBase,
    rows = context$train_rows,
    context = context$row_context
  )
  init <- bigIRT_laplace_pack_item_state(state, sdat, layout = layout)
  if(length(init) == 0) {
    eval0 <- bigIRT_laplace_item_objective(init, state, sdat, thetaBase, prior_precision, jitter = jitter, context = context)
    return(list(state = eval0$state, optim = list(par = init, value = eval0$value, masked_grad_norm = 0, target_evals = 1L)))
  }

  eval_count <- 0L
  get_eval <- function(par){
    eval_count <<- eval_count + 1L
    bigIRT_laplace_item_objective(par, state, sdat, thetaBase, prior_precision, jitter = jitter, context = context)
  }
  target_fg <- function(par){
    res <- get_eval(par)
    list(fn = -res$value, gr = -res$grad)
  }
  target_fn <- function(par){
    -get_eval(par)$value
  }
  target_gr <- function(par){
    -get_eval(par)$grad
  }
  fit <- mize::mize(init, fg = list(fg = target_fg, fn = target_fn, gr = target_gr), max_iter = niter, method = "L-BFGS",
    memory = 30, line_search = "Schmidt", c1 = 1e-10, c2 = 0.9, step0 = "schmidt", ls_max_fn = 1L,
    abs_tol = tol, grad_tol = 0, rel_tol = 0, step_tol = 0, ginf_tol = 0)
  final <- get_eval(fit$par)
  fit$masked_grad_norm <- sqrt(sum(final$grad^2))
  fit$target_evals <- eval_count
  fit$logLik <- final$value
  list(state = final$state, optim = fit, eval = final)
}

## Optimize the direct Laplace objective with person modes solved inside each
## objective evaluation. The current implementation uses the exact direct
## objective value together with the frozen-mode Laplace gradient as a proxy
## gradient for efficient experimentation.
bigIRT_laplace_optimize_direct <- function(state, sdat, prior_precision,
  niter = 50L, tol = 1e-4, jitter = 1e-6, person_tol = 1e-4,
  keep_covariance = FALSE, cores = 1L, estimateAbilityCorr = FALSE,
  corr_paramization = c("normalized_chol", "stan_corsqrt"),
  collect_history = FALSE, plot_callback = NULL, plot_every = 1L){
  corr_paramization <- match.arg(corr_paramization)
  direct_layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = estimateAbilityCorr)
  item_layout <- direct_layout[setdiff(names(direct_layout), "corr")]
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
  history <- list()
  last_value <- NULL
  last_par <- NULL
  last_theta <- NULL
  get_eval <- function(par){
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
    theta_warm <<- res$posterior$theta_mode
    if(isTRUE(collect_history)){
      cur_theta <- res$posterior$theta_mode
      cov_arr <- res$posterior$covariance
      if(!is.null(cov_arr)){
        post_sd <- unlist(lapply(seq_len(dim(cov_arr)[3]), function(ii) sqrt(pmax(diag(cov_arr[,,ii]), 0))))
        mean_post_sd <- mean(post_sd, na.rm = TRUE)
        max_post_sd <- max(post_sd, na.rm = TRUE)
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
    res
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

  fit <- mize::mize(
    init,
    fg = list(fg = target_fg, fn = target_fn, gr = target_gr),
    max_iter = niter,
    method = "L-BFGS",
    memory = 30,
    line_search = "Schmidt",
    c1 = 1e-10,
    c2 = 0.9,
    step0 = "schmidt",
    ls_max_fn = 20L,
    abs_tol = tol,
    grad_tol = tol,
    rel_tol = 0,
    step_tol = 0,
    ginf_tol = 0
  )
  final <- get_eval(fit$par)
  fit$masked_grad_norm <- sqrt(sum(final$approx_grad^2))
  fit$target_evals <- eval_count
  fit$logLik <- final$value
  if(length(history)){
    history[[length(history)]][["optimizerIter"]] <- if(!is.null(fit$iter)) fit$iter else NA_integer_
    history[[length(history)]][["optimizerTerminate"]] <- if(!is.null(fit$terminate$what)) as.character(fit$terminate$what) else NA_character_
    history[[length(history)]][["optimizerTerminateValue"]] <- if(!is.null(fit$terminate$val)) fit$terminate$val else NA_real_
  }
  list(state = final$state, optim = fit, eval = final, history = history)
}

bigIRT_laplace_constrained_pars <- function(state, sdat, posterior = NULL){
  rows <- seq_len(sdat$Nobs)
  row_context <- bigIRT_laplace_row_context(sdat, rows = rows)
  materialized <- bigIRT_laplace_materialize_block_cpp_impl(
    item = sdat$item[rows],
    id = sdat$id[rows],
    score = sdat$score[rows],
    theta_base = state$AbilityBase,
    person_pred = if(sdat$NpersonPreds > 0) as.matrix(sdat$personPreds[rows,, drop = FALSE]) else matrix(0, nrow = length(rows), ncol = 0),
    item_pred = if(sdat$NitemPreds > 0) as.matrix(sdat$itemPreds[rows,, drop = FALSE]) else matrix(0, nrow = length(rows), ncol = 0),
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
    Abilitybeta = state$Abilitybeta,
    invspApars = state$invspApars,
    invspAbeta = state$invspAbeta,
    Bpars = state$Bpars,
    Bbeta = state$Bbeta,
    logitCpars = state$logitCpars,
    logitCbeta = state$logitCbeta,
    logitDpars = state$logitDpars,
    logitDbeta = state$logitDbeta
  )

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

bigIRT_laplace_rescale_state <- function(state, sdat, eps = 1e-6, targetMean = NULL, targetSD = NULL){
  if(!bigIRT_laplace_can_rescale(sdat)) return(state)

  theta <- state$AbilityBase
  freeMask <- (sdat$fixedAbilityLogical == 0L)
  curMean <- rep(0, sdat$Nscales)
  curSD <- rep(1, sdat$Nscales)
  for(si in seq_len(sdat$Nscales)){
    vals <- theta[freeMask[,si], si]
    if(length(vals) == 0) next
    curMean[si] <- mean(vals)
    curSD[si] <- max(sd(vals), eps)
  }

  if(is.null(targetMean)) targetMean <- as.numeric(sdat$AbilityMeandat)
  if(is.null(targetSD)) targetSD <- as.numeric(sdat$AbilitySD)
  targetMean <- as.numeric(targetMean)
  targetSD <- pmax(as.numeric(targetSD), eps)
  scaleFac <- curSD / targetSD
  shift <- curMean - scaleFac * targetMean

  Aold <- matrix(0, nrow = sdat$Nitems, ncol = sdat$Nscales)
  for(i in seq_len(sdat$Nitems)){
    for(si in seq_len(sdat$Nscales)){
      aidx <- (i - 1L) * sdat$Nscales + si
      if(sdat$fixedAlog[aidx] == 1L){
        Aold[i, si] <- sdat$Adata[aidx]
      } else {
        ref <- sdat$freeAref[aidx]
        Aold[i, si] <- afunc(state$invspApars[ref])
      }
    }
  }

  theta <- sweep(theta, 2, curMean, "-")
  theta <- sweep(theta, 2, curSD, "/")
  theta <- sweep(theta, 2, targetSD, "*")
  theta <- sweep(theta, 2, targetMean, "+")
  state$AbilityBase <- theta
  if(sdat$fixedAbilityMean == 0L) state$AbilityMean <- targetMean

  for(i in seq_len(sdat$Nitems)){
    for(si in seq_len(sdat$Nscales)){
      aidx <- (i - 1L) * sdat$Nscales + si
      if(sdat$fixedAlog[aidx] == 0L){
        ref <- sdat$freeAref[aidx]
        state$invspApars[ref] <- afunci(pmax(Aold[i, si] * scaleFac[si], eps))
      }
    }
  }

  if(length(state$Bpars)){
    Bold <- numeric(sdat$Nitems)
    for(i in seq_len(sdat$Nitems)){
      if(sdat$fixedB[i] == 0L){
        Bold[i] <- state$Bpars[sdat$freeBref[i]]
      } else {
        Bold[i] <- sdat$Bdata[i]
      }
    }
    Bnew <- Bold - as.numeric(Aold %*% shift)
    for(i in seq_len(sdat$Nitems)){
      if(sdat$fixedB[i] == 0L){
        state$Bpars[sdat$freeBref[i]] <- Bnew[i]
      }
    }
  }

  state
}
