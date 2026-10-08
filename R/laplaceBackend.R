
## The Laplace approximation uses the OBSERVED curvature at the person mode:
## the actual second derivative of the log posterior, not Fisher information.
## Fisher was selectable via laplaceInfo until September 2026 and is not any
## more -- there was no regime in which it was the right choice here, and it
## made the item gradient miss the true total derivative by 1e-3 to 4e-2 for
## the 3PL and 4PL (the 2PL is exact either way, which is why it went unseen
## for so long). See the note at the top of src/laplace_backend.cpp and
## tests/testthat/test-laplace-direct-gradients.R.
##
## Fisher information is still used, and must be: the Newton step uses it,
## and that is what guarantees a positive definite step away from the mode.

## Counts of persons whose data curvature was clipped to positive
## semidefinite, since the last reset. "full" means every eigenvalue was
## clipped, so log|H| is exactly constant in the item parameters and zeroing
## its gradient contribution is exact -- always the case when K = 1.
## "partial" needs K > 1 and is the only case where that zeroing is an
## approximation, so fitIRT reports it rather than passing over it.
bigIRT_laplace_clip_counts <- function(reset = FALSE)
  .Call(`_bigIRT_laplace_clip_counts_cpp_impl`, as.logical(reset))

## Raw row terms, for the tests only.
bigIRT_laplace_row_terms_debug <- function(y, eta, c, d = 1)
  .Call(`_bigIRT_laplace_row_terms_debug_cpp_impl`,
        as.numeric(y), as.numeric(eta), as.numeric(c), as.numeric(d))

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

## Which free loadings take the identity link (fitIRT's signedLoadings), as
## one integer per free loading in the order of invspApars: 1 for the identity,
## 0 for softplus. Every kernel that forms a loading or its derivative is handed
## this vector explicitly; see the note on the link at the top of
## src/laplace_backend.cpp. `sdat$signedA`, one entry per item-by-scale cell in
## the order of fixedAlog, exists only on fits that asked for signed loadings,
## so standata from any other fit -- including every fit made before the
## option existed -- reads as all softplus.
bigIRT_laplace_signed_A <- function(sdat){
  nfree <- max(0L, as.integer(sdat$NitemScales) - as.integer(sdat$NfixedA))
  s <- sdat$signedA
  if(is.null(s)) return(integer(nfree))
  out <- as.integer(s[as.integer(sdat$fixedAlog) == 0L])
  if(length(out) != nfree) stop("sdat$signedA does not match the free loadings.", call. = FALSE)
  out
}

## The same as an Nitems x Nscales logical matrix, or NULL when nothing is
## signed. Row-major like a_matrix_to_vector(), which is how signedA was built.
bigIRT_signed_loading_matrix <- function(sdat){
  s <- sdat$signedA
  if(is.null(s) || !any(as.integer(s) == 1L)) return(NULL)
  matrix(as.integer(s) == 1L, nrow = as.integer(sdat$Nitems),
    ncol = as.integer(sdat$Nscales), byrow = TRUE)
}

bigIRT_laplace_direct_block_fg_cpp_impl <- function(id, score, theta_init,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred, A_signed,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask,
  jitter, max_attempts, max_iter, tol, keep_covariance = FALSE, grain_size = 64L,
  want_row_effective = FALSE){
  .Call(
    `_bigIRT_laplace_direct_block_fg_cpp_impl`,
    as.integer(id),
    as.integer(score),
    as.matrix(theta_init),
    as.matrix(person_pred),
    bigirt_int_matrix(fixed_ability, nrow(person_pred), ncol(theta_init)),
    as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nrow(person_pred), ncol(theta_init)),
    as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nrow(person_pred), ncol(theta_init)),
    as.matrix(A_pred),
    as.integer(A_signed),
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
    bigirt_int_matrix(free_mask, nrow(theta_init), ncol(theta_init)),
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(max_iter),
    as.numeric(tol),
    as.logical(keep_covariance),
    as.integer(grain_size),
    as.logical(want_row_effective)[1]
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
      ## `ifelse` is vectorised over its condition, so a scalar condition here
      ## returns a length-one result that is then recycled: every response gets
      ## the first item's coefficient index. With item-specific betas that
      ## pointed every row at one coefficient, which absorbed every item's
      ## gradient and diverged to a non-finite difficulty. It needs `if`.
      A_beta_row[, si] <- if(sdat$itemSpecificBetas == 1L) ref else ifelse(ref > 0L, 1L, 0L)
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
    ## Per free loading, indexed by A_ref: 1 for the identity link.
    A_signed = bigIRT_laplace_signed_A(sdat),
    B_ref = B_ref,
    B_fixed = sdat$fixedB[items] == 1L,
    B_fixed_value = sdat$Bdata[items],
    B_beta_row = if(sdat$NBitemPreds > 0) {
      if(sdat$itemSpecificBetas == 1L) B_ref else ifelse(B_ref > 0L, 1L, 0L)
    } else integer(nrows),
    C_ref = C_ref,
    C_fixed = sdat$fixedClogit[items] == 1L,
    C_fixed_value = sdat$Cdata[items],
    C_beta_row = if(sdat$NCitemPreds > 0) {
      if(sdat$itemSpecificBetas == 1L) C_ref else ifelse(C_ref > 0L, 1L, 0L)
    } else integer(nrows),
    D_ref = D_ref,
    D_fixed = sdat$fixedDlogit[items] == 1L,
    D_fixed_value = sdat$Ddata[items],
    D_beta_row = if(sdat$NDitemPreds > 0) {
      if(sdat$itemSpecificBetas == 1L) D_ref else ifelse(D_ref > 0L, 1L, 0L)
    } else integer(nrows),
    person_pred = if(sdat$NpersonPreds > 0) as.matrix(sdat$personPreds[rows,, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    ## Constant for the life of the fit; see the note on the accessor.
    person_pred_varies = if(sdat$NpersonPreds > 0)
      bigIRT_laplace_person_pred_varies(sdat$personPreds[rows,, drop = FALSE], ids) else logical(0),
    ## The native ability-beta kernel wants these in a fixed storage mode. They
    ## do not change while the fit runs, and converting 8 million responses on
    ## every gradient evaluation was showing up in the profile.
    ids_int = as.integer(ids),
    score = as.numeric(sdat$score[rows]),
    fixed_ability_int = matrix(as.integer(fixed_ability), nrow = nrows, ncol = K),
    ## Rows grouped by person. The ability-beta gradient needs a person total
    ## before it can weight that person's responses, and with the rows grouped
    ## that dependency is local: both passes run over one person's handful of
    ## rows rather than over the whole response set twice. It also makes the
    ## sweep parallel, since persons are disjoint. Response data are often
    ## already in this order, but nothing guarantees it.
    person_row_order = as.integer(order(ids)) - 1L,
    person_row_start = as.integer(c(0L, cumsum(tabulate(ids, nbins = sdat$Nsubs)))),
    # Both predictor matrices are long-row matrices in standata.  Index them
    # with `rows`, never with the item id: item ids are not row positions.
    A_pred = if(sdat$NAitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$AitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    B_pred = if(sdat$NBitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$BitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    C_pred = if(sdat$NCitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$CitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0),
    D_pred = if(sdat$NDitemPreds > 0) as.matrix(sdat$itemPreds[rows, sdat$DitemPreds, drop = FALSE]) else matrix(0, nrow = nrows, ncol = 0)
  )
}

## Group means of the predictors. These were accumulated a row at a time in R,
## which is a loop over every response: 12 seconds of an 8.1M-response fit for
## a quantity rowsum produces in one call. As elsewhere in this file, rowsum
## returns groups in the order encountered rather than in level order, so the
## result is scattered back by label rather than assumed aligned.
bigIRT_laplace_group_means <- function(x, group, ngroups, ncols){
  if(ncols == 0) return(matrix(0, nrow = ngroups, ncol = 0))
  counts <- tabulate(group, nbins = ngroups)
  acc <- rowsum(x, group = group, reorder = FALSE)
  out <- matrix(0, nrow = ngroups, ncol = ncols)
  out[as.integer(rownames(acc)), ] <- acc
  out / pmax(counts, 1)
}

bigIRT_laplace_person_means_by_id <- function(sdat)
  bigIRT_laplace_group_means(sdat$personPreds, sdat$id, sdat$Nsubs, sdat$NpersonPreds)

bigIRT_laplace_item_means <- function(sdat)
  bigIRT_laplace_group_means(sdat$itemPreds, sdat$item, sdat$Nitems, sdat$NitemPreds)

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
  signedA <- context$A_signed %||% bigIRT_laplace_signed_A(sdat)
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
    ## A signed loading (signedLoadings) is its raw value, not its softplus.
    if(any(signedA == 1L)){
      sg <- ref > 0L
      sg[sg] <- signedA[ref[sg]] == 1L
      if(any(sg)) loadings[sg, si] <- raw[sg]
    }
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

## How many parameters a layout covers. The layouts are built once per fit and
## then packed and unpacked on every iteration, and max(unlist(layout)) rebuilds
## an index vector the size of the whole parameter set each time it is asked.
## The constructors record the answer instead.
bigIRT_laplace_layout_size <- function(layout){
  n <- attr(layout, "n_par", exact = TRUE)
  if(!is.null(n)) return(n)
  max(unlist(layout), 0L)
}

bigIRT_laplace_layout_with_size <- function(layout){
  attr(layout, "n_par") <- max(unlist(layout), 0L)
  layout
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
  bigIRT_laplace_layout_with_size(list(
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
  ))
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
    A_signed = row_context$A_signed,
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
  out <- numeric(bigIRT_laplace_layout_size(layout))
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
  n_item_par <- bigIRT_laplace_layout_size(item_layout)
  cursor <- n_item_par + 1L
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
  attr(item_layout, "n_par") <- max(unlist(item_layout), 0L)
  attr(item_layout, "n_item_par") <- n_item_par
  attr(item_layout, "beta_scale") <- bigIRT_laplace_beta_scale(sdat)
  attr(item_layout, "par_scale") <- bigIRT_laplace_par_scale(sdat, item_layout)
  item_layout
}

## Whether the person-predictor coefficients were kept out of estimation.
##
## The reported flag used to be `sdat$NpersonPreds > 0`, which is backwards: it
## read TRUE for exactly the fits that estimate the coefficients, and FALSE
## whenever there was nothing that could be frozen. The freezing it was named
## for belonged to the withdrawn alternating backend, which took its item step
## against a frozen posterior; the direct backend puts `ability_beta` into the
## free parameter vector whenever person predictors exist.
##
## Ask the layout rather than the data, so the flag stays true to whatever a
## backend actually does: coefficients are frozen only if they exist and the
## optimiser did not carry them. A missing layout means nothing was optimised,
## which is frozen too.
bigIRT_laplace_beta_frozen <- function(sdat, layout){
  npred <- sdat$NpersonPreds %||% 0L
  if(!isTRUE(npred > 0L)) return(FALSE)
  !isTRUE(length(layout$ability_beta) > 0L)
}

## Per-covariate scale for the ability_beta block, aligned with the column-major
## unrolling of the Nscales x NpersonPreds coefficient matrix.
bigIRT_laplace_beta_scale <- function(sdat){
  npred <- as.integer(sdat$NpersonPreds)
  nscale <- as.integer(sdat$Nscales)
  if(!isTRUE(npred > 0L) || !isTRUE(nscale > 0L)) return(numeric(0))
  nsub <- max(as.numeric(sdat$Nsubs), 1)
  nobs <- max(as.numeric(if(!is.null(sdat$Nobs)) sdat$Nobs else length(sdat$score)), 1)
  nitem <- max(as.numeric(sdat$Nitems), 1)
  ## curvature of an item parameter goes as its response count, of a covariate
  ## coefficient as the person count times the covariate variance
  base <- sqrt((nobs / nitem) / nsub)
  preds <- sdat$personPreds
  sdk <- if(!is.null(preds) && length(preds) && NCOL(preds) == npred){
    apply(as.matrix(preds), 2, stats::sd)
  } else rep(1, npred)
  sdk[!is.finite(sdk) | sdk <= 0] <- 1
  rep(pmin(pmax(base / sdk, 1e-4), 1e4), each = nscale)
}

## Per-parameter optimiser scaling for every block whose gradient aggregates
## over many units while holding few parameters.
##
## Each such block was found the hard way, one at a time. A covariate
## coefficient's gradient sums over every person while an item parameter's sums
## only over its own responses, so one curvature estimate cannot size steps for
## both. The mean hyperparameters have the same shape -- each is informed by
## every item -- and at Mindsteps scale A_mean alone held 78 per cent of the
## gradient norm while the fit reported convergence. So do the item covariates:
## with `itemSpecificBetas = FALSE`, the default, a single shared coefficient
## held 92 per cent of it on a 2,000-item fit. The rule is the same throughout,
## the square root of the ratio of contributing units, and they are listed
## together here so the next block added is not missed as these three were.
bigIRT_laplace_par_scale <- function(sdat, layout){
  n <- bigIRT_laplace_layout_size(layout)
  out <- rep(1, n)
  if(n == 0L) return(out)
  nsub <- max(as.numeric(sdat$Nsubs), 1)
  nobs <- max(as.numeric(if(!is.null(sdat$Nobs)) sdat$Nobs else length(sdat$score)), 1)
  nitem <- max(as.numeric(sdat$Nitems), 1)
  per_item <- nobs / nitem                    # responses behind one item parameter
  bs <- attr(layout, "beta_scale")
  if(length(layout$ability_beta) && length(bs) == length(layout$ability_beta))
    out[layout$ability_beta] <- bs
  ## A mean hyperparameter is informed by every item, an item parameter by its
  ## own responses only.
  mean_scale <- sqrt(per_item / nitem)
  for(nm in c("B_mean", "A_mean", "C_mean", "D_mean")){
    idx <- layout[[nm]]
    if(length(idx)) out[idx] <- min(max(mean_scale, 1e-4), 1e4)
  }
  ## The ability mean is informed by every person.
  if(length(layout$ability_mean))
    out[layout$ability_mean] <- min(max(sqrt(per_item / nsub), 1e-4), 1e4)
  ## So is each latent correlation, and leaving it out was the reason the
  ## correlation estimator saturated. Its gradient sums over every person while
  ## an item parameter's sums over its own responses, so a step sized for the
  ## item block overshoots it by orders of magnitude. The correlation is
  ## bounded -- rho = tanh(par) under the default parameterisation -- so
  ## overshooting does not merely slow convergence, it lands in the flat tail
  ## where the gradient is numerically zero and the fit can never come back.
  ## That produced exactly 1.000 for any generating value from about .6 up,
  ## while still reporting convergence, because the vanished gradient left
  ## nothing for the total norm to notice.
  if(length(layout$corr))
    out[layout$corr] <- min(max(sqrt(per_item / nsub), 1e-4), 1e4)
  ## Item covariates, where the default is the bad case: one coefficient per
  ## predictor whose gradient sums over every response. With item-specific betas
  ## each sees only its own item and is already on the item scale.
  item_beta_preds <- c(A_beta = "AitemPreds", B_beta = "BitemPreds",
                       C_beta = "CitemPreds", D_beta = "DitemPreds")
  shared_beta <- !identical(as.integer(sdat$itemSpecificBetas)[1], 1L)
  ipred <- if(!is.null(sdat$itemPreds)) as.matrix(sdat$itemPreds) else NULL
  for(nm in names(item_beta_preds)){
    idx <- layout[[nm]]
    if(!length(idx)) next
    prednames <- sdat[[item_beta_preds[[nm]]]]
    npred <- max(length(prednames), 1L)
    per_pred <- max(length(idx) %/% npred, 1L)
    sdk <- rep(1, npred)
    if(!is.null(ipred) && length(prednames)){
      cols <- intersect(prednames, colnames(ipred))
      if(length(cols) == npred){
        v <- apply(ipred[, cols, drop = FALSE], 2, stats::sd)
        v[!is.finite(v) | v <= 0] <- 1
        sdk <- as.numeric(v)
      }
    }
    base_ib <- if(shared_beta) sqrt(1 / nitem) else 1
    ## Bbeta is rows-by-predictors and packs column major, so a predictor's
    ## scale repeats across its rows.
    sc <- rep(pmin(pmax(base_ib / sdk, 1e-4), 1e4), each = per_pred)
    if(length(sc) == length(idx)) out[idx] <- sc
  }
  out
}

## Pack the direct Laplace parameter vector, optionally including the latent
## correlation parameters. Inputs: current state, standata, and a direct layout.
bigIRT_laplace_pack_direct_state <- function(state, sdat,
  layout = bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)){
  out <- numeric(bigIRT_laplace_layout_size(layout))
  item_slots <- layout[intersect(names(layout), names(bigIRT_laplace_item_layout(sdat)))]
  ## The item blocks come first in the direct layout, so where they end is
  ## recorded when it is built rather than rediscovered from the indices here.
  n_item_par <- attr(layout, "n_item_par", exact = TRUE)
  if(is.null(n_item_par)) n_item_par <- max(unlist(item_slots), 0L)
  if(n_item_par > 0L){
    out[seq_len(n_item_par)] <- bigIRT_laplace_pack_item_state(state, sdat, layout = item_slots)
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
  ## Hand the optimiser the rescaled coordinates; see bigIRT_laplace_par_scale.
  ps <- attr(layout, "par_scale")
  if(length(ps) == length(out)) out <- out / ps
  out
}

## Unpack the direct Laplace parameter vector, including latent correlation
## parameters when present. Returns an updated state list; mutates nothing.
bigIRT_laplace_unpack_direct_state <- function(par, state, sdat,
  layout = bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE),
  corr_paramization = c("normalized_chol", "stan_corsqrt")){
  corr_paramization <- match.arg(corr_paramization)
  ## Back to natural coordinates before anything reads a parameter.
  ps <- attr(layout, "par_scale")
  if(length(ps) == length(par)) par <- par * ps
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
  n_par <- bigIRT_laplace_layout_size(layout)
  if(!isTRUE(as.logical(sdat$dopriors))) return(list(value = 0, grad = numeric(n_par)))
  grad <- numeric(n_par)
  value <- 0

  signedA <- bigIRT_laplace_signed_A(sdat) == 1L
  if(length(state$invspApars) && any(signedA)){
    ## A signed loading (signedLoadings) is centred at zero with the same SD,
    ## never at the estimated mean: invspAMean is the mean of the softplus
    ## pre-images of the positive loadings, a different scale and a different
    ## population, and only those pull on it.
    muA <- ifelse(signedA, 0, state$invspAMean)
    value <- value + sum(dnorm(state$invspApars, mean = muA, sd = sdat$invspASD, log = TRUE))
    if(length(layout$A)) grad[layout$A] <- grad[layout$A] - (state$invspApars - muA) / (sdat$invspASD^2)
    if(length(layout$A_mean)) grad[layout$A_mean] <- sum(((state$invspApars - state$invspAMean) / (sdat$invspASD^2))[!signedA])
  } else if(length(state$invspApars)){
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

bigIRT_laplace_person_step_block_cpp_impl <- function(id, score, theta_init,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred, A_signed,
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
    bigirt_int_matrix(fixed_ability, nrow(person_pred), ncol(theta_init)),
    as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nrow(person_pred), ncol(theta_init)),
    as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nrow(person_pred), ncol(theta_init)),
    as.matrix(A_pred),
    as.integer(A_signed),
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
    bigirt_int_matrix(free_mask, nrow(theta_init), ncol(theta_init)),
    as.numeric(jitter),
    as.integer(max_attempts),
    as.integer(max_iter),
    as.numeric(tol),
    as.logical(keep_covariance),
    as.integer(grain_size)
  )
}

## Evaluate the fitted objective at `par`, solving every person mode inside the
## call. This is the Laplace objective, or its quadrature (`context$agq`) or
## second-order (`context$laplace2`) refinement; with `context$map` set it is
## the joint-MAP objective instead (R/mapBackend.R), which returns the same
## structure, so the optimiser, metric, certificate and information code work
## on either without knowing which.
bigIRT_laplace_direct_objective <- function(par, state, sdat, prior_precision,
  theta_init = state$AbilityBase, jitter = 1e-6, max_attempts = 8L,
  max_iter = 50L, tol = 1e-6, keep_covariance = FALSE, context = NULL){
  if(isTRUE(context$map))
    return(bigIRT_map_objective(par = par, state = state, sdat = sdat,
      prior_precision = prior_precision, theta_init = theta_init, jitter = jitter,
      max_attempts = max_attempts, max_iter = max_iter, tol = tol,
      keep_covariance = keep_covariance, context = context))
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
  ## Which free loadings take the identity link; every kernel below is told.
  A_signed <- row_context$A_signed %||% bigIRT_laplace_signed_A(sdat)

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
  ## Whether the adjoint gradient path runs decides whether the kernel should
  ## hand back the per-response effective values it derives along the way.
  ## With `context$agq` set, the person integrals are done by adaptive
  ## Gauss-Hermite quadrature centred at the Laplace mode instead of by the
  ## Laplace approximation itself (R/agq.R, src/agq_backend.cpp). The mode
  ## solve is the same; what replaces the Laplace item kernel is a quadrature
  ## over each person's posterior, and every gradient block becomes a
  ## posterior expectation.
  agq_rule <- context$agq
  need_person_terms <- is.null(agq_rule) && (length(context$direct_layout$ability_beta) > 0L ||
    (isTRUE(context$estimateAbilityCorr) && length(context$direct_layout$corr) > 0L) ||
    length(context$direct_layout$ability_mean) > 0L)
  if(!is.null(agq_rule)) {
    t_p0 <- wall_time_sec()
    ## `context$agq_centre`, a posterior from an earlier evaluation, holds the
    ## nodes where they are. The quadrature objective is then an ordinary
    ## smooth function whose exact gradient is the Fisher-identity one, which
    ## is what the gradient tests use.
    posterior_q <- if(!is.null(context$agq_centre)) context$agq_centre else bigIRT_laplace_person_step_block_cpp_impl(
      id = row_context$ids, score = context$score_rows %||% sdat$score[context$train_rows],
      theta_init = theta_init, person_pred = person_pred,
      fixed_ability = row_context$fixed_ability_int %||% row_context$fixed_ability, fixed_ability_value = row_context$fixed_ability_value,
      A_ref = row_context$A_ref, A_fixed_value = row_context$fixed_A_value,
      A_beta_row = row_context$A_beta_row, A_pred = row_context$A_pred, A_signed = A_signed,
      B_ref = row_context$B_ref, B_fixed_value = row_context$B_fixed_value,
      B_beta_row = row_context$B_beta_row, B_pred = row_context$B_pred,
      C_ref = row_context$C_ref, C_fixed_value = row_context$C_fixed_value,
      C_beta_row = row_context$C_beta_row, C_pred = row_context$C_pred,
      D_ref = row_context$D_ref, D_fixed_value = row_context$D_fixed_value,
      D_beta_row = row_context$D_beta_row, D_pred = row_context$D_pred,
      Abilitybeta = ability_beta, invspApars = curState$invspApars, invspAbeta = curState$invspAbeta,
      Bpars = curState$Bpars, Bbeta = curState$Bbeta, logitCpars = curState$logitCpars,
      logitCbeta = curState$logitCbeta, logitDpars = curState$logitDpars, logitDbeta = curState$logitDbeta,
      prior_mean = prior_mean, prior_precision = prior_mats$precision_array,
      free_mask = 1L - sdat$fixedAbilityLogical, jitter = jitter, max_attempts = max_attempts,
      max_iter = max_iter, tol = tol, keep_covariance = keep_covariance,
      grain_size = context$grain_size)
    t_p1 <- wall_time_sec()
    aq <- bigIRT_agq_fg_cpp_impl(
      id = row_context$ids, score = context$score_rows %||% sdat$score[context$train_rows],
      theta_mode = posterior_q$theta_mode, precision = posterior_q$precision,
      person_pred = person_pred, fixed_ability = row_context$fixed_ability_int %||% row_context$fixed_ability,
      fixed_ability_value = row_context$fixed_ability_value,
      A_ref = row_context$A_ref, A_fixed_value = row_context$fixed_A_value,
      A_beta_row = row_context$A_beta_row, A_pred = row_context$A_pred, A_signed = A_signed,
      B_ref = row_context$B_ref, B_fixed_value = row_context$B_fixed_value,
      B_beta_row = row_context$B_beta_row, B_pred = row_context$B_pred,
      C_ref = row_context$C_ref, C_fixed_value = row_context$C_fixed_value,
      C_beta_row = row_context$C_beta_row, C_pred = row_context$C_pred,
      D_ref = row_context$D_ref, D_fixed_value = row_context$D_fixed_value,
      D_beta_row = row_context$D_beta_row, D_pred = row_context$D_pred,
      Abilitybeta = ability_beta, invspApars = curState$invspApars, invspAbeta = curState$invspAbeta,
      Bpars = curState$Bpars, Bbeta = curState$Bbeta, logitCpars = curState$logitCpars,
      logitCbeta = curState$logitCbeta, logitDpars = curState$logitDpars, logitDbeta = curState$logitDbeta,
      prior_mean = prior_mean, prior_precision = prior_mats$precision_array,
      free_mask = 1L - sdat$fixedAbilityLogical, rule = agq_rule,
      want_person = isTRUE(context$agq_want_person), grain_size = context$grain_size,
      ## The certificate's item blocks (R/certify.R), when it asks.
      block_items = if(isTRUE(context$agq_blocks)) row_context$items else integer(0))
    t_p2 <- wall_time_sec()
    direct_fg <- list(value = aq$value, posterior = posterior_q, item_fg = aq,
      timings = list(personKernelSec = t_p1 - t_p0, itemKernelSec = t_p2 - t_p1,
        kernelSec = t_p2 - t_p0))
  } else direct_fg <- bigIRT_laplace_direct_block_fg_cpp_impl(
    id = row_context$ids,
    score = context$score_rows %||% sdat$score[context$train_rows],
    theta_init = theta_init,
    person_pred = person_pred,
    fixed_ability = row_context$fixed_ability_int %||% row_context$fixed_ability,
    fixed_ability_value = row_context$fixed_ability_value,
    A_ref = row_context$A_ref,
    A_fixed_value = row_context$fixed_A_value,
    A_beta_row = row_context$A_beta_row,
    A_pred = row_context$A_pred,
    A_signed = A_signed,
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
    grain_size = context$grain_size,
    want_row_effective = need_person_terms
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
  person_terms <- NULL
  row_effective <- NULL
  if(need_person_terms){
    ## The block kernel already derived these while evaluating the likelihood,
    ## so take them rather than rebuilding the same four quantities in R. That
    ## rebuild was a quarter of every objective evaluation on fits with person
    ## covariates, and keeping one implementation means the gradient and the
    ## objective cannot drift apart.
    row_effective <- direct_fg$item_fg$row_effective
    if(is.null(row_effective))
      row_effective <- bigIRT_laplace_row_effective(
        state = curState, sdat = sdat, thetaBase = posterior$theta_mode,
        rows = context$train_rows, context = row_context)
    ## When a person predictor varies within person the beta gradient needs a
    ## second sweep only if its D accumulator is refused; otherwise both come
    ## out of one.
    beta_vary <- if(length(context$direct_layout$ability_beta))
      which(bigIRT_laplace_person_pred_within_varying(row_context)) else integer(0)
    fused <- if(length(beta_vary))
      bigIRT_laplace_person_beta_fused(sdat = sdat, posterior = posterior,
        row_effective = row_effective, row_context = row_context,
        vary_idx = beta_vary) else NULL
    person_terms <- if(!is.null(fused)) fused[c("score", "slope")] else
      bigIRT_laplace_person_row_terms(
        sdat = sdat, posterior = posterior,
        row_effective = row_effective, row_context = row_context
      )
  }
  if(!is.null(agq_rule)){
    ## Quadrature: each person-side block is a posterior expectation the
    ## kernel has already summed. The ability mean and the covariate
    ## coefficients enter through the prior's quadratic form and each row's
    ## ability; the correlations through the prior alone.
    lay <- context$direct_layout
    if(length(lay$ability_beta))
      approx_grad[lay$ability_beta] <- approx_grad[lay$ability_beta] +
        as.numeric(direct_fg$item_fg$grad_ability_beta)[seq_along(lay$ability_beta)]
    if(length(lay$ability_mean)){
      mu <- as.numeric(curState$AbilityMean)
      g_mu <- as.numeric(direct_fg$item_fg$grad_mean)
      if(isTRUE(as.logical(sdat$dopriors))){
        mu0 <- rep_len(as.numeric(sdat$AbilityMeandat), length(mu))
        mu_sd <- pmax(rep_len(as.numeric(sdat$AbilityMeanSD), length(mu)), 1e-8)
        prior$value <- prior$value + sum(stats::dnorm(mu, mean = mu0, sd = mu_sd, log = TRUE))
        g_mu <- g_mu - (mu - mu0) / (mu_sd^2)
      }
      approx_grad[lay$ability_mean] <- g_mu
    }
    if(isTRUE(context$estimateAbilityCorr) && length(lay$corr))
      approx_grad[lay$corr] <- approx_grad[lay$corr] + bigIRT_agq_corr_grad(
        curState$AbilityCorrPars, direct_fg$item_fg$S, sdat,
        paramization = context$corr_paramization, jitter = jitter)
  }
  if(is.null(agq_rule) && length(context$direct_layout$ability_beta)){
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
        ## The prior precision AT THE CURRENT CORRELATION. This used to pass
        ## the function's `prior_precision` argument -- the precision at the
        ## correlation the fit started from -- so with correlations estimated
        ## the adjoint term Q Sigma g used a stale Q, and the gradient missed
        ## by 1.4 to 4 per cent at rho = 0.5 against central differences.
        ## Each of the two blocks had its own test and neither ran with the
        ## other (test-laplace-direct-gradients.R now does).
        prior_precision = prior_mats$precision_array,
        person_terms = person_terms,
        row_effective = row_effective,
        fused_beta = if(!is.null(fused)) fused$beta else NULL
      )
    )
  }
  ability_mean_sec <- 0
  if(is.null(agq_rule) && length(context$direct_layout$ability_mean)){
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
  if(is.null(agq_rule) && isTRUE(context$estimateAbilityCorr) && length(context$direct_layout$corr)){
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
  ## Second-order Laplace (context$laplace2): add each person's leading
  ## correction to the Laplace value, with its exact gradient; see
  ## src/laplace2_backend.cpp. Everything above is the Laplace objective.
  laplace2_value <- 0; laplace2_skipped <- NA_integer_
  if(isTRUE(context$laplace2) && is.null(agq_rule)){
    l2 <- bigIRT_laplace2_fg_cpp_impl(
      id = row_context$ids, score = context$score_rows %||% sdat$score[context$train_rows],
      theta_mode = posterior$theta_mode, person_pred = person_pred,
      fixed_ability = row_context$fixed_ability_int %||% row_context$fixed_ability, fixed_ability_value = row_context$fixed_ability_value,
      A_ref = row_context$A_ref, A_fixed_value = row_context$fixed_A_value, A_pred = row_context$A_pred,
      A_signed = A_signed,
      B_ref = row_context$B_ref, B_fixed_value = row_context$B_fixed_value, B_pred = row_context$B_pred,
      C_ref = row_context$C_ref, C_fixed_value = row_context$C_fixed_value, C_pred = row_context$C_pred,
      D_ref = row_context$D_ref, D_fixed_value = row_context$D_fixed_value, D_pred = row_context$D_pred,
      Abilitybeta = ability_beta, invspApars = curState$invspApars, invspAbeta = curState$invspAbeta,
      Bpars = curState$Bpars, Bbeta = curState$Bbeta, logitCpars = curState$logitCpars,
      logitCbeta = curState$logitCbeta, logitDpars = curState$logitDpars, logitDbeta = curState$logitDbeta,
      prior_mean = prior_mean, prior_precision = prior_mats$precision_array,
      free_mask = 1L - sdat$fixedAbilityLogical, grain_size = context$grain_size)
    laplace2_value <- l2$value; laplace2_skipped <- l2$skipped
    lay <- context$direct_layout
    add_blk <- function(idx, g) if(length(idx)) approx_grad[idx] <<- approx_grad[idx] + as.numeric(g)[seq_along(idx)]
    add_blk(lay$A, l2$grad_A); add_blk(lay$B, l2$grad_B); add_blk(lay$C, l2$grad_C); add_blk(lay$D, l2$grad_D)
    if(sdat$NAitemPreds > 0) add_blk(lay$A_beta, l2$grad_A_beta)
    if(sdat$NBitemPreds > 0) add_blk(lay$B_beta, l2$grad_B_beta)
    if(sdat$NCitemPreds > 0) add_blk(lay$C_beta, l2$grad_C_beta)
    if(sdat$NDitemPreds > 0) add_blk(lay$D_beta, l2$grad_D_beta)
    add_blk(lay$ability_beta, l2$grad_ability_beta)
    add_blk(lay$ability_mean, l2$grad_mean)
    if(isTRUE(context$estimateAbilityCorr) && length(lay$corr))
      add_blk(lay$corr, bigIRT_laplace2_corr_grad(curState$AbilityCorrPars, l2$grad_P0, sdat,
        paramization = context$corr_paramization, jitter = jitter))
  }
  ## Chain rule for the rescaled coordinates. The optimiser works in
  ## u = theta / par_scale, so dL/du = par_scale * dL/dtheta. Applied once, and
  ## here, so every contributor above -- kernel, adjoint, prior -- stays in the
  ## natural parameter scale.
  par_scale <- attr(context$direct_layout, "par_scale")
  if(length(par_scale) == length(approx_grad))
    approx_grad <- approx_grad * par_scale
  total_sec <- wall_time_sec() - t_total0
  list(
    value = laplace_value + prior_norm_value + prior$value + laplace2_value,
    laplace2_skipped = laplace2_skipped,
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

## One block-Newton direction over the item parameters, from an evaluation that
## retained the person posterior covariances. Returns a full-length step vector
## with zeros outside the item blocks; NULL when no blocks are available.
bigIRT_laplace_newton_step <- function(ev, sdat, context, layout){
  if(is.null(ev$posterior$covariance)) return(NULL)
  ## The evaluation already carries the response rows when person terms were
  ## wanted, and this runs once per polish step, so rebuilding them here was
  ## eight extra sweeps over the response set per fit.
  re <- ev$item_fg$row_effective
  if(is.null(re))
    re <- bigIRT_laplace_row_effective(
      state = ev$state, sdat = sdat, thetaBase = ev$posterior$theta_mode,
      rows = context$train_rows, context = context$row_context)
  bl <- bigIRT_item_info_blocks(state = ev$state, sdat = sdat, context = context,
    row_effective = re, posterior = ev$posterior,
    thetaBase = ev$posterior$theta_mode)
  if(is.null(bl)) return(NULL)
  P <- bl$P; ni <- bl$ni
  ps <- attr(layout, "par_scale")
  step <- numeric(length(ev$approx_grad))
  ## Each item steps over the parameters it has free, through its free
  ## position (bigIRT_item_free_pos): index j is item j only while no item
  ## has the parameter fixed, and with one difficulty anchored the free ones
  ## used to take no step at all.
  for(j in seq_len(ni)){
    f <- !is.na(bl$free_pos[, j])
    if(!any(f)) next
    idx <- bigIRT_item_coords(bl$free_pos, layout, j)[f]
    if(anyNA(idx) || any(idx < 1L)) next
    H <- matrix(bl$info[, , j], P, P)[f, f, drop = FALSE] + diag(bl$prior_precision[f, j], sum(f))
    ## The gradient arrives in the optimiser scale, and the curvature is in the
    ## natural one, so bring the curvature across before solving.
    if(length(ps) == length(step)){
      s <- ps[idx]
      H <- H * outer(s, s)
    }
    sol <- try(solve(H, ev$approx_grad[idx]), silent = TRUE)
    if(inherits(sol, "try-error") || any(!is.finite(sol))) next
    step[idx] <- sol
  }

  ## The mean hyperparameters too. Nothing else polishes them, and after the
  ## item blocks are cleaned up they are what is left: on an empirical-Bayes
  ## fit they held 41 per cent of the remaining gradient and kept it just the
  ## wrong side of the tolerance. Each enters only through the item prior, so
  ## its curvature is exact and needs no data pass:
  ##   d2/dmu2  =  Nitems / sd^2  +  1 / muSD^2.
  mean_blocks <- list(
    B_mean = c(sd = "BSDx",     musd = "BMeanSD"),
    A_mean = c(sd = "invspASD", musd = "AMeanSD"),
    C_mean = c(sd = "logitCSD", musd = "logitCMeanSD"),
    D_mean = c(sd = "logitDSD", musd = "logitDMeanSD"))
  ## Signed loadings are centred at zero, not at the A mean, so only the
  ## softplus ones count towards its curvature.
  n_free <- c(B_mean = length(ev$state$Bpars),
              A_mean = length(ev$state$invspApars) - sum(bigIRT_laplace_signed_A(sdat) == 1L),
              C_mean = length(ev$state$logitCpars), D_mean = length(ev$state$logitDpars))
  for(nm in names(mean_blocks)){
    idx <- layout[[nm]]
    if(!length(idx) || n_free[[nm]] == 0L) next
    sd_b <- suppressWarnings(as.numeric(sdat[[mean_blocks[[nm]][["sd"]]]])[1])
    mu_sd <- suppressWarnings(as.numeric(sdat[[mean_blocks[[nm]][["musd"]]]])[1])
    if(!isTRUE(is.finite(sd_b)) || sd_b <= 0) next
    H <- n_free[[nm]] / sd_b^2
    if(isTRUE(is.finite(mu_sd)) && mu_sd > 0) H <- H + 1 / mu_sd^2
    if(length(ps) == length(step)) H <- H * ps[idx]^2
    if(!is.finite(H) || H <= 0) next
    step[idx] <- ev$approx_grad[idx] / H
  }

  if(all(step == 0)) return(NULL)
  step
}

## Optimise the fitted objective over the item-side parameters, the person
## modes solved inside each evaluation. `objective` is "laplace" (the marginal,
## refined afterwards when `refine` is given) or "map" (the joint mode,
## R/mapBackend.R, which has no marginal to refine).
bigIRT_laplace_optimize_direct <- function(state, sdat, prior_precision,
  niter = 50L, tol = 1e-4, jitter = 1e-6, person_tol = 1e-6,
  keep_covariance = FALSE, cores = 1L, estimateAbilityCorr = FALSE,
  corr_paramization = c("normalized_chol", "stan_corsqrt"),
  collect_history = FALSE, plot_callback = NULL, plot_every = 1L,
  verbose = 0L, trace_fn = NULL, polish_steps = NA_integer_,
  polish_tol = 1e-8, optimizer = c("fisher", "mize"), refine = NULL,
  certify = NULL, objective = c("laplace", "map")){
  corr_paramization <- match.arg(corr_paramization)
  optimizer <- match.arg(optimizer)
  objective <- match.arg(objective)
  ## The joint-MAP objective has no correlation coordinate (see the note at
  ## the end of R/mapBackend.R) and no marginal to refine.
  if(identical(objective, "map")){ estimateAbilityCorr <- FALSE; refine <- NULL }
  direct_layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = estimateAbilityCorr)
  item_layout <- direct_layout[setdiff(names(direct_layout), c("corr", "ability_mean", "ability_beta"))]
  context <- bigIRT_laplace_item_context(sdat, layout = item_layout)
  context$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  context$direct_layout <- direct_layout
  context$map <- identical(objective, "map")
  ## Data, not parameters: taken once rather than subset on every evaluation.
  context$score_rows <- as.integer(sdat$score[context$train_rows])
  context$cache <- new.env(parent = emptyenv())
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
        ## Diagonals of a K x K x N array sit at a fixed stride; an lapply over
        ## persons to take them is a loop over the whole sample.
        Kc <- dim(cov_arr)[1]; Nc <- dim(cov_arr)[3]
        sel <- rep((seq_len(Kc) - 1L) * (Kc + 1L) + 1L, times = Nc) +
          rep((seq_len(Nc) - 1L) * Kc * Kc, each = Kc)
        post_sd <- sqrt(pmax(as.numeric(cov_arr)[sel], 0))
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
  minv <- NULL
  fg_f <- function(par){
    r <- get_eval(par)
    list(f = -r$value, g = -r$approx_grad, ev = r, x = par)
  }
  gfun <- function(par) -get_eval(par)$approx_grad
  ## `context` is read when called, so the solver follows the objective when
  ## the quadrature refinement below switches it.
  solver <- function(e) bigIRT_laplace_metric_solver(e, sdat, context, direct_layout, gfun)
  if(identical(optimizer, "fisher")){
    minv <- solver(fg_f(init))
    ## Finite-difference columns move the cache and the warm start; put both
    ## back at the point the optimiser is about to use.
    get_eval(init)
    ## No metric -- the information would be too dense to hold, or could not
    ## be factored -- means no reliable predicted gain either, so run the mize
    ## path with its polish rather than an unshaped L-BFGS that would stop on
    ## a number it cannot estimate.
    if(is.null(minv)){
      optimizer <- "mize"
      if(as.integer(verbose) >= 1L) message("Laplace: Fisher metric unavailable; using the mize optimiser.")
    }
  }
  if(identical(optimizer, "fisher") && !is.null(minv)){
    ## L-BFGS shaped by the person-coupled Fisher information; see
    ## R/laplaceOptimizer.R. `tol` is the predicted gain, in nats, below which
    ## it stops.
    refresh <- function(e){ m <- solver(e); get_eval(e$x); m }
    ## When quadrature refinement follows, polishing the Laplace optimum is
    ## wasted: the refinement's target sits several nats of predicted gain
    ## away from it on 3PL and 4PL data. So the Laplace stage stops early.
    lap_tol <- if(!is.null(refine)) max(tol, getOption("bigIRT.agq_switch_gain", 0.1)) else tol
    fit <- bigIRT_laplace_lbfgs(init, fg_f, minv = minv, maxit = as.integer(niter),
      gain_tol = lap_tol, refresh = refresh,
      refresh_every = as.integer(getOption("bigIRT.fisher_refresh", 10L)))
    fit$metric <- attr(minv, "metric")
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
  ## Newton polish.
  ##
  ## L-BFGS gives up early on large problems: at 8 million responses it stopped
  ## reporting progress with a Newton decrement of 300, and a single Newton step
  ## from that point recovered 146 units of objective against 149.5 predicted.
  ## The curvature needed is the per-item information the empirical-Bayes path
  ## already builds, so each step costs one closed-form assembly plus a short
  ## backtracking search. The item blocks carry essentially all of the remaining
  ## gradient, so only they are stepped.
  polish <- list(steps = 0L, gain = 0)
  ## The polish is a block-diagonal Newton step, the item-diagonal part of the
  ## curvature the Fisher-shaped optimiser already uses; after it there is
  ## nothing for the polish to find, so it runs after mize only. `NA` (the
  ## default) defers to that: 8 steps under mize, none under fisher -- the
  ## same resolved `optimizer`, so a fisher run that fell back from an
  ## unavailable metric (above) still gets the mize default. An explicit
  ## nonzero count under fisher would silently do nothing, which is worse
  ## than refusing it by name.
  resolved_polish_steps <- if(is.na(polish_steps)){
    if(identical(optimizer, "fisher")) 0L else 8L
  } else if(identical(optimizer, "fisher") && as.integer(polish_steps) != 0L){
    stop("laplacePolish has no effect under laplaceOptimizer = \"fisher\": the ",
      "block-Newton polish only runs after the \"mize\" optimizer. Leave ",
      "laplacePolish at its default (NA), pass 0, or set laplaceOptimizer = \"mize\".",
      call. = FALSE)
  } else as.integer(polish_steps)
  if(resolved_polish_steps > 0L && !identical(optimizer, "fisher")){
    for(sIt in seq_len(resolved_polish_steps)){
      cur <- bigIRT_laplace_direct_objective(
        par = fit$par, state = state, sdat = sdat, prior_precision = prior_precision,
        theta_init = theta_warm, jitter = jitter, tol = person_tol,
        keep_covariance = TRUE, context = context)
      stp <- try(bigIRT_laplace_newton_step(cur, sdat, context, direct_layout), silent = TRUE)
      if(inherits(stp, "try-error") || is.null(stp)) break
      predicted <- 0.5 * sum(cur$approx_grad * stp)
      if(!is.finite(predicted) || predicted <= polish_tol) break
      moved <- FALSE
      for(alpha in c(1, 0.5, 0.25, 0.1, 0.05)){
        cand <- fit$par + alpha * stp
        ## Covariances must be retained here. With person covariates, an
        ## estimated ability mean or an estimated correlation, the gradient
        ## path needs them and throws without them -- which a tryCatch turns
        ## into a rejected step, so the polish silently does nothing on exactly
        ## the fits that need it most.
        v <- tryCatch(bigIRT_laplace_direct_objective(
          par = cand, state = state, sdat = sdat, prior_precision = prior_precision,
          theta_init = theta_warm, jitter = jitter, tol = person_tol,
          keep_covariance = TRUE, context = context)$value,
          error = function(e) -Inf)
        if(is.finite(v) && v > cur$value){
          fit$par <- cand
          polish$gain <- polish$gain + (v - cur$value)
          polish$steps <- polish$steps + 1L
          moved <- TRUE
          break
        }
      }
      if(!moved) break
    }
  }
  fit$polish <- polish

  ## Quadrature refinement. From the Laplace optimum, continue on the
  ## objective with the person integrals done by adaptive Gauss-Hermite
  ## quadrature (`refine`, rules from bigIRT_agq_rules), with the same
  ## optimiser and metric. Laplace is biased for free asymptotes and can hold
  ## a fit at an optimum the exact marginal does not have (src/agq_backend.cpp);
  ## starting where it ended makes the refinement a short run rather than a
  ## second fit. The convergence verdict is then the refinement's.
  if(!is.null(refine)){
    lap_value <- get_eval(fit$par)$value
    lap_par <- fit$par
    lap_evals <- eval_count
    if(inherits(refine, "bigIRT_agq_rules")) context$agq <- refine else context$laplace2 <- TRUE
    ## Nodes are held where the Laplace posterior puts them and re-centred at
    ## each metric refresh, not at every evaluation. With the nodes fixed the
    ## objective is an ordinary smooth function whose Fisher-identity gradient
    ## is exact, and an evaluation skips the person mode solve -- about half
    ## its cost. Re-centring moves the value by the change in quadrature
    ## error, so the search takes the re-evaluated point from there, and the
    ## fit only stops once a re-centred point shows no gain left.
    fixed_nodes <- inherits(refine, "bigIRT_agq_rules") && isTRUE(getOption("bigIRT.agq_fixed_nodes", TRUE))
    held_nodes <- fixed_nodes
    recentre <- function(par){
      context$agq_centre <<- NULL
      cache_par <<- NULL; cache_eval <<- NULL
      e <- fg_f(par)
      if(fixed_nodes) context$agq_centre <<- e$ev$posterior
      e
    }
    ## Held nodes can be exploited. Away from where they were placed they are
    ## a fixed discrete ability distribution, and an item made a step between
    ## two of them fits that distribution better the steeper it is, without
    ## bound. Each such stretch of steps gains on the held nodes and loses on
    ## the objective, so a run of them walks the item out along its own ray,
    ## every re-centring a little lower than the last. A three-dimensional
    ## 2PL with priors = FALSE took an item from a discrimination of 58 to
    ## 6197 that way and ended 46 nats below where the refinement started.
    ## So each re-centred point is judged against `best`, the best re-centred
    ## point so far, and a stretch that ends lower by more than `slack` is
    ## undone: the fit goes back to `best` and on with nodes that move, where
    ## every evaluation is the objective itself and cannot be exploited this
    ## way. Re-centring also moves the value by the change in quadrature
    ## error, so near an optimum a stretch can end a little lower without
    ## anything being exploited: on regularised 2PL to 4PL fits in three
    ## dimensions by up to 6e-6 nats per person (in five, under 1e-4 nats in
    ## all at 3,000 persons), where the exploited stretches of seven
    ## unregularised benchmark fits lost from 2e-3 per person up.
    ## `slack` is 1e-4 per person, between the two; below it the fit goes on,
    ## `best` unchanged, and returns `best` if it ends lower (refine_from), so
    ## the end is never more than `tol` below the best point seen.
    best <- NULL; held_lost <- NA_real_
    slack <- max(tol, 1e-4 * as.numeric(sdat$Nsubs))
    judge <- function(ec){
      if(is.finite(ec$f) && ec$f <= best$f + slack){
        if(ec$f < best$f) best <<- ec
        return(TRUE)
      }
      held_lost <<- if(is.finite(ec$f)) ec$f - best$f else Inf
      FALSE
    }
    ## An item that has run off (R/certify.R) is stuck rather than at an
    ## optimum wherever pulling it back along its own ray gains: the gradient
    ## along a step item's ray vanishes, so no optimiser walks one back, held
    ## nodes or not. The unregularised Laplace 2PL hands the refinement such
    ## items (a discrimination of 58 where the exact marginal's maximum was
    ## 3.3, on the case above), and with moving nodes the refinement stopped
    ## at 52. So the refinement starts, and when it ends starts again, from
    ## the run-off items pulled back wherever that gains on its own objective.
    ## With `to` NULL each item goes to the best of the certificate's probe
    ## points short of the screen itself (half, a quarter and an eighth of
    ## it: an item put exactly on the screen is no longer screened, so a
    ## refinement that could not move it would leave it there unnamed, as two
    ## of 80 unregularised 3D fits did), in order of gain, one at a time while
    ## the value rises; with
    ## `to` a fraction of the screen, every run-off item goes there, gain or
    ## not (the retries below). NULL when nothing has run off and no
    ## evaluation was made; otherwise the re-centred evaluation at the point
    ## reached, the `items` moved (none when nothing gained) and the gain.
    pull_back <- function(e, to = NULL){
      ra <- tryCatch(bigIRT_runaway_items(e$ev$state, sdat, direct_layout), error = function(err) NULL)
      if(!length(ra$item)) return(NULL)
      x <- e$x; moved <- integer(0)
      if(is.null(to)){
        value_at <- function(p) tryCatch(-recentre(p)$f, error = function(err) -Inf)
        pb <- bigIRT_runaway_pullback(value_at, e$x, ra, sdat, direct_layout, -e$f,
          fractions = c(1/2, 1/4, 1/8))
        ord <- order(-pb$gain)
        ord <- ord[is.finite(pb$gain[ord]) & pb$gain[ord] > tol]
        v <- -e$f
        for(i in ord){
          p <- bigIRT_runaway_pull(x, ra[ra$item == pb$item[i], , drop = FALSE],
            pb$to[i] / pb$magnitude[i], sdat, direct_layout)
          vp <- value_at(p)
          if(is.finite(vp) && vp > v + tol){ x <- p; v <- vp; moved <- c(moved, pb$item[i]) }
        }
      } else for(it in unique(ra$item)){
        r <- ra[ra$item == it, , drop = FALSE]
        f <- to * bigIRT_runaway_slope() / bigIRT_runaway_magnitude(r, sdat)
        if(is.finite(f) && f > 0 && f < 1){
          x <- bigIRT_runaway_pull(x, r, f, sdat, direct_layout)
          moved <- c(moved, it)
        }
      }
      ec <- recentre(x)
      list(e = ec, items = moved, gain = e$f - ec$f)
    }
    pulled <- NULL
    note_pull <- function(pb, at, kept = TRUE)
      pulled <<- rbind(pulled, data.frame(at = at, items = paste(pb$items, collapse = ","),
        gain = pb$gain, kept = kept, stringsAsFactors = FALSE))
    refresh_r <- function(e){
      m <- solver(e)
      ec <- recentre(e$x)
      if(fixed_nodes && !judge(ec)) return(list(stop = TRUE))
      list(minv = m, e = ec)
    }
    rf_iter <- 0L; rounds <- 0L; restarts <- 0L
    ## The refinement from `x`: rounds of held nodes, each judged, then
    ## moving nodes if a stretch lost; and from an end with run-off items
    ## that gain when pulled back (at most three times), again from there.
    ## `start`, when given, is x re-centred already, the nodes held there if
    ## they are to be. Returns the end's re-centred evaluation and the last
    ## search.
    refine_from <- function(x, start = NULL){
      fixed_nodes <<- held_nodes
      if(is.null(start)) start <- recentre(x)
      best <<- start
      x_r <- start$x
      m_r <- solver(start)
      get_eval(x_r)
      nround <- 0L; nrestart <- 0L
      repeat {
        rounds <<- rounds + 1L; nround <- nround + 1L
        rf <- bigIRT_laplace_lbfgs(x_r, fg_f, minv = m_r, maxit = as.integer(niter),
          gain_tol = tol, refresh = refresh_r,
          refresh_every = as.integer(getOption("bigIRT.fisher_refresh", 10L)))
        rf_iter <<- rf_iter + rf$iter
        x_r <- rf$par
        more <- FALSE
        if(fixed_nodes){
          ## The round's end, re-centred and judged; a round a refresh
          ## stopped was judged there.
          e_c <- if(!identical(rf$terminate$what, "refresh_stop")) recentre(x_r)
          if(is.null(e_c) || !judge(e_c)){
            fixed_nodes <<- FALSE
            best <<- recentre(best$x)
            x_r <- best$x
            m_r <- solver(best)
            get_eval(x_r)
            next
          }
          ## Converged on these nodes: go on, re-centred, only if that left
          ## something to gain.
          more <- rf$iter > 0L && nround < 5L && rf$terminate$what %in% c("gain", "noise_floor")
        } else {
          e_c <- fg_f(x_r)
          if(e_c$f < best$f) best <<- e_c
        }
        if(!more){
          pb <- if(nrestart < 3L) pull_back(e_c)
          if(!is.null(pb) && length(pb$items) && pb$e$f < best$f - tol){
            nrestart <- nrestart + 1L; restarts <<- restarts + 1L
            note_pull(pb, "end")
            best <<- pb$e
            x_r <- best$x
            m_r <- solver(best)
            get_eval(x_r)
            next
          }
          break
        }
        m_r <- solver(e_c)
        get_eval(x_r)
      }
      ## A held-node stretch that lost less than `slack` can leave the end
      ## below the best point; the best point is then the result.
      if(best$f < e_c$f - tol){
        e_c <- best
        rf$par <- best$x
      }
      rf$f <- e_c$f
      list(e = e_c, rf = rf, metric = attr(m_r, "metric"))
    }
    e_r <- recentre(fit$par)
    ## pb$e is re-centred wherever the probes left it, and is the start
    ## whether or not anything moved.
    pb <- pull_back(e_r)
    if(!is.null(pb) && length(pb$items)) note_pull(pb, "start")
    start <- if(is.null(pb)) e_r else pb$e
    res <- refine_from(start$x, start)
    ## Pulling back only where it gains is not enough. A run-off item's
    ## neighbours adapt to it, so at the end of the case above pulling item
    ## 12 back alone lost 1.4 nats -- and yet the objective had an optimum 20
    ## nats higher with no item run off, which the refinement reached from
    ## every run-off item pulled to a quarter of the screen together, a pull
    ## that first cost 23. So while the best end so far has run-off items,
    ## they are all pulled to a quarter, an eighth and a half of the screen
    ## (5, 2.5 and 10 logits per SD by default) in turn and the refinement
    ## rerun; the best end is kept, and the retries stop at one that gained
    ## and left nothing run off. Every end is a refinement's own, so the
    ## result is never worse than not having tried.
    for(to in c(1/4, 1/8, 1/2)){
      pb <- pull_back(res$e, to = to)
      if(is.null(pb) || !length(pb$items)) break
      alt <- refine_from(pb$e$x)
      kept <- alt$e$f < res$e$f - tol
      note_pull(pb, sprintf("retry at %g", to * bigIRT_runaway_slope()), kept = kept)
      if(kept){
        res <- alt
        ra <- tryCatch(bigIRT_runaway_items(res$e$ev$state, sdat, direct_layout), error = function(err) NULL)
        if(!length(ra$item)) break
      }
    }
    rf <- res$rf
    m_r <- structure(list(), metric = res$metric)
    rf$iter <- rf_iter
    ## What is reported is the objective with the nodes centred at the end
    ## point itself, whatever round the loop stopped in: the final evaluation
    ## below re-centres.
    context$agq_centre <- NULL
    fit$refine <- list(approx = if(inherits(refine, "bigIRT_agq_rules")) "agq" else "laplace2",
      nodes = paste(refine$desc, collapse = ","),
      rule = list(nodes = refine$nodes, points = refine$points, inflate = refine$inflate,
        pairs = isTRUE(refine$pairs), nodes3 = refine$nodes3),
      iter = rf$iter, evals = eval_count - lap_evals, terminate = rf$terminate,
      rounds = rounds, fixed_nodes = held_nodes,
      ## NA, or what the stretch of held-node steps that ended them lost on
      ## the re-centred objective (Inf: it was not finite there).
      held_nodes_lost = held_lost,
      ## Run-off items pulled back, by index, with what the pull itself
      ## gained: at the start, at each end that restarted the refinement
      ## (`restarts`), and each retry, `kept` when its end was the better.
      ## NULL when none was.
      pulled_back = pulled, restarts = restarts,
      laplace_value = lap_value, agq_start = -e_r$f, agq_end = -rf$f,
      metric = attr(m_r, "metric"), laplace_iter = fit$iter,
      laplace_terminate = fit$terminate)
    fit$par <- rf$par
    fit$iter <- rf$iter
    fit$terminate <- rf$terminate
    ## Guard. A legitimate refinement moves the objective by a few nats (17
    ## on a 20,000-person 3PL); an approximation the optimiser has learned to
    ## exploit moves it without bound (a second-order Laplace refinement of a
    ## two-dimensional 3PL went to 1e13). Past half a nat per person, go back
    ## to the Laplace optimum rather than report the exploited point.
    ##
    ## A loss is another matter. Each stretch above is judged against the best
    ## re-centred point and only moves that gain are taken, so the end cannot
    ## be more than `tol` below the start, and the half-a-nat-per-person
    ## allowance a gain needs has no counterpart for a loss: a refinement
    ## that ended lower on its own objective did not follow it, and nothing
    ## it reached can be trusted. That once let a 46-nat loss through.
    gain_r <- -rf$f - (-e_r$f)
    if(!is.finite(gain_r) || gain_r < -tol || gain_r > max(50, 0.5 * as.numeric(sdat$Nsubs))){
      warning(sprintf(paste0(if(isTRUE(gain_r < 0))
          "The %s refinement ended %.3g nats lower on its own objective than it started, which a descent cannot do; " else
          "The %s refinement moved the objective by %.3g nats, which no approximation error of this size can explain; ",
        "the fit reverts to the Laplace optimum. ",
        if(identical(fit$refine$approx, "laplace2"))
          "Second-order Laplace is not reliable with free asymptotes; laplaceRefine = \"agq\" is." else
          "Refit with more quadrature points (agqNodes, agqPoints), or laplaceRefine = \"none\"."),
        fit$refine$approx, if(isTRUE(gain_r < 0)) -gain_r else gain_r), call. = FALSE)
      context$agq <- NULL; context$laplace2 <- FALSE; context$agq_centre <- NULL
      cache_par <- NULL; cache_eval <- NULL
      e_l <- fg_f(lap_par)
      rl <- bigIRT_laplace_lbfgs(lap_par, fg_f, minv = solver(e_l), maxit = as.integer(niter),
        gain_tol = tol, refresh = function(e){ m <- solver(e); get_eval(e$x); m },
        refresh_every = as.integer(getOption("bigIRT.fisher_refresh", 10L)))
      fit$refine$rejected_par <- rf$par   # where the refinement went, for diagnosis
      fit$par <- rl$par; fit$iter <- rl$iter; fit$terminate <- rl$terminate
      fit$refine$reverted <- TRUE
    }
  }
  ## The final evaluation is fresh, with the clipping counters reset, so they
  ## describe the fitted point rather than every trial point the line search
  ## visited: counted along the path, whether a fit "clipped a lot" depended
  ## on the optimiser that got there.
  bigIRT_laplace_clip_counts(reset = TRUE)
  cache_par <- NULL; cache_eval <- NULL
  final <- get_eval(fit$par)
  fit$clip_final <- bigIRT_laplace_clip_counts(reset = FALSE)
  fit$masked_grad_norm <- sqrt(sum(final$approx_grad^2))
  ## Keep the vector, not only its norm.  A single scaled norm cannot say which
  ## block failed to settle, and that is exactly the question asked whenever a
  ## fit stops short; it is one double per free item parameter.
  fit$grad <- final$approx_grad
  fit$layout <- direct_layout
  fit$target_evals <- eval_count
  fit$logLik <- final$value
  fit$timings <- final$timings
  ## The curvature certificate (R/certify.R), on the objective the fit ended
  ## on and at the point it ended at. `certify` is list(budget, tolerance), or
  ## NULL on the empirical-Bayes rounds, whose estimates are thrown away. After
  ## the fitted point's clip count is taken and outside `get_eval`, so neither
  ## that count nor the optimiser's warm start sees its evaluations.
  if(!is.null(certify))
    fit$certificate <- tryCatch(bigIRT_certify(final = final, par = fit$par,
      context = context, state = state, sdat = sdat, prior_precision = prior_precision,
      layout = direct_layout, jitter = jitter, route = certify$route %||% "fast",
      budget = certify$budget %||% Inf, tolerance = certify$tolerance,
      item_labels = certify$item_labels, scale_labels = certify$scale_labels),
      error = function(e) list(status = "unknown", certified = FALSE, converged = NA,
        reason = paste0("the certificate could not be computed: ", conditionMessage(e))))
  ## Items that have run off (R/certify.R), screened whether or not the
  ## certificate ran: one pass over the items, no evaluations.
  fit$runaway <- tryCatch(bigIRT_runaway_items(final$state, sdat, direct_layout),
    error = function(e) NULL)
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
    ## d loading / d raw: logistic(raw) under softplus, 1 for a signed loading.
    a_slope <- inv_logit(state$invspApars)
    signedA <- bigIRT_laplace_signed_A(sdat) == 1L
    if(length(signedA) == length(a_slope) && any(signedA)) a_slope[signedA] <- 1
    if(nrow(state$invspAbeta) == 1L){
      a_mult <- mean(a_slope, na.rm = TRUE)
      if(!is.finite(a_mult)) a_mult <- 0
      out$Abeta <- state$invspAbeta * a_mult
    } else {
      out$Abeta <- state$invspAbeta * matrix(
        a_slope,
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
bigIRT_laplace_person_row_terms_cpp_impl <- function(ids, y, eta, c_row, d_row,
  loadings, sigma, N, K){
  .Call(`_bigIRT_laplace_person_row_terms_cpp_impl`, as.integer(ids),
    as.numeric(y), as.numeric(eta), as.numeric(c_row), as.numeric(d_row),
    as.matrix(loadings), as.numeric(sigma), as.integer(N), as.integer(K))
}

## Per-person score and log-determinant-slope terms.
##
## This was a third of every objective evaluation on fits with person
## covariates -- the adjoint gradient path needs it, and in R it walked
## Nobs-length vectors a dozen times over. The arithmetic is elementwise plus
## one scatter by person, so it moved to C++ whole. `impl = "R"` keeps the
## original available, and the two are checked against each other in the tests.
bigIRT_laplace_person_row_terms <- function(sdat, posterior, row_effective,
  row_context, impl = c("cpp", "R")){
  impl <- match.arg(impl)
  if(identical(impl, "R"))
    return(bigIRT_laplace_person_row_terms_R(sdat, posterior, row_effective, row_context))
  if(is.null(posterior$covariance))
    stop("bigIRT_laplace_person_row_terms needs posterior covariances; the caller must set keep_covariance.")
  K <- as.integer(sdat$Nscales)
  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)
  bigIRT_laplace_person_row_terms_cpp_impl(
    ids = row_context$ids,
    y = sdat$score[row_context$rows],
    eta = row_effective$eta_row,
    c_row = row_effective$c_row,
    d_row = row_effective$d_row,
    loadings = lo,
    sigma = posterior$covariance,
    N = as.integer(sdat$Nsubs),
    K = K)
}

## Per-response pieces of the ability gradient and of the log-determinant
## slope. Both the person-level accumulation below and the ability-beta
## gradient need them, and a covariate that varies within person has to weight
## them BEFORE they are summed over responses, so they are built once here
## rather than derived twice.
bigIRT_laplace_row_gradient_pieces <- function(sdat, posterior, row_effective, row_context){
  K <- as.integer(sdat$Nscales)
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
  co <- (y - p) / r
  grad_eta <- co * s
  dq_deta <- qq * (1 - 2 * gg)
  ds_deta <- u * dq_deta
  dr_deta <- s * (1 - 2 * p)

  ## The observed curvature and its eta derivative, mirroring
  ## bigirt_row_terms() in src/laplace_backend.cpp. This used to be Fisher
  ## information, s^2/r, and its derivative; it has to track the C++ or the
  ## parity test in test-inference.R fails, which is how the mismatch after
  ## the curvature change was caught.
  d2q_deta <- dq_deta * (1 - 2 * gg) - 2 * qq^2
  d2s_deta <- u * d2q_deta
  d2r_deta <- ds_deta * (1 - 2 * p) - 2 * s^2
  e <- y - p
  Nn <- -s * r - e * dr_deta
  dco <- Nn / r^2
  dNn <- -ds_deta * r - e * d2r_deta
  d2co <- (dNn * r - 2 * Nn * dr_deta) / r^3
  w_obs <- -(dco * s + co * ds_deta)
  dw_deta <- -(d2co * s + 2 * dco * ds_deta + co * d2s_deta)

  ## aSa = a' Sigma a per response, assembled from the K^2 posterior blocks.
  Sig <- posterior$covariance
  aSa <- numeric(length(ids))
  for(k in seq_len(K)) for(l in seq_len(K))
    aSa <- aSa + lo[, k] * lo[, l] * Sig[k, l, ids]

  ## hess_w is the weight each response puts into the person precision,
  ## H_i = sum_j hess_w_ij lo_ij lo_ij' + Q. The within-person beta gradient
  ## needs it separately; the person-level accumulation does not. It is the
  ## observed curvature, matching the C++; Fisher information s^2/r is only
  ## used for the Newton step, which this path is not part of.
  list(grad_eta = grad_eta, slope_w = dw_deta * aSa, hess_w = w_obs,
       loadings = lo, ids = ids)
}

bigIRT_laplace_person_row_terms_R <- function(sdat, posterior, row_effective, row_context){
  if(is.null(posterior$covariance)) stop("bigIRT_laplace_person_row_terms needs posterior covariances; the caller must set keep_covariance.")
  K <- as.integer(sdat$Nscales)
  N <- as.integer(sdat$Nsubs)
  pieces <- bigIRT_laplace_row_gradient_pieces(sdat, posterior, row_effective, row_context)
  ids <- pieces$ids
  lo <- pieces$loadings
  grad_eta <- pieces$grad_eta

  ## rowsum(reorder = FALSE) returns groups in the order they are encountered,
  ## not in level order, so the result has to be scattered back by label rather
  ## than assumed aligned. Response data are usually ordered by person, which
  ## makes the two coincide here and hid the difference; they do not coincide
  ## when the grouping is by item.
  acc_raw <- rowsum(cbind(grad_eta * lo, pieces$slope_w * lo), group = ids)
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
## Which person predictors vary within a person? A covariate that is constant
## within person can be folded out of the sum over that person's responses; one
## that varies cannot, and the two need different gradients.
bigIRT_laplace_person_pred_within_varying <- function(row_context, tol = 1e-10){
  ## Computed once when the row context is built and carried on it: the
  ## predictors are data and cannot change during a fit, and rederiving this
  ## from every response on every gradient evaluation cost about a sixth of
  ## each one.
  if(!is.null(row_context$person_pred_varies)) return(row_context$person_pred_varies)
  bigIRT_laplace_person_pred_varies(row_context$person_pred, row_context$ids, tol)
}

bigIRT_laplace_person_pred_varies <- function(X, ids, tol = 1e-10){
  if(is.null(X) || !ncol(X)) return(logical(0))
  X <- as.matrix(X)
  ids <- as.integer(ids)
  n <- rowsum(rep(1, length(ids)), group = ids)
  m <- rowsum(X, group = ids) / as.numeric(n)
  idx <- match(ids, as.integer(rownames(m)))
  apply(abs(X - m[idx, , drop = FALSE]), 2L, max) > tol
}

bigIRT_laplace_ability_beta_contribution <- function(state, sdat, posterior, row_context, layout, prior_precision, person_terms, row_effective = NULL, impl = c("cpp", "R"),
                                                   fused_beta = NULL){
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

  fixedmask <- matrix(FALSE, N, K)
  fixedmask[ids, ] <- as.logical(row_context$fixed_ability)

  ## contrib_i = s_i - (1/2) Q_i Sigma_i g_i, built blockwise across persons.
  prior_is_array <- length(dim(prior_precision)) == 3L
  QS <- array(0, c(K, K, N))
  for(k in seq_len(K)) for(l in seq_len(K)) for(m in seq_len(K)){
    qkm <- if(prior_is_array) prior_precision[k, m, ] else prior_precision[k, m]
    QS[k, l, ] <- QS[k, l, ] + qkm * Sig[m, l, ]
  }

  reduce <- function(sx, gx){
    contrib <- sx
    for(k in seq_len(K)) for(l in seq_len(K))
      contrib[, k] <- contrib[, k] - 0.5 * QS[k, l, ] * gx[, l]
    contrib[fixedmask] <- 0
    contrib
  }

  varying <- bigIRT_laplace_person_pred_within_varying(row_context)

  ## Ability enters response j of person i as theta_i + x_ij' beta, so the
  ## derivative with respect to beta_p weights each RESPONSE by x_ijp. When x is
  ## constant within person that weight factors out of the sum over responses
  ## and the person-level totals can simply be multiplied by it. When it varies
  ## within person it cannot: summing first and weighting afterwards multiplies
  ## a person's whole score by one arbitrary response's covariate value, which
  ## is not the derivative of anything. Recovery of a known within-person effect
  ## was the symptom -- a true 0.50 came back as -0.31, sign and all -- while
  ## between-person effects, where the two agree, recovered correctly.
  out <- matrix(0, K, P)

  ## Person-constant covariates keep the cheap route: their weight factors out
  ## of the sum over a person's responses, so the person-level totals need only
  ## be multiplied by it. Mixed sets are common -- a within-person time term
  ## beside between-person ones -- and only the varying columns should pay for
  ## the exact path.
  const_idx <- which(!varying)
  if(length(const_idx)){
    xmat <- matrix(0, N, length(const_idx))
    xmat[ids, ] <- as.matrix(row_context$person_pred)[, const_idx, drop = FALSE]
    out[, const_idx] <- t(reduce(sacc, gacc)) %*% xmat
  }

  vary_idx <- which(varying)
  if(length(vary_idx)){
    if(is.null(row_effective))
      stop("laplace_direct Abilitybeta gradients need the response rows when a person predictor varies within person.")
    ## The reduce() above is not the general gradient. It is the general
    ## gradient AFTER a cancellation that needs x outside the sum over a
    ## person's responses. With b_i the person's base ability, the mode shifts
    ## under beta as
    ##
    ##   db_i/dbeta_kp = -H_i^-1 sum_j hess_w_ij lo_ijk x_ijp lo_ij
    ##
    ## and for x constant within person that is -(I - H_i^-1 Q) x_ip, which
    ## makes the explicit and mode-driven halves of dlog|H_i|/dbeta cancel down
    ## to the single -1/2 Q Sigma g term. When x varies within person they do
    ## not cancel, and weighting the collapsed form by x --- which is what an
    ## earlier version of this function did --- weights an expression whose
    ## surviving terms have already been discarded. Central differences on the
    ## objective put that at a relative error of order one, sign included, while
    ## the between-person columns of the same fit matched to 7e-06.
    ##
    ## Written out and kept row-level, the three terms are
    ##
    ##   T1 = sum_j g_ij lo_ijk x_ijp
    ##   T2 = -1/2 sum_j slope_w_ij lo_ijk x_ijp
    ##   T3 = +1/2 sum_j hess_w_ij q_ij lo_ijk x_ijp,  q_ij = c_i' Sigma_i lo_ij
    ##
    ## with c_i = sum_j slope_w_ij lo_ij, which is exactly the per-person slope
    ## the caller has already accumulated. Every term is then a sum over
    ## responses of something times x_ijp, so the whole gradient is one
    ## crossprod and needs no per-covariate aggregation: it does not grow with
    ## the number of covariates, and there is no scattered write.
    out[, vary_idx] <- if(!is.null(fused_beta)) fused_beta else
      bigIRT_laplace_ability_beta_rows(
        sdat = sdat, posterior = posterior, row_effective = row_effective,
        row_context = row_context, gacc = gacc, vary_idx = vary_idx,
        impl = impl)
  }

  dimnames(out) <- NULL
  out
}

## One pass over the responses for the within-person ability-beta gradient.
## The C++ path repeats about twenty flops per row rather than materialising
## three vectors the length of the response set; the R path is kept as the
## reference the two are checked against.
bigIRT_laplace_ability_beta_rows <- function(sdat, posterior, row_effective,
                                             row_context, gacc, vary_idx,
                                             impl = c("cpp", "R")){
  impl <- match.arg(impl)
  K <- as.integer(sdat$Nscales)
  N <- as.integer(sdat$Nsubs)
  ids <- row_context$ids_int
  if(is.null(ids)) ids <- as.integer(row_context$ids)
  X <- as.matrix(row_context$person_pred)
  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)

  if(identical(impl, "cpp")){
    fixed <- row_context$fixed_ability_int
    if(is.null(fixed))
      fixed <- matrix(as.integer(row_context$fixed_ability), nrow = length(ids), ncol = K)
    y <- row_context$score
    if(is.null(y)) y <- as.numeric(sdat$score[row_context$rows])
    return(bigIRT_laplace_ability_beta_rows_cpp_impl(
      ids = ids, y = y,
      eta = row_effective$eta_row, c_row = row_effective$c_row,
      d_row = row_effective$d_row,
      loadings = lo, sigma = posterior$covariance,
      cg = gacc, x = X, vary_idx = as.integer(vary_idx - 1L),
      fixed = fixed, N = N, K = K))
  }

  pieces <- bigIRT_laplace_row_gradient_pieces(sdat, posterior, row_effective, row_context)
  Sig <- posterior$covariance
  q <- numeric(length(ids))
  for(m in seq_len(K)) for(n in seq_len(K))
    q <- q + gacc[ids, m] * Sig[m, n, ids] * lo[, n]
  W <- (pieces$grad_eta - 0.5 * pieces$slope_w + 0.5 * pieces$hess_w * q) * lo
  W[matrix(as.logical(row_context$fixed_ability), nrow = length(ids), ncol = K)] <- 0
  crossprod(W, X[, vary_idx, drop = FALSE])
}

bigIRT_laplace_ability_beta_rows_cpp_impl <- function(ids, y, eta, c_row, d_row,
    loadings, sigma, cg, x, vary_idx, fixed, N, K){
  .Call(`_bigIRT_laplace_ability_beta_rows_cpp_impl`, as.integer(ids),
    as.numeric(y), as.numeric(eta), as.numeric(c_row), as.numeric(d_row),
    loadings, as.numeric(sigma), cg, x, as.integer(vary_idx), fixed,
    as.integer(N), as.integer(K))
}

## Person row terms and the within-person ability-beta gradient from a single
## sweep over the responses. Returns NULL when there is nothing within-varying
## to fuse, or when the D accumulator would be too large, and the caller then
## takes the two-sweep route.
bigIRT_laplace_person_beta_fused <- function(sdat, posterior, row_effective,
                                             row_context, vary_idx,
                                             max_doubles = 2e7,
                                             grain_size = 64L){
  if(!length(vary_idx)) return(NULL)
  K <- as.integer(sdat$Nscales)
  N <- as.integer(sdat$Nsubs)
  if(as.numeric(N) * K * K * length(vary_idx) > max_doubles) return(NULL)
  ids <- row_context$ids_int
  if(is.null(ids)) ids <- as.integer(row_context$ids)
  y <- row_context$score
  if(is.null(y)) y <- as.numeric(sdat$score[row_context$rows])
  fixed <- row_context$fixed_ability_int
  if(is.null(fixed))
    fixed <- matrix(as.integer(row_context$fixed_ability), nrow = length(ids), ncol = K)
  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)
  X <- row_context$person_pred
  if(!is.matrix(X)) X <- as.matrix(X)
  vi <- as.integer(vary_idx - 1L)

  ## Grouped by person: no accumulator that scales with N * K^2 * P, and the
  ## sweep runs in parallel over disjoint persons.
  if(!is.null(row_context$person_row_order) && !is.null(row_context$person_row_start))
    return(.Call(`_bigIRT_laplace_person_beta_grouped_cpp_impl`,
      row_context$person_row_order, row_context$person_row_start, y,
      row_effective$eta_row, row_effective$c_row, row_effective$d_row, lo,
      posterior$covariance, X, vi, fixed, N, K, grain_size))

  res <- .Call(`_bigIRT_laplace_person_beta_fused_cpp_impl`, ids, y,
    row_effective$eta_row, row_effective$c_row, row_effective$d_row, lo,
    posterior$covariance, X, vi, fixed, N, K, as.numeric(max_doubles))
  if(!isTRUE(res$fused)) return(NULL)
  res
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
    ## The free base abilities, persons within dimension (column major).
    Abilitypars = as.numeric(state$AbilityBase[sdat$fixedAbilityLogical == 0L]),
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

## Empirical Bayes hyperparameter update, for either estimator.
##
## This replaced, rather than ported, the rule the Stan joint-MAP path used
## (withdrawn in 0.2.0), whose logic had three
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
  ## Fisher information, and deliberately so. This is information about an
  ## ITEM parameter, feeding Var(theta_j|data) = 1/(I_j + 1/tau^2) in an
  ## empirical-Bayes hyperparameter update -- a different object from the
  ## person-level Laplace curvature, which is the observed Hessian. Its
  ## guaranteed positivity is what keeps that reciprocal well behaved, and
  ## the shrinkage it produces is a prior the model states rather than an
  ## artefact of an approximation. Do not "correct" it to match the Laplace
  ## curvature.
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

  ## The refs are free-parameter indices, so each information vector has one
  ## entry per FREE parameter, aligned with the estimates. It used to be sized
  ## at Nitems: with f parameters of a block fixed (anchors through itemDat,
  ## fixed loadings) its last f entries stayed zero, entered
  ## mean(1 / pmax(info, 1e-8)) at 1e8 each, and the prior SD floored at
  ## min_sd whatever the data said. With nothing fixed the two lengths agree,
  ## which is why it never showed.
  if(length(state$Bpars) >= 3L)
    set_block("BSDx", "BMeandat", state$Bpars,
      sum_by(w, context$B_ref, length(state$Bpars)), sdat$fixedBMean)

  ## Free-parameter length is the reliable signal: a 2PL leaves logitCpars
  ## empty rather than recording a model order in standata.
  if(length(state$logitCpars) >= 3L){
    ic <- sum_by(((1 - gg) * bigIRT_dc_dx(cr))^2 / r, context$C_ref, length(state$logitCpars))
    set_block("logitCSD", "logitCMeandat", state$logitCpars, ic, sdat$fixedCMean)
  }
  if(length(state$logitDpars) >= 3L){
    id <- sum_by((gg * bigIRT_dd_dx(dr))^2 / r, context$D_ref, length(state$logitDpars))
    set_block("logitDSD", "logitDMeandat", state$logitDpars, id, sdat$fixedDMean)
  }
  ## Discrimination enters through a*theta, so its information carries theta^2
  ## and the softplus derivative. Only the unidimensional case is handled; with
  ## several scales the mapping from rows to per-scale loadings is not a single
  ## index and the prior is left alone rather than updated approximately.
  ## Signed loadings (signedLoadings) are left out: their prior is centred at
  ## zero, so they are not draws from the population this block describes.
  signedA <- bigIRT_laplace_signed_A(sdat) == 1L
  if(as.integer(sdat$Nscales) == 1L && sum(!signedA) >= 3L){
    ## The row ability, covariate term included: that is what A multiplies.
    th <- as.numeric(bigIRT_row_ability(state, sdat, row_context, thetaBase, row_effective))
    dsp <- inv_logit(as.numeric(state$invspApars))
    if(any(signedA)) dsp[signedA] <- 1
    aref <- as.integer(context$A_ref[, 1])
    dspr <- rep(1, length(th)); keep <- aref > 0L
    dspr[keep] <- dsp[aref[keep]]
    ia <- sum_by(w * (th * dspr)^2, aref, length(state$invspApars))
    if(any(signedA))
      set_block("invspASD", "invspAMeandat", state$invspApars[!signedA], ia[which(!signedA)], sdat$fixedAMean)
    else
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
  ## Signed loadings (signedLoadings): slope one, and left out of the
  ## discrimination update, since their prior is centred at zero rather than
  ## drawn from the population this block describes.
  signedA <- bigIRT_laplace_signed_A(sdat) == 1L
  if(any(signedA)) sig[signedA] <- 1
  aref <- as.integer(context$A_ref[, 1])
  sigr <- rep(1, length(ids)); keep <- aref > 0L; sigr[keep] <- sig[aref[keep]]
  th1 <- bigIRT_row_ability(state, sdat, row_context, thetaBase, row_effective)[, 1]
  V <- list(B = -s, A = s * th1 * sigr,
            C = (1 - gg) * bigIRT_dc_dx(cr), D = gg * bigIRT_dd_dx(dr))
  est <- list(B = as.numeric(state$Bpars),
              A = if(any(signedA)) as.numeric(state$invspApars)[!signedA] else as.numeric(state$invspApars),
              C = as.numeric(state$logitCpars), D = as.numeric(state$logitDpars))
  ## Only blocks with enough free parameters to have a dispersion are updated.
  ## Discrimination is skipped beyond one dimension: its information carries a
  ## per-scale theta and the row-to-loading map is not a single index there.
  active <- names(est)[vapply(est, function(z) length(z) >= 3L, logical(1))]
  if(K > 1L) active <- setdiff(active, "A")
  if(!length(active)) return(NULL)
  V <- V[active]; est <- est[active]; P <- length(active)
  ## One block per ITEM, over that item's FREE parameters. This used to group
  ## by B_ref, which is the free-difficulty index: the same thing while every
  ## difficulty is free, but with anchors the rows of an item with a fixed
  ## difficulty fell into group 0 and were dropped from every block, the
  ## unused trailing slices were prior-only and each added log tau to the
  ## marginal, and a fixed loading or asymptote was integrated over as if it
  ## were free. A fixed parameter is not integrated at all, so its V is zeroed
  ## on that item's rows and the objective puts a unit on its diagonal, which
  ## leaves the determinant that of the item's free block.
  item <- as.integer(row_context$items)
  free_row <- list(B = context$B_ref > 0L, A = aref > 0L,
                   C = context$C_ref > 0L, D = context$D_ref > 0L)[active]
  for(b in active){
    off <- !free_row[[b]]
    if(any(off)) V[[b]][off] <- 0
  }
  ## The A block is active only at K = 1, where fixedAlog is per item.
  fixed_flag <- list(B = sdat$fixedB, A = sdat$fixedAlog,
                     C = sdat$fixedClogit, D = sdat$fixedDlogit)[active]
  fixed_item <- matrix(vapply(fixed_flag, function(z) as.integer(z) == 1L, logical(ni)),
                       nrow = ni, ncol = P)
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
  ## The centre each block's spread is measured from: where estMeans leaves a
  ## centre fixed, that fixed value, which is the centre the fit's prior uses;
  ## otherwise NA, and the objective profiles it out as the estimates' mean,
  ## which is what the update then sets it to. It used the estimates' mean in
  ## both cases, so with a fixed centre tau was the spread about a point the
  ## prior was not centred on: joint-MAP loadings averaging 2-3 on the raw
  ## scale, against a fixed centre of .542, got a tight tau about their own
  ## mean that the fit then applied about .542.
  fixed_mean <- c(B = "fixedBMean", A = "fixedAMean", C = "fixedCMean", D = "fixedDMean")
  mean_names <- c(B = "BMeandat", A = "invspAMeandat", C = "logitCMeandat", D = "logitDMeandat")
  centre <- vapply(active, function(b)
    if(identical(as.integer(sdat[[fixed_mean[[b]]]])[1], 1L)) as.numeric(sdat[[mean_names[[b]]]])[1]
    else NA_real_, numeric(1))
  out <- list(Sbase = Sbase, est = est, active = active, P = P, ni = ni, centre = centre,
              fixed_item = if(any(fixed_item)) fixed_item else NULL)
  ## Items whose loading is signed are still integrated over, with the A
  ## prior precision held where it is, so that tau_A is fitted to the
  ## softplus loadings alone.
  if(any(signedA) && "A" %in% active){
    srow <- rep(0, length(ids)); srow[keep] <- as.numeric(signedA[aref[keep]])
    out$signed_item <- scat(srow) > 0
    out$tauA_held <- max(as.numeric(sdat$invspASD)[1], 1e-8)
  }
  out
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
    e <- pieces$est[[b]]
    mu <- if(length(pieces$centre) >= b && is.finite(pieces$centre[b])) pieces$centre[b] else mean(e)
    pen <- pen - sum((e - mu)^2) / (2 * tau[b]^2) - length(e) * log(tau[b])
  }
  add <- diag(1 / tau^2, P)
  ## An item with a signed loading keeps its A precision at the held value.
  add_signed <- NULL
  if(!is.null(pieces$signed_item)){
    add_signed <- add
    iA <- match("A", pieces$active)
    add_signed[iA, iA] <- 1 / pieces$tauA_held^2
  }
  ld <- 0
  for(j in seq_len(pieces$ni)){
    Pj <- if(!is.null(add_signed) && pieces$signed_item[j]) add_signed else add
    ## A parameter fixed on this item is not integrated over: unit diagonal,
    ## and its row and column of Sbase are zero.
    if(!is.null(pieces$fixed_item) && any(pieces$fixed_item[j, ]))
      diag(Pj)[pieces$fixed_item[j, ]] <- 1
    Sj <- pieces$Sbase[, , j] + Pj
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

## Diagonals of a K x K x N covariance array. Taking them with an lapply over
## persons is a loop over the whole sample for a strided read.
bigIRT_posterior_sd_vector <- function(cov_arr){
  if(is.null(cov_arr)) return(numeric(0))
  K <- dim(cov_arr)[1]; N <- dim(cov_arr)[3]
  sel <- rep((seq_len(K) - 1L) * (K + 1L) + 1L, times = N) +
    rep((seq_len(N) - 1L) * K * K, each = K)
  sqrt(pmax(as.numeric(cov_arr)[sel], 0))
}

## data.table or data.frame of predictors to a plain numeric matrix, without
## unlisting several million values into an intermediate vector first.
bigIRT_as_numeric_matrix <- function(x){
  m <- if(is.matrix(x)) x else as.matrix(x)
  storage.mode(m) <- "double"
  dimnames(m) <- NULL
  m
}
