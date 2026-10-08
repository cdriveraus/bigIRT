## Post-fit inference for Laplace fits: item information and standard errors,
## block-resolved convergence, information criteria, held-out scoring, and
## PSIS-LOO.
##
## The pieces here all come off one object. The Laplace objective's curvature in
## the item block is the observed information, and the empirical-Bayes M-step
## was already building it per item and discarding it. Exposing it gives item
## standard errors, an effective parameter count for penalised fits, and a
## scale-free convergence measure, none of which the package could report
## before.

## Rebuild the direct-Laplace evaluation context from a fitted object and
## evaluate once at the fitted parameters. Inputs: a fit carrying internals.
## Returns the evaluation, context, layout, standata and state. Mutates nothing.
bigIRT_direct_eval_from_fit <- function(fit, keep_covariance = TRUE, cores = 1L, jitter = 1e-6){
  if(is.null(fit$internals) || is.null(fit$internals$sdat))
    stop("This needs the prepared data. Refit with `keepInternals = TRUE`.")
  sdat <- fit$internals$sdat
  state <- fit$internals$state
  ecorr <- isTRUE(fit$laplaceStatus$estimated_corr)
  layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = ecorr)
  ilay <- layout[setdiff(names(layout), c("corr", "ability_mean", "ability_beta"))]
  ctx <- bigIRT_laplace_item_context(sdat, layout = ilay)
  ctx$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  ctx$direct_layout <- layout
  ctx$estimateAbilityCorr <- ecorr
  ctx$corr_paramization <- if(!is.null(state$laplaceCorrParam)) state$laplaceCorrParam else "normalized_chol"
  state$laplaceCorrParam <- ctx$corr_paramization
  ## A fit refined by quadrature is at the optimum of the quadrature objective,
  ## not of Laplace; anything differenced around it must use the same one.
  if(identical(fit$laplaceStatus$marginal, "laplace2")) ctx$laplace2 <- TRUE
  if(identical(fit$laplaceStatus$marginal, "agq"))
    ctx$agq <- bigIRT_agq_rules(sdat$Nscales, nodes = fit$laplaceStatus$agq_rule$nodes,
      points = fit$laplaceStatus$agq_rule$points, inflate = fit$laplaceStatus$agq_rule$inflate,
      pairs = isTRUE(fit$laplaceStatus$agq_rule$pairs),
      nodes3 = fit$laplaceStatus$agq_rule$nodes3 %||% 0L)
  par <- bigIRT_laplace_pack_direct_state(state, sdat, layout = layout)
  ev <- bigIRT_laplace_direct_objective(
    par = par, state = state, sdat = sdat,
    prior_precision = fit$internals$priorPrecision,
    theta_init = fit$internals$thetaBase, jitter = jitter,
    keep_covariance = keep_covariance, context = ctx)
  list(eval = ev, context = ctx, layout = layout, sdat = sdat, state = ev$state, par = par)
}

## Per-item observed information blocks for the free item parameters.
##
## This is the same construction the empirical-Bayes M-step uses, with two
## restrictions lifted. That code skips any block with fewer than three free
## parameters, because a hyperparameter needs a dispersion to estimate; a
## standard error does not, so the restriction does not apply here. It also
## skips discrimination beyond one dimension. Lifting that one needs the
## dimension-aware derivative written out below, because the obvious reading of
## `A_ref` is its first column, which silently zeroes every item that loads on a
## later dimension and sends its standard error to the prior.
##
## Returns a P x P x Nitems array of likelihood information, the matching prior
## precision, and the block labels. Mutates nothing.
## Chain-rule factors of the asymptote links, written in terms of the link's
## value: c = plogis(x)/2 gives dc/dx = c(1 - 2c); d = plogis(x)/2 + 1/2 gives
## dd/dx = (2d - 1)(1 - d). Using the plain-logistic c(1 - c) and d(1 - d)
## instead overstates their information by (1-c)^2/(1-2c)^2 and d^2/(2d-1)^2.
bigIRT_dc_dx <- function(c) c * (1 - 2 * c)
bigIRT_dd_dx <- function(d) (2 * d - 1) * (1 - d)

## Per-row ability, base ability plus the person-covariate term, as the
## likelihood uses it. Taken from `row_effective` when it carries it (the R
## builder does, the kernel's does not). Returns an Nobs x K matrix.
bigIRT_row_ability <- function(state, sdat, row_context, thetaBase, row_effective = NULL){
  K <- as.integer(sdat$Nscales)
  if(!is.null(row_effective$row_ability))
    return(matrix(as.numeric(row_effective$row_ability), ncol = K))
  th <- matrix(as.numeric(thetaBase), ncol = K)
  rab <- th[as.integer(row_context$ids), , drop = FALSE]
  if(isTRUE(sdat$NpersonPreds > 0) && length(state$Abilitybeta))
    rab <- rab + row_context$person_pred %*% t(state$Abilitybeta)
  fx <- row_context$fixed_ability
  if(!is.null(fx) && any(fx)) rab[fx] <- row_context$fixed_ability_value[fx]
  rab
}

## Which free parameter each item holds in each per-item block.
##
## A per-item block describes one parameter type with one parameter per item.
## Every construction built on these blocks used to require that type free on
## EVERY item and paired item j with free index j -- right only then, because
## free parameters are stored in item order and an anchored item has none. So
## one difficulty anchored through itemDat removed the B block everywhere, and
## every free difficulty lost its standard error. The pairing is read here
## from the fixed flags and the free-index maps instead (cf. the grouping in
## bigIRT_laplace_hyper_pieces).
##
## Returns a P x Nitems integer matrix, one row per block in B, A, C, D order:
## item j's position in that block's free vector (state$Bpars, ...; its
## optimiser coordinate is layout[[b]][pos]), NA where item j has it fixed. A
## block is present when any item has it free -- discrimination only while no
## item has more than one free loading, since free cross-loadings are not one
## parameter per item. `active` picks rows instead, NA throughout for a block
## this rule does not describe.
bigIRT_item_free_pos <- function(sdat, active = NULL){
  ni <- as.integer(sdat$Nitems); K <- as.integer(sdat$Nscales)
  pos <- function(fixed, ref){
    p <- as.integer(ref); p[as.integer(fixed) == 1L] <- NA_integer_; p
  }
  ## fixedAlog and freeAref run item-major over the (item, scale) cells.
  pA <- matrix(pos(sdat$fixedAlog, sdat$freeAref), ni, K, byrow = TRUE)
  nA <- rowSums(!is.na(pA))
  A <- rep(NA_integer_, ni)
  if(all(nA <= 1L)){
    pA[is.na(pA)] <- 0L
    A <- as.integer(rowSums(pA)); A[nA == 0L] <- NA_integer_
  }
  fp <- rbind(B = pos(sdat$fixedB, sdat$freeBref), A = A,
              C = pos(sdat$fixedClogit, sdat$freeCref),
              D = pos(sdat$fixedDlogit, sdat$freeDref))
  if(is.null(active)) active <- rownames(fp)[rowSums(!is.na(fp)) > 0L]
  out <- fp[match(active, rownames(fp)), , drop = FALSE]
  rownames(out) <- active
  out
}

## Optimiser coordinates of item j's parameters in `active` blocks, NA where
## the item has one fixed. `fp` is bigIRT_item_free_pos() over `active`.
bigIRT_item_coords <- function(fp, layout, j){
  vapply(rownames(fp), function(b) layout[[b]][fp[b, j]], integer(1))
}

bigIRT_item_info_blocks <- function(state, sdat, context, row_effective, posterior, thetaBase){
  if(is.null(posterior$covariance))
    stop("Item information needs the person posterior covariances.")
  K <- as.integer(sdat$Nscales); ni <- as.integer(sdat$Nitems)
  ids <- as.integer(context$row_context$ids)
  eta <- as.numeric(row_effective$eta_row)
  cr <- as.numeric(row_effective$c_row); dr <- as.numeric(row_effective$d_row)
  gg <- inv_logit(eta); qq <- gg * (1 - gg); u <- dr - cr
  p <- pmin(pmax(cr + u * gg, 1e-12), 1 - 1e-12)
  s <- u * qq
  r <- pmax(p * (1 - p), 1e-12)

  lo <- row_effective$loadings
  if(is.null(dim(lo))) lo <- matrix(lo, ncol = K)
  Sig <- posterior$covariance
  ## One pass rather than K^2 passes over the response set.
  aSa <- .Call(`_bigIRT_row_aSa_cpp_impl`, ids, lo, as.numeric(Sig),
               as.integer(sdat$Nsubs), K)
  ## The Laplace correction: information in an item parameter net of what the
  ## person's own uncertainty already absorbs. Without it these are conditional
  ## (JML-like) standard errors and are too small.
  wcorr <- pmax(1 / r - (s^2 * aSa) / r^2, 0)

  th <- if(K == 1L) matrix(as.numeric(thetaBase), ncol = 1L) else as.matrix(thetaBase)
  ## Discrimination multiplies the ROW ability, which carries the person
  ## covariate term x_row'beta on top of the base ability. The kernel's
  ## row_effective does not return it, so rebuild it when absent.
  rab <- bigIRT_row_ability(state, sdat, context$row_context, th, row_effective)
  invspA <- as.numeric(state$invspApars)

  ## Discrimination enters through the loading, and `A_ref` carries one
  ## parameter index per (row, latent dimension). Reading only its first column
  ## -- which is all the empirical-Bayes path ever needed, because it dropped
  ## this block entirely beyond one dimension -- leaves every item loading on a
  ## later dimension with no likelihood information at all, so its standard
  ## error collapses to the prior. Sum over the dimensions instead:
  ##   d eta_r / d invspA_q = sigmoid(invspA_q) * sum_k [A_ref[r,k]==q] *
  ##                          (loading_rk / A_q) * theta_ik,
  ## where the loading-to-A ratio carries any fixed pattern multiplying A_q, and
  ## the whole thing collapses to sigmoid * theta in the one-dimensional case.
  A_ref <- context$A_ref
  if(is.null(dim(A_ref))) A_ref <- matrix(as.integer(A_ref), ncol = 1L)
  qrow <- integer(length(ids)); contrib <- numeric(length(ids))
  if(length(invspA)){
    sig <- inv_logit(invspA)
    Aval <- pmax(afunc(invspA), 1e-12)
    ## A signed loading (signedLoadings) is invspA_q itself, plus any covariate
    ## term: slope one and no ratio to carry, so its derivative is theta alone.
    signedA <- bigIRT_laplace_signed_A(sdat) == 1L
    if(any(signedA)) sig[signedA] <- 1
    for(k in seq_len(ncol(A_ref))){
      hit <- A_ref[, k] > 0L & A_ref[, k] <= length(invspA)
      if(!any(hit)) next
      q <- A_ref[hit, k]
      qrow[hit] <- q
      ratio <- lo[hit, k] / Aval[q]
      if(any(signedA)) ratio[signedA[q]] <- 1
      contrib[hit] <- contrib[hit] + ratio * rab[hit, k]
    }
  }
  V_A <- numeric(length(ids))
  ok <- qrow > 0L
  if(any(ok)) V_A[ok] <- s[ok] * sig[qrow[ok]] * contrib[ok]

  ## Every entry is dp/dparameter, so information is V V' / (p(1-p)). The
  ## asymptotes are c = plogis(x)/2 and d = plogis(x)/2 + 1/2 (cfunc, dfunc),
  ## so their chain-rule factors are those of bigIRT_dc_dx and bigIRT_dd_dx,
  ## as in the kernel's gradient -- not the plain-logistic c(1-c) and d(1-d).
  V <- list(B = -s, A = V_A,
            C = (1 - gg) * bigIRT_dc_dx(cr), D = gg * bigIRT_dd_dx(dr))
  est <- list(B = as.numeric(state$Bpars), A = invspA,
              C = as.numeric(state$logitCpars), D = as.numeric(state$logitDpars))
  ## A block for each parameter type some item has free, one parameter per
  ## item (bigIRT_item_free_pos); free multidimensional loadings are a
  ## different parameterisation that this construction does not describe.
  free_pos <- bigIRT_item_free_pos(sdat)
  active <- rownames(free_pos)
  if(!length(active)) return(NULL)
  V <- V[active]; est <- est[active]; P <- length(active)

  ## All P(P+1)/2 pairs gathered by item in one pass, rather than a rowsum over
  ## every response for each pair. Gathered by ITEM: this used to read B_ref,
  ## the free-difficulty index, which is the item only while every difficulty
  ## is free. With anchored difficulties and free loadings the A block is still
  ## active, and the anchored items' rows carried index 0, which stopped the
  ## accumulator -- and every caller that caught the error (the Newton polish,
  ## the marginal covariate errors, summary, checkConvergence) silently went
  ## without. A parameter an item has fixed is not estimated, so its V is
  ## zeroed on that item's rows and its entries are NA, as in
  ## bigIRT_laplace_hyper_pieces: each item's block is over its free
  ## parameters, and the item's other parameters sit at their fixed values.
  item <- as.integer(context$items)
  for(b in active){
    off <- is.na(free_pos[b, item])
    if(any(off)) V[[b]][off] <- 0
  }
  S <- .Call(`_bigIRT_item_info_accum_cpp_impl`, item,
             matrix(unlist(V, use.names = FALSE), ncol = P), wcorr, ni)
  dimnames(S) <- list(active, active, NULL)

  prior_sd <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")
  prec <- matrix(0, nrow = P, ncol = ni, dimnames = list(active, NULL))
  if(isTRUE(as.logical(sdat$dopriors))){
    for(b in active){
      sd_b <- as.numeric(sdat[[prior_sd[[b]]]])
      if(!length(sd_b)) next
      prec[b, ] <- 1 / rep_len(pmax(sd_b, 1e-8), ni)^2
    }
  }
  fixed <- is.na(free_pos)
  if(any(fixed)){
    prec[fixed] <- NA_real_
    for(j in which(colSums(fixed) > 0L)){
      S[fixed[, j], , j] <- NA_real_
      S[, fixed[, j], j] <- NA_real_
    }
  }
  list(info = S, prior_precision = prec, active = active, P = P, ni = ni, est = est,
       free_pos = free_pos)
}

## Item information, standard errors and effective parameter count.
##
## With `priors = TRUE` the fit is penalised and the standard errors describe
## posterior curvature, not repeated sampling.
##
## Two methods, and the difference between them has been measured rather than
## assumed. On a 40-item, 1200-person two-parameter fit, checked against the
## full numerical Hessian with the person modes re-solved to 1e-8:
##
##   "profile"  the empirical-Bayes profile weight, in closed form and free.
##              Standard errors run about 6 per cent small at the median and
##              17 per cent small at worst. Two things cause that: the profile
##              weight is not quite the objective's curvature, and the
##              block-diagonal form drops item-item covariance through shared
##              persons. Splitting them, block-diagonality alone is worth only
##              1 per cent at the median and 5.8 per cent at worst, so most of
##              the gap is the weight, not the block structure.
##
##   "hessian"  central differences of the analytic gradient, per item block.
##              Exact up to that same 1 per cent block-diagonal term. Costs
##              2 x P objective evaluations per item, so it is for checking a
##              fit or for small problems, not for a hundred thousand items.
##
## The person modes must be re-solved at each perturbed point or the result is
## the frozen-mode curvature instead, which is a different and larger quantity.
## That is what `person_tol` is for, and its default here is far tighter than
## the one used during fitting.
#' Item information, standard errors and effective parameters
#'
#' Observed information for each item's free parameters, with the standard
#' errors and effective parameter count that follow from it. With
#' `priors = TRUE` the fit is penalised, so these describe posterior curvature
#' rather than repeated sampling.
#'
#' @param fit A fitted object from [fitIRT()], fitted with `keepInternals = TRUE`
#'   (not needed for `"exact"` when the certificate kept the covariance).
#' @param cores Integer. Cores for the objective evaluations.
#' @param method Character. `"exact"` takes each item's block of the full
#'   inverse of the exact Hessian of the fitted objective -- the refined
#'   marginal when `laplaceRefine` ran -- so every standard error allows for
#'   the other items and the shared parameters being estimated too; it is read
#'   from the fit's convergence certificate when that kept it (see
#'   `laplaceCertify` in [fitIRT()]), which costs nothing and needs no
#'   internals, and computed otherwise. `"auto"` takes `"exact"` when the
#'   certificate kept it, and otherwise times one evaluation and takes
#'   `"hessian"` when it fits `time_budget`. `"hessian"` differences the
#'   analytic gradient per item block and inverts each block alone, which
#'   holds everything else at its estimate. `"profile"` uses a closed-form
#'   weight and costs nothing, but its standard errors run about six per cent
#'   small.
#' @param person_tol Numeric. Person-mode tolerance for the differenced method.
#'   Must be far tighter than the fitting default, or the difference quotient
#'   measures frozen-mode curvature instead.
#' @param eps Numeric. Step size for the difference quotients.
#' @param max_blocks Integer. Refuse `"hessian"` above this many items.
#' @param time_budget Numeric. Seconds `"auto"` may spend before falling back.
#'
#' @return An object of class `bigIRT_iteminfo`. P counts the parameter
#'   types free on at least one item; a parameter an item has fixed (an
#'   anchor through `itemDat`) is `NA` in every per-item entry below, and the
#'   item's block covers only the parameters it has free.
#' \describe{
#'   \item{information}{P x P x Nitems array of likelihood information; for
#'     `"exact"`, the inverse of each item's covariance block, prior included.}
#'   \item{prior_precision}{Matching prior precision, zero without priors or
#'     where `information` already carries it.}
#'   \item{covariance}{Inverse of each block; for `"exact"`, each item's block
#'     of the full inverse.}
#'   \item{se}{Nitems x P matrix of standard errors.}
#'   \item{edf}{Effective parameters per item and block.}
#'   \item{free}{Nitems x P logical matrix, `TRUE` where the item has that
#'     parameter free.}
#'   \item{se_loadings}{For `"exact"` on a fit with signed loadings
#'     (`signedLoadings` in [fitIRT()]) only: the standard error of every free
#'     loading's parameter, Nitems x Nscales, `NA` where the loading is fixed.
#'     With cross-loadings free there is more than one loading per item, which
#'     the per-item blocks do not describe.}
#' }
#'
#' @seealso [checkConvergence()]
#' @export
itemInformation <- function(fit, cores = 1L, method = c("auto", "exact", "profile", "hessian"),
                            person_tol = 1e-8, eps = 1e-4, max_blocks = 2000L,
                            time_budget = 60){
  method <- match.arg(method)
  ## The full inverse of the exact Hessian (R/certify.R), free when the fit's
  ## certificate kept it.
  if(identical(method, "exact") || (identical(method, "auto") &&
     !is.null(fit$laplaceStatus$certificate$covariance$items)))
    return(bigIRT_iteminfo_exact(fit, cores = cores))
  t0 <- as.numeric(proc.time()[["elapsed"]])
  d <- bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE, cores = cores)
  eval_sec <- max(as.numeric(proc.time()[["elapsed"]]) - t0, 1e-4)
  re <- bigIRT_laplace_row_effective(
    state = d$state, sdat = d$sdat, thetaBase = d$eval$posterior$theta_mode,
    rows = d$context$train_rows, context = d$context$row_context)
  blocks <- bigIRT_item_info_blocks(
    state = d$state, sdat = d$sdat, context = d$context, row_effective = re,
    posterior = d$eval$posterior, thetaBase = d$eval$posterior$theta_mode)
  if(is.null(blocks)) stop("No per-item free parameter blocks were found in this fit.")
  P <- blocks$P; ni <- blocks$ni; active <- blocks$active
  if(identical(method, "auto")){
    ## The differenced method is the accurate one, so take it whenever it fits
    ## in the budget. Cost is two evaluations per item parameter, and one
    ## evaluation has just been timed above.
    projected <- 2 * P * ni * eval_sec
    method <- if(projected <= time_budget && ni <= max_blocks) "hessian" else "profile"
    if(identical(method, "profile"))
      message(sprintf(paste0("itemInformation: exact differencing would need about %.0f s ",
        "(%d evaluations); using the closed-form weight, whose standard errors run ",
        "about 6%% small. Pass method = \"hessian\" to insist, or raise time_budget."),
        projected, 2L * P * ni))
  }
  if(identical(method, "hessian")){
    if(ni > max_blocks)
      stop(sprintf(paste0("method = \"hessian\" needs %d objective evaluations for %d items. ",
        "Raise `max_blocks` to insist, or use method = \"profile\"."), 2L * P * ni, ni))
    blocks$info <- bigIRT_item_hessian_blocks(d, fit, blocks, person_tol = person_tol, eps = eps)
    ## The differenced curvature already carries the prior, so do not add it twice.
    blocks$prior_precision[!is.na(blocks$prior_precision)] <- 0
  }
  se <- matrix(NA_real_, nrow = ni, ncol = P, dimnames = list(NULL, active))
  edf <- matrix(NA_real_, nrow = ni, ncol = P, dimnames = list(NULL, active))
  cov_arr <- array(NA_real_, c(P, P, ni), dimnames = list(active, active, NULL))
  ## Each item's block over the parameters it has free; one it has fixed keeps
  ## NA, and is held at its value rather than integrated over.
  free <- !is.na(blocks$free_pos)
  for(j in seq_len(ni)){
    f <- free[, j]
    if(!any(f)) next
    Sj <- matrix(blocks$info[, , j], P, P)[f, f, drop = FALSE]
    Hj <- Sj + diag(blocks$prior_precision[f, j], sum(f))
    Vj <- try(solve(Hj), silent = TRUE)
    if(inherits(Vj, "try-error")) next
    cov_arr[f, f, j] <- Vj
    dv <- diag(Vj)
    se[j, f] <- sqrt(pmax(dv, 0))
    ## Effective parameters: how much of each block the data, rather than the
    ## prior, is responsible for. Equals P when the prior is flat.
    edf[j, f] <- pmax(pmin(diag(Sj %*% Vj), 1), 0)
  }
  structure(list(
    information = blocks$info, prior_precision = blocks$prior_precision,
    covariance = cov_arr, se = se, edf = edf, active = active,
    method = method,
    penalised = isTRUE(as.logical(d$sdat$dopriors)),
    n_items = ni, edf_total = sum(edf, na.rm = TRUE),
    free = matrix(t(free), ni, P, dimnames = list(NULL, active))
  ), class = "bigIRT_iteminfo")
}

## Per-item curvature by central differences of the analytic gradient, with the
## person modes re-solved at every perturbed point.  Returns the same
## P x P x Nitems array shape as the closed-form construction, NA where the
## item has the parameter fixed.
bigIRT_item_hessian_blocks <- function(d, fit, blocks, person_tol = 1e-8, eps = 1e-4){
  lay <- d$layout; P <- blocks$P; ni <- blocks$ni; active <- blocks$active
  gf <- function(p) bigIRT_laplace_direct_objective(par = p, state = d$state,
    sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
    theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
    context = d$context, tol = person_tol, max_iter = 200L)$approx_grad
  out <- array(0, c(P, P, ni), dimnames = list(active, active, NULL))
  ## Item j's coordinate in each block, through its free position: index j
  ## is item j only while no item has that parameter fixed.
  cj <- vapply(seq_len(ni), function(j) bigIRT_item_coords(blocks$free_pos, lay, j), integer(P))
  cj <- matrix(cj, P, ni)
  for(a in seq_len(P)){
    for(j in seq_len(ni)){
      if(is.na(cj[a, j])) next
      pu <- d$par; pu[cj[a, j]] <- pu[cj[a, j]] + eps
      pl <- d$par; pl[cj[a, j]] <- pl[cj[a, j]] - eps
      col <- (gf(pu) - gf(pl)) / (2 * eps)
      for(b in seq_len(P)) if(!is.na(cj[b, j])) out[b, a, j] <- -col[cj[b, j]]
    }
  }
  for(j in seq_len(ni)){
    M <- matrix(out[, , j], P, P)
    out[, , j] <- 0.5 * (M + t(M))
    fx <- is.na(cj[, j])
    if(any(fx)){ out[fx, , j] <- NA_real_; out[, fx, j] <- NA_real_ }
  }
  out
}

#' @export
print.bigIRT_iteminfo <- function(x, ...){
  cat(sprintf("bigIRT item information: %d items, blocks [%s]\n", x$n_items,
    paste(x$active, collapse = ", ")))
  cat(sprintf("Standard errors are %s, by the %s method%s.\n",
    if(isTRUE(x$penalised)) "posterior (the fit is penalised by priors)" else "likelihood-based",
    if(is.null(x$method)) "profile" else x$method,
    switch(if(is.null(x$method)) "profile" else x$method,
      profile = " (about 6% small; see ?itemInformation)",
      exact = " (the inverse of the convergence certificate's curvature)", "")))
  cat(sprintf("Effective parameters: %.1f of %d\n", x$edf_total,
    if(!is.null(x$free)) sum(x$free) else x$n_items * length(x$active)))
  print(summary(as.data.frame(x$se)))
  invisible(x)
}

## Standard errors for the ability covariate coefficients.
##
## The block is tiny -- a handful of coefficients -- so its curvature is taken
## by central differences of the analytic gradient rather than derived. That
## costs two objective evaluations per coefficient and needs no new mathematics.
##
## Inverting that block alone would answer the wrong question. It gives the
## variance of the coefficient with every item parameter held at its estimate,
## and against a simulation the resulting standard errors ran about 20 per cent
## small, with z scores spread 1.23 rather than 1. What is wanted is the
## marginal variance, allowing the item parameters to be uncertain too, which is
## the Schur complement
##   V_bb = ( H_bb - H_bp H_pp^-1 H_pb )^-1.
## The cross term H_bp falls out of the same difference quotients, and H_pp is
## block-diagonal by item, so the correction costs nothing extra.
bigIRT_covariate_vcov <- function(fit, eps = 1e-4, cores = 1L, person_tol = 1e-8,
                                  marginal = TRUE){
  d <- bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE, cores = cores)
  idx <- d$layout$ability_beta
  if(!length(idx)) return(NULL)
  g <- function(par){
    ev <- bigIRT_laplace_direct_objective(
      par = par, state = d$state, sdat = d$sdat,
      prior_precision = fit$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode,
      keep_covariance = TRUE, context = d$context,
      ## Without a tight person tolerance the modes do not move under a small
      ## perturbation and this returns frozen-mode curvature, a larger and
      ## different quantity.
      tol = person_tol, max_iter = 200L)
    ev$approx_grad
  }
  n <- length(idx)
  cols <- matrix(NA_real_, length(d$par), n)
  for(i in seq_len(n)){
    pu <- d$par; pu[idx[i]] <- pu[idx[i]] + eps
    pl <- d$par; pl[idx[i]] <- pl[idx[i]] - eps
    cols[, i] <- (g(pu) - g(pl)) / (2 * eps)
  }
  ## The objective is a log density, so its curvature is the negative Hessian.
  Hbb <- -cols[idx, , drop = FALSE]
  Hbb <- 0.5 * (Hbb + t(Hbb))
  prec <- Hbb
  if(isTRUE(marginal)){
    ii <- try(itemInformation(fit, cores = cores, method = "profile"), silent = TRUE)
    if(!inherits(ii, "try-error")){
      P <- length(ii$active); ni <- ii$n_items
      ## Over each item's free parameters, found through its free position:
      ## with an anchored difficulty the free ones used to be left out of the
      ## correction, which matters wherever a covariate is not centred.
      fp <- bigIRT_item_free_pos(d$sdat, ii$active)
      for(j in seq_len(ni)){
        f <- !is.na(fp[, j])
        if(!any(f)) next
        rows <- bigIRT_item_coords(fp, d$layout, j)[f]
        if(anyNA(rows)) next
        Vj <- matrix(ii$covariance[, , j], P, P)[f, f, drop = FALSE]
        if(anyNA(Vj)) next
        Hpb <- -cols[rows, , drop = FALSE]          # P x n
        prec <- prec - t(Hpb) %*% Vj %*% Hpb
      }
    }
  }
  V <- try(solve(prec), silent = TRUE)
  if(inherits(V, "try-error")) return(NULL)
  ## Undo the internal rescaling so the result is on the coefficient's own scale.
  bs <- attr(d$layout, "beta_scale")
  if(length(bs) == n) V <- V * outer(bs, bs)
  nm <- colnames(fit$pars$Abilitybeta)
  if(length(nm) && nrow(fit$pars$Abilitybeta) * length(nm) == n)
    dimnames(V) <- list(rep(nm, each = nrow(fit$pars$Abilitybeta)),
                        rep(nm, each = nrow(fit$pars$Abilitybeta)))
  V
}

#' @export
vcov.bigIRT_fit <- function(object, what = c("item", "covariate", "person"), cores = 1L, ...){
  what <- match.arg(what)
  switch(what,
    item = itemInformation(object, cores = cores)$covariance,
    covariate = bigIRT_covariate_vcov(object, cores = cores),
    person = if(!is.null(object$personPosterior$covariance)) object$personPosterior$covariance
             else stop("Person covariances were not retained. Refit with `laplaceKeepCovariance = TRUE`."))
}

## ---------------------------------------------------------------------------
## Convergence
## ---------------------------------------------------------------------------

## Block-resolved convergence report.
##
## A single scaled norm is not enough on its own, for two reasons that pull in
## opposite directions. It sums over every parameter, so a small block whose
## gradient is naturally larger -- a covariate coefficient sums over all
## persons, an item parameter only over its own responses -- can dominate the
## total and block convergence while every item is settled. And because it is a
## sum, one badly stuck parameter among tens of thousands is invisible in it.
##
## So three things are reported rather than one. The per-block norms say which
## block is responsible. The worst per-parameter gradient in units of that
## parameter's own standard error says how far the least-settled parameter is
## from stationary, in a unit that means something. The Newton decrement
## g' H^-1 g is scale-free and estimates how much objective is still on the
## table, which is the quantity one actually cares about.
##
## One caution on the decrement: H here is the block-diagonal item curvature,
## the same model the Newton polish steps in. That makes it self-certifying --
## a run driven mainly by block-Newton will report a very small decrement
## because it has converged to the block-diagonal model, not necessarily to a
## better optimum. Measured on a 2,000-item fit, a mostly-Newton run reported a
## decrement of 8e-08 against 5e-06 for L-BFGS with a polish, while reaching a
## log-likelihood 0.06 lower. Compare decrements within one algorithm, and
## compare algorithms by the objective.
#' Convergence, resolved by parameter block
#'
#' A single scaled gradient norm cannot say which block failed to settle, it
#' hides one stuck parameter among thousands, and a small block whose gradient
#' aggregates over many units can dominate it. This reports per-block norms and
#' shares, the worst parameter measured in its own standard errors, and the
#' Newton decrement, which estimates how much objective is still available.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param cores Integer. Cores for the information calculation.
#' @param se Logical. Compute the item information needed for the Newton
#'   decrement and the worst-parameter measure.
#'
#' @return An object of class `bigIRT_convergence`, carrying `blocks`,
#'   `newton_decrement`, `objective_remaining`, `worst_parameter_se_units` and
#'   the fit's `certificate`.
#'
#' @section Note:
#' The decrement uses the block-diagonal item curvature, the same model the
#' Newton polish steps in, so it is self-certifying. Compare decrements within
#' one algorithm, and compare algorithms by the objective. The certificate a
#' Laplace fit carries (`fitIRT(laplaceCertify = )`) is computed on the exact
#' Hessian of the fitted objective instead; it is returned as `certificate`,
#' and it is the verdict `converged` follows.
#'
#' @export
checkConvergence <- function(fit, cores = 1L, se = TRUE){
  g <- fit$optim$grad
  lay <- fit$optim$layout
  if(is.null(g) || is.null(lay))
    stop("This fit did not retain its gradient. Refit with a current bigIRT.")
  st <- fit$laplaceStatus
  slots <- lay[vapply(lay, length, integer(1)) > 0L]
  blocks <- data.frame(
    block = names(slots),
    n = vapply(slots, length, integer(1)),
    norm = vapply(slots, function(i) sqrt(sum(g[i]^2)), numeric(1)),
    max_abs = vapply(slots, function(i) max(abs(g[i])), numeric(1)),
    rms = vapply(slots, function(i) sqrt(mean(g[i]^2)), numeric(1)),
    row.names = NULL, stringsAsFactors = FALSE)
  blocks$share <- blocks$norm^2 / sum(blocks$norm^2)
  blocks <- blocks[order(-blocks$share), ]

  info <- NULL; newton <- NA_real_; worst <- NA_real_; worst_at <- NA_character_
  if(isTRUE(se)){
    info <- try(itemInformation(fit, cores = cores), silent = TRUE)
    if(!inherits(info, "try-error")){
      P <- length(info$active); ni <- info$n_items
      nd <- 0; wz <- 0; wlab <- NA_character_
      ## Each item over its free parameters (bigIRT_item_free_pos).
      fp <- bigIRT_item_free_pos(fit$prepared %||% fit$internals$sdat, info$active)
      for(j in seq_len(ni)){
        f <- !is.na(fp[, j])
        if(!any(f)) next
        idx <- bigIRT_item_coords(fp, lay, j)[f]
        if(anyNA(idx)) next
        gj <- g[idx]; Vj <- matrix(info$covariance[, , j], P, P)[f, f, drop = FALSE]
        if(anyNA(Vj)) next
        nd <- nd + as.numeric(t(gj) %*% Vj %*% gj)
        ## |g_i| * se_i is the parameter's distance from stationarity measured
        ## in its own standard errors, which is comparable across blocks.
        z <- abs(gj) * info$se[j, f]
        if(max(z, na.rm = TRUE) > wz){
          wz <- max(z, na.rm = TRUE)
          wlab <- sprintf("%s[%d]", info$active[f][which.max(z)], j)
        }
      }
      newton <- nd; worst <- wz; worst_at <- wlab
    } else info <- NULL
  }

  out <- list(
    converged = isTRUE(st$converged),
    reason = st$reason,
    scaled_gradient = as.numeric(st$last_item_grad_scaled),
    blocks = blocks,
    newton_decrement = newton,
    objective_remaining = if(is.finite(newton)) newton / 2 else NA_real_,
    worst_parameter_se_units = worst,
    worst_parameter = worst_at,
    certificate = st$certificate,
    n_parameters = length(g))
  class(out) <- "bigIRT_convergence"
  out
}

#' @export
print.bigIRT_convergence <- function(x, ...){
  cat(sprintf("bigIRT convergence: %s (%s)\n",
    if(isTRUE(x$converged)) "converged" else "NOT converged", x$reason %||% "unknown"))
  cat(sprintf("Scaled total gradient: %.3e over %d parameters\n",
    x$scaled_gradient, x$n_parameters))
  if(!is.null(x$certificate)) cat(bigIRT_certificate_line(x$certificate), "\n", sep = "")
  if(is.finite(x$newton_decrement))
    cat(sprintf("Newton decrement: %.3e  (objective still available: %.3e)\n",
      x$newton_decrement, x$objective_remaining))
  if(is.finite(x$worst_parameter_se_units))
    cat(sprintf("Worst parameter: %s at %.3f standard errors from stationary\n",
      x$worst_parameter, x$worst_parameter_se_units))
  cat("\nBy block:\n")
  b <- x$blocks
  b$share <- sprintf("%5.1f%%", 100 * b$share)
  b$norm <- signif(b$norm, 4); b$max_abs <- signif(b$max_abs, 4); b$rms <- signif(b$rms, 4)
  print(b, row.names = FALSE)
  ## Only worth saying when the fit has not settled. After the Newton polish the
  ## item blocks sit near zero, so whatever the polish does not touch holds most
  ## of a negligible total -- true, and no cause for concern.
  if(nrow(b) && !isTRUE(x$converged) && max(x$blocks$share) > 0.5 &&
     x$blocks$n[which.max(x$blocks$share)] < 20)
    cat("\nNote: most of the gradient norm sits in a very small block. The total\n",
        "norm is not a good convergence test in that situation.\n", sep = "")
  invisible(x)
}

## ---------------------------------------------------------------------------
## Information criteria
## ---------------------------------------------------------------------------

## Marginal log-likelihood, with person parameters integrated out.
##
## The Laplace objective is a log marginal likelihood, so the person parameters
## are not free parameters of it and must not be counted. With priors on, the
## objective is penalised and the reported value has the item prior density
## subtracted back off, so that what is returned is a likelihood rather than a
## posterior. The degrees of freedom are then the effective ones, which fall
## below the nominal count in proportion to how much the priors are doing.
#' @export
logLik.bigIRT_fit <- function(object, effective = TRUE, cores = 1L, ...){
  val <- object$optim$logLik
  if(is.null(val)) val <- object$logLik
  if(is.null(val)) stop("This fit carries no objective value.")
  sdat <- object$prepared
  penalised <- isTRUE(as.logical(sdat$dopriors))
  N <- as.numeric(sdat$Nsubs); K <- as.numeric(sdat$Nscales)

  ## The optimiser objective is not a log-likelihood and must not be reported
  ## as one. Two things separate them, both constant in the parameters and so
  ## invisible to the fit, but not to AIC or to anyone comparing against other
  ## software.
  ##
  ## First, with priors on, the objective carries the item prior density; a
  ## likelihood does not.
  ##
  ## Second, the person prior enters as a kernel without its normalising
  ## constant, so the objective is high by N*(K/2)*log(2*pi) and low by
  ## (N/2)*log|precision| -- the latter already added when the latent
  ## correlation is estimated, and omitted otherwise.
  ##
  ## Checked against dense quadrature at the fitted parameters: uncorrected the
  ## objective was 6.5 per cent adrift, corrected it agrees to 0.04 per cent,
  ## the remainder being the Laplace approximation itself.
  ipv <- object$laplaceStatus$item_prior_value
  if(is.finite(ipv)) val <- val - as.numeric(ipv)
  if(!isTRUE(object$laplaceStatus$estimated_corr)){
    Lam <- object$abilityPrior$precision
    if(!is.null(Lam)){
      ld <- suppressWarnings(determinant(as.matrix(Lam), logarithm = TRUE))
      if(is.finite(as.numeric(ld$modulus)) && ld$sign > 0)
        val <- val + 0.5 * N * as.numeric(ld$modulus)
    }
  }
  val <- val - N * (K / 2) * log(2 * pi)
  nominal <- length(object$optim$grad %||% numeric(0))
  df <- nominal
  if(penalised && isTRUE(effective)){
    ii <- try(itemInformation(object, cores = cores), silent = TRUE)
    if(!inherits(ii, "try-error")){
      ## Item blocks contribute their effective count; anything outside them
      ## (covariates, means, correlations) is unpenalised or weakly so and is
      ## counted at face value. The blocks hold only the parameters items have
      ## free, not n_items of each.
      item_nominal <- sum(!is.na(bigIRT_item_free_pos(sdat, ii$active)))
      df <- ii$edf_total + max(nominal - item_nominal, 0)
    }
  }
  structure(as.numeric(val), df = df, nobs = as.integer(sdat$Nsubs),
    nominal_df = nominal, penalised = penalised, class = "logLik")
}

#' @export
nobs.bigIRT_fit <- function(object, type = c("persons", "responses"), ...){
  type <- match.arg(type)
  if(identical(type, "persons")) as.integer(object$prepared$Nsubs) else as.integer(object$prepared$Nobs)
}

## Information criteria for a marginal IRT fit.
##
## Person parameters are integrated out of the Laplace objective, so they are
## not counted here. That is the standard marginal (MML) convention and it is
## also the only defensible one for these fits: the person parameters are not
## estimated freely, and counting them would penalise a model for the size of
## its sample.
##
## Two sample sizes are in play for BIC and they answer different questions.
## Persons are the independent units of the marginal likelihood, which is the
## conventional choice. Responses are what the item parameters are actually
## estimated from. Both are reported rather than one being chosen silently.
#' Information criteria for a marginal fit
#'
#' Person parameters are integrated out of the Laplace objective, so they are
#' not free parameters of it and are not counted; counting them would penalise
#' a model for the size of its sample. Effective and nominal degrees of freedom
#' are both reported, as are BIC on persons and on responses, since the two
#' answer different questions.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param cores Integer. Cores for the effective-parameter calculation.
#'
#' @return A data frame of criterion, degrees-of-freedom type, sample-size
#'   type, degrees of freedom and value.
#'
#' @export
IRTic <- function(fit, cores = 1L){
  ll <- logLik.bigIRT_fit(fit, effective = TRUE, cores = cores)
  edf <- as.numeric(attr(ll, "df"))
  ndf <- as.numeric(attr(ll, "nominal_df"))
  v <- as.numeric(ll)
  np <- as.integer(fit$prepared$Nsubs); nr <- as.integer(fit$prepared$Nobs)
  out <- data.frame(
    criterion = c("AIC", "AIC", "BIC", "BIC", "BIC", "BIC"),
    df_type   = c("effective", "nominal", "effective", "nominal", "effective", "nominal"),
    n_type    = c(NA, NA, "persons", "persons", "responses", "responses"),
    df        = c(edf, ndf, edf, ndf, edf, ndf),
    value     = c(-2 * v + 2 * edf, -2 * v + 2 * ndf,
                  -2 * v + log(np) * edf, -2 * v + log(np) * ndf,
                  -2 * v + log(nr) * edf, -2 * v + log(nr) * ndf),
    stringsAsFactors = FALSE)
  attr(out, "logLik") <- v
  attr(out, "penalised") <- isTRUE(attr(ll, "penalised"))
  out
}

## ---------------------------------------------------------------------------
## Held-out scoring
## ---------------------------------------------------------------------------

## Predictive performance on the responses excluded from fitting.
##
## The item base rate is the honest benchmark: it predicts each item's training
## success rate and ignores the person, so it is the fitted model with its
## discrimination set to zero and therefore sits inside the model's own
## parameter space.
#' Predictive performance on held-out responses
#'
#' Scores the rows excluded by `trainingRows` against the item base rate, which
#' predicts each item's training success rate and ignores the person. That is
#' the fitted model with zero discrimination, so it lies inside the model's own
#' parameter space and is a fair floor.
#'
#' @param fit A fitted object from [fitIRT()], fitted with `trainingRows`.
#' @param eps Numeric. Probability clamp for the log loss.
#'
#' @return A list of held-out response count, log loss, Brier score, AUC,
#'   accuracy and the item-base-rate Brier score, or `NULL` when nothing was
#'   held out.
#'
#' @export
heldoutMetrics <- function(fit, eps = 1e-12){
  sdat <- fit$prepared
  ord <- fit$pars$originalRow
  p_in <- fit$pars$pcorrect
  if(is.null(p_in) || is.null(ord))
    stop("This fit carries no per-response predictions.")
  ## sdat is in internal (person-sorted) order; predictions are mapped back to
  ## input order, so cross between them with originalRow rather than assuming.
  heldout <- which(as.integer(sdat$trainingLogical) == 0L)
  if(!length(heldout)) return(NULL)
  p <- p_in[ord[heldout]]
  y <- as.numeric(sdat$score)[heldout]
  ok <- is.finite(p) & is.finite(y)
  if(!any(ok)) return(NULL)
  ph <- pmin(pmax(p[ok], eps), 1 - eps); yh <- y[ok]
  logloss <- -mean(yh * log(ph) + (1 - yh) * log(1 - ph))
  brier <- mean((ph - yh)^2)
  ## AUC by rank, which needs no package and no binning.
  r <- rank(ph); n1 <- sum(yh == 1); n0 <- sum(yh == 0)
  auc <- if(n1 > 0 && n0 > 0) (sum(r[yh == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_
  ## Item base rate: each item predicted at its training success rate, ignoring
  ## the person.  A two-parameter logistic with zero discrimination, so it lies
  ## inside the fitted model own parameter space and is a fair floor.
  base_brier <- NA_real_
  itm <- as.integer(sdat$item)
  if(length(itm) == length(sdat$score)){
    trn <- which(as.integer(sdat$trainingLogical) == 1L)
    rate <- tapply(as.numeric(sdat$score)[trn], itm[trn], mean)
    bp <- rate[as.character(itm[heldout][ok])]
    bp[is.na(bp)] <- mean(as.numeric(sdat$score)[trn])
    base_brier <- mean((as.numeric(bp) - yh)^2)
  }
  list(heldout_responses = sum(ok), log_loss = logloss, brier = brier, auc = auc,
       accuracy = mean((ph > 0.5) == (yh == 1)), item_base_brier = base_brier)
}

## ---------------------------------------------------------------------------
## PSIS-LOO
## ---------------------------------------------------------------------------

## Approximate leave-one-out cross-validation over responses.
##
## The Laplace backend leaves a Gaussian posterior for each person, so draws
## from it can be importance-weighted in the usual way and handed to `loo`.
## What this does *not* carry is item parameter uncertainty: the item block
## stays at its mode, so the result is conditional on it. That is mild when
## persons greatly outnumber the responses per item and it is the same
## approximation any empirical-Bayes LOO makes, but it should be stated.
##
## Pareto k is the check that matters. If it is large for many responses the
## Gaussian posterior is a poor importance proposal and the estimate should not
## be trusted; held-out cross-validation does not have that failure mode and is
## the better tool when it happens.
#' Approximate leave-one-out cross-validation
#'
#' The Laplace backend leaves a Gaussian posterior for each person, so draws
#' from it can be importance-weighted and handed to `loo`. Item parameters stay
#' at their mode, so the result is conditional on them; that is mild when
#' persons greatly outnumber the responses per item. Pareto k is the check that
#' matters, and held-out cross-validation is the better tool when it is large.
#'
#' @param fit A fitted object from [fitIRT()], fitted with
#'   `laplaceKeepCovariance = TRUE`.
#' @param draws Integer. Posterior draws per person.
#' @param persons Integer. Optional person subsample, for datasets too large to
#'   hold a draws-by-responses matrix.
#' @param seed Integer. Optional seed for the draws.
#' @param cores Integer. Passed to `loo::loo()`.
#'
#' @return An object of class `loo`.
#'
#' @seealso [heldoutMetrics()]
#' @export
looIRT <- function(fit, draws = 500L, persons = NULL, seed = NULL, cores = 1L){
  if(!requireNamespace("loo", quietly = TRUE))
    stop("looIRT needs the 'loo' package. install.packages('loo')")
  if(is.null(fit$personPosterior$covariance))
    stop("PSIS-LOO needs the person posterior covariances. Refit with `laplaceKeepCovariance = TRUE`.")
  if(!is.null(seed)) set.seed(seed)
  sdat <- fit$prepared
  K <- as.integer(sdat$Nscales)
  mode <- as.matrix(fit$personPosterior$mode)
  Sig <- fit$personPosterior$covariance
  ids <- as.integer(sdat$id)
  keep_person <- if(is.null(persons)) seq_len(sdat$Nsubs) else
    sort(sample.int(sdat$Nsubs, min(as.integer(persons), sdat$Nsubs)))
  sel <- ids %in% keep_person
  ## `sdat` (fit$prepared) carries the training flag as `trainingLogical`; it
  ## has no `trainingRows` field at all, so reading that name here used to be
  ## a silent no-op and every held-out response was scored as if it had been
  ## part of the training likelihood.
  sel <- sel & (as.integer(sdat$trainingLogical) == 1L)
  rows <- which(sel)
  S <- as.integer(draws)
  if(S * length(rows) > 4e8)
    stop(sprintf("That is %.1f billion log-likelihood entries. Pass `persons=` to subsample.",
      S * length(rows) / 1e9))

  ## Draw once per person, then evaluate every retained response under those
  ## draws. Cholesky per person; K is small, so this is cheap.
  theta <- array(0, c(S, sdat$Nsubs, K))
  for(i in keep_person){
    Si <- matrix(Sig[, , i], K, K)
    ch <- tryCatch(chol(Si + diag(1e-10, K)), error = function(e) NULL)
    if(is.null(ch)) ch <- diag(sqrt(pmax(diag(Si), 0)), K)
    theta[, i, ] <- matrix(rep(mode[i, ], each = S), S, K) +
      matrix(stats::rnorm(S * K), S, K) %*% ch
  }

  re <- fit$rowEffective
  if(is.null(re) || is.null(re$loadings))
    stop("This fit carries no row-effective item parameters; PSIS-LOO needs them.")
  lo <- as.matrix(re$loadings)[rows, , drop = FALSE]
  b <- as.numeric(re$b)[rows]; cc <- as.numeric(re$c)[rows]; dd <- as.numeric(re$d)[rows]
  y <- as.numeric(sdat$score)[rows]; pid <- ids[rows]

  ## `theta` draws are around the BASE ability (personPosterior$mode omits the
  ## person-covariate term, like thetaBase everywhere else in this file); the
  ## offset x_row'beta has to be added per row, using that row's own covariate
  ## value, the same way bigIRT_row_ability() builds the reported
  ## fit$pars$Ability. personPreds are stored per response, not per person, so
  ## a within-person covariate is not necessarily constant across a person's
  ## own rows.
  ability_offset <- matrix(0, length(rows), K)
  if(isTRUE(sdat$NpersonPreds > 0) && length(fit$pars$Abilitybeta)){
    person_pred <- as.matrix(sdat$personPreds)[rows, , drop = FALSE]
    ability_offset <- person_pred %*% t(as.matrix(fit$pars$Abilitybeta))
  }

  ll <- matrix(NA_real_, S, length(rows))
  for(s in seq_len(S)){
    th <- theta[s, pid, , drop = TRUE]
    if(is.null(dim(th))) th <- matrix(th, ncol = K)
    th <- th + ability_offset
    eta <- rowSums(lo * th) - b
    g <- inv_logit(eta)
    p <- pmin(pmax(cc + (dd - cc) * g, 1e-12), 1 - 1e-12)
    ll[s, ] <- y * log(p) + (1 - y) * log(1 - p)
  }
  r_eff <- loo::relative_eff(exp(ll), chain_id = rep(1L, S))
  res <- loo::loo(ll, r_eff = r_eff, cores = cores)
  attr(res, "bigIRT") <- list(persons = length(keep_person), responses = length(rows),
    draws = S, conditional_on_item_parameters = TRUE)
  res
}
