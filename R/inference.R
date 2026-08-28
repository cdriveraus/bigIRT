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
  aSa <- numeric(length(ids))
  for(k in seq_len(K)) for(l in seq_len(K))
    aSa <- aSa + lo[, k] * lo[, l] * Sig[k, l, ids]
  ## The Laplace correction: information in an item parameter net of what the
  ## person's own uncertainty already absorbs. Without it these are conditional
  ## (JML-like) standard errors and are too small.
  wcorr <- pmax(1 / r - (s^2 * aSa) / r^2, 0)

  th <- if(K == 1L) matrix(as.numeric(thetaBase), ncol = 1L) else as.matrix(thetaBase)
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
  a_multi <- FALSE
  if(length(invspA)){
    sig <- inv_logit(invspA)
    Aval <- pmax(log1p(exp(invspA)), 1e-12)
    ## More than one free discrimination on a single row cannot be represented
    ## by one column of V, and that is the free multidimensional loading case.
    a_multi <- any(rowSums(A_ref > 0L) > 1L)
    for(k in seq_len(ncol(A_ref))){
      hit <- A_ref[, k] > 0L & A_ref[, k] <= length(invspA)
      if(!any(hit)) next
      q <- A_ref[hit, k]
      qrow[hit] <- q
      contrib[hit] <- contrib[hit] + (lo[hit, k] / Aval[q]) * th[ids[hit], k]
    }
  }
  V_A <- numeric(length(ids))
  ok <- qrow > 0L
  if(any(ok)) V_A[ok] <- s[ok] * sig[qrow[ok]] * contrib[ok]

  ## Every entry is dp/dparameter, so information is V V' / (p(1-p)).
  V <- list(B = -s, A = V_A,
            C = (1 - gg) * cr * (1 - cr), D = gg * dr * (1 - dr))
  est <- list(B = as.numeric(state$Bpars), A = invspA,
              C = as.numeric(state$logitCpars), D = as.numeric(state$logitDpars))
  ## A block counts only when it holds one free parameter per item; anything
  ## else is a different parameterisation (free multidimensional loadings, say)
  ## that this construction does not describe.
  active <- names(est)[vapply(est, function(z) length(z) == ni, logical(1))]
  if(isTRUE(a_multi)) active <- setdiff(active, "A")
  if(!length(active)) return(NULL)
  V <- V[active]; est <- est[active]; P <- length(active)

  item <- as.integer(context$B_ref)
  scat <- function(x){
    agg <- rowsum(x, item); out <- numeric(ni)
    out[as.integer(rownames(agg))] <- as.numeric(agg); out
  }
  S <- array(0, c(P, P, ni), dimnames = list(active, active, NULL))
  for(b1 in seq_len(P)) for(b2 in b1:P){
    v <- scat(V[[b1]] * V[[b2]] * wcorr)
    S[b1, b2, ] <- v
    if(b2 > b1) S[b2, b1, ] <- v
  }

  prior_sd <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")
  prec <- matrix(0, nrow = P, ncol = ni, dimnames = list(active, NULL))
  if(isTRUE(as.logical(sdat$dopriors))){
    for(b in active){
      sd_b <- as.numeric(sdat[[prior_sd[[b]]]])
      if(!length(sd_b)) next
      prec[b, ] <- 1 / rep_len(pmax(sd_b, 1e-8), ni)^2
    }
  }
  list(info = S, prior_precision = prec, active = active, P = P, ni = ni, est = est)
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
#' @param fit A fitted object from [fitIRT()], fitted with `keepInternals = TRUE`.
#' @param cores Integer. Cores for the objective evaluations.
#' @param method Character. `"auto"` times one evaluation and takes the exact
#'   route when it fits `time_budget`. `"hessian"` differences the analytic
#'   gradient per item block and is exact up to a block-diagonal term worth
#'   about one per cent. `"profile"` uses a closed-form weight and costs
#'   nothing, but its standard errors run about six per cent small.
#' @param person_tol Numeric. Person-mode tolerance for the differenced method.
#'   Must be far tighter than the fitting default, or the difference quotient
#'   measures frozen-mode curvature instead.
#' @param eps Numeric. Step size for the difference quotients.
#' @param max_blocks Integer. Refuse `"hessian"` above this many items.
#' @param time_budget Numeric. Seconds `"auto"` may spend before falling back.
#'
#' @return An object of class `bigIRT_iteminfo`:
#' \describe{
#'   \item{information}{P x P x Nitems array of likelihood information.}
#'   \item{prior_precision}{Matching prior precision, zero without priors.}
#'   \item{covariance}{Inverse of each block.}
#'   \item{se}{Nitems x P matrix of standard errors.}
#'   \item{edf}{Effective parameters per item and block.}
#' }
#'
#' @seealso [checkConvergence()]
#' @export
itemInformation <- function(fit, cores = 1L, method = c("auto", "profile", "hessian"),
                            person_tol = 1e-8, eps = 1e-4, max_blocks = 2000L,
                            time_budget = 60){
  method <- match.arg(method)
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
    blocks$prior_precision[] <- 0
  }
  se <- matrix(NA_real_, nrow = ni, ncol = P, dimnames = list(NULL, active))
  edf <- matrix(NA_real_, nrow = ni, ncol = P, dimnames = list(NULL, active))
  cov_arr <- array(NA_real_, c(P, P, ni), dimnames = list(active, active, NULL))
  for(j in seq_len(ni)){
    Sj <- matrix(blocks$info[, , j], P, P)
    Hj <- Sj + diag(blocks$prior_precision[, j], P)
    Vj <- try(solve(Hj), silent = TRUE)
    if(inherits(Vj, "try-error")) next
    cov_arr[, , j] <- Vj
    dv <- diag(Vj)
    se[j, ] <- sqrt(pmax(dv, 0))
    ## Effective parameters: how much of each block the data, rather than the
    ## prior, is responsible for. Equals P when the prior is flat.
    edf[j, ] <- pmax(pmin(diag(Sj %*% Vj), 1), 0)
  }
  structure(list(
    information = blocks$info, prior_precision = blocks$prior_precision,
    covariance = cov_arr, se = se, edf = edf, active = active,
    method = method,
    penalised = isTRUE(as.logical(d$sdat$dopriors)),
    n_items = ni, edf_total = sum(edf, na.rm = TRUE)
  ), class = "bigIRT_iteminfo")
}

## Per-item curvature by central differences of the analytic gradient, with the
## person modes re-solved at every perturbed point.  Returns the same
## P x P x Nitems array shape as the closed-form construction.
bigIRT_item_hessian_blocks <- function(d, fit, blocks, person_tol = 1e-8, eps = 1e-4){
  lay <- d$layout; P <- blocks$P; ni <- blocks$ni; active <- blocks$active
  gf <- function(p) bigIRT_laplace_direct_objective(par = p, state = d$state,
    sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
    theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
    context = d$context, tol = person_tol, max_iter = 200L)$approx_grad
  out <- array(0, c(P, P, ni), dimnames = list(active, active, NULL))
  idx <- lapply(active, function(b) lay[[b]])
  names(idx) <- active
  for(a in seq_len(P)){
    ia <- idx[[a]]
    for(j in seq_len(ni)){
      pu <- d$par; pu[ia[j]] <- pu[ia[j]] + eps
      pl <- d$par; pl[ia[j]] <- pl[ia[j]] - eps
      col <- (gf(pu) - gf(pl)) / (2 * eps)
      for(b in seq_len(P)) out[b, a, j] <- -col[idx[[b]][j]]
    }
  }
  for(j in seq_len(ni)){
    M <- matrix(out[, , j], P, P)
    out[, , j] <- 0.5 * (M + t(M))
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
    if(identical(if(is.null(x$method)) "profile" else x$method, "profile"))
      " (about 6% small; see ?itemInformation)" else ""))
  cat(sprintf("Effective parameters: %.1f of %d\n", x$edf_total, x$n_items * length(x$active)))
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
      for(j in seq_len(ni)){
        rows <- vapply(ii$active, function(b) d$layout[[b]][j], integer(1))
        if(anyNA(rows)) next
        Vj <- matrix(ii$covariance[, , j], P, P)
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
#'   `newton_decrement`, `objective_remaining` and `worst_parameter_se_units`.
#'
#' @section Note:
#' The decrement uses the block-diagonal item curvature, the same model the
#' Newton polish steps in, so it is self-certifying. Compare decrements within
#' one algorithm, and compare algorithms by the objective.
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
      for(j in seq_len(ni)){
        idx <- vapply(info$active, function(b) lay[[b]][j], integer(1))
        if(anyNA(idx)) next
        gj <- g[idx]; Vj <- matrix(info$covariance[, , j], P, P)
        if(anyNA(Vj)) next
        nd <- nd + as.numeric(t(gj) %*% Vj %*% gj)
        ## |g_i| * se_i is the parameter's distance from stationarity measured
        ## in its own standard errors, which is comparable across blocks.
        z <- abs(gj) * info$se[j, ]
        if(max(z, na.rm = TRUE) > wz){
          wz <- max(z, na.rm = TRUE)
          wlab <- sprintf("%s[%d]", info$active[which.max(z)], j)
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
  sdat <- object$dat
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
      ## counted at face value.
      item_nominal <- ii$n_items * length(ii$active)
      df <- ii$edf_total + max(nominal - item_nominal, 0)
    }
  }
  structure(as.numeric(val), df = df, nobs = as.integer(sdat$Nsubs),
    nominal_df = nominal, penalised = penalised, class = "logLik")
}

#' @export
nobs.bigIRT_fit <- function(object, type = c("persons", "responses"), ...){
  type <- match.arg(type)
  if(identical(type, "persons")) as.integer(object$dat$Nsubs) else as.integer(object$dat$Nobs)
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
  np <- as.integer(fit$dat$Nsubs); nr <- as.integer(fit$dat$Nobs)
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
  sdat <- fit$dat
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
  sdat <- fit$dat
  K <- as.integer(sdat$Nscales)
  mode <- as.matrix(fit$personPosterior$mode)
  Sig <- fit$personPosterior$covariance
  ids <- as.integer(sdat$id)
  keep_person <- if(is.null(persons)) seq_len(sdat$Nsubs) else
    sort(sample.int(sdat$Nsubs, min(as.integer(persons), sdat$Nsubs)))
  sel <- ids %in% keep_person
  tr <- sdat$trainingRows
  if(!is.null(tr)){ inbag <- rep(FALSE, sdat$Nobs); inbag[tr] <- TRUE; sel <- sel & inbag }
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

  ll <- matrix(NA_real_, S, length(rows))
  for(s in seq_len(S)){
    th <- theta[s, pid, , drop = TRUE]
    if(is.null(dim(th))) th <- matrix(th, ncol = K)
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
