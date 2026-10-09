## Score contributions, and score-based tests of covariate relations.
##
## WHAT. For a fitted model, each person's contribution to the score of the
## marginal likelihood -- the derivative of log p(y_i) in the item parameters
## and in the ability distribution's mean -- and the matching observed
## information, response by response. From those alone, any covariate can be
## tested against any item parameter (differential item functioning) or
## against ability, after one fit and without refitting.
##
## HOW. By the Fisher identity, a person's marginal score is the posterior
## expectation of the complete-data score,
##
##   d/dpsi log p(y_i) = E[ d/dpsi log p(y_i | theta) | y_i ],
##
## taken by adaptive Gauss-Hermite quadrature centred at the person's Laplace
## mode (src/agq_backend.cpp, which already formed these terms for the fit's
## gradient and now writes them out per row). The information is the Louis
## identity on the same nodes. With one node the expectation collapses to the
## mode, which is the plug-in score of a joint (fixed-effects) estimator --
## kept as `nodes = 1` for comparison, not as a recommendation.
##
## WHY THE MARGINAL SCORE. Score-based tests need contributions that have mean
## zero under the null, conditionally on the covariate. The plug-in score
## evaluated at an estimated ability does not: the ability estimate's own error
## (shrinkage, or the incidental-parameter bias of joint ML) leaves a residual
## pattern in theta, and when the covariate is correlated with ability that
## pattern is ordered by the covariate. That is the inflated Type I error the
## fixed-effects score tests showed under impact, and the reason they had to
## residualise the covariate on estimated ability. The marginal score has
## conditional mean zero when the model is right -- which includes the
## covariate in the ability model (`personPreds`) when ability depends on it.
## Impact is then modelled, not residualised away, and the test keeps its power
## for a covariate that is related to ability (residualising destroys a binary
## covariate that coincides with the ability groups).
##
## THE NULL DISTRIBUTION. Each item's process is tested on its own, with that
## item's other parameters projected out exactly: for tested slots P among the
## item's free slots F, and a covariate basis g,
##
##   U* = sum_u g_u s_uP - I_gF I_FF^-1 sum_u s_uF,
##   V  = I_gg - I_gF I_FF^-1 I_Fg,      LM = U*' V^-1 U* ~ chi2,
##
## sums over the item's responses u, I_.. the summed Louis information. This is
## Neyman's C(alpha) form: valid at any root-n consistent estimate, so the
## item-prior's pull on the fit (which leaves sum_u s_uF slightly off zero) is
## handled rather than assumed away. V^-1 U* is the one-step estimate of the
## covariate's coefficient -- what a refit with the corresponding `BitemPreds`
## (`itemSpecificBetas = TRUE`) would estimate -- with standard error
## sqrt(diag(V^-1)).
##
## The fluctuation (CUSUM) tests are the same construction with g a step
## 1(z <= t) at every breakpoint t. Their classical Brownian-bridge limit needs
## information to accrue uniformly along the ordering, which impact breaks:
## persons ordered by a covariate correlated with ability carry different
## information about an item's discrimination and difficulty, and the
## difficulty's process then carries the discrimination's estimation error in a
## covariate-dependent way. Here the covariance of the projected process is
## computed exactly from the information accrued between breakpoints, and the
## p-value is simulated from it, so no uniformity is assumed.
##
## WHAT IS LEFT OUT. The coupling between items through shared persons, and
## the ability-side parameters, are not projected out of an item's test. Both
## are O(1/n) per person in the responses a person gives, and omitting a
## nuisance parameter's projection overstates V, so the omission errs
## conservative. Repeated responses of one person to one item are treated as
## separate units, dropping their posterior covariance.

## Kernel slot names: B, the loading on each dimension, C, D.
bigIRT_score_slot_names <- function(K) c("B", if(K == 1L) "A" else paste0("A", seq_len(K)), "C", "D")

## The slots free on at least one item, as 0-based kernel slots, named.
bigIRT_score_free_slots <- function(sdat){
  K <- as.integer(sdat$Nscales)
  ni <- as.integer(sdat$Nitems)
  freeA <- matrix(as.integer(sdat$fixedAlog) == 0L, nrow = ni, ncol = K, byrow = TRUE)
  keep <- c(any(as.integer(sdat$fixedB) == 0L), colSums(freeA) > 0,
    any(as.integer(sdat$fixedClogit) == 0L), any(as.integer(sdat$fixedDlogit) == 0L))
  s <- which(keep) - 1L
  names(s) <- bigIRT_score_slot_names(K)[keep]
  s
}

## Which of the output slots each item has free: Nitems x length(slots) logical.
bigIRT_score_item_free <- function(sdat, slots){
  K <- as.integer(sdat$Nscales); ni <- as.integer(sdat$Nitems)
  freeA <- matrix(as.integer(sdat$fixedAlog) == 0L, nrow = ni, ncol = K, byrow = TRUE)
  all_free <- cbind(as.integer(sdat$fixedB) == 0L, freeA,
    as.integer(sdat$fixedClogit) == 0L, as.integer(sdat$fixedDlogit) == 0L)
  out <- all_free[, as.integer(slots) + 1L, drop = FALSE]
  colnames(out) <- names(slots)
  out
}

## Everything the quadrature kernel needs at a fitted point, rebuilt from the
## fit: the prepared data, the parameter state, the training rows, the prior,
## and each person's Laplace mode and precision, solved afresh at the fitted
## item parameters. Uses `fit$internals` when the fit kept them; otherwise the
## state is read back from `fit$pars`, and checked against the fit's own
## row-level parameters, which a normalised fit no longer matches.
bigIRT_score_setup <- function(fit, cores = 1L, jitter = 1e-6, person_tol = 1e-10){
  if(!inherits(fit, "bigIRT_fit")) stop("`fit` must be a fit from fitIRT().")
  int <- fit$internals
  sdat <- if(!is.null(int$sdat)) int$sdat else fit$prepared
  if(is.null(sdat)) stop("This fit carries no prepared data.")
  K <- as.integer(sdat$Nscales); N <- as.integer(sdat$Nsubs)
  mat0 <- function(x, nr){
    if(is.null(x) || !length(x)) return(matrix(0, nrow = nr, ncol = 0))
    as.matrix(x)
  }
  state <- int$state
  if(is.null(state)){
    p <- fit$pars
    state <- list(
      AbilityMean = if(as.integer(sdat$fixedAbilityMean) == 0L && length(p$AbilityMeanpar))
        as.numeric(p$AbilityMeanpar) else rep_len(as.numeric(sdat$AbilityMeandat), K),
      Abilitybeta = if(length(p$Abilitybeta)) as.matrix(p$Abilitybeta) else matrix(0, K, 0),
      invspApars = as.numeric(p$invspApars), invspAbeta = mat0(p$invspAbeta, 1L),
      Bpars = as.numeric(p$Bpars), Bbeta = mat0(p$Bbeta, 1L),
      logitCpars = as.numeric(p$logitCpars), logitCbeta = mat0(p$logitCbeta, 1L),
      logitDpars = as.numeric(p$logitDpars), logitDbeta = mat0(p$logitDbeta, 1L))
  }
  rc <- bigIRT_laplace_row_context(sdat)
  if(!length(rc$rows)) stop("This fit has no training responses.")
  ## The state must reproduce the fit's own row-level item parameters. A fit
  ## with normalise = TRUE moved those (and the person side) onto another
  ## metric while the raw item parameters stayed put, so the two no longer
  ## describe one model and nothing computed from them would mean anything.
  if(is.null(int$state) && !is.null(fit$pars$b_row)){
    eff <- bigIRT_laplace_row_effective(state, sdat,
      thetaBase = matrix(0, N, K), rows = rc$rows, context = rc)
    b_fit <- as.numeric(fit$pars$b_row)[rc$rows]
    l_fit <- matrix(as.numeric(fit$pars$row_loadings), ncol = K)[rc$rows, , drop = FALSE]
    gap <- max(abs(as.numeric(eff$b_row) - b_fit) / (1 + abs(b_fit)),
      abs(as.matrix(eff$loadings) - l_fit) / (1 + abs(l_fit)))
    if(!is.finite(gap) || gap > 1e-6)
      stop("The fit's parameters no longer reproduce its own row-level parameters (",
        signif(gap, 3), " apart); a fit with normalise = TRUE does this. Refit with ",
        "normalise = FALSE, or with keepInternals = TRUE.")
  }
  person_pred <- rc$person_pred
  ability_beta <- state$Abilitybeta
  if(!length(ability_beta)) ability_beta <- matrix(0, nrow = K, ncol = 0)
  if(ncol(person_pred) == 0L){
    person_pred <- matrix(0, nrow = nrow(person_pred), ncol = 1L)
    ability_beta <- matrix(0, nrow = K, ncol = 1L)
  }
  prior_mean <- matrix(rep(rep_len(as.numeric(state$AbilityMean), K), each = N), nrow = N)
  prior_prec <- if(!is.null(int$priorPrecision)) int$priorPrecision else
    bigIRT_laplace_prior_mats(sdat, jitter = jitter)$precision_array
  theta_init <- fit$personPosterior$mode
  if(is.null(theta_init) || !identical(dim(as.matrix(theta_init)), c(N, K)))
    theta_init <- matrix(0, N, K)
  RcppParallel::setThreadOptions(numThreads = max(1L, as.integer(cores)))
  grain <- bigIRT_laplace_subject_grain(N, cores)
  A_signed <- rc$A_signed %||% bigIRT_laplace_signed_A(sdat)
  args <- list(id = rc$ids, score = sdat$score[rc$rows],
    person_pred = person_pred,
    fixed_ability = rc$fixed_ability_int %||% rc$fixed_ability,
    fixed_ability_value = rc$fixed_ability_value,
    A_ref = rc$A_ref, A_fixed_value = rc$fixed_A_value, A_beta_row = rc$A_beta_row,
    A_pred = rc$A_pred, A_signed = A_signed,
    B_ref = rc$B_ref, B_fixed_value = rc$B_fixed_value, B_beta_row = rc$B_beta_row, B_pred = rc$B_pred,
    C_ref = rc$C_ref, C_fixed_value = rc$C_fixed_value, C_beta_row = rc$C_beta_row, C_pred = rc$C_pred,
    D_ref = rc$D_ref, D_fixed_value = rc$D_fixed_value, D_beta_row = rc$D_beta_row, D_pred = rc$D_pred,
    Abilitybeta = ability_beta, invspApars = state$invspApars, invspAbeta = mat0(state$invspAbeta, 1L),
    Bpars = state$Bpars, Bbeta = mat0(state$Bbeta, 1L),
    logitCpars = state$logitCpars, logitCbeta = mat0(state$logitCbeta, 1L),
    logitDpars = state$logitDpars, logitDbeta = mat0(state$logitDbeta, 1L),
    prior_mean = prior_mean, prior_precision = prior_prec,
    free_mask = 1L - sdat$fixedAbilityLogical, grain_size = grain)
  post <- do.call(bigIRT_laplace_person_step_block_cpp_impl, c(args, list(
    theta_init = theta_init, jitter = jitter, max_attempts = 8L, max_iter = 100L,
    tol = person_tol, keep_covariance = FALSE)))
  if(!all(post$converged))
    warning(sum(!post$converged), " person modes did not converge; their scores are ",
      "taken at the last iterate.", call. = FALSE)
  list(sdat = sdat, state = state, rc = rc, args = args, posterior = post, K = K, N = N,
    prior_mean = prior_mean, prior_prec = prior_prec)
}

## The quadrature rule for scoring: the fit's own rule when it was refined by
## quadrature, the default rule otherwise, or `nodes` per dimension. One node
## puts the expectation at the mode (the plug-in score).
bigIRT_score_rule <- function(fit, K, nodes = NULL){
  if(!is.null(nodes) && as.integer(nodes) == 1L){
    Z <- lapply(seq_len(K), function(d) matrix(0, 1L, d))
    return(structure(list(Z = Z, log_weights = rep(list(0), K), groups = vector("list", K),
      coefs = vector("list", K), desc = rep("mode", K)), class = "bigIRT_agq_rules"))
  }
  r <- fit$laplaceStatus$agq_rule
  if(is.null(nodes) && identical(fit$laplaceStatus$marginal, "agq") && !is.null(r))
    return(bigIRT_agq_rules(K, nodes = r$nodes, points = r$points, inflate = r$inflate,
      pairs = isTRUE(r$pairs), nodes3 = r$nodes3 %||% 0L))
  bigIRT_agq_rules(K, nodes = nodes)
}

#' Score contributions of a fitted model
#'
#' Each response's contribution to the score of the marginal likelihood in
#' its item's parameters, with the matching observed information, and each
#' person's contribution to the score in the ability distribution's mean. They
#' are what score-based tests of differential item functioning and of ability
#' covariates are built from (see [scoreTest()]), and they can be summed by any
#' grouping for other diagnostics.
#'
#' A person's marginal score is the posterior expectation of the
#' complete-data score (the Fisher identity), taken here by adaptive
#' Gauss-Hermite quadrature centred at the person's Laplace mode; the
#' information is the Louis identity on the same nodes. Under the fitted model
#' these contributions have mean zero for every person, conditionally on the
#' person's covariates, which is the property score-based tests rest on. The
#' plug-in score at an estimated ability does not have it: the ability
#' estimate's own error leaves a residual pattern in ability, and when a
#' covariate is related to ability that pattern is ordered by the covariate.
#' `nodes = 1` returns that plug-in score, at the posterior mode, for
#' comparison.
#'
#' Scores are in the coordinates the model is estimated in, which are also the
#' ones item covariate effects act on: `B` is the intercept in
#' \eqn{\eta = a'\theta - B} (positive is harder); `A` the loading's raw
#' coordinate (softplus link, or the loading itself when signed); `C` and `D`
#' the logits of the half-range asymptote links.
#'
#' @param fit A fit from [fitIRT()], by either estimator, with
#'   `normalise = FALSE` (or `keepInternals = TRUE`).
#' @param nodes Integer. Quadrature nodes per dimension. Default: the fit's own
#'   rule if it was refined by quadrature, otherwise 9, 7 and 5 for one to
#'   three dimensions and the anchored pairwise rule beyond. `1` gives the
#'   plug-in score at the posterior mode.
#' @param information Logical. Also return each response's observed
#'   information in its item's slots. Default TRUE.
#' @param cores Integer. Threads for the person-level kernels.
#'
#' @return An object of class `bigIRT_scores`, a list with
#'   \describe{
#'   \item{`rows`}{A data.table, one row per training response: `row` (its
#'     row in the data passed to [fitIRT()]), `id`, `Item`, internal indices
#'     `person` and `item`, the response `y`, the score in each free parameter
#'     type (`B`, `A` or `A1`, `A2`, ..., `C`, `D`; zero where the response's
#'     item has that parameter fixed), `eta`, the score in the linear
#'     predictor (for the 2PL, the response minus its posterior-expected
#'     probability), and, with `information`, the information entries
#'     `I.<a>.<b>`.}
#'   \item{`persons`}{A data.table per person: `id`, `person`, the posterior
#'     mean and SD of (base) ability on each dimension, the score in the
#'     ability mean (`mu`, `mu1`, ...) and its information `Imu.<k>.<l>`.}
#'   \item{`slots`, `K`, `rule`, `item_free`, `item_labels`, `id_name`,
#'     `item_name`}{Bookkeeping used by [scoreTest()].}
#'   }
#'
#' @seealso [scoreTest()]
#' @export
scoreContributions <- function(fit, nodes = NULL, information = TRUE, cores = 1L){
  st <- bigIRT_score_setup(fit, cores = cores)
  sdat <- st$sdat; K <- st$K; N <- st$N; rc <- st$rc
  slots <- bigIRT_score_free_slots(sdat)
  rule <- bigIRT_score_rule(fit, K, nodes)
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(
    theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
    rule = rule, want_person = TRUE, row_out = if(isTRUE(information)) 2L else 1L,
    slot_out = as.integer(slots))))
  item_labels <- rownames(fit$pars$B) %||% as.character(seq_len(sdat$Nitems))
  person_labels <- rownames(fit$pars$Ability) %||% as.character(seq_len(N))
  id_name <- names(fit$personPars)[1] %||% "id"
  item_name <- names(fit$itemPars)[1] %||% "Item"
  orig <- fit$pars$originalRow
  rows <- data.table::data.table(
    row = if(!is.null(orig)) as.integer(orig)[rc$rows] else rc$rows,
    id = person_labels[rc$ids], Item = item_labels[rc$items],
    person = as.integer(rc$ids), item = as.integer(rc$items),
    y = as.integer(sdat$score[rc$rows]))
  sm <- aq$row_scores
  for(jj in seq_along(slots)) data.table::set(rows, j = names(slots)[jj], value = sm[, jj])
  data.table::set(rows, j = "eta", value = sm[, length(slots) + 1L])
  if(isTRUE(information)){
    jj <- 0L
    for(b in seq_along(slots)) for(a in seq_len(b)){
      jj <- jj + 1L
      data.table::set(rows, j = paste0("I.", names(slots)[a], ".", names(slots)[b]),
        value = aq$row_info[, jj])
    }
  }
  ## Person side: the score in the ability mean, P0 (E[theta] - mu), and its
  ## Louis information, P0 - P0 Cov[theta] P0, both from the same quadrature.
  eap <- matrix(aq$eap, N, K)
  pc <- aq$post_cov
  P0a <- st$prior_prec
  P0_at <- function(i) if(length(dim(P0a)) == 3L) matrix(P0a[, , i], K, K) else matrix(P0a, K, K)
  dn <- if(K == 1L) "" else as.character(seq_len(K))
  persons <- data.table::data.table(id = person_labels, person = seq_len(N))
  smu <- matrix(0, N, K); imu <- array(0, c(K, K, N))
  for(i in seq_len(N)){
    P0 <- P0_at(i); Ci <- matrix(pc[, , i], K, K)
    smu[i, ] <- P0 %*% (eap[i, ] - st$prior_mean[i, ])
    imu[, , i] <- P0 - P0 %*% Ci %*% P0
  }
  for(k in seq_len(K)){
    data.table::set(persons, j = paste0("eap", dn[k]), value = eap[, k])
    data.table::set(persons, j = paste0("sd", dn[k]), value = sqrt(pmax(pc[k, k, ], 0)))
  }
  for(k in seq_len(K)) data.table::set(persons, j = paste0("mu", dn[k]), value = smu[, k])
  for(l in seq_len(K)) for(k in seq_len(l))
    data.table::set(persons, j = paste0("Imu.", k, ".", l), value = imu[k, l, ])
  structure(list(rows = rows, persons = persons, slots = slots, K = K,
    rule = paste(unique(rule$desc), collapse = ","),
    item_free = bigIRT_score_item_free(sdat, slots),
    item_labels = item_labels, person_labels = person_labels,
    id_name = id_name, item_name = item_name,
    n_input_rows = length(fit$pars$p %||% numeric(0)),
    person_preds = if(isTRUE(sdat$NpersonPreds > 0)) colnames(as.matrix(sdat$personPreds)) else character(0),
    person_pred_means = if(isTRUE(sdat$NpersonPreds > 0)) bigIRT_laplace_person_means_by_id(sdat) else NULL,
    value = aq$value),
    class = "bigIRT_scores")
}

#' @export
print.bigIRT_scores <- function(x, ...){
  cat(sprintf("Score contributions: %d responses, %d persons, slots %s, rule %s\n",
    nrow(x$rows), nrow(x$persons), paste(names(x$slots), collapse = ", "), x$rule))
  tot <- x$rows[, lapply(.SD, function(v) sum(v)), .SDcols = names(x$slots)]
  cat("Summed over all responses (near zero at a fitted optimum, up to the item prior):\n")
  print(tot)
  invisible(x)
}

## ---------------------------------------------------------------------------
## Tests
## ---------------------------------------------------------------------------

## Packed column of the information entry for slots a, b (indices into the
## output slots), upper triangle by column.
bigIRT_score_pack_index <- function(a, b){
  lo <- pmin(a, b); hi <- pmax(a, b)
  hi * (hi - 1L) / 2L + lo
}

## A symmetric pseudo-inverse and its rank, dropping directions below a
## relative tolerance: a slot can be free and still carry no information here.
bigIRT_score_pinv <- function(M, rtol = 1e-10){
  M <- (M + t(M)) / 2
  if(!length(M)) return(list(inv = M, rank = 0L))
  e <- eigen(M, symmetric = TRUE)
  tol <- rtol * max(abs(e$values), 1e-300)
  keep <- e$values > tol
  inv <- if(any(keep)) e$vectors[, keep, drop = FALSE] %*%
    (t(e$vectors[, keep, drop = FALSE]) / e$values[keep]) else M * 0
  list(inv = inv, rank = sum(keep), root = if(any(keep)) e$vectors[, keep, drop = FALSE] %*%
    (t(e$vectors[, keep, drop = FALSE]) / sqrt(e$values[keep])) else M * 0)
}

## Kronecker-structured information between two sets of directions, each a
## basis (columns of X, Y over the units) times slots: entry
## sum_u X_ua Y_ub I_u[f, g], ordered slot-major, basis-minor.
bigIRT_score_kron <- function(X, Y, info, sx, sy){
  nx <- ncol(X); ny <- ncol(Y)
  out <- matrix(0, nx * length(sx), ny * length(sy))
  for(i in seq_along(sx)) for(j in seq_along(sy)){
    w <- info[, bigIRT_score_pack_index(sx[i], sy[j])]
    out[(i - 1L) * nx + seq_len(nx), (j - 1L) * ny + seq_len(ny)] <- crossprod(X * w, Y)
  }
  out
}

## Score in a set of directions: sum_u X_ua s_u[f], slot-major, basis-minor.
bigIRT_score_kron_s <- function(X, S, sx)
  as.numeric(vapply(sx, function(f) as.numeric(crossprod(X, S[, f])), numeric(ncol(X))))

## The C(alpha) pieces for one item (or one person-side block): the efficient
## score and its variance for directions G x test slots, with Nb x nuis slots
## projected out. Returns U (raw), Ustar, V.
bigIRT_score_lm_pieces <- function(G, Nb, S, info, test, nuis){
  Sg <- bigIRT_score_kron_s(G, S, test)
  Sn <- bigIRT_score_kron_s(Nb, S, nuis)
  Igg <- bigIRT_score_kron(G, G, info, test, test)
  Ign <- bigIRT_score_kron(G, Nb, info, test, nuis)
  Inn <- bigIRT_score_kron(Nb, Nb, info, nuis, nuis)
  pin <- bigIRT_score_pinv(Inn)
  Bt <- Ign %*% pin$inv
  list(U = Sg, Ustar = as.numeric(Sg - Bt %*% Sn), V = Igg - Bt %*% t(Ign))
}

## Basis for a covariate over the units: linear, natural spline (knots fixed
## from all units, so every item shares one basis), or treatment dummies.
bigIRT_score_basis <- function(z, form, df){
  if(form == "factor"){
    f <- droplevels(as.factor(z))
    if(nlevels(f) < 2L) return(NULL)
    X <- stats::model.matrix(~ f)[, -1L, drop = FALSE]
    colnames(X) <- levels(f)[-1L]
    return(X)
  }
  z <- as.numeric(z)
  if(form == "linear"){
    X <- matrix(z - mean(z), ncol = 1L); colnames(X) <- "linear"; return(X)
  }
  X <- splines::ns(z, df = df)
  X <- matrix(as.numeric(X), nrow = length(z))
  colnames(X) <- paste0("ns", seq_len(ncol(X)))
  X
}

## The covariates as one value per unit (response row or person).
bigIRT_score_covariates <- function(sc, covariates, level){
  if(is.atomic(covariates) && is.null(dim(covariates)))
    covariates <- data.frame(z = covariates)
  covariates <- as.data.frame(covariates)
  id_col <- intersect(c(sc$id_name, "id"), names(covariates))[1]
  if(identical(level, "auto")) level <- if(!is.na(id_col)) "person" else "row"
  if(identical(level, "person")){
    if(is.na(id_col)) stop("Person-level covariates need an `", sc$id_name, "` column.")
    key <- as.character(covariates[[id_col]])
    if(anyDuplicated(key)) stop("Person-level covariates must have one row per person.")
    vars <- setdiff(names(covariates), id_col)
    m <- match(sc$person_labels, key)
    if(anyNA(m)) stop(sum(is.na(m)), " persons in the fit have no row in `covariates`.")
    out <- lapply(vars, function(v) covariates[[v]][m])
    names(out) <- vars
    return(list(level = "person", values = out))
  }
  if(nrow(covariates) != sc$n_input_rows)
    stop("Row-level covariates need one row per row of the data passed to fitIRT() (",
      sc$n_input_rows, "); for person-level ones include an `", sc$id_name, "` column.")
  out <- lapply(covariates, function(v) v)
  list(level = "row", values = out)
}

## Grid of breakpoints for a fluctuation test: blocks of consecutive units in
## covariate order, ties kept together, at most `ngrid` of them.
bigIRT_score_blocks <- function(z, ngrid){
  o <- order(z)
  zs <- z[o]
  u <- unique(zs)
  grp <- match(zs, u)
  if(length(u) > ngrid){
    cut <- unique(stats::quantile(seq_along(zs), probs = seq_len(ngrid - 1L) / ngrid, type = 1, names = FALSE))
    brk <- unique(grp[cut])
    grp <- findInterval(grp, brk + 0.5) + 1L
  }
  block <- integer(length(z)); block[o] <- grp
  block
}

## Kolmogorov-type tail of sup |Brownian bridge|.
bigIRT_sup_bb_cdf <- function(x){
  if(x <= 0) return(0)
  k <- 1:100
  1 - 2 * sum((-1)^(k + 1) * exp(-2 * k^2 * x^2))
}

## The fluctuation test for one unit set: the process of the efficient score
## at every breakpoint, its exact covariance from the information accrued
## between breakpoints, and simulated p-values for three functionals.
bigIRT_score_fluct <- function(z, S, info, Nb, test, nuis, ngrid, trim, nsim){
  n <- length(z)
  block <- bigIRT_score_blocks(z, ngrid)
  M <- max(block)
  if(M < 3L) return(NULL)
  p <- length(test)
  ## Block sums of the tested scores, the tested information, and the
  ## tested-by-nuisance information.
  Sb <- rowsum(S[, test, drop = FALSE], block, reorder = TRUE)
  Ipp <- vapply(seq_len(p * p), function(ij){
    i <- (ij - 1L) %% p + 1L; j <- (ij - 1L) %/% p + 1L
    rowsum(info[, bigIRT_score_pack_index(test[i], test[j])], block, reorder = TRUE)[, 1]
  }, numeric(M))
  Ipp <- matrix(Ipp, nrow = M)
  ## Tested-by-nuisance blocks: for slot test[i] and nuisance direction
  ## (basis column a of Nb, slot nuis[f]).
  nn <- ncol(Nb) * length(nuis)
  Ipn <- array(0, c(M, p, nn))
  for(i in seq_len(p)) for(f in seq_along(nuis)) for(a in seq_len(ncol(Nb))){
    w <- info[, bigIRT_score_pack_index(test[i], nuis[f])] * Nb[, a]
    Ipn[, i, (f - 1L) * ncol(Nb) + a] <- rowsum(w, block, reorder = TRUE)[, 1]
  }
  Inn <- bigIRT_score_kron(Nb, Nb, info, nuis, nuis)
  Sn <- bigIRT_score_kron_s(Nb, S, nuis)
  pin <- bigIRT_score_pinv(Inn)
  ## Cumulate over blocks; drop the last breakpoint (the whole sample).
  cS <- apply(Sb, 2, cumsum); if(is.null(dim(cS))) cS <- matrix(cS, nrow = M)
  cI <- apply(Ipp, 2, cumsum); if(is.null(dim(cI))) cI <- matrix(cI, nrow = M)
  cN <- apply(Ipn, c(2, 3), cumsum); if(length(dim(cN)) < 3L) cN <- array(cN, c(M, p, nn))
  m_use <- seq_len(M - 1L)
  m1 <- length(m_use)
  ## Projected process C*(m) = C(m) - Ipn(m) Inn^- Sn, and the covariance
  ## K(m, m') = Ipp(min) - Ipn(m) Inn^- Ipn(m')'.
  proj <- pin$inv %*% Sn
  Cst <- matrix(0, m1, p)
  L <- matrix(0, m1 * p, nn)   # rows (m, i), m fastest
  for(i in seq_len(p)){
    Li <- matrix(cN[m_use, i, ], nrow = m1)
    L[(i - 1L) * m1 + seq_len(m1), ] <- Li
    Cst[, i] <- cS[m_use, i] - Li %*% proj
  }
  Kmat <- matrix(0, m1 * p, m1 * p)
  mm <- outer(m_use, m_use, pmin)
  for(i in seq_len(p)) for(j in seq_len(p)){
    Kmat[(i - 1L) * m1 + seq_len(m1), (j - 1L) * m1 + seq_len(m1)] <-
      matrix(cI[mm, (j - 1L) * p + i], m1, m1)
  }
  Kmat <- Kmat - L %*% pin$inv %*% t(L)
  Kmat <- (Kmat + t(Kmat)) / 2
  ## Standardisation for DM and CvM: the total information in the tested
  ## slots. Not after projection -- the tested slots are among the projected
  ## ones, so that is zero -- but before: under uniform accrual the projected
  ## process has covariance t(1 - t) times this, a standard bridge once scaled.
  Dr <- bigIRT_score_pinv(matrix(cI[M, ], p, p))$root
  if(!all(is.finite(Dr)) || all(Dr == 0)) return(NULL)
  tau <- cumsum(tabulate(block, M))[m_use] / n
  w <- diff(c(0, tau, 1))[seq_len(m1)]
  inner <- which(tau >= trim & tau <= 1 - trim)
  if(!length(inner)) inner <- seq_len(m1)
  Kd <- lapply(inner, function(m){
    ix <- (seq_len(p) - 1L) * m1 + m
    bigIRT_score_pinv(Kmat[ix, ix, drop = FALSE])$inv
  })
  ## The three functionals for a batch of paths X (nd x m1 p, breakpoint
  ## fastest within slot), vectorised over the paths.
  stats_of <- function(X){
    nd <- nrow(X)
    blk <- function(i) X[, (i - 1L) * m1 + seq_len(m1), drop = FALSE]
    dm <- numeric(nd); cvm <- numeric(nd)
    for(i in seq_len(p)){
      Wi <- 0
      for(j in seq_len(p)) if(Dr[i, j] != 0) Wi <- Wi + Dr[i, j] * blk(j)
      if(length(Wi) == 1L) Wi <- matrix(0, nd, m1)
      dm <- pmax(dm, apply(abs(Wi), 1, max))
      cvm <- cvm + as.numeric(Wi^2 %*% w)
    }
    lm_t <- rep(-Inf, nd)
    for(k in seq_along(inner)){
      v <- X[, (seq_len(p) - 1L) * m1 + inner[k], drop = FALSE]
      lm_t <- pmax(lm_t, rowSums((v %*% Kd[[k]]) * v))
    }
    cbind(DM = dm, CvM = cvm, maxLM = lm_t)
  }
  obs <- stats_of(matrix(as.numeric(Cst), nrow = 1L))[1, ]
  ## Simulate the Gaussian process with covariance K, adaptively: more draws
  ## for a p-value near the resolution of the first batch.
  e <- eigen(Kmat, symmetric = TRUE)
  keep_e <- e$values > 1e-12 * max(e$values, 1e-300)
  R <- e$vectors[, keep_e, drop = FALSE] %*% diag(sqrt(e$values[keep_e]), sum(keep_e))
  draw_p <- function(R0, nd){
    Z <- matrix(stats::rnorm(nd * ncol(R0)), nd, ncol(R0))
    sims <- stats_of(Z %*% t(R0))
    colSums(sims >= rep(obs, each = nd))
  }
  ge <- draw_p(R, nsim); tot <- nsim
  if(any((ge + 1) / (tot + 1) < 20 / nsim)){
    ge <- ge + draw_p(R, 9L * nsim); tot <- tot + 9L * nsim
  }
  pv <- (ge + 1) / (tot + 1)
  list(n = n, blocks = M, stat = obs, p = pv,
    p_bb = 1 - bigIRT_sup_bb_cdf(obs[["DM"]])^p)
}

#' Score-based tests of covariate relations after a fit
#'
#' Tests whether item parameters depend on a covariate (differential item
#' functioning, uniform or not), or whether ability does, using only the
#' fitted model's score contributions ([scoreContributions()]). No model is
#' refitted, so any number of covariates can be screened after one large fit.
#'
#' Two families of test are available.
#' \describe{
#'   \item{Parametric (`form = "linear"`, `"spline"`, `"factor"`)}{The score
#'     (Lagrange multiplier) test of adding the covariate, through the chosen
#'     basis, to the tested parameters. Exact chi-square reference. Each test
#'     also gives the one-step estimate of the coefficients it tests -- for
#'     items, what [fitIRT()] with the covariate in `BitemPreds` (or
#'     `AitemPreds`, ...) and `itemSpecificBetas = TRUE` would estimate, on the
#'     same coordinates; for ability, what it would estimate with the
#'     covariate in `personPreds` -- with a standard error.}
#'   \item{Fluctuation (`form = "fluctuation"`)}{The nonparametric CUSUM tests
#'     of Merkle and Zeileis (double maximum `DM`, Cramer-von Mises `CvM`,
#'     `maxLM`), which order persons by the covariate and need no functional
#'     form. The process's covariance is computed from the information
#'     accrued between breakpoints and the p-values are simulated from it,
#'     so they stay valid when information does not accrue uniformly along
#'     the covariate (impact does that). `p_DM_bb` is the classical Brownian
#'     bridge p-value, for comparison.}
#' }
#' In both, each item's own other parameters are projected out exactly
#' (Neyman's C(alpha) construction), so testing one parameter while the others
#' are estimated is accounted for, and the small pull of the item priors on the
#' fit is too.
#'
#' **Impact.** If the covariate is related to ability, it must be in the
#' fitted ability model (`personPreds`, in a form flexible enough to carry the
#' relation -- a factor for groups, a spline basis for a nonlinear trend);
#' otherwise every item inherits the unmodelled ability difference and the DIF
#' tests are not valid. `scoreTest()` checks this for person-level covariates
#' that are not in the ability model, with the ability test below, and warns.
#' The same is true of the uniform part of DIF that every item shares: it is
#' indistinguishable from an ability difference, and DIF is measured relative
#' to it.
#'
#' **Ability.** `parameters = "Ability"` tests the covariate as a predictor of
#' ability (on every dimension, or those in `dims`), with the ability mean
#' and the fit's existing person predictors projected out.
#'
#' @param fit A fit from [fitIRT()], or its [scoreContributions()].
#' @param covariates The covariates to test. Person-level: a data frame with
#'   the fit's id column (or `id`) and one column per covariate. Row-level: a
#'   vector or data frame with one row per row of the data passed to
#'   [fitIRT()] (a covariate can then vary within person, e.g. age at the
#'   time of the response).
#' @param parameters Which parameters to test: any of `"B"` (uniform DIF),
#'   `"A"` (every loading of the item), `"C"`, `"D"`, tested jointly; or
#'   `"Ability"`.
#' @param form `"linear"`, `"spline"` (natural cubic spline with `df` degrees
#'   of freedom), `"factor"`, or `"fluctuation"`. Default: `"factor"` for a
#'   factor, character or logical covariate, `"linear"` otherwise.
#' @param by `"item"` for one test per item, `"all"` for one coefficient
#'   shared by every item, or a vector naming a group for each item (in the
#'   order of `fit$itemPars`) for coefficients shared within groups.
#' @param items Optional item labels to test (default all).
#' @param df Degrees of freedom of the spline basis.
#' @param dims For `"Ability"`, the dimensions tested (default all).
#' @param level `"auto"`, `"person"` or `"row"`; see `covariates`.
#' @param ngrid,trim,nsim Fluctuation tests: maximum number of breakpoints
#'   (blocks of persons in covariate order), the trimming fraction for
#'   `maxLM`, and the number of simulated null paths (increased tenfold
#'   automatically when a p-value lies near the first batch's resolution).
#' @param seed Optional seed for the simulated p-values.
#' @param minN Minimum number of responses for an item to be tested.
#' @param nodes,cores Passed to [scoreContributions()] when `fit` is a fit.
#'
#' @return An object of class `bigIRT_scoretest`: a list with `tests`, a
#'   data.table with one row per covariate and item (or group): the number of
#'   responses `n`, the statistic(s), degrees of freedom and p-values, with
#'   Benjamini-Hochberg (`p_fdr`) and Holm (`p_holm`) adjustments within each
#'   covariate; and, for the parametric forms, `coefficients`, a data.table of
#'   one-step estimates (`estimate`), standard errors and z values per tested
#'   parameter and basis column.
#'
#' @seealso [scoreContributions()]
#' @export
scoreTest <- function(fit, covariates, parameters = "B",
  form = c("auto", "linear", "spline", "factor", "fluctuation"),
  by = "item", items = NULL, df = 3L, dims = NULL,
  level = c("auto", "person", "row"),
  ngrid = 50L, trim = 0.1, nsim = 1000L, seed = NULL, minN = 20L,
  nodes = NULL, cores = 1L){
  form <- match.arg(form); level <- match.arg(level)
  sc <- if(inherits(fit, "bigIRT_scores")) fit else scoreContributions(fit, nodes = nodes, cores = cores)
  if(!any(grepl("^I\\.", names(sc$rows))))
    stop("These score contributions carry no information; recompute with information = TRUE.")
  if(!is.null(seed)){
    old <- if(exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if(!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  cv <- bigIRT_score_covariates(sc, covariates, level)
  ability <- identical(parameters, "Ability") || identical(parameters, "ability")
  res <- list(); coefs <- list()
  for(v in names(cv$values)){
    zall <- cv$values[[v]]
    f_v <- form
    if(f_v == "auto") f_v <- if(is.factor(zall) || is.character(zall) || is.logical(zall)) "factor" else "linear"
    if(ability){
      out <- bigIRT_score_test_ability(sc, zall, cv$level, f_v, df, dims, ngrid, trim, nsim)
    } else {
      if(cv$level == "person" && !(v %in% sc$person_preds))
        bigIRT_score_impact_check(sc, zall, v)
      out <- bigIRT_score_test_items(sc, zall, cv$level, parameters, f_v, by, items, df,
        ngrid, trim, nsim, minN)
    }
    if(is.null(out)) next
    out$tests[, covariate := v]
    if(!is.null(out$coefficients)) out$coefficients[, covariate := v]
    res[[v]] <- out$tests; coefs[[v]] <- out$coefficients
  }
  tests <- data.table::rbindlist(res, fill = TRUE)
  if(nrow(tests)){
    pcol <- intersect(c("p", "p_DM"), names(tests))[1]
    tests[, p_fdr := stats::p.adjust(get(pcol), "BH"), by = covariate]
    tests[, p_holm := stats::p.adjust(get(pcol), "holm"), by = covariate]
    data.table::setcolorder(tests, c("covariate", setdiff(names(tests), "covariate")))
  }
  coefficients <- data.table::rbindlist(coefs, fill = TRUE)
  if(nrow(coefficients))
    data.table::setcolorder(coefficients, c("covariate", setdiff(names(coefficients), "covariate")))
  structure(list(tests = tests, coefficients = if(nrow(coefficients)) coefficients else NULL,
    parameters = parameters, rule = sc$rule),
    class = "bigIRT_scoretest")
}

#' @export
print.bigIRT_scoretest <- function(x, n = 10L, ...){
  t <- x$tests
  cat(sprintf("Score tests of %s: %d tests, rule %s\n",
    paste(x$parameters, collapse = "+"), nrow(t), x$rule))
  if(!nrow(t)) return(invisible(x))
  pcol <- intersect(c("p", "p_DM"), names(t))[1]
  for(v in unique(t$covariate)){
    tv <- t[covariate == v]
    cat(sprintf("  %s: %d of %d at p < .05 (%d after FDR); smallest p %.3g\n", v,
      sum(tv[[pcol]] < .05, na.rm = TRUE), nrow(tv), sum(tv$p_fdr < .05, na.rm = TRUE),
      suppressWarnings(min(tv[[pcol]], na.rm = TRUE))))
  }
  o <- order(t[[pcol]])
  print(utils::head(t[o], n))
  invisible(x)
}

## Warn when a person-level covariate tested for DIF is related to ability but
## absent from the ability model: its impact would be read as DIF.
bigIRT_score_impact_check <- function(sc, z, v){
  ok <- !is.na(z)
  form <- if(is.factor(z) || is.character(z) || is.logical(z)) "factor" else "linear"
  a <- tryCatch(bigIRT_score_test_ability(sc, z, "person", form, 3L, NULL, 50L, .1, 0L),
    error = function(e) NULL)
  if(is.null(a) || !nrow(a$tests)) return(invisible(NULL))
  if(isTRUE(min(a$tests$p, na.rm = TRUE) < 0.01))
    warning("Covariate '", v, "' is related to ability (score test p = ",
      format.pval(min(a$tests$p), digits = 2), ") but is not in the fitted ability model. ",
      "Its impact will appear as DIF in every item. Refit with it in `personPreds` ",
      "(as a factor, or a spline basis for a nonlinear relation) and test again.",
      call. = FALSE)
  invisible(NULL)
}

bigIRT_score_test_items <- function(sc, zall, zlevel, parameters, form, by, items, df,
  ngrid, trim, nsim, minN){
  R <- sc$rows
  slots <- names(sc$slots)
  pmap <- list(B = "B", A = grep("^A[0-9]*$", slots, value = TRUE), C = "C", D = "D")
  bad <- setdiff(parameters, names(pmap))
  if(length(bad)) stop("Unknown parameters: ", paste(bad, collapse = ", "))
  test_names <- unlist(pmap[parameters], use.names = FALSE)
  test_names <- intersect(test_names, slots)
  if(!length(test_names)) stop("None of the tested parameters is free in this fit.")
  z <- if(zlevel == "person") zall[R$person] else zall[R$row]
  keep <- !is.na(z)
  ni <- length(sc$item_labels)
  grp <- if(identical(by, "item")) sc$item_labels else if(identical(by, "all"))
    rep("all", ni) else {
      if(length(by) != ni) stop("`by` must be \"item\", \"all\", or one group label per item.")
      as.character(by)
    }
  if(!is.null(items)) keep <- keep & R$Item %in% as.character(items)
  R <- R[keep]; z <- z[keep]
  if(!nrow(R)) return(NULL)
  S <- as.matrix(R[, slots, with = FALSE])
  info_cols <- character(0)
  for(b in seq_along(slots)) for(a in seq_len(b)) info_cols <- c(info_cols, paste0("I.", slots[a], ".", slots[b]))
  info <- as.matrix(R[, info_cols, with = FALSE])
  G <- if(form == "fluctuation") NULL else bigIRT_score_basis(z, form, df)
  if(form != "fluctuation" && is.null(G)) return(NULL)
  free <- sc$item_free
  units <- split(seq_len(nrow(R)), grp[R$item])
  tests <- list(); coefs <- list()
  for(g in names(units)){
    u <- units[[g]]
    if(length(u) < minN) next
    its <- sort(unique(R$item[u]))
    if(form == "fluctuation"){
      if(length(its) > 1L) stop("Fluctuation tests are per item; use by = \"item\".")
      j <- its
      nuis <- which(free[j, ])
      test <- match(test_names, slots); test <- test[test %in% nuis]
      if(!length(test)) next
      fl <- bigIRT_score_fluct(z[u], S[u, , drop = FALSE], info[u, , drop = FALSE],
        matrix(1, length(u), 1), test, nuis, ngrid, trim, nsim)
      if(is.null(fl)) next
      tests[[g]] <- data.table::data.table(item = g, parameters = paste(slots[test], collapse = "+"),
        n = fl$n, blocks = fl$blocks,
        DM = fl$stat[["DM"]], p_DM = fl$p[["DM"]], p_DM_bb = fl$p_bb,
        CvM = fl$stat[["CvM"]], p_CvM = fl$p[["CvM"]],
        maxLM = fl$stat[["maxLM"]], p_maxLM = fl$p[["maxLM"]])
      next
    }
    ## Parametric: sum the C(alpha) pieces over the group's items, each item
    ## projecting out its own free slots.
    Us <- 0; V <- 0; tests_ok <- TRUE; test_used <- NULL
    for(j in its){
      uj <- u[R$item[u] == j]
      nuis <- which(free[j, ])
      test <- match(test_names, slots); test <- test[test %in% nuis]
      if(!length(test)){ next }
      if(!is.null(test_used) && !identical(test_used, test)){ tests_ok <- FALSE; break }
      test_used <- test
      pc <- bigIRT_score_lm_pieces(G[uj, , drop = FALSE], matrix(1, length(uj), 1),
        S[uj, , drop = FALSE], info[uj, , drop = FALSE], test, nuis)
      Us <- Us + pc$Ustar; V <- V + pc$V
    }
    if(!tests_ok || is.null(test_used)) next
    pv <- bigIRT_score_pinv(V)
    lm <- as.numeric(crossprod(Us, pv$inv %*% Us))
    est <- as.numeric(pv$inv %*% Us)
    se <- sqrt(pmax(diag(pv$inv), 0))
    tests[[g]] <- data.table::data.table(item = g, parameters = paste(slots[test_used], collapse = "+"),
      form = form, n = length(u), LM = lm, df = pv$rank,
      p = stats::pchisq(lm, pv$rank, lower.tail = FALSE))
    coefs[[g]] <- data.table::data.table(item = g,
      parameter = rep(slots[test_used], each = ncol(G)),
      term = rep(colnames(G), times = length(test_used)),
      estimate = est, se = se, z = est / se)
  }
  if(!length(tests)) return(NULL)
  out <- list(tests = data.table::rbindlist(tests, fill = TRUE),
    coefficients = if(length(coefs)) data.table::rbindlist(coefs) else NULL)
  if(!identical(by, "item")){
    data.table::setnames(out$tests, "item", "group")
    if(!is.null(out$coefficients)) data.table::setnames(out$coefficients, "item", "group")
  }
  out
}

bigIRT_score_test_ability <- function(sc, zall, zlevel, form, df, dims, ngrid, trim, nsim){
  P <- sc$persons; K <- sc$K
  dn <- if(K == 1L) "" else as.character(seq_len(K))
  z <- zall
  if(zlevel == "row"){
    ## One value per person: the person's mean over their responses.
    R <- sc$rows
    if(is.factor(z) || is.character(z) || is.logical(z)){
      zr <- z[R$row]
      first <- !duplicated(R$person)
      z <- rep(NA, nrow(P)); z[R$person[first]] <- as.character(zr[first])
    } else {
      zr <- as.numeric(z[R$row])
      zz <- rowsum(zr, R$person, reorder = TRUE)[, 1] / tabulate(R$person, nrow(P))
      z <- rep(NA_real_, nrow(P)); z[sort(unique(R$person))] <- zz
    }
  }
  keep <- !is.na(z)
  S <- as.matrix(P[keep, paste0("mu", dn), with = FALSE])
  info_cols <- character(0)
  for(l in seq_len(K)) for(k in seq_len(l)) info_cols <- c(info_cols, paste0("Imu.", k, ".", l))
  info <- as.matrix(P[keep, info_cols, with = FALSE])
  ## Nuisance: the ability mean and any person predictors already in the fit.
  Nb <- matrix(1, sum(keep), 1)
  if(!is.null(sc$person_pred_means) && length(sc$person_preds))
    Nb <- cbind(Nb, as.matrix(sc$person_pred_means)[keep, , drop = FALSE])
  test <- if(is.null(dims)) seq_len(K) else as.integer(dims)
  nuis <- seq_len(K)
  if(form == "fluctuation"){
    fl <- bigIRT_score_fluct(as.numeric(z[keep]), S, info, Nb, test, nuis, ngrid, trim, max(nsim, 1L))
    if(is.null(fl)) return(NULL)
    return(list(tests = data.table::data.table(item = "Ability", parameters = "Ability",
      n = fl$n, blocks = fl$blocks, DM = fl$stat[["DM"]], p_DM = fl$p[["DM"]], p_DM_bb = fl$p_bb,
      CvM = fl$stat[["CvM"]], p_CvM = fl$p[["CvM"]], maxLM = fl$stat[["maxLM"]],
      p_maxLM = fl$p[["maxLM"]]), coefficients = NULL))
  }
  G <- bigIRT_score_basis(z[keep], form, df)
  if(is.null(G)) return(NULL)
  pc <- bigIRT_score_lm_pieces(G, Nb, S, info, test, nuis)
  pv <- bigIRT_score_pinv(pc$V)
  lm <- as.numeric(crossprod(pc$Ustar, pv$inv %*% pc$Ustar))
  est <- as.numeric(pv$inv %*% pc$Ustar); se <- sqrt(pmax(diag(pv$inv), 0))
  list(tests = data.table::data.table(item = "Ability", parameters = "Ability", form = form,
    n = sum(keep), LM = lm, df = pv$rank, p = stats::pchisq(lm, pv$rank, lower.tail = FALSE)),
    coefficients = data.table::data.table(item = "Ability",
      parameter = rep(paste0("Ability", dn[test]), each = ncol(G)),
      term = rep(colnames(G), times = length(test)), estimate = est, se = se, z = est / se))
}
