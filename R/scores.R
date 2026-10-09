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
## sums over the item's responses u, I_.. the summed information. This is
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
## WHICH INFORMATION. A response's marginal score is exactly the score of the
## Bernoulli p(y_r | y_-r) -- the response given the person's other
## responses -- whose probability is the row's leave-one-out predictive. That
## Bernoulli's Fisher information is the score's variance conditional on the
## other responses: positive semidefinite, free of the response, and the
## default. The observed (Louis) information is the alternative; a single
## response's can be negative, and on thin items (tens of responses) the
## observed efficient information for a discrimination split by a factor
## collapsed while the scores still varied (0.0007 against 0.46 from the
## outer product, LM 1257 on 35 responses). On a simulated thin bank with no
## DIF (50 to 200 responses an item) the tests rejected at .05 as follows:
## expected 3.8-5.5 per cent (discrimination) and 5.0 (intercept); observed
## 4.6-5.8 once the collapsed ones were withheld; outer product 7-12.
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
bigIRT_score_setup <- function(fit, cores = 1L, jitter = 1e-6, person_tol = 1e-10,
  exclude_items = NULL, add_preds = NULL){
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
  ## Responses to excluded items are held out: they stay in the scoring pass
  ## but leave every person's posterior (anchoring on the rest; see
  ## bigIRT_score_refit_ability). A person left with none has the prior as
  ## posterior.
  hold <- rc$items %in% as.integer(exclude_items)
  if(all(hold)) stop("Excluding those items leaves no responses.")
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
  ## Predictors added to the ability regression after the fit (`adjust` in
  ## scoreTest), one row per training response, their coefficients starting
  ## at zero for bigIRT_score_refit_ability to estimate with the items held.
  P_real <- if(isTRUE(sdat$NpersonPreds > 0)) as.integer(sdat$NpersonPreds) else 0L
  pred_names <- if(P_real > 0L) colnames(as.matrix(state$Abilitybeta)) %||% paste0("pred", seq_len(P_real)) else character(0)
  if(length(add_preds)){
    add_preds <- as.matrix(add_preds)
    if(nrow(add_preds) != length(rc$rows))
      stop("Added ability predictors need one row per training response.")
    zero <- matrix(0, K, ncol(add_preds))
    if(P_real > 0L){
      args$person_pred <- cbind(args$person_pred, add_preds)
      args$Abilitybeta <- cbind(args$Abilitybeta, zero)
    } else {
      args$person_pred <- add_preds
      args$Abilitybeta <- zero
    }
    P_real <- P_real + ncol(add_preds)
    pred_names <- c(pred_names, colnames(add_preds) %||% paste0("added", seq_len(ncol(add_preds))))
  }
  ## The person modes (the quadrature's centres) come from the rows that are
  ## not held out.
  args_post <- if(any(hold)) bigIRT_score_args_rows(args, !hold) else args
  post <- do.call(bigIRT_laplace_person_step_block_cpp_impl, c(args_post, list(
    theta_init = theta_init, jitter = jitter, max_attempts = 8L, max_iter = 100L,
    tol = person_tol, keep_covariance = FALSE)))
  if(!all(post$converged))
    warning(sum(!post$converged), " person modes did not converge; their scores are ",
      "taken at the last iterate.", call. = FALSE)
  list(sdat = sdat, state = state, rc = rc, args = args, args_post = args_post,
    hold = as.integer(hold), posterior = post, K = K, N = N,
    prior_mean = prior_mean, prior_prec = prior_prec, P_real = P_real, pred_names = pred_names)
}

## The per-row kernel arguments restricted to some rows.
bigIRT_score_args_rows <- function(args, keep){
  rowwise <- c("id", "score", "person_pred", "fixed_ability", "fixed_ability_value",
    "A_ref", "A_fixed_value", "A_beta_row", "A_pred", "B_ref", "B_fixed_value", "B_beta_row",
    "B_pred", "C_ref", "C_fixed_value", "C_beta_row", "C_pred", "D_ref", "D_fixed_value",
    "D_beta_row", "D_pred")
  for(nm in rowwise){
    x <- args[[nm]]
    args[[nm]] <- if(is.matrix(x)) x[keep, , drop = FALSE] else x[keep]
  }
  args
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

## Each person's score in the ability mean, P0 (E[theta] - mu), and its Louis
## information, P0 - P0 Cov[theta] P0, from a quadrature pass that returned
## posterior moments (want_person = TRUE). Returns N x K scores and the
## information packed by column of the upper triangle, N x K(K + 1)/2.
bigIRT_score_person_terms <- function(aq, st){
  K <- st$K; N <- st$N
  eap <- matrix(aq$eap, N, K); pc <- aq$post_cov; P0a <- st$prior_prec
  P0_at <- function(i) if(length(dim(P0a)) == 3L) matrix(P0a[, , i], K, K) else matrix(P0a, K, K)
  pm <- st$args$prior_mean
  smu <- matrix(0, N, K); imu <- matrix(0, N, K * (K + 1L) / 2L)
  for(i in seq_len(N)){
    P0 <- P0_at(i); Ci <- matrix(pc[, , i], K, K)
    smu[i, ] <- P0 %*% (eap[i, ] - pm[i, ])
    I <- P0 - P0 %*% Ci %*% P0
    imu[i, ] <- I[upper.tri(I, diag = TRUE)]
  }
  list(eap = eap, post_cov = pc, score = smu, info = imu)
}

## Re-estimate the ability distribution's mean (when the fit estimated it) and
## the person-covariate coefficients -- the fit's own and any added after it --
## with the item parameters held, by Newton on the quadrature marginal plus the
## fit's penalty on them. Used after excluding items (the anchors alone set the
## ability side) and after adding a covariate's basis (`adjust`).
##
## Predictors constant within person enter through each person's score in the
## ability mean and its Louis information, exact Newton. A predictor that
## varies within person moves each response's ability on its own, so its score
## is summed over the person's responses (linear-predictor score times loading
## times value) and the step uses the outer product of the per-person scores,
## whose root is the same. Updates st$args and st$args_post (prior mean,
## coefficients) and the posterior.
##
## With `est_sd`, the ability SDs are re-estimated too (EM, correlation held).
## A fit identifies its scale by fixing the ability SD; with the items held
## the items fix it instead, and a covariate added after the fit takes up part
## of the variance the fit attributed to the residual, which must then shrink
## to stay on the items' metric. Measured: 1 SD of impact on a step, added
## after a fit without it, put the residual SD at .89 to .91 over six
## datasets, against sqrt(1 / 1.25) = .894; the adjusted tests matched those
## of a refit with the covariate (p values correlating 1.000).
##
## The iteration converges linearly. EM for the SD does so at the fraction of
## its information that is missing (successive changes shrank by .69 with
## eight responses per person and .85 with four: 33 and 67 iterations); the
## outer-product steps taken when predictors vary within person have a slow
## mode of their own (.84 and .82 on Mindsteps, the latter 93 iterations);
## and the two are coupled. Extrapolating each on its own (Aitken) worked
## when one was slow and fought when both were, ending further from the
## optimum after 50 iterations than plain iteration. So the whole vector --
## mean, coefficients, log SDs -- is extrapolated jointly by SQUAREM
## (Varadhan & Roland, 2008, scheme S3): two plain passes, a step along their
## squared extrapolation, and a pass from there. Never next to a capped
## step, which is far from the asymptotic regime (extrapolating capped steps
## once threw the SD to 59 and never converged); the step length is capped,
## and a point that moves an SD by more than a factor of two, or whose next
## pass is no closer to a fixed point than the plain one, is not taken.
bigIRT_score_refit_ability <- function(st, rule, maxit = 50L, tol = 1e-7, jitter = 1e-6,
  est_sd = FALSE, accelerate = TRUE, max_alpha = 50){
  sdat <- st$sdat; K <- st$K; N <- st$N
  free_mean <- as.integer(sdat$fixedAbilityMean) == 0L
  P <- as.integer(st$P_real %||% 0L)
  if(!free_mean && P == 0L && !isTRUE(est_sd)) return(st)
  ap <- st$args_post
  Xr <- if(P > 0L) as.matrix(ap$person_pred)[, seq_len(P), drop = FALSE] else NULL
  varies <- P > 0L && any(bigIRT_laplace_person_pred_varies(Xr, ap$id))
  if(!varies){
    Xp <- NULL
    if(P > 0L){
      Xp <- matrix(0, N, P)
      sm <- rowsum(Xr, ap$id, reorder = TRUE); ii <- as.integer(rownames(sm))
      Xp[ii, ] <- sm / tabulate(ap$id, N)[ii]
    }
    X <- cbind(if(free_mean) matrix(1, N, 1) else NULL, Xp)
  } else {
    keep <- st$hold == 0L
    lo <- bigIRT_laplace_row_effective(st$state, sdat, thetaBase = matrix(0, N, K),
      rows = st$rc$rows, context = st$rc)$loadings[keep, , drop = FALSE]
    free_k <- as.matrix(ap$fixed_ability) == 0
    X <- cbind(if(free_mean) matrix(1, nrow(Xr), 1) else NULL, Xr)
  }
  nc <- if(is.null(X)) 0L else ncol(X)
  ## The fit's own penalty on these (R/laplaceBackend.R): the mean's
  ## hyperprior and the coefficients' N(0, betaScale^2), both under `priors`.
  ## Without them the re-estimate sits off the fit's optimum even on the full
  ## data, by the prior's pull (about 1e-3 on 600 persons).
  pri <- isTRUE(as.logical(sdat$dopriors))
  mu0 <- rep_len(as.numeric(sdat$AbilityMeandat), K)
  mu_sd <- pmax(rep_len(as.numeric(sdat$AbilityMeanSD), K), 1e-8)
  bsc <- as.numeric(sdat$betaScale)
  P0_of <- function(pa) if(length(dim(pa)) == 3L) matrix(pa[, , 1], K, K) else matrix(pa, K, K)
  sd_of <- function(st) sqrt(diag(solve(P0_of(st$prior_prec))))
  R0 <- stats::cov2cor(solve(P0_of(st$prior_prec)))
  ## No jitter on Sig: it is positive definite by construction, and a jitter
  ## of 1e-6 put a floor of 1e-6 / (2 SD^2) under every SD change, above the
  ## tolerance for any SD below .7.
  set_sd <- function(st, sdv){
    pa <- bigIRT_prior_precision_array(solve(diag(sdv, K) %*% R0 %*% diag(sdv, K)), Nsubs = N, K = K)
    st$prior_prec <- pa; st$args$prior_precision <- pa; st$args_post$prior_precision <- pa
    st
  }
  solve_posterior <- function(st){
    st$args_post$prior_mean <- st$args$prior_mean
    st$args_post$Abilitybeta <- st$args$Abilitybeta
    st$posterior <- do.call(bigIRT_laplace_person_step_block_cpp_impl, c(st$args_post, list(
      theta_init = st$posterior$theta_mode, jitter = jitter, max_attempts = 8L, max_iter = 100L,
      tol = 1e-10, keep_covariance = FALSE)))
    st
  }
  ## One pass: a Newton (or outer-product) step for the mean and coefficients,
  ## an EM step for the SDs, the posterior modes at the new values.
  pass <- function(st){
    if(!varies){
      aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args_post, list(
        theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
        rule = rule, want_person = TRUE)))
      if(nc > 0L){
        pt <- bigIRT_score_person_terms(aq, st)
        U <- bigIRT_score_kron_s(X, pt$score, seq_len(K))
        H <- bigIRT_score_kron(X, X, pt$info, seq_len(K), seq_len(K))
      }
    } else {
      aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args_post, list(
        theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
        rule = rule, row_out = 1L, slot_out = integer(0), want_person = isTRUE(est_sd))))
      seta <- aq$row_scores[, 1]
      Ui <- matrix(0, N, K * nc)
      for(k in seq_len(K)){
        smk <- rowsum(X * (lo[, k] * seta * free_k[, k]), st$args_post$id, reorder = TRUE)
        Ui[as.integer(rownames(smk)), (k - 1L) * nc + seq_len(nc)] <- smk
      }
      U <- colSums(Ui); H <- crossprod(Ui)
    }
    if(nc > 0L && pri) for(k in seq_len(K)){
      ix <- (k - 1L) * nc + seq_len(nc)
      cur <- c(if(free_mean) st$args$prior_mean[1, k], if(P > 0L) st$args$Abilitybeta[k, seq_len(P)])
      cen <- c(if(free_mean) mu0[k], if(P > 0L) rep(0, P))
      prec <- c(if(free_mean) 1 / mu_sd[k]^2, if(P > 0L) rep(if(bsc > 0) 1 / bsc^2 else 0, P))
      U[ix] <- U[ix] - prec * (cur - cen)
      H[cbind(ix, ix)] <- H[cbind(ix, ix)] + prec
    }
    step <- if(nc > 0L) as.numeric(bigIRT_score_pinv(H)$inv %*% U) else 0
    ## The outer-product step is not exact Newton; keep an early one from
    ## overshooting on an ability scale.
    capped <- varies && max(abs(step)) > 1
    if(capped) step <- step / max(abs(step))
    if(nc > 0L) for(k in seq_len(K)){
      d <- step[(k - 1L) * nc + seq_len(nc)]
      if(free_mean){ st$args$prior_mean[, k] <- st$args$prior_mean[, k] + d[1]; d <- d[-1] }
      if(P > 0L) st$args$Abilitybeta[k, seq_len(P)] <- st$args$Abilitybeta[k, seq_len(P)] + d
    }
    ## EM for the SDs, from this pass's posterior moments of the base ability
    ## about its prior mean, the correlation held.
    sd_change <- 0
    if(isTRUE(est_sd)){
      eap <- matrix(aq$eap, N, K); pc <- aq$post_cov
      dv <- eap - matrix(st$args$prior_mean, N, K)
      Mv <- crossprod(dv) / N + matrix(rowMeans(matrix(pc, K * K, N)), K, K)
      sd_old <- sd_of(st)
      sd_new <- sqrt(pmax(diag(Mv), 1e-6))
      sd_change <- max(abs(sd_new / sd_old - 1))
      st <- set_sd(st, sd_new)
    }
    list(st = solve_posterior(st), step = max(abs(step)), sd_change = sd_change, capped = capped)
  }
  ## The parameters as one vector, and back.
  sd_ix <- if(isTRUE(est_sd)) (if(free_mean) K else 0L) + K * P + seq_len(K) else integer(0)
  theta_of <- function(st) c(if(free_mean) st$args$prior_mean[1, ],
    if(P > 0L) as.numeric(st$args$Abilitybeta[, seq_len(P), drop = FALSE]),
    if(isTRUE(est_sd)) log(sd_of(st)))
  set_theta <- function(st, th){
    j <- 0L
    if(free_mean){ for(k in seq_len(K)) st$args$prior_mean[, k] <- th[k]; j <- K }
    if(P > 0L) st$args$Abilitybeta[, seq_len(P)] <- matrix(th[j + seq_len(K * P)], K, P)
    if(isTRUE(est_sd)) st <- set_sd(st, exp(th[sd_ix]))
    solve_posterior(st)
  }
  trace <- list(); it <- 0L; n_accel <- 0L; conv <- FALSE
  record <- function(p, alpha = NA_real_){
    it <<- it + 1L
    trace[[it]] <<- c(step = p$step, sd_change = p$sd_change, capped = p$capped, alpha = alpha,
      sd = sd_of(p$st))
    conv <<- p$step < tol && p$sd_change < 10 * tol
  }
  resid <- function(p) max(p$step, p$sd_change)
  while(it < maxit){
    st0 <- st
    p1 <- pass(st0); record(p1); st <- p1$st
    if(conv || it >= maxit) break
    p2 <- pass(st); record(p2); st <- p2$st
    if(conv || it >= maxit) break
    if(!isTRUE(accelerate) || p1$capped || p2$capped) next
    th0 <- theta_of(st0); th1 <- theta_of(p1$st); th2 <- theta_of(p2$st)
    r <- th1 - th0; v <- th2 - 2 * th1 + th0
    nr <- sqrt(sum(r^2)); nv <- sqrt(sum(v^2))
    if(!(nv > 0) || !is.finite(nr / nv)) next
    alpha <- max(-nr / nv, -max_alpha)
    if(alpha > -1) next
    thp <- th0 - 2 * alpha * r + alpha^2 * v
    if(!all(is.finite(thp)) || any(abs(thp[sd_ix] - th2[sd_ix]) > log(2))) next
    p3 <- pass(set_theta(st0, thp)); record(p3, alpha)
    if(!p3$capped && is.finite(resid(p3)) && resid(p3) <= resid(p2)){
      st <- p3$st; n_accel <- n_accel + 1L
    } else conv <- FALSE
    if(conv) break
  }
  st$prior_mean <- st$args$prior_mean
  st$ability_sd <- sd_of(st)
  st$refit_iter <- it; st$refit_converged <- conv; st$refit_accel <- n_accel
  st$refit_trace <- do.call(rbind, trace)
  st
}

## The fast post-hoc adjustment. The fit's own coefficients are held and only
## the added ones estimated, by Newton on the quadrature marginal with each
## person's Louis information: a regression of the posterior means on the
## added basis, corrected for their shrinkage. Without that correction the
## regression recovers only about the reliability's share of the impact (.71
## of 1.01 with ten items per person, .87 of .96 with forty).
##
## The residual variance starts from a decomposition -- the held items fix the
## metric, so the fit's residual variance is split between the added
## covariate's fitted part and what remains -- and is then solved exactly:
## its gradient is the Fisher identity's, sum_i (E[(theta_i - mu_i)^2 | y_i] -
## s2) / (2 s2^2), from the same pass's posterior means and variances, and the
## root is found by secant steps on it (the EM step when a secant step is not
## safe). The decomposition alone was close at moderate impact (within .003 to
## .006 SD of the full re-estimate at 1 SD of impact) but .07 to .10 off at
## 3.5 SD with ten items, where the fit's normal ability distribution no longer
## has the total variance the decomposition assumes, and the tests went with
## it (40 per cent of clean items rejected, against 15 for the full one).
## Newton and secant both converge superlinearly, so a few passes.
##
## Returns NULL -- the caller then takes the full re-estimation -- when the
## added basis varies within person (no person-level Newton step), K > 1, or
## the residual variance falls below a twentieth of the fit's.
bigIRT_score_adjust_fast <- function(st, rule, q, maxit = 20L, tol = 1e-5){
  K <- st$K; N <- st$N
  if(K != 1L || q < 1L) return(NULL)
  idx <- st$P_real - q + seq_len(q)
  ap <- st$args_post
  Xr <- as.matrix(ap$person_pred)[, idx, drop = FALSE]
  if(any(bigIRT_laplace_person_pred_varies(Xr, ap$id))) return(NULL)
  nresp <- tabulate(ap$id, N); has <- nresp > 0L
  Xp <- matrix(0, N, q)
  sm <- rowsum(Xr, ap$id, reorder = TRUE); ii <- as.integer(rownames(sm))
  Xp[ii, ] <- sm / nresp[ii]
  sdat <- st$sdat
  bsc <- as.numeric(sdat$betaScale)
  prec_b <- if(isTRUE(as.logical(sdat$dopriors)) && bsc > 0) 1 / bsc^2 else 0
  p0_of <- function(pa) if(length(dim(pa)) == 3L) pa[1, 1, 1] else pa[1]
  var_fit <- 1 / p0_of(st$prior_prec); var_cur <- var_fit

  trace <- vector("list", maxit); conv <- FALSE
  for(it in seq_len(maxit)){
    aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args_post, list(
      theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
      rule = rule, want_person = TRUE)))
    sv <- var_cur
    dev <- as.numeric(aq$eap) - st$args$prior_mean[, 1]
    pv <- as.numeric(aq$post_cov)
    gam <- st$args$Abilitybeta[1, idx]
    ## Gradients, exact by the Fisher identity: the added coefficients', and
    ## the residual variance's (zero where EM would stop).
    U <- colSums(Xp * dev) / sv - prec_b * gam
    g <- sum(dev^2 + pv - sv) / (2 * sv^2)
    ## Curvature by Louis: the coefficients' block exactly; the variance's and
    ## the cross term need the posterior's third and fourth moments, taken from
    ## a normal posterior with the same mean and variance. Only the curvature
    ## is approximate, so the iteration still stops at the exact maximum.
    Hgg <- -crossprod(Xp * (1 / sv - pv / sv^2), Xp) - diag(prec_b, q)
    Hgs <- colSums(Xp * (dev * pv / sv^3 - dev / sv^2))
    Hss <- sum(1 / (2 * sv^2) - (dev^2 + pv) / sv^3 + (dev^2 * pv + pv^2 / 2) / sv^4)
    H <- rbind(cbind(Hgg, Hgs), c(Hgs, Hss))
    joint <- it > 1L && all(is.finite(H)) && max(eigen(H, symmetric = TRUE, only.values = TRUE)$values) < 0
    if(joint){
      d <- -as.numeric(solve(H, c(U, g)))
      step <- d[seq_len(q)]; dv <- d[q + 1L]
    } else {
      step <- -as.numeric(solve(Hgg, U))
      dv <- if(it == 1L) NA_real_ else sum(dev^2 + pv) / N - sv
    }
    if(it > 1L && max(abs(step)) < tol && abs(dv) / sv < tol){ conv <- TRUE; break }
    gam <- gam + step
    st$args$Abilitybeta[1, idx] <- gam
    var_new <- if(it == 1L){
      ## The first value by decomposition, a close start at moderate impact.
      f <- as.numeric(Xp %*% gam)[has]
      var_fit - mean((f - mean(f))^2)
    } else sv + sign(dv) * min(abs(dv), sv / 2)
    if(!is.finite(var_new) || var_new < var_fit / 20) return(NULL)
    var_cur <- var_new
    h <- if(is.na(dv)) 0 else dv
    pa <- bigIRT_prior_precision_array(matrix(1 / var_cur, 1, 1), Nsubs = N, K = 1L)
    st$prior_prec <- pa; st$args$prior_precision <- pa; st$args_post$prior_precision <- pa
    st$args_post$Abilitybeta <- st$args$Abilitybeta
    st$posterior <- do.call(bigIRT_laplace_person_step_block_cpp_impl, c(st$args_post, list(
      theta_init = st$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L, max_iter = 100L,
      tol = 1e-10, keep_covariance = FALSE)))
    trace[[it]] <- c(step = max(abs(step)), h_rel = h / var_cur, sd = sqrt(var_cur))
  }
  st$prior_mean <- st$args$prior_mean
  st$ability_sd <- sqrt(var_cur)
  st$refit_iter <- it; st$refit_converged <- conv; st$refit_accel <- 0L
  st$refit_trace <- do.call(rbind, trace[seq_len(it - conv)])
  st
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
#' The scores are those of the quadrature marginal, so they sum to (nearly)
#' zero over an item's responses only when the item parameters sit at that
#' marginal's optimum. A fit that ended on the Laplace objective (the default
#' for a one-dimensional 2PL) is close to it but not at it; [scoreTest()]
#' allows for the gap, but with few responses per person a fit refined with
#' `laplaceRefine = "agq"` is the cleaner basis. Measured with ten items per
#' person, 5,000 persons and a covariate related to ability: refining shrank
#' the item score sums fifteenfold and moved the LM test's false-positive rate
#' from 5.3 to 4.0 per cent (600 tests each).
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
#' @param exclude Optional item labels whose responses are held out of every
#'   person's posterior -- the anchoring step of DIF purification (see
#'   `purify` in [scoreTest()]). The ability distribution's mean (if the fit
#'   estimated it) and the person-covariate coefficients are then
#'   re-estimated on the remaining items, the item parameters held, since the
#'   excluded items' DIF pulled on them too. Held-out responses are still
#'   scored (column `held`), each against the posterior from the remaining
#'   items with its own likelihood added, as if it were the person's only
#'   other response.
#'
#' @return An object of class `bigIRT_scores`, a list with
#'   \describe{
#'   \item{`rows`}{A data.table, one row per training response: `row` (its
#'     row in the data passed to [fitIRT()]), `id`, `Item`, internal indices
#'     `person` and `item`, the response `y`, the score in each free parameter
#'     type (`B`, `A` or `A1`, `A2`, ..., `C`, `D`; zero where the response's
#'     item has that parameter fixed), `eta`, the score in the linear
#'     predictor (for the 2PL, the response minus its posterior-expected
#'     probability), `ppred`, the probability of a correct response given the
#'     person's other responses (the row's leave-one-out posterior
#'     predictive), and, with `information`, the observed information entries
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
scoreContributions <- function(fit, nodes = NULL, information = TRUE, cores = 1L,
  exclude = NULL){
  bigIRT_score_compute(fit, nodes = nodes, information = information, cores = cores,
    exclude = exclude, refit = length(exclude) > 0L)
}

## The work of scoreContributions(); `refit` re-estimates the ability side on
## the rows that are not held out.
bigIRT_score_compute <- function(fit, nodes = NULL, information = TRUE, cores = 1L,
  exclude = NULL, refit = FALSE, add_preds = NULL, adjust_method = "full"){
  item_labels <- rownames(fit$pars$B) %||% as.character(seq_len(fit$prepared$Nitems))
  ex <- NULL
  if(length(exclude)){
    ex <- match(as.character(exclude), item_labels)
    if(anyNA(ex)) stop("Unknown items in `exclude`: ", paste(exclude[is.na(ex)], collapse = ", "))
  }
  st <- bigIRT_score_setup(fit, cores = cores, exclude_items = ex, add_preds = add_preds)
  sdat <- st$sdat; K <- st$K; N <- st$N
  slots <- bigIRT_score_free_slots(sdat)
  rule <- bigIRT_score_rule(fit, K, nodes)
  method_used <- NA_character_
  if(isTRUE(refit) || length(add_preds)){
    stf <- if(!isTRUE(refit) && length(add_preds) && identical(adjust_method, "fast"))
      bigIRT_score_adjust_fast(st, rule, ncol(as.matrix(add_preds))) else NULL
    st <- stf %||% bigIRT_score_refit_ability(st, rule, est_sd = length(add_preds) > 0L)
    if(length(add_preds)) method_used <- if(is.null(stf)) "full" else "fast"
  }
  rc <- st$rc
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(
    theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
    rule = rule, want_person = TRUE, row_out = if(isTRUE(information)) 2L else 1L,
    slot_out = as.integer(slots), row_hold = if(any(st$hold == 1L)) st$hold else integer(0))))
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
  data.table::set(rows, j = "ppred", value = sm[, length(slots) + 2L])
  ## Each response's derivative of its probability in a common shift of the
  ## person's ability, a (P - c)(d - P) / (d - c) at the predictive
  ## probability: what turns an expected-score change into an ability-
  ## equivalent one (scoreTest's effects).
  re <- bigIRT_laplace_row_effective(st$state, sdat, thetaBase = matrix(0, N, K),
    rows = rc$rows, context = rc)
  pp <- rows$ppred
  wq <- (pp - re$c_row) * (re$d_row - pp) / pmax(re$d_row - re$c_row, 1e-12)
  for(k in seq_len(K))
    data.table::set(rows, j = paste0("dpdtheta", if(K == 1L) "" else k), value = re$loadings[, k] * wq)
  rm(re, wq, pp)
  if(any(st$hold == 1L)) data.table::set(rows, j = "held", value = st$hold == 1L)
  if(isTRUE(information)){
    jj <- 0L
    for(b in seq_along(slots)) for(a in seq_len(b)){
      jj <- jj + 1L
      data.table::set(rows, j = paste0("I.", names(slots)[a], ".", names(slots)[b]),
        value = aq$row_info[, jj])
    }
  }
  ## Person side: the score in the ability mean and its information, from
  ## the same quadrature.
  pt <- bigIRT_score_person_terms(aq, st)
  dn <- if(K == 1L) "" else as.character(seq_len(K))
  persons <- data.table::data.table(id = person_labels, person = seq_len(N))
  for(k in seq_len(K)){
    data.table::set(persons, j = paste0("eap", dn[k]), value = pt$eap[, k])
    data.table::set(persons, j = paste0("sd", dn[k]), value = sqrt(pmax(pt$post_cov[k, k, ], 0)))
  }
  for(k in seq_len(K)) data.table::set(persons, j = paste0("mu", dn[k]), value = pt$score[, k])
  ## Each person's ability-regression prediction (the mean plus person-mean
  ## predictors times their coefficients) and total ability estimate (the
  ## base's posterior mean plus the same regression part).
  Pr <- as.integer(st$P_real %||% 0L)
  xb <- matrix(0, N, K)
  if(Pr > 0L){
    Xr <- as.matrix(st$args$person_pred)[, seq_len(Pr), drop = FALSE]
    smx <- rowsum(Xr, st$args$id, reorder = TRUE); ix <- as.integer(rownames(smx))
    Xbar <- matrix(0, N, Pr); Xbar[ix, ] <- smx / tabulate(st$args$id, N)[ix]
    xb <- Xbar %*% t(matrix(st$args$Abilitybeta[, seq_len(Pr)], K, Pr))
  }
  for(k in seq_len(K)){
    data.table::set(persons, j = paste0("pred", dn[k]), value = st$args$prior_mean[, k] + xb[, k])
    data.table::set(persons, j = paste0("ability", dn[k]), value = pt$eap[, k] + xb[, k])
  }
  jj <- 0L
  for(l in seq_len(K)) for(k in seq_len(l)){
    jj <- jj + 1L
    data.table::set(persons, j = paste0("Imu.", k, ".", l), value = pt$info[, jj])
  }
  structure(list(rows = rows, persons = persons, slots = slots, K = K,
    rule = paste(unique(rule$desc), collapse = ","),
    item_free = bigIRT_score_item_free(sdat, slots),
    item_labels = item_labels, person_labels = person_labels,
    id_name = id_name, item_name = item_name,
    n_input_rows = length(fit$pars$p %||% numeric(0)),
    person_preds = if(isTRUE(sdat$NpersonPreds > 0)) colnames(as.matrix(fit$pars$Abilitybeta)) %||% character(0) else character(0),
    person_pred_means = if(isTRUE(sdat$NpersonPreds > 0)) bigIRT_laplace_person_means_by_id(sdat) else NULL,
    excluded = if(length(ex)) item_labels[ex] else character(0),
    ability_beta = { ab <- st$args$Abilitybeta[, seq_len(st$P_real), drop = FALSE]
      colnames(ab) <- st$pred_names; ab },
    ability_mean = st$args$prior_mean[1, ],
    ability_sd = st$ability_sd %||% sqrt(diag(solve(matrix(if(length(dim(st$prior_prec)) == 3L)
      st$prior_prec[, , 1] else st$prior_prec, K, K)))),
    adjusted = colnames(add_preds) %||% character(0),
    refit_iter = st$refit_iter %||% NA_integer_, refit_converged = st$refit_converged %||% NA,
    adjust_method = method_used,
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

#' Score contributions as an estimating-function matrix
#'
#' Arranges the score contributions of [scoreContributions()] as the matrix
#' [sandwich::estfun()] returns: one row per person (the person's marginal
#' score, summed over their responses) or per response, one column per free
#' item parameter. Tools built on `estfun`, such as the generalised
#' fluctuation tests of `strucchange::gefp()` and `strucchange::sctest()`,
#' then run on a bigIRT fit as they do on `mirt` fits.
#'
#' Persons are independent and are the units such tools assume; responses
#' within a person are not, so `level = "row"` is for bookkeeping (a
#' covariate given per response), not for tests that treat rows as
#' independent -- [scoreTest()] handles those. Those tools decorrelate with
#' the outer product of these scores; [scoreTest()] uses each response's
#' expected information instead, which on thin items is better calibrated.
#'
#' @param x A `bigIRT_scores` object.
#' @param items Items to include (default all). Each free parameter is a
#'   column, so for large banks choose items.
#' @param parameters Parameter types to include: any of `"B"`, `"A"` (all
#'   loadings), `"C"`, `"D"`.
#' @param level `"person"` (default) or `"row"`.
#' @param ... Unused.
#' @return A matrix with rows in the order of `x$persons` (or `x$rows`) and
#'   columns named `item:parameter`.
#' @examples
#' \dontrun{
#' sc <- scoreContributions(fit)
#' z <- covariate_per_person[as.integer(sc$person_labels)]
#' gp <- strucchange::gefp(sc, fit = NULL, order.by = z,
#'   scores = function(x) sandwich::estfun(x, items = "item7"))
#' strucchange::sctest(gp, functional = "DM")
#' }
#' @rawNamespace S3method(sandwich::estfun, bigIRT_scores)
estfun.bigIRT_scores <- function(x, items = NULL, parameters = c("B", "A", "C", "D"),
  level = c("person", "row"), ...){
  level <- match.arg(level)
  R <- x$rows; slots <- names(x$slots)
  pmap <- list(B = "B", A = grep("^A[0-9]*$", slots, value = TRUE), C = "C", D = "D")
  use <- intersect(unlist(pmap[intersect(parameters, names(pmap))], use.names = FALSE), slots)
  ii <- if(is.null(items)) seq_along(x$item_labels) else match(as.character(items), x$item_labels)
  if(anyNA(ii)) stop("Unknown items: ", paste(items[is.na(ii)], collapse = ", "))
  free <- x$item_free[ii, use, drop = FALSE]
  if(!any(free)) stop("No free parameters of those types for those items.")
  if(sum(free) > 2000L && is.null(items))
    stop(sum(free), " columns; choose `items`.")
  col_of <- matrix(0L, length(x$item_labels), length(use), dimnames = list(NULL, use))
  col_of[ii, ][free] <- seq_len(sum(free))
  keep <- R$item %in% ii
  Rk <- R[keep]
  M <- matrix(0, nrow(Rk), sum(free))
  for(s in use){
    cc <- col_of[Rk$item, s]
    ok <- cc > 0L
    M[cbind(which(ok), cc[ok])] <- Rk[[s]][ok]
  }
  wh <- which(col_of > 0L, arr.ind = TRUE)
  colnames(M)[col_of[wh]] <- paste0(x$item_labels[wh[, 1]], ":", use[wh[, 2]])
  if(level == "row"){
    out <- matrix(0, nrow(R), ncol(M), dimnames = list(NULL, colnames(M)))
    out[keep, ] <- M
    return(out)
  }
  out <- matrix(0, nrow(x$persons), ncol(M), dimnames = list(x$person_labels, colnames(M)))
  sm <- rowsum(M, Rk$person, reorder = TRUE)
  out[as.integer(rownames(sm)), ] <- sm
  out
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

## The outer product of each unit's scores, packed like the information
## columns: the OPG estimate of each unit's information.
bigIRT_score_opg <- function(S){
  ns <- ncol(S); out <- matrix(0, nrow(S), ns * (ns + 1L) / 2L)
  for(b in seq_len(ns)) for(a in seq_len(b)) out[, bigIRT_score_pack_index(a, b)] <- S[, a] * S[, b]
  out
}

## Each unit's expected information. A response's marginal score in its
## item's parameters is exactly the score of the Bernoulli p(y_r | y_-r), the
## response given the person's other responses, whose probability ppred
## depends on the item's parameters through every node: s = (y - ppred) /
## (ppred (1 - ppred)) d ppred/d psi. That model's Fisher information,
## (d ppred)(d ppred)' / (ppred (1 - ppred)), is the conditional variance of
## the score given the other responses -- positive semidefinite, free of the
## response itself, and unbiased for the score's variance -- and recovered
## from the score as s s' ppred (1 - ppred) / (y - ppred)^2.
bigIRT_score_expected_info <- function(S, y, ppred){
  f <- ppred * (1 - ppred) / (y - ppred)^2
  bigIRT_score_opg(S) * f
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
  ## `at` evaluates the basis at given covariate values, for effect contrasts.
  if(form == "linear"){
    mz <- mean(z)
    X <- matrix(z - mz, ncol = 1L); colnames(X) <- "linear"
    attr(X, "at") <- function(q) matrix(q - mz, ncol = 1L)
    return(X)
  }
  nsb <- splines::ns(z, df = df)
  X <- matrix(as.numeric(nsb), nrow = length(z))
  colnames(X) <- paste0("ns", seq_len(ncol(X)))
  attr(X, "at") <- function(q) matrix(stats::predict(nsb, q), nrow = length(q))
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
#' **Impact.** A covariate related to ability has to be in the ability model
#' for its DIF tests to be valid; otherwise every item inherits the
#' unmodelled ability difference. When `fit` is a fit and the covariate is not
#' among its `personPreds`, `adjust = "auto"` (the default) puts it there
#' after the fact: a natural spline of it (`adjustDf` degrees of freedom), or
#' indicators for a factor, centred, enters the ability regression, its
#' coefficients are estimated with the item parameters held, and the
#' contributions are recomputed under that model before testing. A
#' response-level covariate enters per response, so ability may also change
#' with it within person (age at the response, say). The item parameters stay
#' as the fit estimated them, without the covariate; each item's test projects
#' out the resulting discrepancy in its own parameters. `adjust = "none"`
#' tests against the fit as it is, and then warns when a person-level
#' covariate is related to ability but absent from the model.
#' The same is true of the uniform part of DIF that every item shares: it is
#' indistinguishable from an ability difference, and DIF is measured relative
#' to it.
#'
#' **Purification.** When several items have DIF in the same direction, their
#' DIF pulls every person's ability posterior (and the ability model's
#' coefficient on the covariate) towards explaining it, and the clean items
#' then show DIF the other way. `purify = TRUE` screens, flags the items whose
#' Benjamini-Hochberg adjusted p is below `purifyAlpha`, recomputes the score
#' contributions with the flagged items' responses left out of every person's
#' posterior and the ability coefficients re-estimated on the rest
#' (`exclude` in [scoreContributions()]), and retests the rest against that
#' anchor; flagged items are retested against the same anchor, each response
#' scored against the anchor posterior with its own likelihood added, in the
#' same pass. It repeats until the flagged set stops changing. Only the person side is
#' recomputed, so this needs the fit, not its score contributions, but no
#' refit.
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
#' @param purify,purifyAlpha,purifyMaxIter Iterative purification against
#'   DIF contamination (see above), the adjusted-p threshold for flagging,
#'   and the maximum number of anchoring rounds. Item tests by item only.
#' @param information Which information the item tests use.
#'   `"expected"` (default): each response's score is exactly the score of
#'   the Bernoulli for that response given the person's other responses (with
#'   probability `ppred`), and this is that Bernoulli's Fisher information --
#'   the score's variance given the other responses, never negative. On a
#'   simulated thin item bank with no DIF (50 to 200 responses an item) it
#'   held the nominal rate for intercepts and discriminations alike.
#'   `"observed"`: each response's Louis information. A single response's can
#'   be negative, and on thin items the observed efficient information of a
#'   comparison the item cannot support (a discrimination split by a factor
#'   with few responses at one level) can collapse while the scores still
#'   vary; such a test is withheld (`p = NA`) when that information is below a
#'   fifth of the outer-product one (`info_ratio`). `"opg"`: the outer
#'   product of the scores, the classical choice for fluctuation tests, but
#'   liberal on thin items (7 to 12 per cent at a nominal 5 in the same
#'   simulation). The ability tests always use the observed information of
#'   the ability mean, which is not subject to these problems.
#' @param adjust,adjustDf `"auto"` (default) adds a covariate missing from
#'   the fit's ability model to it before testing, as a natural spline with
#'   `adjustDf` degrees of freedom (indicators for a factor or a covariate
#'   with few values), items held; `"none"` does not. Needs `fit` to be a
#'   fit; the result records it in `ability_adjusted`.
#' @param adjustMethod How the added covariate's coefficients and the
#'   residual ability SD are estimated. `"fast"` (default) holds the fit's own
#'   coefficients and takes Newton steps for the added ones -- a regression of
#'   the posterior means on the basis, corrected for their shrinkage -- with
#'   the residual SD by variance decomposition; a few quadrature passes. It
#'   needs a covariate constant within person and a one-dimensional model, and
#'   otherwise falls back to `"full"`: every coefficient and the SD
#'   re-estimated jointly to convergence, which can take dozens of passes
#'   over the data.
#' @param effects Report DIF effect sizes (parametric forms). Each response's
#'   score is the score of a Bernoulli given the person's other responses, so
#'   the derivative of its predicted probability in the item's parameters is
#'   exact; applied to the one-step DIF estimate it gives the response's
#'   first-order change in the probability of a correct answer at the
#'   person's own ability. These are averaged per item (`effects`) and, with
#'   `purify` and a person-level covariate, summed per person over the
#'   flagged items they answered (`dtf`).
#' @param nodes,cores Passed to [scoreContributions()] when `fit` is a fit.
#'
#' @return An object of class `bigIRT_scoretest`: a list with `tests`, a
#'   data.table with one row per covariate and item (or group): the number of
#'   responses `n`, the statistic(s), degrees of freedom and p-values (with
#'   `purify`, also whether the item ended as an `anchor` and the `stage` its
#'   test comes from), with
#'   Benjamini-Hochberg (`p_fdr`) and Holm (`p_holm`) adjustments within each
#'   covariate; and, for the parametric forms, `coefficients`, a data.table of
#'   one-step estimates (`estimate`), standard errors and z values per tested
#'   parameter and basis column; and, when a covariate was added to the
#'   ability model (`adjust`), `adjustment`: its basis terms' estimated
#'   coefficients, the re-estimated ability SD against the fit's, and the
#'   iterations taken and whether they converged (a warning is given if not).
#'   With `effects`: `effects`, per item and contrast (a factor level against
#'   the reference level; a continuous covariate's 90th against its 10th
#'   percentile over responses), the mean change in the probability of a
#'   correct answer over the item's responses at equal ability, signed (`dP`)
#'   and unsigned (`dP_abs`), first-order in the DIF estimate. They are
#'   relative to the scale the other items set: with `purify`, the anchors;
#'   without, all items, which absorb any net DIF into the estimated impact.
#'   With `purify` and a person-level covariate (one-dimensional models), also
#'   `dtf`, the test-level effect of the flagged items per level or covariate
#'   quintile, relative to the reference group (the reference level, or the
#'   middle quintile): the mean change in a person's expected number correct
#'   over the items they answered (`dT_flagged`), and the ability shift that
#'   would change their expected score as much (`dtheta_flagged`) -- the bias
#'   DIF puts into a person's ability estimate when scored with these item
#'   parameters, to first order. Without anchors there is no test-level
#'   effect to report: with the covariate's ability effect estimated freely,
#'   the DIF effects of all items sum to about zero for each group.
#'
#' @seealso [scoreContributions()]
#' @export
scoreTest <- function(fit, covariates, parameters = "B",
  form = c("auto", "linear", "spline", "factor", "fluctuation"),
  by = "item", items = NULL, df = 3L, dims = NULL,
  level = c("auto", "person", "row"),
  ngrid = 50L, trim = 0.1, nsim = 1000L, seed = NULL, minN = 20L,
  purify = FALSE, purifyAlpha = 0.05, purifyMaxIter = 5L,
  information = c("expected", "observed", "opg"),
  adjust = c("auto", "none"), adjustDf = 3L, adjustMethod = c("fast", "full"),
  effects = TRUE, nodes = NULL, cores = 1L){
  information <- match.arg(information); adjust <- match.arg(adjust)
  adjustMethod <- match.arg(adjustMethod)
  form <- match.arg(form); level <- match.arg(level)
  if(isTRUE(purify) && !inherits(fit, "bigIRT_fit"))
    stop("purify needs the fit itself, to recompute the person side; pass the fit, not its scores.")
  if(isTRUE(purify) && !identical(by, "item")) stop("purify works with by = \"item\".")
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
  res <- list(); coefs <- list(); adjs <- list(); effl <- list(); ownl <- list()
  for(v in names(cv$values)){
    zall <- cv$values[[v]]
    f_v <- form
    if(f_v == "auto") f_v <- if(is.factor(zall) || is.character(zall) || is.logical(zall)) "factor" else "linear"
    if(ability){
      out <- bigIRT_score_test_ability(sc, zall, cv$level, f_v, df, dims, ngrid, trim, nsim)
    } else {
      ## A covariate the ability model lacks: add a flexible basis of it to the
      ## ability regression, the items held, and score under that model.
      adj <- identical(adjust, "auto") && inherits(fit, "bigIRT_fit") && !(v %in% sc$person_preds)
      Badj <- if(adj) bigIRT_score_adjust_basis(zall, cv$level, sc, adjustDf, v) else NULL
      if(adj && is.null(Badj)) adj <- FALSE
      in_model <- v %in% sc$person_preds
      if(adj && identical(cv$level, "person") && bigIRT_score_in_span(Badj, sc)){
        adj <- FALSE; Badj <- NULL; in_model <- TRUE
      }
      scv <- if(adj) bigIRT_score_compute(fit, nodes = nodes, cores = cores, add_preds = Badj,
        adjust_method = adjustMethod) else sc
      if(adj){
        if(!isTRUE(scv$refit_converged))
          warning(sprintf("Adding %s to the ability model did not converge in %d iterations; its tests may be miscalibrated.",
            v, scv$refit_iter), call. = FALSE)
        ab <- scv$ability_beta[, scv$adjusted, drop = FALSE]
        adjs[[v]] <- data.table::data.table(covariate = v, term = rep(colnames(ab), each = nrow(ab)),
          dim = rep(seq_len(nrow(ab)), ncol(ab)), estimate = as.numeric(ab),
          ability_sd = scv$ability_sd[rep(seq_len(nrow(ab)), ncol(ab))],
          fit_ability_sd = sc$ability_sd[rep(seq_len(nrow(ab)), ncol(ab))],
          method = scv$adjust_method, iterations = scv$refit_iter, converged = scv$refit_converged)
      }
      if(!adj && cv$level == "person" && !in_model)
        bigIRT_score_impact_check(sc, zall, v)
      out <- bigIRT_score_test_items(scv, zall, cv$level, parameters, f_v, by, items, df,
        ngrid, trim, nsim, minN, information, effects = effects)
      persons_anchor <- NULL
      if(isTRUE(purify) && !is.null(out)){
        pu <- bigIRT_score_purify(fit, out, zall, cv$level, parameters, f_v, items, df,
          ngrid, trim, nsim, minN, purifyAlpha, purifyMaxIter, nodes, cores, information,
          add_preds = Badj, effects = effects)
        ## Effects relative to the anchors when anything was flagged; with
        ## nothing flagged every item is an anchor and the first stage stands.
        if(!is.null(pu$effects)){ out$effects <- pu$effects; out$own <- pu$own }
        persons_anchor <- pu$persons_anchor %||% scv$persons
        out$tests <- pu$tests; out$coefficients <- pu$coefficients
      }
      if(!is.null(out$effects)){
        effl[[v]] <- out$effects[, covariate := v]
        ownl[[v]] <- list(own = out$own, zall = zall, level = cv$level, form = f_v,
          persons_all = scv$persons, persons_anchor = persons_anchor)
      }
      if(!is.null(out)) out$tests[, ability_adjusted := adj]
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
  eff <- if(length(effl)) data.table::rbindlist(effl) else NULL
  if(!is.null(eff)) data.table::setcolorder(eff, c("covariate", setdiff(names(eff), "covariate")))
  dtf <- data.table::rbindlist(lapply(names(ownl), function(v){
    o <- ownl[[v]]
    flagged <- if("item" %in% names(tests)) tests[covariate == v & !is.na(p_fdr) & p_fdr < .05, item] else character(0)
    d <- bigIRT_score_dtf(o$own, o$zall, o$level, o$form, flagged, o$persons_all, o$persons_anchor)
    if(!is.null(d)) d[, covariate := v]
    d
  }), fill = TRUE)
  if(nrow(dtf)) data.table::setcolorder(dtf, c("covariate", setdiff(names(dtf), "covariate")))
  structure(list(tests = tests, coefficients = if(nrow(coefficients)) coefficients else NULL,
    adjustment = if(length(adjs)) data.table::rbindlist(adjs) else NULL,
    effects = eff, dtf = if(nrow(dtf)) dtf else NULL,
    parameters = parameters, rule = sc$rule),
    class = "bigIRT_scoretest")
}

## Test-level DIF effects for a person-level covariate, which need anchors.
## With the covariate's ability effect estimated freely, its score equation
## makes the DIF effects of all items sum to about zero for each group: net
## DIF is absorbed into the estimated impact (simulated: +0.43 expected
## correct answers of true DIF read as -0.06). So they come from the anchored
## stage of `purify`, over the flagged items: per group, relative to the
## reference group (a factor's reference level; for a continuous covariate
## the middle quintile), the mean change in a person's expected number
## correct (`dT_flagged`; simulated .437 against a true .426) and the ability
## shift that would change their expected score on all their items as much
## (`dtheta_flagged`): the bias DIF puts into a person's ability estimate
## when scored with these item parameters, as in a calibrated bank
## (simulated .074 against a true .080). It is not the bias in the fit's own
## group gap, which also moves with the item parameters the DIF items
## contaminated (.10 in the same simulations); re-estimating the ability side
## on the anchors with the items held recovered under half of that, because
## the anchors' own parameters carry part of it, and is not reported. One-
## dimensional models.
bigIRT_score_dtf <- function(own, zall, zlevel, form, flagged, persons_all, persons_anchor){
  if(!identical(zlevel, "person") || is.null(persons_anchor) || is.null(own) || !nrow(own) ||
    all(is.na(own$dpdtheta))) return(NULL)
  N <- nrow(persons_anchor)
  zp <- zall[seq_len(N)]
  grp <- if(form == "factor") factor(zp) else {
    q <- unique(stats::quantile(as.numeric(zp), 0:5 / 5, na.rm = TRUE))
    cut(as.numeric(zp), q, include.lowest = TRUE)
  }
  ref <- if(form == "factor") levels(grp)[1] else levels(grp)[ceiling(nlevels(grp) / 2)]
  per <- own[, list(items = .N, dT = sum(dP[Item %in% flagged]), slope = sum(dpdtheta)), by = person]
  pg <- grp[per$person]
  ft <- tapply(per$dT, pg, mean); fth <- tapply(per$dT / per$slope, pg, mean)
  ni <- tapply(per$items, pg, mean)
  data.table::data.table(group = levels(grp), reference = levels(grp) == ref,
    persons = as.integer(table(grp)), items = as.numeric(ni[levels(grp)]),
    flagged_items = length(flagged),
    dT_flagged = as.numeric(ft[levels(grp)] - ft[[ref]]),
    dtheta_flagged = as.numeric(fth[levels(grp)] - fth[[ref]]))
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
    av <- x$adjustment[x$adjustment$covariate == v]
    if(!is.null(av) && nrow(av))
      cat(sprintf("    added to the ability model after the fit: %d terms, ability SD %s (fit: %s), %s, %d %s%s\n",
        data.table::uniqueN(av$term), paste(sprintf("%.3f", unique(av$ability_sd)), collapse = ", "),
        paste(sprintf("%.3f", unique(av$fit_ability_sd)), collapse = ", "), av$method[1], av$iterations[1],
        if(identical(av$method[1], "fast")) "steps" else "passes",
        if(isTRUE(av$converged[1])) "" else ", NOT converged"))
  }
  o <- order(t[[pcol]])
  print(utils::head(t[o], n))
  invisible(x)
}

## Iterative purification: flag, anchor on the rest (person posteriors and
## ability coefficients from unflagged items only), retest the rest, repeat.
bigIRT_score_purify <- function(fit, out, zall, zlevel, parameters, form, items, df,
  ngrid, trim, nsim, minN, alpha, maxit, nodes, cores, information = "observed",
  add_preds = NULL, effects = FALSE){
  oA <- NULL; scA <- NULL; anchored_on <- character(0)
  pcol <- if(form == "fluctuation") "p_DM" else "p"
  tst <- data.table::copy(out$tests); cf <- out$coefficients
  tst[, `:=`(anchor = TRUE, stage = 1L)]
  flag_of <- function(t) t$item[!is.na(t[[pcol]]) & stats::p.adjust(t[[pcol]], "BH") < alpha]
  flagged <- flag_of(tst)
  tst[item %in% flagged, anchor := FALSE]
  all_items <- if(is.null(items)) unique(tst$item) else as.character(items)
  it <- 0L
  while(length(flagged) && it < maxit){
    it <- it + 1L
    ## One pass: every person's posterior and the ability coefficients from
    ## the unflagged items; the unflagged items scored as usual against it,
    ## and each flagged item's responses scored against the same anchor with
    ## their own likelihood added (held-out rows, src/agq_backend.cpp).
    scA_new <- bigIRT_score_compute(fit, nodes = nodes, cores = cores, exclude = flagged, refit = TRUE,
      add_preds = add_preds)
    oA_new <- bigIRT_score_test_items(scA_new, zall, zlevel, parameters, form, "item", all_items, df,
      ngrid, trim, nsim, minN, information, effects = effects)
    if(is.null(oA_new)) break
    oA <- oA_new; scA <- scA_new; anchored_on <- flagged
    new_t <- oA$tests
    new_t[, stage := it + 1L]
    new_flag <- flag_of(new_t)
    new_t[, anchor := !(item %in% new_flag)]
    tst <- new_t
    if(!is.null(cf)) cf <- oA$coefficients
    if(setequal(new_flag, flagged)) break
    flagged <- new_flag
  }
  ## The last anchored stage: its effects are relative to the anchors, and its
  ## persons carry the ability side estimated on the anchors alone.
  list(tests = tst, coefficients = cf, effects = oA$effects, own = oA$own,
    persons_anchor = scA$persons, anchored_on = anchored_on)
}

## The basis a covariate enters the ability regression with when `adjust`
## adds it after the fit: natural spline with `df` degrees of freedom, or
## treatment indicators for a factor (or a numeric covariate with few values),
## centred, so it moves groups relative to one another and not the whole
## scale the held item parameters were estimated on. Built on the persons for
## a person-level covariate (knots from the person distribution), on the
## responses for a response-level one, and returned one row per training
## response in the order of the score contributions' rows. Missing values
## enter at the centre.
## Is an added person-level basis already in the span of the fit's person
## predictors? Matching by name misses a covariate passed under another name
## (`gender` for the fit's `gender1`), and adding it again put a redundant
## term in the ability model and more than doubled the time of a purified
## screen on Mindsteps (1,222 s against 494 s).
bigIRT_score_in_span <- function(B, sc, tol = 1e-8){
  PM <- sc$person_pred_means
  if(is.null(PM) || !length(PM)) return(FALSE)
  R <- sc$rows
  sm <- rowsum(as.matrix(B), R$person, reorder = TRUE); ii <- as.integer(rownames(sm))
  Bp <- sm / tabulate(R$person, nrow(sc$persons))[ii]
  X <- cbind(1, as.matrix(PM)[ii, , drop = FALSE])
  res <- as.matrix(stats::lm.fit(X, Bp)$residuals)
  tot <- colSums(scale(Bp, scale = FALSE)^2)
  all(colSums(res^2) <= tol * pmax(tot, .Machine$double.eps))
}

bigIRT_score_adjust_basis <- function(zall, level, sc, df, v){
  fac <- is.factor(zall) || is.character(zall) || is.logical(zall) ||
    length(unique(stats::na.omit(zall))) <= df + 1L
  if(fac){
    f <- factor(zall)
    if(nlevels(f) < 2L) return(NULL)
    B <- vapply(levels(f)[-1L], function(l) as.numeric(f == l), numeric(length(f)))
    B <- matrix(B, nrow = length(f))
  } else {
    z <- as.numeric(zall); ok <- !is.na(z)
    if(sum(ok) < 3L) return(NULL)
    B <- matrix(NA_real_, length(z), df)
    B[ok, ] <- matrix(as.numeric(splines::ns(z[ok], df = df)), nrow = sum(ok))
  }
  idx <- if(identical(level, "person")) sc$rows$person else sc$rows$row
  B <- B[idx, , drop = FALSE]
  w <- if(identical(level, "person")) 1 / tabulate(idx, length(zall))[idx] else rep(1, length(idx))
  for(j in seq_len(ncol(B))){
    ok <- !is.na(B[, j])
    B[, j] <- B[, j] - sum(B[ok, j] * w[ok]) / sum(w[ok])
    B[!ok, j] <- 0
  }
  colnames(B) <- paste0(v, ".adj", seq_len(ncol(B)))
  B
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
  ngrid, trim, nsim, minN, information = "observed", effects = FALSE){
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
  info_opg <- bigIRT_score_opg(S)
  info <- switch(information,
    opg = info_opg,
    expected = {
      if(is.null(R$ppred)) stop("These score contributions predate `ppred`; recompute them.")
      bigIRT_score_expected_info(S, R$y, R$ppred)
    },
    as.matrix(R[, info_cols, with = FALSE]))
  G <- if(form == "fluctuation") NULL else bigIRT_score_basis(z, form, df)
  if(form != "fluctuation" && is.null(G)) return(NULL)
  free <- sc$item_free
  units <- split(seq_len(nrow(R)), grp[R$item])
  tests <- list(); coefs <- list(); effs <- list()
  ## Effect contrasts: a factor's levels against its reference; a continuous
  ## covariate's 90th against its 10th percentile (over responses), and each
  ## response against the median.
  effects <- isTRUE(effects) && form != "fluctuation" && !is.null(R$ppred)
  if(effects){
    own_dP <- rep(NA_real_, nrow(R))
    if(form != "factor"){
      at <- attr(G, "at"); qz <- stats::quantile(as.numeric(z), c(.1, .5, .9), names = FALSE)
      dg <- at(qz[3]) - at(qz[1]); g_med <- as.numeric(at(qz[2]))
    }
  }
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
    Us <- 0; V <- 0; Vo <- 0; tests_ok <- TRUE; test_used <- NULL
    ## Each item's rows, split once: finding them by scanning the group per
    ## item is quadratic, which on a shared coefficient over a 15,000-item
    ## bank (20 million rows) ran for hours.
    by_item <- split(u, R$item[u])
    for(j in its){
      uj <- by_item[[as.character(j)]]
      nuis <- which(free[j, ])
      test <- match(test_names, slots); test <- test[test %in% nuis]
      if(!length(test)){ next }
      if(!is.null(test_used) && !identical(test_used, test)){ tests_ok <- FALSE; break }
      test_used <- test
      pc <- bigIRT_score_lm_pieces(G[uj, , drop = FALSE], matrix(1, length(uj), 1),
        S[uj, , drop = FALSE], info[uj, , drop = FALSE], test, nuis)
      Us <- Us + pc$Ustar; V <- V + pc$V
      Vo <- Vo + bigIRT_score_lm_pieces(G[uj, , drop = FALSE], matrix(1, length(uj), 1),
        S[uj, , drop = FALSE], info_opg[uj, , drop = FALSE], test, nuis)$V
    }
    if(!tests_ok || is.null(test_used)) next
    pv <- bigIRT_score_pinv(V)
    lm <- as.numeric(crossprod(Us, pv$inv %*% Us))
    est <- as.numeric(pv$inv %*% Us)
    se <- sqrt(pmax(diag(pv$inv), 0))
    ## Is the efficient information determined? A single response's observed
    ## information can be negative (its posterior variance exceeds its
    ## curvature), so on a thin item -- a discrimination split by a factor
    ## with few responses at one level, say -- the observed efficient
    ## information can collapse while the scores still vary, and the statistic
    ## explodes. Measured on 35 responses: 0.0007 observed against 0.46 from
    ## the outer product, LM 1257. Below a fifth of the outer-product value
    ## the comparison is reported as not determinable rather than tested.
    ro <- bigIRT_score_pinv(Vo)$root
    ratio <- if(all(ro == 0)) NA_real_ else
      min(eigen(ro %*% ((V + t(V)) / 2) %*% ro, symmetric = TRUE, only.values = TRUE)$values[seq_len(max(1L, bigIRT_score_pinv(Vo)$rank))])
    determined <- pv$rank > 0L && (!identical(information, "observed") || isTRUE(ratio >= 0.2))
    if(!determined){ lm <- NA_real_; est[] <- NA_real_; se[] <- NA_real_ }
    tests[[g]] <- data.table::data.table(item = g, parameters = paste(slots[test_used], collapse = "+"),
      form = form, n = length(u), LM = lm, df = pv$rank,
      p = if(determined) stats::pchisq(lm, pv$rank, lower.tail = FALSE) else NA_real_,
      info_ratio = ratio)
    coefs[[g]] <- data.table::data.table(item = g,
      parameter = rep(slots[test_used], each = ncol(G)),
      term = rep(colnames(G), times = length(test_used)),
      estimate = est, se = se, z = est / se)
    ## Effects on the probability scale. A response's score is the score of a
    ## Bernoulli with probability ppred given the person's other responses, so
    ## ppred's derivative in each tested slot is exactly
    ## s ppred (1 - ppred) / (y - ppred); applied to the one-step DIF
    ## estimate it is that response's first-order change in the probability
    ## of a correct answer, at the person's own ability.
    if(effects && determined){
      pp <- R$ppred[u]
      dPs <- S[u, test_used, drop = FALSE] * (pp * (1 - pp) / (R$y[u] - pp))
      E <- matrix(est, ncol(G), length(test_used))
      Gu <- G[u, , drop = FALSE]
      if(form == "factor"){
        for(l in seq_len(ncol(G))){
          rl <- Gu[, l] == 1
          if(!any(rl)) next
          d <- as.numeric(dPs[rl, , drop = FALSE] %*% E[l, ])
          effs[[length(effs) + 1L]] <- data.table::data.table(item = g,
            contrast = paste(colnames(G)[l], "vs reference"), n = sum(rl), dP = mean(d), dP_abs = mean(abs(d)))
        }
        own_dP[u] <- rowSums(dPs * (Gu %*% E))
      } else {
        d <- as.numeric(dPs %*% as.numeric(dg %*% E))
        effs[[length(effs) + 1L]] <- data.table::data.table(item = g,
          contrast = sprintf("%.3g vs %.3g (90th vs 10th percentile)", qz[3], qz[1]),
          n = length(u), dP = mean(d), dP_abs = mean(abs(d)))
        own_dP[u] <- rowSums(dPs * (sweep(Gu, 2, g_med) %*% E))
      }
    }
  }
  if(!length(tests)) return(NULL)
  out <- list(tests = data.table::rbindlist(tests, fill = TRUE),
    coefficients = if(length(coefs)) data.table::rbindlist(coefs) else NULL)
  if(effects && length(effs)){
    out$effects <- data.table::rbindlist(effs)
    out$own <- data.table::data.table(person = R$person, Item = R$Item, dP = own_dP,
      dpdtheta = if(!is.null(R$dpdtheta)) R$dpdtheta else NA_real_)[!is.na(dP)]
  }
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
