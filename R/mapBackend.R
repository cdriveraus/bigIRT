## Joint MAP: the posterior mode over item parameters and person abilities
## together, by profiling the persons out (src/map_backend.cpp).
##
## The objective over the item-side parameters is
##
##   f(psi) = sum_i max_theta_i lp_i(theta_i; psi) + log p(psi),
##
## with lp_i the same person kernel the Laplace path maximises. That is the
## Laplace objective without its log-determinant, so it runs through the same
## machinery: the parameter layout and scaling, the Fisher-shaped L-BFGS (its
## metric is the Schur complement of the joint information over the persons,
## which is this objective's curvature), the empirical-Bayes rounds, the
## convergence certificate and the run-off screen. What differs is confined to
## the evaluation below, which bigIRT_laplace_direct_objective() hands over to
## whenever `context$map` is set.
##
## Its gradient is exact without an adjoint: at each person's mode the
## derivative of lp_i in theta_i is zero, so only the partial derivatives in
## psi survive (the envelope theorem). The value and the gradient are checked
## against central differences in tests/testthat/test-map.R.

bigIRT_map_fg_cpp_impl <- function(id, score, theta_init, person_pred,
  fixed_ability, fixed_ability_value, A_ref, A_fixed_value, A_pred, A_signed,
  B_ref, B_fixed_value, B_pred, C_ref, C_fixed_value, C_pred,
  D_ref, D_fixed_value, D_pred, Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta,
  logitCpars, logitCbeta, logitDpars, logitDbeta, prior_mean, prior_precision,
  free_mask, jitter, max_attempts, max_iter, tol, keep_covariance = FALSE,
  grain_size = 64L, want_row_effective = FALSE, block_items = integer(0)){
  nr <- length(id); K <- ncol(theta_init)
  .Call(`_bigIRT_map_fg_cpp_impl`,
    as.integer(id), as.integer(score), as.matrix(theta_init), as.matrix(person_pred),
    bigirt_int_matrix(fixed_ability, nr, K), as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nr, K), as.matrix(A_fixed_value), as.matrix(A_pred),
    as.integer(A_signed),
    as.integer(B_ref), as.numeric(B_fixed_value), as.matrix(B_pred),
    as.integer(C_ref), as.numeric(C_fixed_value), as.matrix(C_pred),
    as.integer(D_ref), as.numeric(D_fixed_value), as.matrix(D_pred),
    as.matrix(Abilitybeta), as.numeric(invspApars), as.matrix(invspAbeta),
    as.numeric(Bpars), as.matrix(Bbeta), as.numeric(logitCpars), as.matrix(logitCbeta),
    as.numeric(logitDpars), as.matrix(logitDbeta), as.matrix(prior_mean), prior_precision,
    bigirt_int_matrix(free_mask, nrow(theta_init), K),
    as.numeric(jitter), as.integer(max_attempts), as.integer(max_iter), as.numeric(tol),
    as.logical(keep_covariance)[1], as.integer(grain_size), as.logical(want_row_effective)[1],
    as.integer(block_items))
}

## One evaluation of the joint-MAP objective at `par`, in the optimiser's
## coordinates: the person modes are solved inside it, warm-started from
## `theta_init`. Returns what bigIRT_laplace_direct_objective() returns -- value,
## gradient (`approx_grad`, exact here), the unpacked state, the person
## posterior at the modes and the item prior -- so that every consumer of an
## evaluation (optimiser, metric, certificate, information) takes either.
## `context$map_blocks` asks for the item blocks of the Hessian.
bigIRT_map_objective <- function(par, state, sdat, prior_precision,
  theta_init = state$AbilityBase, jitter = 1e-6, max_attempts = 8L,
  max_iter = 50L, tol = 1e-6, keep_covariance = FALSE, context){
  t0 <- as.numeric(proc.time()[["elapsed"]])
  lay <- context$direct_layout
  st <- bigIRT_laplace_unpack_direct_state(par, state, sdat, layout = lay,
    corr_paramization = context$corr_paramization %||% "normalized_chol")
  rc <- context$row_context
  prior_mean <- matrix(rep(as.numeric(st$AbilityMean), each = sdat$Nsubs), nrow = sdat$Nsubs)
  prior_mats <- list(corr = as.matrix(sdat$AbilityCorr),
    precision = if(length(dim(prior_precision)) == 3L) prior_precision[, , 1] else prior_precision,
    precision_array = prior_precision)
  fg <- bigIRT_map_fg_cpp_impl(
    id = rc$ids_int %||% rc$ids, score = context$score_rows %||% sdat$score[context$train_rows],
    theta_init = theta_init, person_pred = rc$person_pred,
    fixed_ability = rc$fixed_ability_int %||% rc$fixed_ability,
    fixed_ability_value = rc$fixed_ability_value,
    A_ref = rc$A_ref, A_fixed_value = rc$fixed_A_value, A_pred = rc$A_pred,
    A_signed = rc$A_signed %||% bigIRT_laplace_signed_A(sdat),
    B_ref = rc$B_ref, B_fixed_value = rc$B_fixed_value, B_pred = rc$B_pred,
    C_ref = rc$C_ref, C_fixed_value = rc$C_fixed_value, C_pred = rc$C_pred,
    D_ref = rc$D_ref, D_fixed_value = rc$D_fixed_value, D_pred = rc$D_pred,
    Abilitybeta = st$Abilitybeta, invspApars = st$invspApars, invspAbeta = st$invspAbeta,
    Bpars = st$Bpars, Bbeta = st$Bbeta, logitCpars = st$logitCpars, logitCbeta = st$logitCbeta,
    logitDpars = st$logitDpars, logitDbeta = st$logitDbeta,
    prior_mean = prior_mean, prior_precision = prior_precision,
    free_mask = 1L - sdat$fixedAbilityLogical, jitter = jitter, max_attempts = max_attempts,
    max_iter = max_iter, tol = tol, keep_covariance = keep_covariance,
    grain_size = context$grain_size %||% 64L,
    ## The polish and the certificate ask for covariances, and both would
    ## otherwise rebuild these rows in R.
    want_row_effective = keep_covariance,
    block_items = if(isTRUE(context$map_blocks)) rc$items else integer(0))
  ig <- fg$item_fg
  ## The item prior covers the item parameters, their means and every
  ## covariate coefficient (ability ones included).
  prior <- bigIRT_laplace_item_prior(st, sdat, layout = lay)
  grad <- numeric(length(par))
  add <- function(idx, g) if(length(idx)) grad[idx] <<- grad[idx] + as.numeric(g)[seq_along(idx)]
  add(lay$A, ig$grad_A); add(lay$B, ig$grad_B); add(lay$C, ig$grad_C); add(lay$D, ig$grad_D)
  if(sdat$NAitemPreds > 0) add(lay$A_beta, ig$grad_A_beta)
  if(sdat$NBitemPreds > 0) add(lay$B_beta, ig$grad_B_beta)
  if(sdat$NCitemPreds > 0) add(lay$C_beta, ig$grad_C_beta)
  if(sdat$NDitemPreds > 0) add(lay$D_beta, ig$grad_D_beta)
  if(length(prior$grad)) grad[seq_along(prior$grad)] <- grad[seq_along(prior$grad)] + prior$grad
  add(lay$ability_beta, bigIRT_beta_collapse(ig$grad_ability_beta, sdat, "sum"))
  if(length(lay$ability_mean)){
    g_mu <- as.numeric(ig$grad_mean)
    if(isTRUE(as.logical(sdat$dopriors))){
      mu <- as.numeric(st$AbilityMean)
      mu0 <- rep_len(as.numeric(sdat$AbilityMeandat), length(mu))
      mu_sd <- pmax(rep_len(as.numeric(sdat$AbilityMeanSD), length(mu)), 1e-8)
      prior$value <- prior$value + sum(stats::dnorm(mu, mean = mu0, sd = mu_sd, log = TRUE))
      g_mu <- g_mu - (mu - mu0) / (mu_sd^2)
    }
    add(lay$ability_mean, g_mu)
  }
  ## Into the optimiser's coordinates (bigIRT_laplace_par_scale).
  ps <- attr(lay, "par_scale")
  if(length(ps) == length(grad)) grad <- grad * ps
  sec <- as.numeric(proc.time()[["elapsed"]]) - t0
  list(value = fg$value + prior$value, laplace2_skipped = NA_integer_,
    approx_grad = grad, state = st, prior_mats = prior_mats, posterior = fg$posterior,
    prior = prior, rowEffective = NULL, item_fg = ig,
    timings = list(totalSec = sec, kernelSec = fg$timings$kernelSec,
      personKernelSec = fg$timings$kernelSec, itemKernelSec = 0,
      postKernelSec = sec - fg$timings$kernelSec))
}

## The marginal log-likelihood of the model at a joint-MAP fit's estimates.
##
## Joint MAP has no marginal of its own, but it estimates the same model as
## the Laplace path, whose likelihood integrates the abilities out; logLik()
## and the information criteria report that, at the MAP estimates, so fits by
## either estimator are compared on one scale. `rule` is the refinement the
## Laplace path would use (fitIRT's laplaceRefine): an AGQ rule, the
## second-order Laplace marker, or NULL for plain Laplace. The correlation is
## held at its value, as a hyperparameter. Returns the value, the item prior
## density it includes, and the approximation used.
bigIRT_map_marginal <- function(state, sdat, prior_precision, theta_init, rule = NULL,
  cores = 1L, jitter = 1e-6, person_tol = 1e-6, corr_paramization = "normalized_chol"){
  layout <- bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
  ilay <- layout[setdiff(names(layout), c("corr", "ability_mean", "ability_beta"))]
  ctx <- bigIRT_laplace_item_context(sdat, layout = ilay)
  ctx$grain_size <- bigIRT_laplace_subject_grain(sdat$Nsubs, cores)
  ctx$direct_layout <- layout
  ctx$estimateAbilityCorr <- FALSE
  ctx$corr_paramization <- corr_paramization
  ctx$score_rows <- as.integer(sdat$score[ctx$train_rows])
  approx <- "laplace"
  if(inherits(rule, "bigIRT_agq_rules")){ ctx$agq <- rule; approx <- "agq" }
  else if(inherits(rule, "bigIRT_laplace2")){ ctx$laplace2 <- TRUE; approx <- "laplace2" }
  state$laplaceCorrParam <- corr_paramization
  par <- bigIRT_laplace_pack_direct_state(state, sdat, layout = layout)
  ev <- bigIRT_laplace_direct_objective(par = par, state = state, sdat = sdat,
    prior_precision = prior_precision, theta_init = theta_init, jitter = jitter,
    max_iter = 200L, tol = person_tol, keep_covariance = TRUE, context = ctx)
  list(value = ev$value, item_prior = as.numeric(ev$prior$value), approx = approx)
}

## Why joint MAP does not estimate the latent correlation.
##
## fitIRT refuses estimateAbilityCorr under marginalApprox = "map", because no
## way of estimating it alongside the joint mode was found to work, and each
## failure looked like an estimate.
##
## As a coordinate of the objective it is degenerate. Profiling the persons out
## leaves N/2 log|P0| from the prior's normalising constant, which goes to
## infinity as a correlation goes to one while the likelihood stays bounded if
## each person's abilities become equal. (The Laplace objective's
## log-determinant charges for the lost posterior volume, which is why it can.)
##
## As a hyperparameter set by EM from the person posteriors it is biased, and
## the reason is the joint mode's metric. The likelihood is unchanged by
## scaling every loading up and every ability down together, so under joint
## MAP the latent scale is set by the priors alone: the persons' N(0, 1) prior
## pulls the abilities in, the items' loading prior pushes back, and the more
## persons per item the further the abilities shrink. On simulated 2PL data
## with true loadings near 1.2, joint-mode loadings averaged 2.3 to 3.8 in one
## dimension (the Stan implementation this replaced gave identical values) and
## 7.3 in two. Against a prior that is then far wider than the abilities it
## describes, the moment identities EM rests on fail. Measured, for true
## correlations of .6 and -.3 where the Laplace estimator found .61 and -.28:
## the M-step with the SDs held at the prior's ran to .999 and -.999; the
## parameter-expanded M-step, R = cov2cor(sum_i m_i m_i' + V_i), settled at .39
## to .50 and -.17. Freeing the SDs as well spirals, the joint mode shrinking
## the abilities further each round.
