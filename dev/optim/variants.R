# Optimiser variants for the Laplace path, installed into the bigIRT namespace
# at run time so the package's own front end (data preparation, layout,
# starting state, output assembly) is shared by every variant and only the
# optimiser differs.
#
#   base       the package as installed: mize L-BFGS + block-Newton polish
#   mize_unit  the same, but mize's per-iteration initial step is 1 (the
#              quasi-Newton step) instead of its default quadratic-interpolation
#              guess, which is capped at 1 and is usually below it
#   lb_*       own L-BFGS (proto_lbfgs below): unit step, Armijo backtracking
#              with quadratic interpolation, first step shortened only.
#              _id    H0 = gamma I (par_scale coordinates, as mize sees them)
#              _info  H0 = gamma M^-1, M the block-diagonal item information
#                     the polish already builds (the metric sets the SHAPE,
#                     the secant ratio gamma = s'y / y'M^-1 y its SCALE)
#              _gain  stop on predicted gain 0.5 g'Hg in nats, not on
#                     |delta f| and ||g||, then the package's Newton polish

VARIANTS <- list(
  base = list(),
  mize_unit = list(mize_args = list(step_next_init = 1)),
  lb_id = list(proto = list(metric = FALSE, stop = "pkg")),
  lb_info = list(proto = list(metric = TRUE, stop = "pkg")),
  lb_id_gain = list(proto = list(metric = FALSE, stop = "gain")),
  lb_info_gain = list(proto = list(metric = TRUE, stop = "gain")),
  ## References: run far past any practical stopping rule, to find the optimum
  ## every variant is scored against.
  ref = list(proto = list(metric = TRUE, stop = "gain", gain_tol = 1e-9,
    maxit_mult = 10, polish_steps = 40L, polish_tol = 1e-12)),
  ## Fisher scoring on the item block with the person-coupled information
  ## (fisher.R), from the start, to a predicted gain of 1e-6 nats. No polish:
  ## the polish is a weaker version of the same step.
  nt = list(proto = list(method = "newton", gain_tol = 1e-6, polish_steps = 0L)),
  ## L-BFGS (info metric) to a predicted gain of 1 nat, then Fisher scoring.
  lb_nt = list(proto = list(method = "hybrid", metric = TRUE, stop = "gain",
    gain_tol = 1, nt_gain_tol = 1e-6, polish_steps = 0L)),
  ## L-BFGS with H0 = gamma F^-1: the Fisher matrix sets the shape. Measured
  ## condition of F^-1 H at the optimum 2.5 (2PL + covariates) and 4.6 (3PL),
  ## against 13.5 and 106 for the block-diagonal metric -- a good
  ## preconditioner but not a Newton Hessian (eigenvalues up to 2.3), which is
  ## what the secant pairs correct. Stops on the predicted gain; no polish.
  lb_fisher = list(proto = list(metric = "fisher", stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L)),
  ## The same, with the person modes solved to 1e-6 instead of 1e-4, which
  ## takes the warm-start noise in the objective from ~1e-3 to ~1e-5 nats.
  lb_fisher_pt6 = list(proto = list(metric = "fisher", stop = "gain", gain_tol = 1e-4,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  base_pt6 = list(fit = list(laplacePersonTol = 1e-6)),
  ## The ported optimiser, run through fitIRT's own argument, on a build that
  ## has R/laplaceOptimizer.R. No namespace replacement: this is the package.
  pkg_fisher = list(fit = list(laplaceOptimizer = "fisher")),
  pkg_mize = list(fit = list(laplaceOptimizer = "mize")),
  ## The Fisher metric built once, at the start, and never refreshed; and
  ## refreshed every 5 iterations. The build is the dominant cost when items
  ## are many (about 6 s at 3000 parameters, dev1, against 0.4 s per
  ## evaluation), so whether refreshing pays is worth knowing.
  pkg_fisher_r0 = list(fit = list(laplaceOptimizer = "fisher"), opts = list(bigIRT.fisher_refresh = 0L)),
  pkg_fisher_r5 = list(fit = list(laplaceOptimizer = "fisher"), opts = list(bigIRT.fisher_refresh = 5L)),
  pkg_fisher_pt6 = list(fit = list(laplaceOptimizer = "fisher", laplacePersonTol = 1e-6)),
  ## Scale metrics (scale_metric.R): L-BFGS with H0 = gamma M^-1, stop on
  ## predicted gain 1e-3, no polish, person modes to 1e-6.
  sm_bd = list(proto = list(metric = "sm_bd", stop = "gain", gain_tol = 1e-3, polish_steps = 0L),
    fit = list(laplacePersonTol = 1e-6)),
  sm_bd_g4 = list(proto = list(metric = "sm_bd", stop = "gain", gain_tol = 1e-4, polish_steps = 0L),
    fit = list(laplacePersonTol = 1e-6)),
  ## The package's Fisher path (dense metric where allowed, mize fallback)
  ## at the tolerances this round proposes: predicted gain 1e-4, modes 1e-6.
  pkg_fisher_g4 = list(fit = list(laplaceOptimizer = "fisher", laplaceTol = 1e-4,
    laplacePersonTol = 1e-6)),
  pkg_mize_g4 = list(fit = list(laplaceOptimizer = "mize", laplaceTol = 1e-4,
    laplacePersonTol = 1e-6)),
  ## The package as it stands, defaults only (for the final comparison; run
  ## against the new build), with and without empirical Bayes.
  pkg_default = list(fit = list()),
  pkg_default_eb = list(fit = list(ebayes = TRUE)),
  ## The Laplace path alone, without the quadrature refinement that the
  ## default now applies to free asymptotes.
  pkg_norefine = list(fit = list(laplaceRefine = "none")),
  ## Where the Laplace stage hands over to the quadrature refinement: 1e-4 is
  ## the full Laplace tolerance (no early switch), 0.1 the default.
  pkg_sw4 = list(fit = list(), opts = list(bigIRT.agq_switch_gain = 1e-4)),
  pkg_sw1 = list(fit = list(), opts = list(bigIRT.agq_switch_gain = 1)),
  ## Quadrature nodes re-centred at every evaluation instead of at each metric
  ## refresh (the default holds them fixed in between).
  pkg_movenodes = list(fit = list(), opts = list(bigIRT.agq_fixed_nodes = FALSE)),
  ## Empirical Bayes with the quadrature refinement in every round, not only
  ## the final one.
  pkg_default_eb_allq = list(fit = list(ebayes = TRUE), opts = list(bigIRT.agq_eb_rounds = TRUE)),
  pkg_norefine_eb = list(fit = list(laplaceRefine = "none", ebayes = TRUE)),
  pkg_stage = list(fit = list(), opts = list(bigIRT.laplace_stage_asymptotes = TRUE)),
  ## Threaded kernel: the evaluations get faster and the metric build (R,
  ## single-threaded) takes a larger share.
  pkg_default_c8 = list(fit = list(cores = 8L)),
  pkg_mize_c8 = list(fit = list(laplaceOptimizer = "mize", cores = 8L)),
  pkg_mize_eb_new = list(fit = list(laplaceOptimizer = "mize", ebayes = TRUE)),
  sm_add100 = list(proto = list(metric = "sm_add", m = 100L, stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  sm_bal1 = list(proto = list(metric = "sm_bal", m = 1L, stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  sm_bal30 = list(proto = list(metric = "sm_bal", m = 30L, stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  sm_bal100 = list(proto = list(metric = "sm_bal", m = 100L, stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  sm_bal300 = list(proto = list(metric = "sm_bal", m = 300L, stop = "gain", gain_tol = 1e-3,
    polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  sm_pcg = list(proto = list(metric = "sm_pcg", m = 100L, rtol = 1e-3, stop = "gain",
    gain_tol = 1e-3, polish_steps = 0L), fit = list(laplacePersonTol = 1e-6)),
  ## Reference for the scale regimes: tight everything.
  sm_ref = list(proto = list(metric = "sm_bal", m = 100L, stop = "gain", gain_tol = 1e-7,
    maxit_mult = 4, polish_steps = 0L), fit = list(laplacePersonTol = 1e-8)),
  pkg_fisher_pt4 = list(fit = list(laplaceOptimizer = "fisher", laplacePersonTol = 1e-4)),
  pkg_fisher_pt8 = list(fit = list(laplaceOptimizer = "fisher", laplacePersonTol = 1e-8)),
  pkg_mize_pt6 = list(fit = list(laplaceOptimizer = "mize", laplacePersonTol = 1e-6)),
  ## End to end with the package default ebayes = TRUE: several coarse rounds
  ## and a final pass. The objectives differ between the two (each ends with
  ## its own hyperparameters), so only cost is comparable, plus how far apart
  ## the final hyperparameters land.
  pkg_fisher_eb = list(fit = list(laplaceOptimizer = "fisher", ebayes = TRUE)),
  pkg_mize_eb = list(fit = list(laplaceOptimizer = "mize", ebayes = TRUE)),
  lb_info_pt6 = list(proto = list(metric = TRUE, stop = "gain", gain_tol = 1e-4),
    fit = list(laplacePersonTol = 1e-6))
)

variant_fit_args <- function(v) VARIANTS[[v]]$fit %||% list()

install_variant <- function(v){
  spec <- VARIANTS[[v]]
  if(is.null(spec)) stop("unknown variant ", v)
  if(!is.null(spec$opts)) options(spec$opts)
  if(!is.null(spec$mize_args)){
    mns <- asNamespace("mize")
    orig_mize <- get("mize", mns)
    extra <- spec$mize_args
    wrapper <- function(...){
      a <- list(...)
      a[names(extra)] <- extra
      do.call(orig_mize, a)
    }
    unlockBinding("mize", mns); assign("mize", wrapper, envir = mns); lockBinding("mize", mns)
  }
  if(!is.null(spec$proto)){
    options(bigirt.proto = spec$proto)
    ns <- asNamespace("bigIRT")
    ## All three are evaluated in the package namespace so they can call its
    ## internals; they are then reached through the global bindings, which is
    ## why the metric and L-BFGS are rebound here as well.
    environment(proto_optimize_direct) <- ns
    environment(proto_metric) <- ns
    environment(proto_lbfgs) <- ns
    environment(proto_fisher_hessian) <- ns
    if(exists("sm_ops", envir = globalenv())){
      for(f in c("sm_ops", "sm_coarse", "sm_precond", "sm_pcg", "sm_minv", "tabulate_sum", "sm_row_q", "sm_block_inverse")){
        g <- get(f, envir = globalenv()); environment(g) <- ns; assign(f, g, envir = globalenv())
      }
    }
    environment(proto_newton) <- ns
    environment(proto_fd_columns) <- ns
    assign("proto_fisher_hessian", proto_fisher_hessian, envir = globalenv())
    assign("proto_newton", proto_newton, envir = globalenv())
    assign("proto_fd_columns", proto_fd_columns, envir = globalenv())
    assign("proto_metric", proto_metric, envir = globalenv())
    assign("proto_lbfgs", proto_lbfgs, envir = globalenv())
    assign("proto_optimize_direct", proto_optimize_direct, envir = globalenv())
    unlockBinding("bigIRT_laplace_optimize_direct", ns)
    assign("bigIRT_laplace_optimize_direct", proto_optimize_direct, envir = ns)
    lockBinding("bigIRT_laplace_optimize_direct", ns)
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if(is.null(a)) b else a

## ---------------------------------------------------------------- the metric
## Block-diagonal curvature of the MINIMISED objective in the optimiser's
## (par_scale) coordinates: one P x P block per item from the item information
## plus prior, and the exact prior curvature for the mean hyperparameters.
## Coordinates with no analytic curvature (person-covariate coefficients,
## correlations) get a diagonal entry from `other_diag`. Returns a function
## applying M^-1, and the blocks it used.
proto_metric <- function(ev, sdat, context, layout, other_diag = NULL){
  re <- ev$item_fg$row_effective
  if(is.null(re))
    re <- bigIRT_laplace_row_effective(state = ev$state, sdat = sdat,
      thetaBase = ev$posterior$theta_mode, rows = context$train_rows,
      context = context$row_context)
  bl <- bigIRT_item_info_blocks(state = ev$state, sdat = sdat, context = context,
    row_effective = re, posterior = ev$posterior, thetaBase = ev$posterior$theta_mode)
  n <- length(ev$approx_grad)
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  blocks <- list(); covered <- logical(n)
  if(!is.null(bl)){
    P <- bl$P
    for(j in seq_len(bl$ni)){
      idx <- vapply(bl$active, function(b) layout[[b]][j], integer(1))
      if(anyNA(idx) || any(idx < 1L)) next
      H <- matrix(bl$info[, , j], P, P) + diag(bl$prior_precision[, j], P)
      H <- H * outer(ps[idx], ps[idx])
      ch <- tryCatch(chol(H), error = function(e) NULL)
      if(is.null(ch)) next
      blocks[[length(blocks) + 1L]] <- list(idx = idx, chol = ch)
      covered[idx] <- TRUE
    }
  }
  dg <- rep(NA_real_, n)
  mean_blocks <- list(B_mean = c("BSDx", "BMeanSD"), A_mean = c("invspASD", "AMeanSD"),
    C_mean = c("logitCSD", "logitCMeanSD"), D_mean = c("logitDSD", "logitDMeanSD"))
  nfree <- c(B_mean = length(ev$state$Bpars), A_mean = length(ev$state$invspApars),
    C_mean = length(ev$state$logitCpars), D_mean = length(ev$state$logitDpars))
  for(nm in names(mean_blocks)){
    idx <- layout[[nm]]
    if(!length(idx) || nfree[[nm]] == 0L) next
    sd_b <- as.numeric(sdat[[mean_blocks[[nm]][1]]])[1]
    mu_sd <- as.numeric(sdat[[mean_blocks[[nm]][2]]])[1]
    h <- nfree[[nm]] / sd_b^2 + if(isTRUE(mu_sd > 0)) 1 / mu_sd^2 else 0
    dg[idx] <- h * ps[idx]^2
  }
  if(!is.null(other_diag)) dg[!is.na(other_diag) & is.na(dg) & !covered] <-
    other_diag[!is.na(other_diag) & is.na(dg) & !covered]
  ## Anything still uncovered: the median diagonal, so it is at least on the
  ## scale of the rest; the secant ratio corrects the overall level.
  bd <- unlist(lapply(blocks, function(b) diag(crossprod(b$chol))))
  fill <- if(length(bd)) stats::median(bd) else 1
  miss <- !covered & (is.na(dg) | !(dg > 0))
  dg[miss] <- fill
  solve_fn <- function(v){
    out <- v
    for(b in blocks) out[b$idx] <- backsolve(b$chol, forwardsolve(t(b$chol), v[b$idx]))
    nb <- !covered
    out[nb] <- v[nb] / dg[nb]
    out
  }
  list(solve = solve_fn, nblocks = length(blocks), uncovered = which(!covered))
}

## ---------------------------------------------------------------- L-BFGS
## Minimises fg(x)$f. H0 = gamma * Minv; with no metric Minv = identity.
## First step (no pairs): s = -Minv g scaled to length first_len in the metric
## norm when there is no metric, and the full metric (block-Newton) step when
## there is one -- that step already has the right scale.
proto_lbfgs <- function(x0, fg, minv = NULL, memory = 20L, maxit = 500L,
  c1 = 1e-4, maxback = 30L, first_len = 1, stop = c("pkg", "gain"),
  abs_tol = 1e-3, grad_tol = 1e-3, gain_tol = 0.1, refresh = NULL,
  refresh_every = 0L){
  stop <- match.arg(stop)
  S <- list(); Y <- list(); rho <- numeric(0)
  have_metric <- !is.null(minv)
  mv <- if(have_metric) minv else function(v) v
  hmul <- function(q){
    k <- length(S)
    if(k == 0L) return(h0 * mv(q))
    a <- numeric(k)
    for(i in k:1){ a[i] <- rho[i] * sum(S[[i]] * q); q <- q - a[i] * Y[[i]] }
    y <- Y[[k]]; my <- mv(y)
    gamma <- sum(S[[k]] * y) / sum(y * my)
    r <- gamma * mv(q)
    for(i in 1:k){ b <- rho[i] * sum(Y[[i]] * r); r <- r + (a[i] - b) * S[[i]] }
    r
  }
  x <- x0; e <- fg(x); f <- e$f; g <- e$g
  h0 <- if(have_metric) 1 else first_len / max(sum(abs(g)), 1e-300)
  it <- 0L; nls <- 0L; term <- "max_iter"; gain <- NA_real_
  nback_total <- 0L; retried <- FALSE
  while(it < maxit){
    d <- -hmul(g)
    dphi <- sum(g * d)
    if(!is.finite(dphi) || dphi >= 0){
      if(!length(S)){ term <- "not_descent"; break }
      S <- list(); Y <- list(); rho <- numeric(0); next
    }
    gain <- -0.5 * dphi
    if(stop == "gain" && gain < gain_tol){ term <- "gain"; break }
    alpha <- 1
    xn <- x + d; en <- fg(xn); fn <- en$f
    ok <- is.finite(fn) && fn <= f + c1 * alpha * dphi
    k <- 0L
    while(!ok && k < maxback){
      k <- k + 1L
      den <- 2 * (fn - f - dphi * alpha)
      tr <- if(is.finite(fn) && den > 0) -dphi * alpha^2 / den else 0.5 * alpha
      alpha <- min(max(tr, 0.1 * alpha), 0.5 * alpha)
      xn <- x + alpha * d; en <- fg(xn); fn <- en$f
      ok <- is.finite(fn) && fn <= f + c1 * alpha * dphi
    }
    nback_total <- nback_total + k
    if(!ok){
      if(!retried && length(S)){ retried <- TRUE; S <- list(); Y <- list(); rho <- numeric(0); next }
      term <- "linesearch_failed"; break
    }
    retried <- FALSE
    it <- it + 1L
    s <- xn - x; yv <- en$g - g
    sy <- sum(s * yv)
    if(sy > 1e-12 * sqrt(sum(s^2) * sum(yv^2))){
      S[[length(S) + 1L]] <- s; Y[[length(Y) + 1L]] <- yv; rho <- c(rho, 1 / sy)
      if(length(S) > memory){ S <- S[-1]; Y <- Y[-1]; rho <- rho[-1] }
    }
    df <- abs(f - fn)
    x <- xn; f <- fn; g <- en$g
    if(stop == "pkg"){
      if(sqrt(sum(g^2)) <= grad_tol){ term <- "grad_tol"; break }
      if(df < abs_tol){ term <- "abs_tol"; break }
    }
    if(refresh_every > 0L && it %% refresh_every == 0L && !is.null(refresh)){
      m <- refresh(en)
      if(!is.null(m)) mv <- m
    }
  }
  list(par = x, f = f, g = g, iter = it, terminate = list(what = term, val = gain),
    backtracks = nback_total)
}

## ---------------------------------------------------------------- replacement
## Same contract as bigIRT_laplace_optimize_direct; the package's own polish and
## return shape, with the optimiser swapped.
proto_optimize_direct <- function(state, sdat, prior_precision,
  niter = 50L, tol = 1e-4, jitter = 1e-6, person_tol = 1e-4,
  keep_covariance = FALSE, cores = 1L, estimateAbilityCorr = FALSE,
  corr_paramization = c("normalized_chol", "stan_corsqrt"),
  collect_history = FALSE, plot_callback = NULL, plot_every = 1L,
  verbose = 0L, trace_fn = NULL, stochastic = FALSE, polish_steps = 8L,
  polish_tol = 1e-8, optimizer = NULL){
  P <- getOption("bigirt.proto")
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
  if(!is.null(P$polish_steps)) polish_steps <- P$polish_steps
  if(!is.null(P$polish_tol)) polish_tol <- P$polish_tol
  use_metric <- isTRUE(P$metric) || identical(P$metric, "fisher")
  kc <- keep_covariance || use_metric || polish_steps > 0L
  theta_warm <- state$AbilityBase
  eval_count <- 0L
  cache <- new.env()
  ev_at <- function(par){
    if(!is.null(cache$par) && identical(as.numeric(par), cache$par)) return(cache$ev)
    eval_count <<- eval_count + 1L
    res <- bigIRT_laplace_direct_objective(par = par, state = state, sdat = sdat,
      prior_precision = prior_precision, theta_init = theta_warm, jitter = jitter,
      max_attempts = 8L, max_iter = max(20L, as.integer(niter * 2L)), tol = person_tol,
      keep_covariance = kc, context = context)
    theta_warm <<- res$posterior$theta_mode
    cache$par <- as.numeric(par); cache$ev <- res
    res
  }
  fg <- function(par){
    r <- ev_at(par)
    list(f = -r$value, g = -r$approx_grad, ev = r, x = par)
  }
  ## Coordinates with no analytic curvature: one forward difference of the
  ## gradient each, at the start. They are few (covariate coefficients and
  ## correlations), and each costs one evaluation.
  n <- length(init)
  other <- c(direct_layout$ability_beta, direct_layout$corr, direct_layout$ability_mean)
  minv <- NULL; refresh <- NULL; t_metric <- 0
  hess <- function(ev, x, g){
    H <- proto_fisher_hessian(ev, sdat, context, direct_layout)
    und <- attr(H, "undescribed")
    if(length(und)) H <- proto_fd_columns(H, x, g, function(xp) -ev_at(xp)$approx_grad, und)
    H
  }
  ## H0 = gamma F^-1, F the person-coupled Fisher information (fisher.R),
  ## refreshed at the accepted point every `refresh_every` iterations.
  fisher_solve <- function(ev, x){
    H <- hess(ev, x, -ev$approx_grad)
    H <- (H + t(H)) / 2
    ch <- tryCatch(chol(H), error = function(e) NULL)
    if(is.null(ch)){
      eg <- eigen(H, symmetric = TRUE)
      lam <- pmax(eg$values, 1e-8 * max(abs(eg$values)))
      V <- eg$vectors
      return(function(v) as.numeric(V %*% (crossprod(V, v) / lam)))
    }
    function(v) backsolve(ch, forwardsolve(t(ch), v))
  }
  if(identical(P$metric, "fisher")){
    ev0 <- ev_at(init)
    tm <- proc.time()[["elapsed"]]
    minv <- fisher_solve(ev0, init)
    t_metric <- proc.time()[["elapsed"]] - tm
    ev_at(init)
    refresh <- function(en){
      tm <- proc.time()[["elapsed"]]
      m <- tryCatch(fisher_solve(en$ev, en$x), error = function(e) NULL)
      t_metric <<- t_metric + proc.time()[["elapsed"]] - tm
      ev_at(en$x)
      m
    }
  }
  sm_type <- if(is.character(P$metric) && startsWith(P$metric, "sm_")) sub("^sm_", "", P$metric) else NULL
  if(!is.null(sm_type)){
    ## Matrix-free / sparse metrics (scale_metric.R), rebuilt at the accepted
    ## point every refresh_every iterations.
    build <- function(e){
      tm <- proc.time()[["elapsed"]]
      m <- tryCatch(sm_minv(e, sdat, context, direct_layout, type = sm_type,
        m = P$m %||% 100L, rtol = P$rtol %||% 1e-3), error = function(err){
          message("sm metric failed: ", conditionMessage(err)); NULL })
      t_metric <<- t_metric + proc.time()[["elapsed"]] - tm
      m
    }
    minv <- build(ev_at(init))
    refresh <- function(en) build(en$ev)
  }
  if(isTRUE(P$metric)){
    ev0 <- ev_at(init)
    od <- rep(NA_real_, n)
    for(j in other){
      h <- 1e-4 * max(1, abs(init[j]))
      xp <- init; xp[j] <- xp[j] + h
      gp <- -ev_at(xp)$approx_grad[j]
      od[j] <- max((gp - (-ev0$approx_grad[j])) / h, 1e-8)
    }
    ev_at(init)
    tm <- proc.time()[["elapsed"]]
    M <- proto_metric(ev0, sdat, context, direct_layout, other_diag = od)
    t_metric <- proc.time()[["elapsed"]] - tm
    minv <- M$solve
    refresh <- function(en){
      tm <- proc.time()[["elapsed"]]
      m <- tryCatch(proto_metric(en$ev, sdat, context, direct_layout, other_diag = od)$solve,
        error = function(e) NULL)
      t_metric <<- t_metric + proc.time()[["elapsed"]] - tm
      m
    }
  }
  maxit <- as.integer(niter * (P$maxit_mult %||% 1))
  method <- P$method %||% "lbfgs"
  hess_sec <- 0
  if(method %in% c("lbfgs", "hybrid")){
    fit <- proto_lbfgs(init, fg, minv = minv, memory = P$memory %||% 30L, maxit = maxit,
      stop = P$stop %||% "pkg", abs_tol = tol, grad_tol = tol,
      gain_tol = P$gain_tol %||% 0.1, refresh = refresh,
      refresh_every = if(use_metric || !is.null(sm_type)) (P$refresh_every %||% 10L) else 0L)
  }
  evals_lbfgs <- eval_count
  if(method == "newton"){
    fit <- proto_newton(init, fg, hess, maxit = P$nt_maxit %||% 50L,
      gain_tol = P$gain_tol %||% 1e-6)
    hess_sec <- fit$hess_sec
  }
  if(method == "hybrid"){
    lb <- fit
    fit <- proto_newton(lb$par, fg, hess, maxit = P$nt_maxit %||% 50L,
      gain_tol = P$nt_gain_tol %||% 1e-6)
    hess_sec <- fit$hess_sec
    fit$iter <- lb$iter + fit$iter
    fit$backtracks <- lb$backtracks + fit$backtracks
  }
  ## The package's Newton polish, reusing the accepted evaluation as the next
  ## base point rather than evaluating it twice.
  polish <- list(steps = 0L, gain = 0)
  if(polish_steps > 0L){
    cur <- ev_at(fit$par)
    for(sIt in seq_len(as.integer(polish_steps))){
      stp <- try(bigIRT_laplace_newton_step(cur, sdat, context, direct_layout), silent = TRUE)
      if(inherits(stp, "try-error") || is.null(stp)) break
      predicted <- 0.5 * sum(cur$approx_grad * stp)
      if(!is.finite(predicted) || predicted <= polish_tol) break
      moved <- FALSE
      for(alpha in c(1, 0.5, 0.25, 0.1, 0.05)){
        cand <- fit$par + alpha * stp
        v <- tryCatch(ev_at(cand), error = function(e) NULL)
        if(!is.null(v) && is.finite(v$value) && v$value > cur$value){
          fit$par <- cand
          polish$gain <- polish$gain + (v$value - cur$value)
          polish$steps <- polish$steps + 1L
          cur <- v; moved <- TRUE; break
        }
      }
      if(!moved) break
    }
  }
  fit$polish <- polish
  final <- ev_at(fit$par)
  fit$masked_grad_norm <- sqrt(sum(final$approx_grad^2))
  fit$grad <- final$approx_grad
  fit$layout <- direct_layout
  fit$target_evals <- eval_count
  fit$logLik <- final$value
  fit$timings <- final$timings
  fit$proto <- list(evals_lbfgs = evals_lbfgs, backtracks = fit$backtracks,
    metric_sec = t_metric, hess_sec = hess_sec, lbfgs_terminate = fit$terminate$what)
  ## fitIRT copies only named fields out of $optim, so leave this where the
  ## harness can find it.
  assign("PROTO_EXTRA", fit$proto, envir = globalenv())
  list(state = final$state, optim = fit, eval = final, history = list())
}
