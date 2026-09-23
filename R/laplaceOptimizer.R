## The Laplace path's own optimiser: L-BFGS whose initial inverse Hessian is
## shaped by the Fisher information of the marginal, person coupling included.
##
## Why not mize alone. mize's L-BFGS is configured sensibly (secant-scaled H0,
## strong Wolfe search), and a unit initial step changes nothing measurable.
## What it lacks is a metric: in the optimiser's coordinates every item starts
## with the same curvature, whatever its response count, and nothing knows
## that a common shift of every difficulty is absorbed by the persons. Measured
## at the optimum on dev1 (dev/optim/probe_rate.R), the condition number of
## M^-1 H was 13.5 (2PL with covariates) and 106 (3PL) for the block-diagonal
## item information the polish uses, and 2.5 and 4.6 for the coupled Fisher
## matrix built here.
##
## The Fisher matrix is not a Newton Hessian: it misses the second derivative
## of the log-determinant terms, and for the 3PL/4PL the observed-Fisher gap.
## Undamped Fisher scoring was measured to converge at rate 0.95 (2PL) or not
## at all (3PL, eigenvalues of F^-1 H up to 2.3). So it sets the SHAPE of H0 and
## the secant pairs correct it, H0 = gamma F^-1 with gamma = s'y / y'F^-1 y --
## the design that replaced Optim in ctsem's engine.
##
## Stopping is on the predicted gain 0.5 g'Hg, in nats of log-likelihood, not on
## |delta f| or ||g||: the objective itself carries warm-start noise of about
## 1e-3 nats at the default person tolerance (probe_rate.R), so a change in f
## below that says nothing, and a gradient norm has no scale.
##
## Measured against the mize path (dev1, 8 regimes x 3 datasets,
## dev/optim/): 3.0x fewer objective evaluations and 2.1x less wall time
## (geometric means), with the mize path ending more than 1e-3 nats short of
## the optimum in 5 of 24 cells and this one in 2, at most 1.4e-3.

## Row derivatives of the response probability, weighted by 1/sqrt(p(1-p)):
## V against the free parameters, W against the free abilities. Everything the
## Fisher information needs, as sparse matrices. `described` lists the
## parameters V covers; the rest (latent correlations) are left to the caller.
bigIRT_laplace_row_derivatives <- function(ev, sdat, context, layout){
  n <- length(ev$approx_grad)
  K <- as.integer(sdat$Nscales); N <- as.integer(sdat$Nsubs)
  rc <- context$row_context
  re <- bigIRT_laplace_row_effective(state = ev$state, sdat = sdat,
    thetaBase = ev$posterior$theta_mode, rows = context$train_rows, context = rc,
    include_raw = TRUE)
  nr <- length(re$eta_row)
  eta <- as.numeric(re$eta_row); cr <- as.numeric(re$c_row); dr <- as.numeric(re$d_row)
  gg <- inv_logit(eta); u <- dr - cr
  p <- pmin(pmax(cr + u * gg, 1e-12), 1 - 1e-12)
  s <- u * gg * (1 - gg)
  w <- 1 / sqrt(pmax(p * (1 - p), 1e-12))
  rab <- matrix(as.numeric(re$row_ability), nrow = nr)
  alin <- matrix(as.numeric(re$a_linear), nrow = nr)
  st <- ev$state

  n_add <- 0L
  Ii <- vector("list", 32L); Jj <- vector("list", 32L); Xx <- vector("list", 32L)
  add <- function(rows, cols, vals){
    ok <- !is.na(cols) & cols > 0L & is.finite(vals)
    n_add <<- n_add + 1L
    Ii[[n_add]] <<- rows[ok]; Jj[[n_add]] <<- cols[ok]; Xx[[n_add]] <<- vals[ok]
  }
  beta_col <- function(slot, beta, beta_row, m){
    nrb <- max(nrow(as.matrix(beta)), 1L)
    slot[(m - 1L) * nrb + beta_row]
  }
  ## Difficulty: eta = a'theta - b.
  keepB <- which(!rc$B_fixed & rc$B_ref > 0L)
  if(length(layout$B)) add(keepB, layout$B[rc$B_ref[keepB]], -s[keepB] * w[keepB])
  if(length(layout$B_beta)) for(m in seq_len(ncol(rc$B_pred)))
    add(keepB, beta_col(layout$B_beta, st$Bbeta, rc$B_beta_row[keepB], m),
      -s[keepB] * rc$B_pred[keepB, m] * w[keepB])
  ## Discrimination: loading = softplus(raw), raw = invspA + item-covariate
  ## term, so d loading / d raw = plogis(raw), times the row ability.
  for(k in seq_len(K)){
    ref <- rc$A_ref[, k]
    keep <- which(ref > 0L)
    if(!length(keep)) next
    vA <- s[keep] * inv_logit(alin[keep, k]) * rab[keep, k] * w[keep]
    if(length(layout$A)) add(keep, layout$A[ref[keep]], vA)
    if(length(layout$A_beta)) for(m in seq_len(ncol(rc$A_pred)))
      add(keep, beta_col(layout$A_beta, st$invspAbeta, rc$A_beta_row[keep, k], m),
        vA * rc$A_pred[keep, m])
  }
  ## Asymptotes, through their links' own chain-rule factors.
  keepC <- which(!rc$C_fixed & rc$C_ref > 0L)
  vC <- (1 - gg[keepC]) * bigIRT_dc_dx(cr[keepC]) * w[keepC]
  if(length(layout$C)) add(keepC, layout$C[rc$C_ref[keepC]], vC)
  if(length(layout$C_beta)) for(m in seq_len(ncol(rc$C_pred)))
    add(keepC, beta_col(layout$C_beta, st$logitCbeta, rc$C_beta_row[keepC], m), vC * rc$C_pred[keepC, m])
  keepD <- which(!rc$D_fixed & rc$D_ref > 0L)
  vD <- gg[keepD] * bigIRT_dd_dx(dr[keepD]) * w[keepD]
  if(length(layout$D)) add(keepD, layout$D[rc$D_ref[keepD]], vD)
  if(length(layout$D_beta)) for(m in seq_len(ncol(rc$D_pred)))
    add(keepD, beta_col(layout$D_beta, st$logitDbeta, rc$D_beta_row[keepD], m), vD * rc$D_pred[keepD, m])
  ## Person covariates enter each row's ability: Abilitybeta is K x npred,
  ## packed column major.
  lo <- matrix(as.numeric(re$loadings), nrow = nr)
  if(length(layout$ability_beta)) for(m in seq_len(ncol(rc$person_pred))) for(k in seq_len(K)){
    rows <- which(!rc$fixed_ability[, k])
    add(rows, rep(layout$ability_beta[(m - 1L) * K + k], length(rows)),
      s[rows] * lo[rows, k] * rc$person_pred[rows, m] * w[rows])
  }
  V <- Matrix::sparseMatrix(i = unlist(Ii[seq_len(n_add)]), j = unlist(Jj[seq_len(n_add)]),
    x = unlist(Xx[seq_len(n_add)]), dims = c(nr, n))
  ids <- as.integer(rc$ids)
  Wi <- list(); Wj <- list(); Wx <- list()
  for(k in seq_len(K)){
    rows <- which(!rc$fixed_ability[, k])
    Wi[[k]] <- rows; Wj[[k]] <- (ids[rows] - 1L) * K + k
    Wx[[k]] <- s[rows] * lo[rows, k] * w[rows]
  }
  W <- Matrix::sparseMatrix(i = unlist(Wi), j = unlist(Wj), x = unlist(Wx), dims = c(nr, N * K))
  described <- unique(c(layout$B, layout$B_beta, layout$A, layout$A_beta, layout$C, layout$C_beta,
    layout$D, layout$D_beta, layout$B_mean, layout$A_mean, layout$C_mean, layout$D_mean,
    layout$ability_beta, layout$ability_mean))
  list(V = V, W = W, P0 = as.matrix(ev$prior_mats$precision), described = described,
    N = N, K = K, n = n)
}

## Fisher information of the MINIMISED Laplace objective in the optimiser's
## coordinates, as a sparse symmetric matrix.
##
## The joint (items + persons) information is I_ii = V'V, I_ip = V'W,
## I_pp = W'W + P0 per person, and the marginal's is the Schur complement
## V'V - (W'V)' I_pp^-1 (W'V), positive semidefinite because the joint one is.
## The ability mean enters the person prior only, so it adds I_mm = N P0 and
## I_pm = -P0 per person. Priors are quadratic and added exactly. Returns NULL
## when the coupling would be too dense to hold (`max_nnz`).
bigIRT_laplace_fisher_info <- function(ev, sdat, context, layout, max_nnz = 5e7){
  D <- bigIRT_laplace_row_derivatives(ev, sdat, context, layout)
  n <- D$n; N <- D$N; K <- D$K; P0 <- D$P0
  ## An upper bound on the coupling's nonzeros: no more than n^2, and no more
  ## than the sum over persons of the square of the parameters their rows
  ## touch. Beyond `max_nnz` the caller falls back to no metric.
  touch <- tabulate(as.integer(context$row_context$ids), nbins = N)
  per_row <- max(1, Matrix::nnzero(D$V) / max(1, nrow(D$V)))
  if(min(as.numeric(n)^2, sum((as.numeric(touch) * per_row)^2)) > max_nnz)
    return(NULL)
  Ipp <- Matrix::crossprod(D$W) + Matrix::kronecker(Matrix::Diagonal(N),
    Matrix::Matrix(P0, sparse = TRUE))
  Ipi <- Matrix::crossprod(D$W, D$V)
  am <- layout$ability_mean
  if(length(am)){
    M <- Matrix::sparseMatrix(i = rep(seq_len(N * K), times = K), j = rep(am, each = N * K),
      x = as.numeric(vapply(seq_len(K), function(kk) rep(-P0[, kk], times = N), numeric(N * K))),
      dims = c(N * K, n))
    Ipi <- Ipi + M
  }
  H <- Matrix::crossprod(D$V) - Matrix::crossprod(Ipi,
    Matrix::solve(Matrix::forceSymmetric(Ipp), Ipi))
  H <- as(Matrix::forceSymmetric(H), "generalMatrix")
  tr <- list(i = integer(0), j = integer(0), x = numeric(0))
  put <- function(i, j, x){ tr$i <<- c(tr$i, i); tr$j <<- c(tr$j, j); tr$x <<- c(tr$x, x) }
  if(length(am)) for(a in seq_along(am)) for(b in seq_along(am)) put(am[a], am[b], N * P0[a, b])
  if(isTRUE(as.logical(sdat$dopriors))){
    pr <- list(list("B", "B_mean", "BSDx", "BMeanSD"), list("A", "A_mean", "invspASD", "AMeanSD"),
      list("C", "C_mean", "logitCSD", "logitCMeanSD"), list("D", "D_mean", "logitDSD", "logitDMeanSD"))
    for(b in pr){
      idx <- layout[[b[[1]]]]
      if(!length(idx)) next
      prec <- 1 / rep_len(pmax(as.numeric(sdat[[b[[3]]]]), 1e-8), length(idx))^2
      put(idx, idx, prec)
      mi <- layout[[b[[2]]]]
      if(length(mi)){
        put(idx, rep(mi, length(idx)), -prec); put(rep(mi, length(idx)), idx, -prec)
        put(mi, mi, sum(prec) + 1 / max(as.numeric(sdat[[b[[4]]]])[1], 1e-8)^2)
      }
    }
  }
  bs <- as.numeric(sdat$betaScale)
  if(isTRUE(bs > 0)){
    ib <- c(layout$ability_beta, layout$A_beta, layout$B_beta, layout$C_beta, layout$D_beta)
    if(length(ib)) put(ib, ib, rep(1 / bs^2, length(ib)))
  }
  if(length(tr$i)) H <- H + Matrix::sparseMatrix(i = tr$i, j = tr$j, x = tr$x, dims = c(n, n))
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  H <- Matrix::Diagonal(x = ps) %*% H %*% Matrix::Diagonal(x = ps)
  attr(H, "undescribed") <- setdiff(seq_len(n), D$described)
  H
}

## A function applying F^-1, from the Fisher information at `ev` (point `x`,
## minimised-objective gradient `g`). Coordinates the Fisher construction does
## not describe get their rows and columns from forward differences of the
## gradient, one evaluation each through `gfun`. NULL if F cannot be built.
bigIRT_laplace_fisher_solver <- function(ev, x, g, sdat, context, layout, gfun, h = 1e-4){
  H <- tryCatch(bigIRT_laplace_fisher_info(ev, sdat, context, layout), error = function(e) NULL)
  if(is.null(H)) return(NULL)
  und <- attr(H, "undescribed")
  if(length(und)){
    H <- as.matrix(H)
    for(j in und){
      hj <- h * max(1, abs(x[j]))
      xp <- x; xp[j] <- xp[j] + hj
      col <- (gfun(xp) - g) / hj
      H[, j] <- col; H[j, ] <- col
    }
    H <- Matrix::Matrix(H, sparse = TRUE)
  }
  H <- Matrix::forceSymmetric(H)
  ## Positive definite up to rounding by construction; cancellation in the
  ## Schur complement on a weakly identified fit, or a finite-difference
  ## column, can break that, and a small ridge restores it. CHOLMOD warns as
  ## well as failing, and the failure is handled here, so the warning is not
  ## the user's business.
  ridge <- 0
  for(attempt in 1:6){
    ch <- tryCatch(suppressWarnings(Matrix::Cholesky(
      if(ridge > 0) H + Matrix::Diagonal(nrow(H), ridge) else H,
      perm = TRUE, LDL = FALSE)), error = function(e) NULL)
    if(!is.null(ch)) return(function(v) as.numeric(Matrix::solve(ch, v, system = "A")))
    ridge <- if(ridge == 0) 1e-8 * max(abs(Matrix::diag(H))) else ridge * 100
  }
  NULL
}

## L-BFGS minimising fg(x)$f, with H0 = gamma * minv (identity when minv is
## NULL). Unit step, Armijo backtracking by safeguarded quadratic
## interpolation; only a first step without a metric is shortened. Stops when
## the predicted gain 0.5 g'Hg falls below `gain_tol` (nats), and treats an
## Armijo failure below 1e-4 nats of predicted gain as the noise floor of the
## objective rather than a failure. `refresh(e)` may return a new minv every
## `refresh_every` iterations.
bigIRT_laplace_lbfgs <- function(x0, fg, minv = NULL, memory = 30L, maxit = 500L,
  gain_tol = 1e-3, c1 = 1e-4, maxback = 30L, refresh = NULL, refresh_every = 10L){
  S <- list(); Y <- list(); rho <- numeric(0)
  mv <- if(is.null(minv)) function(v) v else minv
  x <- x0; e <- fg(x); f <- e$f; g <- e$g
  h0 <- if(is.null(minv)) 1 / max(sum(abs(g)), 1e-300) else 1
  hmul <- function(q){
    k <- length(S)
    if(k == 0L) return(h0 * mv(q))
    a <- numeric(k)
    for(i in k:1){ a[i] <- rho[i] * sum(S[[i]] * q); q <- q - a[i] * Y[[i]] }
    y <- Y[[k]]
    r <- (sum(S[[k]] * y) / sum(y * mv(y))) * mv(q)
    for(i in seq_len(k)){ b <- rho[i] * sum(Y[[i]] * r); r <- r + (a[i] - b) * S[[i]] }
    r
  }
  it <- 0L; term <- "max_iter"; gain <- NA_real_; nback <- 0L; retried <- FALSE
  while(it < maxit){
    d <- -hmul(g)
    dphi <- sum(g * d)
    if(!is.finite(dphi) || dphi >= 0){
      if(!length(S)){ term <- "not_descent"; break }
      S <- list(); Y <- list(); rho <- numeric(0); next
    }
    gain <- -0.5 * dphi
    if(gain < gain_tol){ term <- "gain"; break }
    alpha <- 1
    en <- fg(x + d); fn <- en$f
    ok <- is.finite(fn) && fn <= f + c1 * alpha * dphi
    k <- 0L
    while(!ok && k < maxback){
      k <- k + 1L
      den <- 2 * (fn - f - dphi * alpha)
      tr <- if(is.finite(fn) && den > 0) -dphi * alpha^2 / den else 0.5 * alpha
      alpha <- min(max(tr, 0.1 * alpha), 0.5 * alpha)
      en <- fg(x + alpha * d); fn <- en$f
      ok <- is.finite(fn) && fn <= f + c1 * alpha * dphi
    }
    nback <- nback + k
    if(!ok){
      if(!retried && length(S)){ retried <- TRUE; S <- list(); Y <- list(); rho <- numeric(0); next }
      term <- if(gain < 1e-4) "noise_floor" else "linesearch_failed"
      break
    }
    retried <- FALSE
    it <- it + 1L
    s <- alpha * d; yv <- en$g - g
    sy <- sum(s * yv)
    if(sy > 1e-12 * sqrt(sum(s^2) * sum(yv^2))){
      S[[length(S) + 1L]] <- s; Y[[length(Y) + 1L]] <- yv; rho <- c(rho, 1 / sy)
      if(length(S) > memory){ S <- S[-1L]; Y <- Y[-1L]; rho <- rho[-1L] }
    }
    x <- x + s; f <- fn; g <- en$g; e <- en
    if(!is.null(refresh) && refresh_every > 0L && it %% refresh_every == 0L){
      m <- refresh(e)
      if(!is.null(m)) mv <- m
    }
  }
  list(par = x, f = f, g = g, iter = it, backtracks = nback,
    terminate = list(what = term, val = gain))
}
