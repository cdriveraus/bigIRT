# Fisher information of the Laplace marginal in the item block, with the
# coupling between items that runs through the persons.
#
# The joint (items + persons) Fisher information of a Bernoulli IRT model is
# sparse and bipartite: an item's parameters meet a person's ability only on
# the rows that person answered. With V the row derivatives dp/d(item
# parameters) and W the row derivatives dp/d(theta), both weighted by
# 1/sqrt(p(1-p)),
#
#     I_ii = V'V,   I_ip = V'W,   I_pp = W'W + P0 (block diagonal by person)
#
# and the information of the MARGINAL -- persons integrated out -- is the Schur
# complement
#
#     I_marg = V'V - (W'V)' I_pp^-1 (W'V)   (+ prior curvature),
#
# which is positive semidefinite because the joint information is. That is the
# Gauss-Newton / Fisher-scoring curvature of the Laplace objective, missing only
# the second derivative of the 0.5 log|H_i| terms (O(1) per person against
# O(n_i) for the rest). The block-diagonal curvature the package's polish uses
# is its item-diagonal blocks; what it leaves out is exactly the off-diagonal
# coupling, which is what carries the directions every item shares -- a common
# shift in difficulty that the persons absorb, for instance.
#
# Parameters entering the person prior mean (the ability mean) contribute
# I_mm = N P0 and I_pm = -P0 per person; person-covariate coefficients enter
# each row's ability, so they are row derivatives like an item parameter,
# dp/dbeta = s a x_row. Anything else (latent correlations, item covariates) is
# left to finite-difference columns by the caller.
#
# Everything is built with Matrix's sparse products; nothing loops over rows.

proto_fisher_hessian <- function(ev, sdat, context, layout){
  n <- length(ev$approx_grad)
  K <- as.integer(sdat$Nscales); N <- as.integer(sdat$Nsubs)
  rc <- context$row_context
  re <- bigIRT_laplace_row_effective(state = ev$state, sdat = sdat,
    thetaBase = ev$posterior$theta_mode, rows = context$train_rows, context = rc)
  nr <- length(re$eta_row)
  eta <- as.numeric(re$eta_row); cr <- as.numeric(re$c_row); dr <- as.numeric(re$d_row)
  gg <- stats::plogis(eta); u <- dr - cr
  p <- pmin(pmax(cr + u * gg, 1e-12), 1 - 1e-12)
  s <- u * gg * (1 - gg)
  w <- 1 / sqrt(pmax(p * (1 - p), 1e-12))
  lo <- matrix(re$loadings, nrow = nr)
  rab <- matrix(re$row_ability, nrow = nr)

  Ii <- integer(0); Jj <- integer(0); Xx <- numeric(0)
  add <- function(rows, cols, vals){
    ok <- !is.na(cols) & cols > 0L & is.finite(vals)
    Ii <<- c(Ii, rows[ok]); Jj <<- c(Jj, cols[ok]); Xx <<- c(Xx, vals[ok])
  }
  st <- ev$state
  ## Difficulty/intercept: eta = a'theta - b.
  if(length(layout$B)){
    keep <- which(!rc$B_fixed & rc$B_ref > 0L)
    add(keep, layout$B[rc$B_ref[keep]], -s[keep] * w[keep])
  }
  ## Discrimination: loading = pattern * softplus(invspA); d/d invspA carries
  ## sigmoid(invspA) and the row ability on that dimension.
  if(length(layout$A) && length(st$invspApars)){
    sigA <- stats::plogis(st$invspApars); Aval <- pmax(log1p(exp(st$invspApars)), 1e-12)
    for(k in seq_len(K)){
      ref <- rc$A_ref[, k]
      keep <- which(ref > 0L)
      if(!length(keep)) next
      q <- ref[keep]
      add(keep, layout$A[q], s[keep] * sigA[q] * (lo[keep, k] / Aval[q]) * rab[keep, k] * w[keep])
    }
  }
  ## Lower asymptote c = 0.5 plogis(x): dc/dx = c (1 - 2c); dp/dc = 1 - g.
  if(length(layout$C)){
    keep <- which(!rc$C_fixed & rc$C_ref > 0L)
    add(keep, layout$C[rc$C_ref[keep]], (1 - gg[keep]) * cr[keep] * (1 - 2 * cr[keep]) * w[keep])
  }
  ## Upper asymptote d = 0.5 plogis(x) + 0.5: dd/dx = (2d - 1)(1 - d); dp/dd = g.
  if(length(layout$D)){
    keep <- which(!rc$D_fixed & rc$D_ref > 0L)
    add(keep, layout$D[rc$D_ref[keep]], gg[keep] * (2 * dr[keep] - 1) * (1 - dr[keep]) * w[keep])
  }
  ## Person-covariate coefficients: Abilitybeta is K x npred, packed column major.
  if(length(layout$ability_beta)){
    X <- rc$person_pred
    npred <- ncol(X)
    for(m in seq_len(npred)) for(k in seq_len(K)){
      col <- layout$ability_beta[(m - 1L) * K + k]
      free_k <- !rc$fixed_ability[, k]
      rows <- which(free_k)
      add(rows, rep(col, length(rows)), s[rows] * lo[rows, k] * X[rows, m] * w[rows])
    }
  }
  V <- Matrix::sparseMatrix(i = Ii, j = Jj, x = Xx, dims = c(nr, n))

  ## Person side.
  ids <- as.integer(rc$ids)
  Wi <- integer(0); Wj <- integer(0); Wx <- numeric(0)
  for(k in seq_len(K)){
    free_k <- which(!rc$fixed_ability[, k])
    Wi <- c(Wi, free_k); Wj <- c(Wj, (ids[free_k] - 1L) * K + k)
    Wx <- c(Wx, s[free_k] * lo[free_k, k] * w[free_k])
  }
  W <- Matrix::sparseMatrix(i = Wi, j = Wj, x = Wx, dims = c(nr, N * K))
  P0 <- as.matrix(ev$prior_mats$precision)
  Ipp <- Matrix::crossprod(W) + Matrix::kronecker(Matrix::Diagonal(N), Matrix::Matrix(P0, sparse = TRUE))
  Ipi <- Matrix::crossprod(W, V)
  Iii <- as.matrix(Matrix::crossprod(V))
  if(length(layout$ability_mean)){
    ## -P0 against every person, one column per scale.
    am <- layout$ability_mean
    M <- Matrix::sparseMatrix(i = rep(seq_len(N * K), times = K),
      j = rep(am, each = N * K),
      x = as.numeric(sapply(seq_len(K), function(kk) rep(-P0[, kk], times = N))),
      dims = c(N * K, n))
    Ipi <- Ipi + M
    Iii[am, am] <- Iii[am, am] + N * P0
  }
  S <- as.matrix(Matrix::crossprod(Ipi, Matrix::solve(Ipp, Ipi)))
  H <- Iii - S

  ## Prior curvature (quadratic, exact), for the minimised objective.
  if(isTRUE(as.logical(sdat$dopriors))){
    pr <- list(B = list(par = "B", mean = "B_mean", sd = "BSDx", musd = "BMeanSD"),
      A = list(par = "A", mean = "A_mean", sd = "invspASD", musd = "AMeanSD"),
      C = list(par = "C", mean = "C_mean", sd = "logitCSD", musd = "logitCMeanSD"),
      D = list(par = "D", mean = "D_mean", sd = "logitDSD", musd = "logitDMeanSD"))
    for(b in pr){
      idx <- layout[[b$par]]
      if(!length(idx)) next
      sd2 <- rep_len(as.numeric(sdat[[b$sd]]), length(idx))^2
      H[cbind(idx, idx)] <- H[cbind(idx, idx)] + 1 / sd2
      mi <- layout[[b$mean]]
      if(length(mi)){
        H[idx, mi] <- H[idx, mi] - 1 / sd2
        H[mi, idx] <- H[mi, idx] - 1 / sd2
        musd <- max(as.numeric(sdat[[b$musd]])[1], 1e-8)
        H[mi, mi] <- H[mi, mi] + sum(1 / sd2) + 1 / musd^2
      }
    }
  }
  bs <- as.numeric(sdat$betaScale)
  if(isTRUE(bs > 0) && length(layout$ability_beta)){
    ib <- layout$ability_beta
    H[cbind(ib, ib)] <- H[cbind(ib, ib)] + 1 / bs^2
  }
  ## To the optimiser's coordinates.
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  H <- H * outer(ps, ps)
  ## Columns this construction does not describe.
  described <- c(layout$B, layout$A, layout$C, layout$D, layout$B_mean, layout$A_mean,
    layout$C_mean, layout$D_mean, layout$ability_beta, layout$ability_mean)
  attr(H, "undescribed") <- setdiff(seq_len(n), described)
  H
}

## Replace the undescribed rows/columns of H with forward differences of the
## gradient (minimised objective): one evaluation each.
proto_fd_columns <- function(H, x, g, gfun, cols, h = 1e-4){
  for(j in cols){
    hj <- h * max(1, abs(x[j]))
    xp <- x; xp[j] <- xp[j] + hj
    col <- (gfun(xp) - g) / hj
    H[, j] <- col; H[j, ] <- col
  }
  H
}

## Damped Newton (Fisher scoring) on the minimised objective. fg(x) returns
## list(f, g, ev); hess(ev, x, g) returns a symmetric curvature matrix.
## Stops when the predicted gain 0.5 g'H^-1 g falls below gain_tol, in nats.
proto_newton <- function(x0, fg, hess, maxit = 50L, gain_tol = 1e-6, c1 = 1e-4,
  maxback = 30L, trace = NULL){
  x <- x0; e <- fg(x); f <- e$f; g <- e$g
  it <- 0L; term <- "max_iter"; gain <- NA_real_; nback <- 0L; mu <- 0
  hsec <- 0
  while(it < maxit){
    t0 <- proc.time()[["elapsed"]]
    H <- hess(e$ev, x, g)
    hsec <- hsec + proc.time()[["elapsed"]] - t0
    Hs <- (H + t(H)) / 2
    ## Cholesky when the curvature is positive definite, which the Fisher
    ## construction guarantees up to rounding; an eigenvalue floor otherwise
    ## (finite-difference columns can break it).
    ch <- tryCatch(chol(Hs), error = function(e) NULL)
    if(!is.null(ch)){
      d <- -backsolve(ch, forwardsolve(t(ch), g))
    } else {
      eg <- eigen(Hs, symmetric = TRUE)
      lmax <- max(abs(eg$values))
      lam <- pmax(eg$values, 1e-8 * lmax)
      d <- -as.numeric(eg$vectors %*% (crossprod(eg$vectors, g) / lam))
    }
    dphi <- sum(g * d)
    gain <- -0.5 * dphi
    if(!is.null(trace)) trace(it, f, gain)
    if(!is.finite(gain)){ term <- "nonfinite"; break }
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
      ## At the noise floor of the objective an Armijo test can fail on a
      ## genuinely converged point; below 1e-4 nats of predicted gain that is
      ## the reading, not a failure.
      term <- if(gain < 1e-4) "noise_floor" else "linesearch_failed"
      break
    }
    it <- it + 1L
    x <- x + alpha * d; f <- fn; g <- en$g; e <- en
  }
  list(par = x, f = f, g = g, iter = it, terminate = list(what = term, val = gain),
    backtracks = nback, hess_sec = hsec)
}
