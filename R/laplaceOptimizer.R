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
  ## term, so d loading / d raw = plogis(raw), times the row ability. A signed
  ## loading (signedLoadings) is raw itself, slope one.
  signedA <- (rc$A_signed %||% bigIRT_laplace_signed_A(sdat)) == 1L
  for(k in seq_len(K)){
    ref <- rc$A_ref[, k]
    keep <- which(ref > 0L)
    if(!length(keep)) next
    slope <- inv_logit(alin[keep, k])
    if(any(signedA)) slope[signedA[ref[keep]]] <- 1
    vA <- s[keep] * slope * rab[keep, k] * w[keep]
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

## Prior curvature of the minimised objective, natural coordinates, n x n
## sparse: the item priors and their mean hyperparameters, the coefficient
## priors, and the ability mean's N P0 (it enters only the person prior).
bigIRT_laplace_prior_curvature <- function(sdat, layout, n, N, P0){
  tr <- list(i = integer(0), j = integer(0), x = numeric(0))
  put <- function(i, j, x){ tr$i <<- c(tr$i, i); tr$j <<- c(tr$j, j); tr$x <<- c(tr$x, x) }
  am <- layout$ability_mean
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
        ## A signed loading's prior is centred at zero, not at the A mean, so
        ## it does not couple to it.
        tied <- if(identical(b[[1]], "A")) bigIRT_laplace_signed_A(sdat) != 1L else NULL
        if(!is.null(tied) && !all(tied)){ idx <- idx[tied]; prec <- prec[tied] }
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
  Matrix::sparseMatrix(i = tr$i, j = tr$j, x = tr$x, dims = c(n, n))
}

## The pieces of the Fisher information in the OPTIMISER's coordinates, and
## F v without forming F.
##
## The joint (items + persons) information is I_ii = V'V, I_ip = V'W,
## I_pp = W'W + P0 per person, and the marginal's is the Schur complement
##   F = V'V - C' Ipp^-1 C + priors,  C = W'V,
## positive semidefinite because the joint one is. The ability mean enters the
## person prior only, so it adds -P0 per person to C and N P0 to the priors.
## F v costs three sparse passes over the rows (V v, C v, C'u) and a
## block-diagonal solve; nothing n x n is formed.
bigIRT_laplace_fisher_parts <- function(ev, sdat, context, layout){
  D <- bigIRT_laplace_row_derivatives(ev, sdat, context, layout)
  n <- D$n; N <- D$N; K <- D$K; P0 <- D$P0
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  S <- Matrix::Diagonal(x = ps)
  Ipp <- Matrix::forceSymmetric(Matrix::crossprod(D$W) +
    Matrix::kronecker(Matrix::Diagonal(N), Matrix::Matrix(P0, sparse = TRUE)))
  IppC <- Matrix::Cholesky(Ipp, perm = FALSE, LDL = FALSE)
  C <- Matrix::crossprod(D$W, D$V)
  am <- layout$ability_mean
  if(length(am)){
    M <- Matrix::sparseMatrix(i = rep(seq_len(N * K), times = K), j = rep(am, each = N * K),
      x = as.numeric(vapply(seq_len(K), function(kk) rep(-P0[, kk], times = N), numeric(N * K))),
      dims = c(N * K, n))
    C <- C + M
  }
  V <- D$V %*% S
  C <- C %*% S
  Pr <- S %*% bigIRT_prior_curvature_cached(sdat, layout, n, N, P0, context) %*% S
  Fv <- function(v){
    v <- as.numeric(v)
    as.numeric(Matrix::crossprod(V, V %*% v) + Pr %*% v -
      Matrix::crossprod(C, Matrix::solve(IppC, C %*% v, system = "A")))
  }
  list(D = D, V = V, C = C, IppC = IppC, Pr = Pr, Fv = Fv, n = n, N = N, K = K, P0 = P0,
    undescribed = setdiff(seq_len(n), D$described))
}

## Fisher information of the MINIMISED Laplace objective in the optimiser's
## coordinates, as a dense-pattern sparse matrix. Returns NULL when the
## coupling would be too dense to hold (`max_nnz`).
bigIRT_laplace_fisher_info <- function(ev, sdat, context, layout,
  max_nnz = getOption("bigIRT.fisher_max_nnz", 5e7), parts = NULL){
  n <- length(ev$approx_grad)
  if(as.numeric(n)^2 > max_nnz) return(NULL)
  P <- if(is.null(parts)) bigIRT_laplace_fisher_parts(ev, sdat, context, layout) else parts
  H <- Matrix::crossprod(P$V) + P$Pr -
    Matrix::crossprod(P$C, Matrix::solve(P$IppC, P$C, system = "A"))
  H <- as(Matrix::forceSymmetric(H), "generalMatrix")
  attr(H, "undescribed") <- P$undescribed
  H
}

## q_r = 1 - w_r' Ipp_i^-1 w_r, Ipp_i = sum_{r in i} w_r w_r' + P0, for rows
## grouped by person `ids`: the share of a row's information that its own
## person does not absorb. W is nr x NK with row r's entries in person i's K
## columns; they are gathered into an nr x K matrix, the K x K blocks summed by
## person and inverted by vectorised Gauss-Jordan across persons. (Solving
## CHOLMOD against the sparse t(W) instead took 42 s at 577k rows.)
bigIRT_laplace_row_q <- function(W, N, K, P0, ids){
  Wt <- as(W, "TsparseMatrix")
  Wd <- matrix(0, nrow(W), K)
  if(length(Wt@x)) Wd[cbind(Wt@i + 1L, (Wt@j %% K) + 1L)] <- Wt@x
  A <- array(0, c(K, K, N))
  for(k in seq_len(K)) for(l in seq_len(K)){
    s <- numeric(N); a <- rowsum(Wd[, k] * Wd[, l], ids)
    s[as.integer(rownames(a))] <- a[, 1]
    A[k, l, ] <- s + P0[k, l]
  }
  Ai <- bigIRT_block_inverse(A)
  q <- rep(1, nrow(W))
  for(k in seq_len(K)) for(l in seq_len(K)) q <- q - Wd[, k] * Ai[k, l, ids] * Wd[, l]
  pmax(q, 0)
}

## Inverse of each K x K slice of a K x K x N array of SPD matrices.
bigIRT_block_inverse <- function(A){
  K <- dim(A)[1]; N <- dim(A)[3]
  if(K == 1L) return(array(1 / A[1, 1, ], c(1L, 1L, N)))
  M <- A; I <- array(0, c(K, K, N)); for(k in seq_len(K)) I[k, k, ] <- 1
  for(k in seq_len(K)){
    piv <- M[k, k, ]
    M[k, , ] <- sweep(matrix(M[k, , ], K), 2, piv, "/")
    I[k, , ] <- sweep(matrix(I[k, , ], K), 2, piv, "/")
    for(r in setdiff(seq_len(K), k)){
      f <- M[r, k, ]
      M[r, , ] <- M[r, , ] - sweep(matrix(M[k, , ], K), 2, f, "*")
      I[r, , ] <- I[r, , ] - sweep(matrix(I[k, , ], K), 2, f, "*")
    }
  }
  I
}

## Coordinates whose curvature is numerically nil next to the rest: a diagonal
## not above `rel` of the largest (non-positive ones included). An unregularised
## loading on the flat tail of its softplus is one -- at a raw value of -124 its
## slope is 1e-54 and its curvature 1e-108. Left in the metric, its inverse
## moved that coordinate by its coupling to every other gradient over that
## curvature: a step of 9e53, which no line search can shorten to sense (30
## shortenings reach 1e-30), so the fit took no step at all (unregularised 3D
## 2PL, M_misspecified_N450_r007, Laplace and quadrature alike). The same rule
## is applied to the item blocks in src/fisher_blocks.cpp.
bigIRT_metric_flat <- function(dg, rel = 1e-12){
  dg <- as.numeric(dg)
  top <- suppressWarnings(max(dg[is.finite(dg)]))
  if(!is.finite(top) || !(top > 0)) return(which(!(dg > 0)))
  which(!(dg > rel * top))
}

## The metric with the tiny-but-positive ones decoupled -- row and column
## zeroed -- and given the median of the other curvatures. Its step is then its
## own gradient over an ordinary curvature, which for a flat coordinate is next
## to nothing: it stays where it is, and the certificate says whether it should
## not. With `nonpos`, a non-positive diagonal (no data, no prior) gets the
## median as the block metrics always gave it, couplings kept; the dense
## metrics never did that, and do not now. Either way a metric without tiny
## diagonals is exactly as it was.
bigIRT_metric_decouple <- function(M, rel = 1e-12, nonpos = TRUE){
  dg <- as.numeric(if(inherits(M, "Matrix")) Matrix::diag(M) else diag(M))
  flat <- bigIRT_metric_flat(dg, rel)
  tiny <- flat[dg[flat] > 0]
  if(!nonpos) flat <- tiny
  if(!length(flat)) return(M)
  ok <- dg[setdiff(seq_along(dg), bigIRT_metric_flat(dg, rel))]
  med <- max(if(length(ok)) stats::median(ok) else 0, 1e-8)
  if(length(tiny)){ M[tiny, ] <- 0; M[, tiny] <- 0 }
  if(inherits(M, "Matrix"))
    M <- M + Matrix::sparseMatrix(i = flat, j = flat, x = rep(med, length(flat)), dims = dim(M))
  else diag(M)[flat] <- med
  M
}

## Factor a symmetric metric, with a small ridge if rounding or a
## finite-difference column has broken positive definiteness. CHOLMOD warns as
## well as failing; the failure is handled here, so the warning is not the
## user's business. Returns a function applying the inverse, or NULL.
bigIRT_laplace_factor_metric <- function(H){
  H <- Matrix::forceSymmetric(H)
  ridge <- 0
  for(attempt in 1:6){
    ch <- tryCatch(suppressWarnings(Matrix::Cholesky(
      if(ridge > 0) H + Matrix::Diagonal(nrow(H), ridge) else H,
      perm = TRUE, LDL = FALSE)), error = function(e) NULL)
    if(!is.null(ch)) return(function(v) as.numeric(Matrix::solve(ch, as.numeric(v), system = "A")))
    ridge <- if(ridge == 0) 1e-8 * max(abs(Matrix::diag(H))) else ridge * 100
  }
  NULL
}

## Fisher information of the latent correlation parameters, in closed form,
## optimiser coordinates.
##
## Person i's Laplace posterior is N(m_i, V_i) with precision H_i = P0 + D_i,
## D_i the information from the data. Read the mode as a measurement of theta_i
## with error covariance D_i^-1; its marginal is then N(mu, Sigma + D_i^-1), so
##
##   F_jl = 1/2 sum_i tr(W_i dSigma_j W_i dSigma_l),
##   W_i  = (Sigma + D_i^-1)^-1 = D_i - D_i V_i D_i,
##
## which never inverts D_i and runs from Sigma^-1 (perfect information) to 0
## (none). Accumulated as sum_i W_i (x) W_i, one crossprod of an N x K^2
## matrix. It replaces a finite-difference column per correlation -- one
## objective evaluation each, 36 per metric build at nine dimensions.
## dSigma is differenced through the correlation constructor, a K x K
## closed form.
bigIRT_laplace_corr_fisher <- function(ev, sdat, layout, paramization, h = 1e-6){
  idx <- layout$corr
  if(!length(idx)) return(NULL)
  K <- as.integer(sdat$Nscales); N <- as.integer(sdat$Nsubs)
  Hp <- array(as.numeric(ev$posterior$precision), c(K, K, N))
  P0 <- as.matrix(ev$prior_mats$precision)
  sdv <- pmax(as.numeric(sdat$AbilitySD), 1e-8)
  cp <- ev$state$AbilityCorrPars
  Sig <- function(p) diag(sdv, K) %*% bigIRT_laplace_corr_from_packed(p, K, paramization = paramization) %*% diag(sdv, K)
  J <- vapply(seq_along(cp), function(j){
    pp <- cp; pm <- cp; pp[j] <- pp[j] + h; pm[j] <- pm[j] - h
    as.numeric(Sig(pp) - Sig(pm)) / (2 * h)
  }, numeric(K * K))
  J <- matrix(J, nrow = K * K)
  Wv <- matrix(0, N, K * K)
  for(i in seq_len(N)){
    H <- Hp[, , i]; D <- H - P0
    W <- D - D %*% solve(H, D)
    Wv[i, ] <- as.numeric((W + t(W)) / 2)
  }
  C <- crossprod(Wv)
  ## C[(a,c),(b,d)] = sum_i W[a,c] W[b,d]; the Kronecker sum wants (a,b),(c,d).
  ix <- as.matrix(expand.grid(a = seq_len(K), b = seq_len(K), c = seq_len(K), d = seq_len(K)))
  M <- matrix(C[cbind((ix[, "c"] - 1L) * K + ix[, "a"], (ix[, "d"] - 1L) * K + ix[, "b"])], K * K, K * K)
  ## Rows of M: (a,b) with a fastest; columns (c,d). vec(dSigma) indexes (row, col), row fastest.
  F <- 0.5 * crossprod(J, M %*% J)
  ps <- attr(layout, "par_scale")
  s <- if(length(ps) >= max(idx)) ps[idx] else rep(1, length(idx))
  (F + t(F)) / 2 * outer(s, s)
}

## Write the closed-form correlation block into a metric matrix M, whose rows
## and columns `pos` hold the correlation parameters in layout order, and
## zero their coupling to the rest. Returns M unchanged when the option asks
## for finite differences or there are no correlations; `done` then says
## which parameters still need differencing.
bigIRT_laplace_use_corr_block <- function(M, pos, ev, sdat, context, layout){
  if(!length(pos) || identical(getOption("bigIRT.corr_metric", "analytic"), "fd"))
    return(list(M = M, done = integer(0)))
  Fc <- tryCatch(bigIRT_laplace_corr_fisher(ev, sdat, layout,
    paramization = context$corr_paramization %||% "normalized_chol"), error = function(e) NULL)
  if(is.null(Fc) || any(!is.finite(Fc))) return(list(M = M, done = integer(0)))
  M[pos, ] <- 0; M[, pos] <- 0
  M[pos, pos] <- Fc
  list(M = M, done = layout$corr)
}

## Columns of F for `cols`: Fisher-vector products for the ones the Fisher
## construction describes, forward differences of the gradient (one
## evaluation each, through `gfun`) for the rest. n x length(cols).
bigIRT_laplace_fisher_columns <- function(P, cols, x, g, gfun, h = 1e-4){
  out <- matrix(0, P$n, length(cols))
  for(k in seq_along(cols)){
    j <- cols[k]
    if(j %in% P$undescribed){
      hj <- h * max(1, abs(x[j]))
      xp <- x; xp[j] <- xp[j] + hj
      out[, k] <- (gfun(xp) - g) / hj
    } else {
      e <- numeric(P$n); e[j] <- 1
      out[, k] <- P$Fv(e)
    }
  }
  out
}

## Dense metric: F itself, with finite-difference rows and columns for the
## coordinates the construction does not describe. NULL if F cannot be built.
bigIRT_laplace_fisher_solver <- function(ev, x, g, sdat, context, layout, gfun, h = 1e-4){
  if(!identical(getOption("bigIRT.dense_impl", "cpp"), "R"))
    return(bigIRT_laplace_fisher_solver_cpp(ev, x, g, sdat, context, layout, gfun, h = h))
  H <- tryCatch(bigIRT_laplace_fisher_info(ev, sdat, context, layout), error = function(e) NULL)
  if(is.null(H)) return(NULL)
  und <- attr(H, "undescribed")
  if(length(und)){
    H <- as.matrix(H)
    cb <- bigIRT_laplace_use_corr_block(H, layout$corr, ev, sdat, context, layout)
    H <- cb$M
    for(j in setdiff(und, cb$done)){
      hj <- h * max(1, abs(x[j]))
      xp <- x; xp[j] <- xp[j] + hj
      col <- (gfun(xp) - g) / hj
      H[, j] <- col; H[j, ] <- col
    }
    H <- Matrix::Matrix(H, sparse = TRUE)
  }
  bigIRT_laplace_factor_metric(bigIRT_metric_decouple(H, nonpos = FALSE))
}

## Block-diagonal metric, for fits too large for the dense one.
##
## F restricted to its item-diagonal blocks, plus the exact block of the
## parameters shared across items, with the coupling between the two dropped:
##
##   Bd = blockdiag( F_jj for each item j,  F_GG ).
##
## W Ipp^-1 W' couples only rows of the same person, and a person answers an
## item once, so the item block F_jj is exactly V_j' diag(q) V_j with q from
## bigIRT_laplace_row_q -- one pass over the rows. G is every parameter whose
## rows span more than one item (shared covariate coefficients, person
## covariates), plus those no row touches (mean hyperparameters, the ability
## mean, latent correlations); its block comes from length(G) Fisher-vector
## products, or finite differences where the construction has no rows. Both
## blocks are principal submatrices of a positive semidefinite F, or blocks
## of one, so Bd is positive semidefinite.
##
## What it leaves out is the coupling between different items through the
## persons who answered both. In a sparse design, where each person sees a
## few dozen items, that is a small part of each row's information, and
## L-BFGS's secant pairs recover the few directions it matters for. Measured
## at 5,000 to 10,000 items (dev1, dev/optim/): as few evaluations as the
## exact F applied matrix-free by conjugate gradients, at a fraction of the
## cost, and 2-4x fewer than mize.
bigIRT_laplace_blockdiag_solver <- function(ev, x, g, sdat, context, layout, gfun, h = 1e-4){
  if(!identical(getOption("bigIRT.blockdiag_impl", "cpp"), "R"))
    return(bigIRT_laplace_blockdiag_solver_cpp(ev, x, g, sdat, context, layout, gfun, h = h))
  P <- tryCatch(bigIRT_laplace_fisher_parts(ev, sdat, context, layout), error = function(e) NULL)
  if(is.null(P)) return(NULL)
  n <- P$n
  V <- as(P$V, "CsparseMatrix")
  items <- as.integer(context$row_context$items)
  ## A column is local when every row it touches belongs to one item: compare
  ## each nonzero's item with the first in its column (CSC keeps a column's
  ## nonzeros together), one vectorised pass.
  cnt <- diff(V@p)
  nzc <- rep.int(seq_len(n), cnt)
  nzi <- items[V@i + 1L]
  has <- cnt > 0L
  first <- rep(NA_integer_, n)
  first[has] <- items[V@i[V@p[which(has)] + 1L] + 1L]
  spans <- logical(n)
  spans[unique(nzc[nzi != first[nzc]])] <- TRUE
  item_par <- c(layout$B, layout$A, layout$C, layout$D)
  local <- which((has & !spans) | (!has & seq_len(n) %in% item_par))
  G <- setdiff(seq_len(n), local)
  q <- bigIRT_laplace_row_q(P$D$W, P$N, P$K, P$P0, as.integer(context$row_context$ids))
  VL <- V[, local, drop = FALSE]
  Bl <- Matrix::crossprod(VL, Matrix::Diagonal(x = q) %*% VL) + P$Pr[local, local, drop = FALSE]
  Bl <- as(Bl, "TsparseMatrix")
  ti <- local[Bl@i + 1L]; tj <- local[Bl@j + 1L]; tx <- Bl@x
  if(length(G)){
    cpos <- match(layout$corr, G)
    FG <- matrix(0, length(G), length(G))
    fd_cols <- setdiff(G, if(identical(getOption("bigIRT.corr_metric", "analytic"), "fd")) integer(0) else layout$corr)
    if(length(fd_cols))
      FG[, match(fd_cols, G)] <- bigIRT_laplace_fisher_columns(P, fd_cols, x, g, gfun, h = h)[G, , drop = FALSE]
    FG <- bigIRT_laplace_use_corr_block(FG, cpos[!is.na(cpos)], ev, sdat, context, layout)$M
    FG <- (FG + t(FG)) / 2
    ti <- c(ti, rep(G, times = length(G))); tj <- c(tj, rep(G, each = length(G)))
    tx <- c(tx, as.numeric(FG))
  }
  Bd <- Matrix::sparseMatrix(i = ti, j = tj, x = tx, dims = c(n, n))
  ## A parameter no data and no prior reach (possible only in odd
  ## specifications) still needs a positive diagonal to factor, and one on a
  ## flat transform must not be moved by its coupling (bigIRT_metric_flat).
  bigIRT_laplace_factor_metric(bigIRT_metric_decouple(Bd))
}

## The prior curvature, cached for the optimiser run: it depends on the
## priors, the layout and the ability prior precision, and within one call of
## bigIRT_laplace_optimize_direct only the last can change (with estimated
## correlations). Rebuilding it cost 0.17 s at every metric refresh on a
## 10,000-parameter fit.
bigIRT_prior_curvature_cached <- function(sdat, layout, n, N, P0, context){
  ce <- context$cache
  if(is.environment(ce) && !is.null(ce$pr_P0) && identical(ce$pr_P0, P0)) return(ce$pr)
  Pr <- bigIRT_laplace_prior_curvature(sdat, layout, n, N, P0)
  if(is.environment(ce)){ ce$pr_P0 <- P0; ce$pr <- Pr }
  Pr
}

## The same metric assembled in C++ (src/fisher_blocks.cpp): one parallel
## pass over persons builds every item block and the exact shared block, with
## no sparse matrices, no factorisation of the person block and no
## Fisher-vector products. The R assembly above remains, selected by
## options(bigIRT.blockdiag_impl = "R"), and the tests hold the two equal.
## Only the coordinates the construction does not describe (latent
## correlations) are still differenced, one gradient evaluation each.

## Marshal the fitted state for the C++ Fisher kernels (src/fisher_blocks.cpp):
## `entry` is "blocks" (block-diagonal) or "dense".
bigIRT_fisher_kernel_call <- function(entry, ev, sdat, context, layout, ps, prior_diag){
  n <- length(ev$approx_grad); K <- as.integer(sdat$Nscales)
  rc <- context$row_context; st <- ev$state; rows <- context$train_rows
  person_pred <- rc$person_pred; ab <- st$Abilitybeta
  if(!length(person_pred) || ncol(person_pred) == 0L){
    person_pred <- matrix(0, length(rows), 1L); ab <- matrix(0, K, 1L)
  }
  item_par <- integer(n); item_par[c(layout$B, layout$A, layout$C, layout$D)] <- 1L
  iv <- function(x) as.integer(if(is.null(x)) integer(0) else x)
  nr <- length(rows)
  a <- list(
    as.integer(sdat$id[rows]), as.integer(rc$items), as.matrix(ev$posterior$theta_mode),
    as.matrix(person_pred), bigirt_int_matrix(rc$fixed_ability, nr, K), as.matrix(rc$fixed_ability_value),
    bigirt_int_matrix(rc$A_ref, nr, K), as.matrix(rc$fixed_A_value), as.matrix(rc$A_pred),
    as.integer(rc$A_signed %||% bigIRT_laplace_signed_A(sdat)),
    as.integer(rc$B_ref), as.numeric(rc$B_fixed_value), as.matrix(rc$B_pred),
    as.integer(rc$C_ref), as.numeric(rc$C_fixed_value), as.matrix(rc$C_pred),
    as.integer(rc$D_ref), as.numeric(rc$D_fixed_value), as.matrix(rc$D_pred),
    as.matrix(ab), as.numeric(st$invspApars), as.matrix(st$invspAbeta), as.numeric(st$Bpars),
    as.matrix(st$Bbeta), as.numeric(st$logitCpars), as.matrix(st$logitCbeta),
    as.numeric(st$logitDpars), as.matrix(st$logitDbeta), as.matrix(ev$prior_mats$precision),
    iv(layout$A), iv(layout$B), iv(layout$C), iv(layout$D),
    iv(layout$A_beta), iv(layout$B_beta), iv(layout$C_beta), iv(layout$D_beta),
    iv(layout$ability_beta), iv(layout$ability_mean), item_par, as.numeric(ps),
    as.numeric(prior_diag), as.integer(context$grain_size %||% 64L))
  ## Literal .Call()s: configure regenerates the native registration by
  ## scanning the R code for them, so the routine must be named at the call.
  if(identical(entry, "dense")) .Call(`_bigIRT_fisher_dense_cpp_impl`, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], a[[7]], a[[8]], a[[9]], a[[10]], a[[11]], a[[12]], a[[13]], a[[14]], a[[15]], a[[16]], a[[17]], a[[18]], a[[19]], a[[20]], a[[21]], a[[22]], a[[23]], a[[24]], a[[25]], a[[26]], a[[27]], a[[28]], a[[29]], a[[30]], a[[31]], a[[32]], a[[33]], a[[34]], a[[35]], a[[36]], a[[37]], a[[38]], a[[39]], a[[40]], a[[41]], a[[42]], a[[43]])
  else .Call(`_bigIRT_fisher_blocks_cpp_impl`, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], a[[7]], a[[8]], a[[9]], a[[10]], a[[11]], a[[12]], a[[13]], a[[14]], a[[15]], a[[16]], a[[17]], a[[18]], a[[19]], a[[20]], a[[21]], a[[22]], a[[23]], a[[24]], a[[25]], a[[26]], a[[27]], a[[28]], a[[29]], a[[30]], a[[31]], a[[32]], a[[33]], a[[34]], a[[35]], a[[36]], a[[37]], a[[38]], a[[39]], a[[40]], a[[41]], a[[42]], a[[43]])
}

## Dense metric, assembled in C++: the full F with its item-item coupling, in
## one parallel pass over persons, plus the priors and the closed-form
## correlation block. The R assembly (bigIRT_laplace_fisher_info) stays behind
## options(bigIRT.dense_impl = "R"), and the tests hold the two equal.
bigIRT_laplace_fisher_solver_cpp <- function(ev, x, g, sdat, context, layout, gfun, h = 1e-4){
  n <- length(ev$approx_grad); N <- as.integer(sdat$Nsubs)
  if(as.numeric(n)^2 > getOption("bigIRT.fisher_max_nnz", 5e7)) return(NULL)
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  P0 <- as.matrix(ev$prior_mats$precision)
  Fm <- tryCatch(bigIRT_fisher_kernel_call("dense", ev, sdat, context,
    layout, ps, numeric(n)), error = function(e) NULL)
  if(is.null(Fm)) return(NULL)
  Fm <- Fm + as.matrix(bigIRT_prior_curvature_cached(sdat, layout, n, N, P0, context)) * outer(ps, ps)
  described <- unique(c(layout$B, layout$B_beta, layout$A, layout$A_beta, layout$C, layout$C_beta,
    layout$D, layout$D_beta, layout$B_mean, layout$A_mean, layout$C_mean, layout$D_mean,
    layout$ability_beta, layout$ability_mean))
  und <- setdiff(seq_len(n), described)
  if(length(und)){
    cb <- bigIRT_laplace_use_corr_block(Fm, layout$corr, ev, sdat, context, layout)
    Fm <- cb$M
    for(j in setdiff(und, cb$done)){
      hj <- h * max(1, abs(x[j]))
      xp <- x; xp[j] <- xp[j] + hj
      col <- (gfun(xp) - g) / hj
      Fm[, j] <- col; Fm[j, ] <- col
    }
  }
  Fm <- bigIRT_metric_decouple((Fm + t(Fm)) / 2, nonpos = FALSE)
  ch <- NULL; ridge <- 0
  for(attempt in 1:6){
    ch <- tryCatch(chol(if(ridge > 0) Fm + diag(ridge, n) else Fm), error = function(e) NULL)
    if(!is.null(ch)) break
    ridge <- if(ridge == 0) 1e-8 * max(abs(diag(Fm))) else ridge * 100
  }
  if(is.null(ch)) return(NULL)
  function(v) backsolve(ch, forwardsolve(t(ch), as.numeric(v)))
}

bigIRT_laplace_blockdiag_solver_cpp <- function(ev, x, g, sdat, context, layout, gfun, h = 1e-4){
  n <- length(ev$approx_grad); K <- as.integer(sdat$Nscales); N <- as.integer(sdat$Nsubs)
  rc <- context$row_context; st <- ev$state; rows <- context$train_rows
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  P0 <- as.matrix(ev$prior_mats$precision)
  Pr <- bigIRT_prior_curvature_cached(sdat, layout, n, N, P0, context)
  out <- tryCatch(bigIRT_fisher_kernel_call("blocks", ev, sdat, context,
    layout, ps, as.numeric(Matrix::diag(Pr)) * ps^2), error = function(e) NULL)
  if(is.null(out)) return(NULL)
  G <- out$G
  described <- unique(c(layout$B, layout$B_beta, layout$A, layout$A_beta, layout$C, layout$C_beta,
    layout$D, layout$D_beta, layout$B_mean, layout$A_mean, layout$C_mean, layout$D_mean,
    layout$ability_beta, layout$ability_mean))
  FG <- NULL
  if(length(G)){
    FG <- out$FGG + as.matrix(Pr[G, G, drop = FALSE]) * outer(ps[G], ps[G])
    cb <- bigIRT_laplace_use_corr_block(FG, match(intersect(layout$corr, G), G), ev, sdat, context, layout)
    FG <- cb$M
    for(j in setdiff(intersect(G, setdiff(seq_len(n), described)), cb$done)){
      hj <- h * max(1, abs(x[j]))
      xp <- x; xp[j] <- xp[j] + hj
      col <- ((gfun(xp) - g) / hj)[G]
      k <- match(j, G)
      FG[, k] <- col; FG[k, ] <- col
    }
    FG <- bigIRT_metric_decouple((FG + t(FG)) / 2)
    ch <- NULL; ridge <- 0
    for(attempt in 1:6){
      ch <- tryCatch(chol(if(ridge > 0) FG + diag(ridge, nrow(FG)) else FG), error = function(e) NULL)
      if(!is.null(ch)) break
      ridge <- if(ridge == 0) 1e-8 * max(abs(diag(FG))) else ridge * 100
    }
    if(is.null(ch)) return(NULL)
  }
  ## Group the blocks by size, so applying them is a few vector operations
  ## whatever the number of items.
  dims <- out$block_dim
  ofs <- c(0L, cumsum(as.numeric(dims)^2))
  pofs <- c(0L, cumsum(dims))
  groups <- lapply(sort(unique(dims)), function(P){
    b <- which(dims == P)
    idx <- vapply(b, function(i) out$block_par[pofs[i] + seq_len(P)], integer(P))
    inv <- vapply(b, function(i) out$block_inv[ofs[i] + seq_len(P * P)], numeric(P * P))
    list(P = P, idx = matrix(idx, nrow = P), inv = matrix(inv, nrow = P * P))
  })
  function(v){
    v <- as.numeric(v)
    res <- numeric(length(v))
    for(gr in groups){
      P <- gr$P
      for(a in seq_len(P)){
        acc <- 0
        for(b in seq_len(P)) acc <- acc + gr$inv[a + P * (b - 1L), ] * v[gr$idx[b, ]]
        res[gr$idx[a, ]] <- acc
      }
    }
    if(length(G)) res[G] <- backsolve(ch, forwardsolve(t(ch), v[G]))
    res
  }
}

## The metric for the Fisher-shaped L-BFGS: dense F where it is cheap and
## worth it, the block-diagonal Bd otherwise.
##
## Measured (dev1, dev/optim/): the dense F is the better metric for small,
## densely linked designs, where the coupling between items carries real
## curvature; at 2,000 items and above in sparse designs its assembly and
## Cholesky cost more than they save, and Bd matched or beat it. So the choice
## is by the number of free parameters, `bigIRT.dense_metric_max_par` (option).
bigIRT_laplace_metric_solver <- function(e, sdat, context, layout, gfun,
  type = getOption("bigIRT.laplace_metric", "auto")){
  n <- length(e$x)
  ## "none" builds no metric, which hands the fit to the mize path.
  if(identical(type, "none")) return(NULL)
  if(identical(type, "auto"))
    type <- if(n <= getOption("bigIRT.dense_metric_max_par", 1000)) "dense" else "blockdiag"
  m <- if(identical(type, "dense"))
    bigIRT_laplace_fisher_solver(e$ev, e$x, e$g, sdat, context, layout, gfun) else NULL
  if(is.null(m)){
    type <- "blockdiag"
    m <- bigIRT_laplace_blockdiag_solver(e$ev, e$x, e$g, sdat, context, layout, gfun)
  }
  if(is.null(m)) return(NULL)
  attr(m, "metric") <- type
  m
}

## L-BFGS minimising fg(x)$f, with H0 = gamma * minv (identity when minv is
## NULL). Unit step, Armijo backtracking by safeguarded quadratic
## interpolation; only a first step without a metric is shortened. Stops when
## the predicted gain 0.5 g'Hg falls below `gain_tol` (nats), and treats an
## Armijo failure below 1e-4 nats of predicted gain as the noise floor of the
## objective rather than a failure. `refresh(e)` may return a new minv every
## `refresh_every` iterations, or list(stop = TRUE) to end the search there
## (terminate "refresh_stop").
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
      ## A refresh may also move the objective itself (the quadrature
      ## refinement re-centres its nodes), in which case it hands back the
      ## current point re-evaluated, and the search continues from that --
      ## or it may say the steps since the last refresh were no progress on
      ## the objective it moved to, and end the search.
      if(is.list(m) && !is.function(m)){
        if(isTRUE(m$stop)){ term <- "refresh_stop"; break }
        if(!is.null(m$e)){ e <- m$e; f <- e$f; g <- e$g }
        m <- m$minv
      }
      if(!is.null(m)) mv <- m
    }
  }
  list(par = x, f = f, g = g, iter = it, backtracks = nback,
    terminate = list(what = term, val = gain))
}
