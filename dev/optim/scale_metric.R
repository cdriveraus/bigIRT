# Metrics for the Laplace item block that do not form the dense Fisher matrix.
#
# Everything is built from the row derivatives the package already assembles
# (bigIRT_laplace_row_derivatives): V, nr x n, rows against item parameters,
# and W, nr x NK, rows against abilities, both weighted by 1/sqrt(p(1-p)).
# The Fisher information of the marginal is
#
#     F = V'V - C' Ipp^-1 C + priors,   C = W'V,   Ipp = W'W + P0 (block diagonal)
#       = V' (I - W Ipp^-1 W') V + priors.
#
# W Ipp^-1 W' couples only rows of the same person, and a person answers an
# item once, so the item-diagonal blocks of F are exactly V' diag(q) V with
# q_r = 1 - w_r' Ipp_i^-1 w_r. That is Bd below: block diagonal by item, plus
# the rows of any parameter shared by all rows (covariate coefficients).
#
# Matrix-free products F v cost three sparse passes over the rows (V v, C v,
# C' u) and never form F. On top of Bd, a coarse space Z -- items grouped into
# clusters by the mean ability of the persons who answered them, one column
# per cluster and parameter type, plus an identity column for every
# non-item parameter -- carries the directions Bd cannot see: a shift shared by
# a group of items that the persons who took them absorb. E = Z'FZ is small
# and dense.
#
# Preconditioners (applications of an approximate F^-1), all SPD:
#   bd   Bd^-1
#   add  Bd^-1 + Z E^-1 Z'                         (additive two-level)
#   bal  Q + (I - QF) Bd^-1 (I - FQ), Q = Z E^-1 Z' (balancing two-level)
#   pcg  F^-1 itself, by preconditioned CG with `bal`, to a relative residual

sm_ops <- function(ev, sdat, context, layout){
  D <- bigIRT_laplace_row_derivatives(ev, sdat, context, layout)
  n <- D$n; N <- D$N; K <- D$K; P0 <- D$P0
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  V <- D$V %*% Matrix::Diagonal(x = ps)
  W <- D$W
  Ipp <- Matrix::forceSymmetric(Matrix::crossprod(W) +
    Matrix::kronecker(Matrix::Diagonal(N), Matrix::Matrix(P0, sparse = TRUE)))
  IppC <- Matrix::Cholesky(Ipp, perm = FALSE, LDL = FALSE)
  C <- Matrix::crossprod(W, V)
  am <- layout$ability_mean
  pr <- list(i = integer(0), j = integer(0), x = numeric(0))
  put <- function(i, j, x){ pr$i <<- c(pr$i, i); pr$j <<- c(pr$j, j); pr$x <<- c(pr$x, x) }
  if(length(am)){
    M <- Matrix::sparseMatrix(i = rep(seq_len(N * K), times = K), j = rep(am, each = N * K),
      x = as.numeric(vapply(seq_len(K), function(kk) rep(-P0[, kk], times = N), numeric(N * K))),
      dims = c(N * K, n))
    C <- C + M %*% Matrix::Diagonal(x = ps)
    for(a in seq_along(am)) for(b in seq_along(am)) put(am[a], am[b], N * P0[a, b])
  }
  if(isTRUE(as.logical(sdat$dopriors))){
    blk <- list(list("B", "B_mean", "BSDx", "BMeanSD"), list("A", "A_mean", "invspASD", "AMeanSD"),
      list("C", "C_mean", "logitCSD", "logitCMeanSD"), list("D", "D_mean", "logitDSD", "logitDMeanSD"))
    for(b in blk){
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
  Pr <- Matrix::sparseMatrix(i = pr$i, j = pr$j, x = pr$x, dims = c(n, n))
  Pr <- Matrix::Diagonal(x = ps) %*% Pr %*% Matrix::Diagonal(x = ps)
  VtV <- Matrix::crossprod(V) + Pr
  Fv <- function(v){
    v <- as.numeric(v)
    as.numeric(VtV %*% v - Matrix::crossprod(C, Matrix::solve(IppC, C %*% v, system = "A")))
  }
  ## q_r = 1 - w_r' Ipp_i^-1 w_r, from the K x K blocks directly. (Solving
  ## CHOLMOD against the sparse t(W) took 42 s at 577k rows for what is a
  ## per-person K x K inverse.)
  q <- sm_row_q(W, N, K, P0, as.integer(context$row_context$ids))
  Bd <- Matrix::forceSymmetric(Matrix::crossprod(V, Matrix::Diagonal(x = q) %*% V) + Pr)
  ## Parameters no row touches (the ability mean, whose curvature is all in
  ## Pr and C) need a positive diagonal for the factorisation.
  dg <- Matrix::diag(Bd)
  fixd <- which(!(dg > 0))
  if(length(fixd)) Bd <- Bd + Matrix::sparseMatrix(i = fixd, j = fixd,
    x = rep(max(median(dg[dg > 0]), 1e-8), length(fixd)), dims = c(n, n))
  BdC <- Matrix::Cholesky(Bd, perm = TRUE, LDL = FALSE)
  list(n = n, V = V, C = C, IppC = IppC, VtV = VtV, Pr = Pr, Fv = Fv, Bd = Bd,
    bd = function(v) as.numeric(Matrix::solve(BdC, as.numeric(v), system = "A")),
    nnz_V = Matrix::nnzero(V), nnz_C = Matrix::nnzero(C), nnz_Bd = Matrix::nnzero(Bd))
}

## q_r = 1 - w_r' Ipp_i^-1 w_r with Ipp_i = sum_{r in i} w_r w_r' + P0, for
## rows grouped by person `ids`. W is nr x NK with row r's entries in person
## i's K columns; they are gathered into an nr x K matrix, the blocks summed
## by person, and inverted by vectorised Gauss-Jordan over persons.
sm_row_q <- function(W, N, K, P0, ids){
  Wt <- as(W, "TsparseMatrix")
  Wd <- matrix(0, nrow(W), K)
  Wd[cbind(Wt@i + 1L, (Wt@j %% K) + 1L)] <- Wt@x
  A <- array(0, c(K, K, N))
  for(k in seq_len(K)) for(l in seq_len(K)){
    s <- numeric(N); a <- rowsum(Wd[, k] * Wd[, l], ids)
    s[as.integer(rownames(a))] <- a[, 1]
    A[k, l, ] <- s + P0[k, l]
  }
  Ai <- sm_block_inverse(A)
  q <- rep(1, nrow(W))
  for(k in seq_len(K)) for(l in seq_len(K)) q <- q - Wd[, k] * Ai[k, l, ids] * Wd[, l]
  pmax(q, 0)
}
## Inverse of each K x K slice of a K x K x N array of SPD matrices.
sm_block_inverse <- function(A){
  K <- dim(A)[1]; N <- dim(A)[3]
  if(K == 1L) return(array(1 / A[1, 1, ], c(1, 1, N)))
  M <- A; I <- array(0, c(K, K, N)); for(k in seq_len(K)) I[k, k, ] <- 1
  for(k in seq_len(K)){
    piv <- M[k, k, ]
    M[k, , ] <- sweep(matrix(M[k, , ], K), 2, piv, "/"); I[k, , ] <- sweep(matrix(I[k, , ], K), 2, piv, "/")
    for(r in setdiff(seq_len(K), k)){
      f <- M[r, k, ]
      M[r, , ] <- M[r, , ] - sweep(matrix(M[k, , ], K), 2, f, "*")
      I[r, , ] <- I[r, , ] - sweep(matrix(I[k, , ], K), 2, f, "*")
    }
  }
  I
}

## Coarse space: m clusters per item parameter type, by mean taker ability.
sm_coarse <- function(ops, layout, context, theta, m){
  n <- ops$n
  rc <- context$row_context
  rab <- as.matrix(theta)[as.integer(rc$ids), 1]
  refs <- list(B = list(rc$B_ref), A = lapply(seq_len(ncol(rc$A_ref)), function(k) rc$A_ref[, k]),
    C = list(rc$C_ref), D = list(rc$D_ref))
  zi <- integer(0); zj <- integer(0); nc <- 0L; covered <- logical(n)
  for(b in names(refs)){
    idx <- layout[[b]]
    if(!length(idx)) next
    s <- numeric(length(idx)); cnt <- numeric(length(idx))
    for(ref in refs[[b]]){
      keep <- ref > 0L & ref <= length(idx)
      if(!any(keep)) next
      s <- s + tabulate_sum(ref[keep], rab[keep], length(idx))
      cnt <- cnt + tabulate(ref[keep], nbins = length(idx))
    }
    mt <- ifelse(cnt > 0, s / pmax(cnt, 1), 0)
    mb <- max(1L, min(as.integer(m), length(idx)))
    grp <- pmin(mb, as.integer(ceiling(rank(mt, ties.method = "first") / length(idx) * mb)))
    zi <- c(zi, idx); zj <- c(zj, nc + grp); nc <- nc + mb
    covered[idx] <- TRUE
  }
  rest <- which(!covered)
  zi <- c(zi, rest); zj <- c(zj, nc + seq_along(rest)); nc <- nc + length(rest)
  Z <- Matrix::sparseMatrix(i = zi, j = zj, x = 1, dims = c(n, nc))
  VZ <- ops$V %*% Z; CZ <- ops$C %*% Z
  E <- as.matrix(Matrix::crossprod(VZ) + Matrix::crossprod(Z, ops$Pr %*% Z) -
    Matrix::crossprod(CZ, Matrix::solve(ops$IppC, CZ, system = "A")))
  E <- (E + t(E)) / 2
  ec <- tryCatch(chol(E), error = function(e) chol(E + diag(1e-10 * max(abs(diag(E))), nrow(E))))
  Qf <- function(v) as.numeric(Z %*% backsolve(ec, forwardsolve(t(ec), as.numeric(Matrix::crossprod(Z, v)))))
  list(Z = Z, Q = Qf, m = nc)
}
tabulate_sum <- function(g, x, n){
  out <- numeric(n); a <- rowsum(x, g); out[as.integer(rownames(a))] <- a[, 1]; out
}

sm_precond <- function(ops, co, type){
  switch(type,
    bd = ops$bd,
    add = function(v) ops$bd(v) + co$Q(v),
    bal = function(v){
      qv <- co$Q(v)
      t2 <- ops$bd(v - ops$Fv(qv))
      t2 - co$Q(ops$Fv(t2)) + qv
    })
}

## Preconditioned CG for F x = b. Returns x, iterations, and the Lanczos
## estimate of the extreme eigenvalues of M^-1 F.
sm_pcg <- function(Fv, b, Minv, rtol = 1e-6, maxit = 500L){
  x <- numeric(length(b)); r <- b; z <- Minv(r); p <- z
  rz <- sum(r * z); b0 <- sqrt(sum(b^2)); it <- 0L
  alphas <- numeric(0); betas <- numeric(0)
  while(it < maxit && sqrt(sum(r^2)) > rtol * b0){
    Ap <- Fv(p); a <- rz / sum(p * Ap)
    x <- x + a * p; r <- r - a * Ap
    z <- Minv(r); rz_new <- sum(r * z); be <- rz_new / rz
    alphas <- c(alphas, a); betas <- c(betas, be)
    p <- z + be * p; rz <- rz_new; it <- it + 1L
  }
  ev <- NA_real_
  if(it >= 2){
    k <- it
    d <- 1 / alphas + c(0, betas[-k] / alphas[-k])
    e <- sqrt(betas[-k]) / alphas[-k]
    T <- diag(d, k); if(k > 1){ T[cbind(1:(k - 1), 2:k)] <- e; T[cbind(2:k, 1:(k - 1))] <- e }
    ev <- range(eigen(T, symmetric = TRUE, only.values = TRUE)$values)
  }
  list(x = x, iter = it, eig = ev)
}

## A function applying the chosen approximate F^-1 at the evaluation `ev`.
sm_minv <- function(ev, sdat, context, layout, type = "bal", m = 100L, rtol = 1e-3){
  ops <- sm_ops(ev, sdat, context, layout)
  if(type == "bd") return(ops$bd)
  co <- sm_coarse(ops, layout, context, ev$posterior$theta_mode, m)
  if(type %in% c("add", "bal")) return(sm_precond(ops, co, type))
  if(type == "pcg"){
    M <- sm_precond(ops, co, "bal")
    return(function(v) sm_pcg(ops$Fv, as.numeric(v), M, rtol = rtol, maxit = 200L)$x)
  }
  stop("unknown metric type ", type)
}
