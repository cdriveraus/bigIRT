## Second-order Laplace (src/laplace2_backend.cpp, context$laplace2).
##
## Three properties, each able to fail on its own: the kernel's correction is
## the formula it claims, computed here the long way; the gradient of the
## corrected objective matches central differences at 1e-6, with the person
## modes solved to 1e-12 (the mode adjoint assumes an exact mode); and the
## correction moves each person's value towards the exact integral.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  l2_evaluator <- function(dat, pl, fit_args = list(), ecorr = FALSE) {
    f <- suppressWarnings(do.call(fitIRT, c(list(dat = dat, pl = pl, cores = 1,
      priors = TRUE, ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace", laplaceOuterIter = 2, verbose = 0, plot = FALSE,
      keepInternals = TRUE, laplaceRefine = "none"), fit_args)))
    sdat <- f$internals$sdat
    st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = ecorr)
    ilay <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, layout = ilay)
    ctx$grain_size <- bigIRT:::bigIRT_laplace_subject_grain(sdat$Nsubs, 1L)
    ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- ecorr
    ctx$corr_paramization <- "normalized_chol"
    st$laplaceCorrParam <- "normalized_chol"
    par <- bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    set.seed(98); par <- par + stats::rnorm(length(par), 0, 0.15)
    ev <- function(v, l2 = TRUE) {
      cx <- ctx; cx$laplace2 <- l2
      bigIRT:::bigIRT_laplace_direct_objective(par = v, state = st, sdat = sdat,
        prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6, max_attempts = 8L,
        max_iter = 4000L, tol = 1e-12, keep_covariance = TRUE, context = cx)
    }
    list(layout = lay, par = par, sdat = sdat, ctx = ctx, ev = ev)
  }

  slot_relerr <- function(d, slots, h = 1e-5) {
    g <- d$ev(d$par)$approx_grad
    fd <- vapply(slots, function(k) {
      up <- d$par; up[k] <- up[k] + h; dn <- d$par; dn[k] <- dn[k] - h
      (d$ev(up)$value - d$ev(dn)$value) / (2 * h)
    }, numeric(1))
    max(abs(g[slots] - fd) / pmax(1, abs(fd)))
  }

  cross_sim <- function(seed, Np = 300, pl = 3, preds = FALSE) {
    set.seed(seed)
    ni <- 24; K <- 2
    A0 <- matrix(0, ni, K); prim <- rep(1:2, each = ni / 2)
    A0[cbind(seq_len(ni), prim)] <- exp(stats::rnorm(ni, log(1.2), .2))
    A0[c(3, 8, 15, 20), ] <- A0[c(3, 8, 15, 20), ] + 0.4 * (A0[c(3, 8, 15, 20), ] == 0)
    args <- list(Nsubs = Np, Nitems = ni, Nscales = K, NitemsAnswered = ni, mirt = TRUE,
      loadings = A0, primaryScale = prim, BSD = 1, AbilityCorr = matrix(c(1, .4, .4, 1), 2),
      logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) .3 else 0)
    if(preds){ args$personPreds <- matrix(stats::rnorm(Np * 2), Np, 2)
      args$AbilityPredEffects <- matrix(c(.4, -.3, .3, -.2), 2, 2) }
    s <- do.call(simIRT, args)
    L <- ifelse(abs(s$A) > 0, NA_real_, 0)
    dimnames(L) <- list(rownames(s$A), sort(unique(as.character(s$dat$Scale))))
    list(sim = s, L = L)
  }

  test_that("the second-order correction is the formula it claims", {
    cs <- cross_sim(6101)
    d <- l2_evaluator(cs$sim$dat, pl = 3, list(loadings = cs$L))
    e0 <- d$ev(d$par, l2 = FALSE); e1 <- d$ev(d$par, l2 = TRUE)
    K <- 2L; sdat <- d$sdat
    re <- bigIRT:::bigIRT_laplace_row_effective(state = e0$state, sdat = sdat,
      thetaBase = e0$posterior$theta_mode, rows = d$ctx$train_rows, context = d$ctx$row_context)
    ids <- as.integer(d$ctx$row_context$ids); y <- sdat$score[d$ctx$train_rows]
    lo <- matrix(as.numeric(re$loadings), ncol = K); eta <- as.numeric(re$eta_row)
    cc <- as.numeric(re$c_row); dd <- as.numeric(re$d_row)
    llr <- function(e, i) { p <- cc[i] + (dd[i] - cc[i]) * plogis(e)
      y[i] * log(p) + (1 - y[i]) * log1p(-p) }
    der <- function(e, i, k) { h <- 2e-3; f <- function(x) llr(x, i)
      switch(k - 1, (f(e + h) - 2 * f(e) + f(e - h)) / h^2,
        (f(e + 2 * h) - 2 * f(e + h) + 2 * f(e - h) - f(e - 2 * h)) / (2 * h^3),
        (f(e + 2 * h) - 4 * f(e + h) + 6 * f(e) - 4 * f(e - h) + f(e - 2 * h)) / h^4) }
    P0 <- as.matrix(e0$prior_mats$precision)
    corr <- 0
    for(i in seq_len(sdat$Nsubs)) {
      r <- which(ids == i); a <- lo[r, , drop = FALSE]
      l2 <- vapply(r, function(k) der(eta[k], k, 2), 1)
      l3 <- vapply(r, function(k) der(eta[k], k, 3), 1)
      l4 <- vapply(r, function(k) der(eta[k], k, 4), 1)
      ## Observed curvature, which for the 3PL can be negative row by row; a
      ## person whose total data curvature is not PSD gets no correction.
      Dw <- crossprod(a, a * (-l2))
      if(min(eigen(Dw, symmetric = TRUE, only.values = TRUE)$values) < 0) next
      H <- P0 + Dw; S <- solve(H)
      C <- a %*% S %*% t(a); cd <- diag(C)
      corr <- corr + sum(l4 * cd^2) / 8 + sum(outer(l3, l3) * (3 * outer(cd, cd) * C + 2 * C^3)) / 24
    }
    expect_lt(abs((e1$value - e0$value) - corr), 1e-4 * abs(corr) + 1e-6)
    expect_gt(abs(corr), 1e-2)
  })

  test_that("second-order Laplace gradients match finite differences (3PL, 4PL, K = 1)", {
    set.seed(6102)
    sim <- simIRT(Nsubs = 250, Nitems = 20, Nscales = 1, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3,
      logitDMean = 2, logitDSD = .3)
    for(pl in 3:4) {
      d <- l2_evaluator(sim$dat, pl = pl)
      L <- d$layout
      for(b in c("A", "B", "C", "D")) if(length(L[[b]]))
        expect_lt(slot_relerr(d, head(L[[b]], 3L)), 1e-6, label = paste(pl, b))
      for(b in c("A_mean", "B_mean", "C_mean", "D_mean")) if(length(L[[b]]))
        expect_lt(slot_relerr(d, L[[b]]), 1e-6, label = paste(pl, b))
    }
  })

  test_that("second-order Laplace gradients match finite differences (K = 2, cross loadings)", {
    cs <- cross_sim(6103, preds = TRUE)
    d <- l2_evaluator(cs$sim$dat, pl = 3, list(loadings = cs$L, personPreds = c("V1", "V2"),
      betaScale = 2, estMeans = c("Ability", "A", "C")), ecorr = TRUE)
    L <- d$layout
    expect_gt(length(L$corr), 0L); expect_gt(length(L$ability_beta), 0L)
    expect_gt(length(L$ability_mean), 0L)
    d$par[L$corr] <- 0.5
    expect_lt(slot_relerr(d, head(L$A, 6L)), 1e-6)
    expect_lt(slot_relerr(d, head(L$B, 3L)), 1e-6)
    expect_lt(slot_relerr(d, head(L$C, 3L)), 1e-6)
    expect_lt(slot_relerr(d, L$ability_beta), 1e-6)
    expect_lt(slot_relerr(d, L$ability_mean), 1e-6)
    expect_lt(slot_relerr(d, L$corr), 1e-6)
  })

  test_that("the correction moves each person towards the exact integral", {
    set.seed(6104)
    sim <- simIRT(Nsubs = 80, Nitems = 15, Nscales = 1, NitemsAnswered = 15,
      AMean = 1.5, ASD = .3, BMean = 0, BSD = 1, logitCMean = -1, logitCSD = .3)
    d <- l2_evaluator(sim$dat, pl = 3)
    ex <- { cx <- d$ctx; cx$agq <- bigIRT:::bigIRT_agq_rules(1L, nodes = 41L)
      bigIRT:::bigIRT_laplace_direct_objective(par = d$par, state = d$ev(d$par)$state,
        sdat = d$sdat, prior_precision = bigIRT:::bigIRT_laplace_prior_mats(d$sdat, jitter = 1e-6)$precision_array,
        theta_init = d$ev(d$par)$posterior$theta_mode, keep_covariance = TRUE, context = cx, tol = 1e-12,
        max_iter = 4000L) }
    lap <- d$ev(d$par, l2 = FALSE)$value; l2v <- d$ev(d$par, l2 = TRUE)$value
    expect_lt(abs(l2v - ex$value), 0.25 * abs(lap - ex$value))
  })
}
