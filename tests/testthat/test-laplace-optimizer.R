## The Fisher-shaped L-BFGS on the Laplace path (R/laplaceOptimizer.R).
##
## Three properties, each able to fail on its own:
## 1. the row derivatives the Fisher information is built from are the
##    derivatives of the row probability -- checked against finite differences
##    of p itself, so nothing statistical enters;
## 2. the assembled information agrees with the central-difference Hessian of
##    the objective to within what it deliberately leaves out (the second
##    derivative of the log-determinant terms, a few per cent here), which an
##    index or sign error in the coupling, the ability mean or the priors would
##    exceed many times over;
## 3. the optimiser reaches the optimum the mize path reaches, in fewer
##    objective evaluations.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  opt_fit <- function(pl = 2, preds = FALSE, seed = 5, ns = 500, ni = 12, K = 1,
    optimizer = "fisher", na = ni, ...){
    set.seed(seed)
    args <- list(Nsubs = ns, Nitems = ni, Nscales = K, AMean = 1.2, ASD = .3, BSD = 1,
      NitemsAnswered = na,
      logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) .5 else 0)
    if(pl == 4){ args$logitDMean <- 2.5; args$logitDSD <- .5 }
    if(preds){
      args$personPreds <- matrix(rnorm(ns * 2), ncol = 2)
      args$AbilityPredEffects <- matrix(c(.5, -.4), 1)
    }
    s <- do.call(simIRT, args)
    fa <- list(dat = s$dat, pl = pl, marginalApprox = "laplace", cores = 1, ebayes = FALSE,
      keepInternals = TRUE, laplaceOptimizer = optimizer, ...)
    if(preds) fa$personPreds <- c("V1", "V2")
    suppressWarnings(do.call(fitIRT, fa))
  }

  test_that("Fisher row derivatives are the derivatives of the row probability", {
    f <- opt_fit(pl = 4, preds = TRUE)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    D <- bigIRT:::bigIRT_laplace_row_derivatives(d$eval, d$sdat, d$context, d$layout)
    ps <- attr(d$layout, "par_scale")
    rowp <- function(par, theta){
      st <- bigIRT:::bigIRT_laplace_unpack_direct_state(par, d$state, d$sdat, layout = d$layout,
        corr_paramization = d$context$corr_paramization)
      re <- bigIRT:::bigIRT_laplace_row_effective(state = st, sdat = d$sdat, thetaBase = theta,
        rows = d$context$train_rows, context = d$context$row_context)
      g <- stats::plogis(as.numeric(re$eta_row))
      as.numeric(re$c_row) + (as.numeric(re$d_row) - as.numeric(re$c_row)) * g
    }
    th <- d$eval$posterior$theta_mode
    p0 <- rowp(d$par, th)
    w <- 1 / sqrt(p0 * (1 - p0))
    h <- 1e-6
    ## V is in natural coordinates; the packed vector is scaled by par_scale.
    cols <- c(d$layout$B[1:3], d$layout$A[1:3], d$layout$C[1:3], d$layout$D[1:3],
      d$layout$ability_beta)
    worst <- 0
    for(j in cols){
      xp <- d$par; xm <- d$par
      xp[j] <- xp[j] + h / ps[j]; xm[j] <- xm[j] - h / ps[j]
      fd <- (rowp(xp, th) - rowp(xm, th)) / (2 * h) * w
      an <- as.numeric(D$V[, j])
      worst <- max(worst, max(abs(an - fd)) / max(abs(fd)))
    }
    expect_lt(worst, 1e-5)
    ## And against the abilities.
    for(i in c(1, 7, 50)){
      thp <- th; thm <- th; thp[i, 1] <- th[i, 1] + h; thm[i, 1] <- th[i, 1] - h
      fd <- (rowp(d$par, thp) - rowp(d$par, thm)) / (2 * h) * w
      expect_lt(max(abs(as.numeric(D$W[, i]) - fd)), 1e-6 * max(1, max(abs(fd))))
    }
  })

  test_that("Fisher row derivatives cover item-covariate coefficients, shared and item-specific", {
    for(specific in c(FALSE, TRUE)){
      set.seed(9)
      ip <- matrix(rnorm(12 * 2, 0, .5), 12, 2)
      s <- simIRT(Nsubs = 500, Nitems = 12, AMean = 1.2, ASD = .2, BSD = .5,
        logitCMean = -1.5, logitCSD = .3, itemPreds = ip,
        AitemPredEffects = matrix(c(.2, -.1), 1, 2), BitemPredEffects = matrix(c(.5, .3), 1, 2))
      f <- suppressWarnings(fitIRT(s$dat, pl = 3, marginalApprox = "laplace", cores = 1,
        ebayes = FALSE, keepInternals = TRUE, AitemPreds = c("V1", "V2"),
        BitemPreds = c("V1", "V2"), CitemPreds = "V1", itemSpecificBetas = specific))
      d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
      D <- bigIRT:::bigIRT_laplace_row_derivatives(d$eval, d$sdat, d$context, d$layout)
      ps <- attr(d$layout, "par_scale")
      th <- d$eval$posterior$theta_mode
      rowp <- function(par){
        st <- bigIRT:::bigIRT_laplace_unpack_direct_state(par, d$state, d$sdat, layout = d$layout,
          corr_paramization = d$context$corr_paramization)
        re <- bigIRT:::bigIRT_laplace_row_effective(state = st, sdat = d$sdat, thetaBase = th,
          rows = d$context$train_rows, context = d$context$row_context)
        g <- stats::plogis(as.numeric(re$eta_row))
        as.numeric(re$c_row) + (as.numeric(re$d_row) - as.numeric(re$c_row)) * g
      }
      p0 <- rowp(d$par); w <- 1 / sqrt(p0 * (1 - p0)); h <- 1e-6
      cols <- unique(c(head(d$layout$A_beta, 3), head(d$layout$B_beta, 3), head(d$layout$C_beta, 3),
        d$layout$A[1], d$layout$C[1]))
      expect_gt(length(cols), 4L)
      for(j in cols){
        xp <- d$par; xm <- d$par
        xp[j] <- xp[j] + h / ps[j]; xm[j] <- xm[j] - h / ps[j]
        fd <- (rowp(xp) - rowp(xm)) / (2 * h) * w
        expect_lt(max(abs(as.numeric(D$V[, j]) - fd)) / max(abs(fd)), 1e-5)
      }
    }
  })

  test_that("Fisher information agrees with the numerical Hessian up to the log-determinant terms", {
    for(cfg in list(list(pl = 2, preds = TRUE, estMeans = c("A", "B", "C", "D")),
                    list(pl = 2, preds = FALSE, estMeans = c("A", "ability")))){
      f <- opt_fit(pl = cfg$pl, preds = cfg$preds, ns = 800, ni = 10, estMeans = cfg$estMeans)
      d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
      gr <- function(p) -bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
        sdat = d$sdat, prior_precision = f$internals$priorPrecision,
        theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
        context = d$context, tol = 1e-10, max_iter = 200L)$approx_grad
      n <- length(d$par); H <- matrix(0, n, n); h <- 1e-4
      for(j in seq_len(n)){
        xp <- d$par; xm <- d$par; xp[j] <- xp[j] + h; xm[j] <- xm[j] - h
        H[, j] <- (gr(xp) - gr(xm)) / (2 * h)
      }
      H <- (H + t(H)) / 2
      Fm <- as.matrix(bigIRT:::bigIRT_laplace_fisher_info(d$eval, d$sdat, d$context, d$layout))
      expect_length(attr(bigIRT:::bigIRT_laplace_fisher_info(d$eval, d$sdat, d$context, d$layout),
        "undescribed"), 0L)
      expect_lt(sqrt(sum((Fm - H)^2)) / sqrt(sum(H^2)), 0.1)
      ## Every coupled block, not only the whole: the ability mean and the
      ## covariate coefficients sit in rows of their own.
      for(b in c("ability_mean", "ability_beta", "B_mean", "A_mean")){
        ix <- d$layout[[b]]
        if(!length(ix)) next
        expect_lt(max(abs(Fm[ix, ] - H[ix, ])) / max(abs(H[ix, ])), 0.1)
      }
    }
  })

  test_that("a refused dense metric falls to the block-diagonal one, and no metric to mize", {
    ## The dense information is refused when too dense to hold; the
    ## block-diagonal metric then takes over and the fit still stops on its
    ## predicted gain.
    old <- options(bigIRT.fisher_max_nnz = 1)
    on.exit(options(old))
    f <- opt_fit(pl = 2, ns = 600, ni = 12)
    expect_identical(f$laplaceStatus$optimizer_metric, "blockdiag")
    expect_identical(f$laplaceStatus$optimizer_terminate, "gain")
    options(old)
    ## With no metric at all there is no reliable predicted gain either, so the
    ## fit must run the mize path with its polish rather than an unshaped
    ## L-BFGS stopping on a number it cannot estimate.
    old2 <- options(bigIRT.laplace_metric = "none")
    on.exit(options(old2), add = TRUE)
    f <- opt_fit(pl = 2, ns = 600, ni = 12)
    expect_false(identical(f$laplaceStatus$optimizer_terminate, "gain"))
    expect_true(is.na(f$laplaceStatus$optimizer_metric))
    expect_gt(f$laplaceStatus$polish_steps, 0L)
    expect_true(isTRUE(f$laplaceStatus$converged))
    options(old2)
    ## And the default at this size is the dense metric.
    f <- opt_fit(pl = 2, ns = 600, ni = 12)
    expect_identical(f$laplaceStatus$optimizer_metric, "dense")
  })

  test_that("the Fisher optimiser reaches the mize optimum in fewer evaluations", {
    for(cfg in list(list(pl = 2, preds = FALSE, K = 1), list(pl = 3, preds = FALSE, K = 1),
                    list(pl = 2, preds = TRUE, K = 1), list(pl = 2, preds = FALSE, K = 2))){
      extra <- if(cfg$K == 2) list(estimateAbilityCorr = TRUE) else list()
      fm <- do.call(opt_fit, c(list(pl = cfg$pl, preds = cfg$preds, K = cfg$K, ns = 1500, ni = 20,
        optimizer = "mize"), extra))
      ff <- do.call(opt_fit, c(list(pl = cfg$pl, preds = cfg$preds, K = cfg$K, ns = 1500, ni = 20,
        optimizer = "fisher"), extra))
      ## Scored on the same path-free number: each final point evaluated with
      ## the person modes cold-started and solved to 1e-10.
      score <- function(f){
        d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
        bigIRT:::bigIRT_laplace_direct_objective(par = d$par, state = d$state, sdat = d$sdat,
          prior_precision = f$internals$priorPrecision,
          theta_init = 0 * d$eval$posterior$theta_mode, keep_covariance = TRUE,
          context = d$context, tol = 1e-10, max_iter = 200L)$value
      }
      expect_gt(score(ff), score(fm) - 2e-3)
      expect_lt(ff$optim$target_evals, fm$optim$target_evals)
      expect_identical(ff$laplaceStatus$optimizer_terminate, "gain")
    }
  })

  ## ---- the block-diagonal metric used at scale ----

  test_that("the matrix-free Fisher product equals the dense Fisher information", {
    f <- opt_fit(pl = 3, preds = TRUE, ns = 600, ni = 20, na = 8)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    P <- bigIRT:::bigIRT_laplace_fisher_parts(d$eval, d$sdat, d$context, d$layout)
    Fd <- as.matrix(bigIRT:::bigIRT_laplace_fisher_info(d$eval, d$sdat, d$context, d$layout))
    set.seed(2)
    for(k in 1:3){
      v <- rnorm(P$n)
      expect_lt(max(abs(P$Fv(v) - Fd %*% v)) / max(abs(Fd %*% v)), 1e-10)
    }
  })

  test_that("row q is one minus the share each row's own person absorbs, at two dimensions", {
    f <- opt_fit(pl = 2, K = 2, ns = 300, ni = 6, na = 4)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    D <- bigIRT:::bigIRT_laplace_row_derivatives(d$eval, d$sdat, d$context, d$layout)
    ids <- as.integer(d$context$row_context$ids)
    q <- bigIRT:::bigIRT_laplace_row_q(D$W, D$N, D$K, D$P0, ids)
    ## Direct: per person, Ipp_i = W_i'W_i + P0 and q_r = 1 - w_r' Ipp_i^-1 w_r.
    W <- as.matrix(D$W)
    for(i in c(1, 17, 150)){
      cols <- (i - 1) * 2 + 1:2
      rows <- which(ids == i)
      Wi <- W[rows, cols, drop = FALSE]
      Ii <- crossprod(Wi) + D$P0
      qd <- 1 - rowSums((Wi %*% solve(Ii)) * Wi)
      expect_equal(q[rows], qd, tolerance = 1e-10)
    }
  })

  test_that("the block-diagonal metric is F's item blocks, shared block and prior border, and nothing else", {
    ## A sparse design with every kind of block: person covariates and an
    ## estimated ability mean (shared), and a 3PL (three parameters an item).
    ## With the item means estimated, the prior ties every item parameter to
    ## its mean: that border is kept, the data's item-shared coupling is not.
    for(cfg in list(list(pl = 3, preds = TRUE, est = c("A", "B", "C", "D")),
                    list(pl = 2, preds = FALSE, est = c("A", "ability")))){
      f <- opt_fit(pl = cfg$pl, preds = cfg$preds, ns = 600, ni = 20, na = 8, estMeans = cfg$est)
      d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
      Fd <- as.matrix(bigIRT:::bigIRT_laplace_fisher_info(d$eval, d$sdat, d$context, d$layout))
      n <- nrow(Fd)
      inv <- bigIRT:::bigIRT_laplace_blockdiag_solver(d$eval, d$par, -d$eval$approx_grad,
        d$sdat, d$context, d$layout, gfun = function(p) stop("no finite differences expected"))
      expect_true(is.function(inv))
      ## Recover Bd from its inverse, column by column.
      Bi <- sapply(seq_len(n), function(j){ e <- numeric(n); e[j] <- 1; inv(e) })
      Bd <- solve(Bi)
      L <- d$layout; ni <- as.integer(d$sdat$Nitems)
      item_of <- rep(NA_integer_, n)
      for(b in c("B", "A", "C", "D")) if(length(L[[b]]) == ni) item_of[L[[b]]] <- seq_len(ni)
      shared <- which(is.na(item_of))
      expect_gt(length(shared), 0L)
      same_block <- outer(item_of, item_of, "==")
      same_block[is.na(same_block)] <- FALSE
      same_block[shared, shared] <- TRUE
      ps <- attr(L, "par_scale")
      Pr <- as.matrix(bigIRT:::bigIRT_laplace_prior_curvature(d$sdat, L, n, as.integer(d$sdat$Nsubs),
        as.matrix(d$eval$prior_mats$precision))) * outer(ps, ps)
      border <- Pr != 0 & !same_block
      border[shared, shared] <- FALSE
      if(any(cfg$est %in% c("A", "B", "C", "D"))) expect_gt(sum(border), 0L)
      same_block <- same_block | border
      scale <- max(abs(Fd))
      expect_lt(max(abs(Bd[same_block] - Fd[same_block])) / scale, 1e-8)
      expect_lt(max(abs(Bd[!same_block])) / scale, 1e-8)
    }
  })

  test_that("with tight item priors the block metric still finds the optimum", {
    ## Item SDs at 0.001, as empirical Bayes can leave them, with the item
    ## means estimated: the prior ties every item to its mean with precision
    ## 1e6. Without that border the block metric took moving a mean (the items
    ## following) to be a million times stiffer than it is, and this fit
    ## stopped after two iterations 8.9 nats short of its optimum; with it, it
    ## is certified in four.
    withr::local_options(bigIRT.laplace_metric = "blockdiag")
    set.seed(3)
    s <- simIRT(Nsubs = 300, Nitems = 30, Nscales = 1, AMean = 1, ASD = .2, BMean = 0, BSD = .5,
      logitCMean = -20, logitCSD = 0)
    f <- suppressWarnings(fitIRT(s$dat, pl = 2, cores = 1, ebayes = FALSE, BSD = .001,
      invspASD = .001, dropPerfectScores = FALSE))
    expect_equal(f$laplaceStatus$optimizer_metric, "blockdiag")
    expect_equal(f$laplaceStatus$certificate$status, "certified")
    expect_lt(f$laplaceStatus$certificate$gap, 1e-3)
  })

  test_that("the C++ block-diagonal metric equals the R assembly", {
    ## The test above holds the default (C++) build against the dense F. This
    ## one covers what it does not: item-specific covariate coefficients, and a
    ## two-dimensional fit with estimated correlations, whose columns are
    ## differenced rather than assembled.
    set.seed(71)
    ip <- matrix(rnorm(20 * 2, 0, .3), 20, 2)
    s1 <- simIRT(Nsubs = 500, Nitems = 20, NitemsAnswered = 10, AMean = 1.2, ASD = .3, BSD = 1,
      logitCMean = -1.5, logitCSD = .5, itemPreds = ip, BitemPredEffects = matrix(c(.5, -.3), 1))
    f1 <- suppressWarnings(fitIRT(dat = s1$dat, pl = 3, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, BitemPreds = c("V1", "V2"), itemSpecificBetas = TRUE,
      laplaceOuterIter = 5))
    s2 <- simIRT(Nsubs = 500, Nitems = 24, Nscales = 2, NitemsAnswered = 12, AMean = 1.2, ASD = .3,
      BSD = 1, logitCMean = -20, logitCSD = 0)
    f2 <- suppressWarnings(fitIRT(dat = s2$dat, pl = 2, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, estimateAbilityCorr = TRUE, laplaceOuterIter = 5))
    for(f in list(f1, f2)){
      d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
      gfun <- function(p) -bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
        sdat = d$sdat, prior_precision = f$internals$priorPrecision,
        theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE, context = d$context,
        tol = 1e-10, max_iter = 200L)$approx_grad
      g0 <- gfun(d$par)
      solve_with <- function(impl){
        old <- options(bigIRT.blockdiag_impl = impl); on.exit(options(old))
        bigIRT:::bigIRT_laplace_blockdiag_solver(d$eval, d$par, g0, d$sdat, d$context,
          d$layout, gfun = gfun)
      }
      mc <- solve_with("cpp"); mr <- solve_with("R")
      expect_true(is.function(mc)); expect_true(is.function(mr))
      n <- length(d$par)
      V <- matrix(rnorm(n * 5), n, 5)
      rel <- max(abs(apply(V, 2, mc) - apply(V, 2, mr))) / max(abs(apply(V, 2, mr)))
      expect_lt(rel, 1e-8)
    }
    expect_gt(length(bigIRT:::bigIRT_direct_eval_from_fit(f1)$layout$B_beta), 20L)
    expect_gt(length(bigIRT:::bigIRT_direct_eval_from_fit(f2)$layout$corr), 0L)
  })

  test_that("the C++ dense metric equals the R assembly", {
    set.seed(72)
    s2 <- simIRT(Nsubs = 400, Nitems = 24, Nscales = 2, NitemsAnswered = 12, AMean = 1.2, ASD = .3,
      BSD = 1, logitCMean = -20, logitCSD = 0)
    f2 <- suppressWarnings(fitIRT(dat = s2$dat, pl = 2, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, estimateAbilityCorr = TRUE, laplaceOuterIter = 5))
    f1 <- opt_fit(pl = 3, preds = TRUE, ns = 400, ni = 16, na = 10, estMeans = c("A", "ability"))
    for(f in list(f1, f2)){
      d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
      gfun <- function(p) stop("no differences expected")
      solve_with <- function(impl){
        old <- options(bigIRT.dense_impl = impl); on.exit(options(old))
        bigIRT:::bigIRT_laplace_fisher_solver(d$eval, d$par, -d$eval$approx_grad, d$sdat,
          d$context, d$layout, gfun = gfun)
      }
      mc <- solve_with("cpp"); mr <- solve_with("R")
      n <- length(d$par)
      V <- matrix(rnorm(n * 5), n, 5)
      rel <- max(abs(apply(V, 2, mc) - apply(V, 2, mr))) / max(abs(apply(V, 2, mr)))
      expect_lt(rel, 1e-8)
    }
  })

  test_that("the closed-form correlation information matches its direct definition", {
    ## 1/2 sum_i tr(W_i dS_j W_i dS_l), W_i = (Sigma + D_i^-1)^-1, computed the
    ## long way -- explicit inverses and traces -- against the Kronecker
    ## accumulation and the D - D V D identity the package uses.
    set.seed(3)
    K <- 3L; N <- 40L; sdv <- c(1, 1.5, 0.8)
    cp <- c(0.4, -0.3, 0.2)
    Sig <- function(p) diag(sdv) %*% bigIRT:::bigIRT_laplace_corr_from_packed(p, K,
      paramization = "normalized_chol") %*% diag(sdv)
    S0 <- Sig(cp); P0 <- solve(S0)
    Dl <- lapply(seq_len(N), function(i){ X <- matrix(rnorm(K * 5), 5, K); crossprod(X) })
    H <- array(0, c(K, K, N)); for(i in seq_len(N)) H[, , i] <- P0 + Dl[[i]]
    ev <- list(posterior = list(precision = H), prior_mats = list(precision = P0),
      state = list(AbilityCorrPars = cp))
    sdat <- list(Nscales = K, Nsubs = N, AbilitySD = sdv)
    lay <- list(corr = 1:3); attr(lay, "par_scale") <- c(1, 1, 1)
    Fa <- bigIRT:::bigIRT_laplace_corr_fisher(ev, sdat, lay, paramization = "normalized_chol")
    dS <- lapply(1:3, function(j){ h <- 1e-6; pp <- cp; pm <- cp; pp[j] <- pp[j] + h; pm[j] <- pm[j] - h
      (Sig(pp) - Sig(pm)) / (2 * h) })
    Fd <- matrix(0, 3, 3)
    for(i in seq_len(N)){
      W <- solve(S0 + solve(Dl[[i]]))
      for(j in 1:3) for(l in 1:3) Fd[j, l] <- Fd[j, l] + 0.5 * sum(diag(W %*% dS[[j]] %*% W %*% dS[[l]]))
    }
    expect_lt(max(abs(Fa - Fd)) / max(abs(Fd)), 1e-6)
  })

  test_that("forced to the block-diagonal metric, the optimiser still reaches the mize optimum", {
    for(cfg in list(list(pl = 2, preds = TRUE), list(pl = 3, preds = FALSE))){
      fm <- opt_fit(pl = cfg$pl, preds = cfg$preds, ns = 3000, ni = 120, na = 25, optimizer = "mize")
      old <- options(bigIRT.laplace_metric = "blockdiag"); on.exit(options(old))
      fb <- opt_fit(pl = cfg$pl, preds = cfg$preds, ns = 3000, ni = 120, na = 25)
      options(old)
      expect_identical(fb$laplaceStatus$optimizer_metric, "blockdiag")
      score <- function(f){
        d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
        bigIRT:::bigIRT_laplace_direct_objective(par = d$par, state = d$state, sdat = d$sdat,
          prior_precision = f$internals$priorPrecision,
          theta_init = 0 * d$eval$posterior$theta_mode, keep_covariance = TRUE,
          context = d$context, tol = 1e-10, max_iter = 200L)$value
      }
      expect_gt(score(fb), score(fm) - 2e-3)
      expect_lt(fb$optim$target_evals, fm$optim$target_evals)
    }
  })

  test_that("an explicit laplacePolish under the fisher optimizer is refused, not silently ignored", {
    ## The block-Newton polish only runs after mize (see the comment above the
    ## polish block in bigIRT_laplace_optimize_direct); under fisher it used
    ## to be a silent no-op whatever laplacePolish was set to. A nonzero,
    ## explicitly requested value should now fail loudly by name instead.
    expect_error(
      opt_fit(pl = 2, ns = 300, ni = 12, optimizer = "fisher", laplacePolish = 4L),
      "fisher"
    )
    ## The default (NA) resolves to the optimiser's own choice, silently:
    ## none under fisher, some under mize.
    f_fisher <- opt_fit(pl = 2, ns = 300, ni = 12, optimizer = "fisher")
    expect_identical(f_fisher$laplaceStatus$polish_steps, 0L)
    f_mize <- opt_fit(pl = 2, ns = 300, ni = 12, optimizer = "mize")
    expect_gt(f_mize$laplaceStatus$polish_steps, 0L)
    ## An explicit 0 is always allowed, on either optimiser -- it asks for no
    ## polish, which fisher already gives it.
    f0 <- opt_fit(pl = 2, ns = 300, ni = 12, optimizer = "fisher", laplacePolish = 0L)
    expect_identical(f0$laplaceStatus$polish_steps, 0L)
  })

  ## When the gradient is not the derivative of what is evaluated -- one
  ## person whose mode cannot be resolved does this -- the predicted gain need
  ## never fall below the tolerance while line searches keep accepting steps
  ## that gain nothing. A 3-D 3PL fit ran to the iteration cap this way on
  ## Linux (3,700 evaluations with the objective flat to 3e-5 nats), where on
  ## Windows the noise happened to end it at iteration 31. Here the gradient
  ## promises a constant gain and the objective falls by 1e-7 an evaluation:
  ## the search ran all 500 iterations; it must stop at the noise floor. And a
  ## badly conditioned quadratic, slow but real progress, still ends on the
  ## gain criterion.
  test_that("the L-BFGS search stops when the objective has stopped falling", {
    n <- 0L
    fg <- function(x){ n <<- n + 1L; list(f = 1 - 1e-7 * n, g = rep(.1, 5)) }
    r <- bigIRT:::bigIRT_laplace_lbfgs(rep(1, 5), fg, minv = function(v) v, maxit = 500L, gain_tol = 1e-3)
    expect_identical(r$terminate$what, "noise_floor")
    expect_lt(r$iter, 20L)
    H <- diag(10^seq(0, 4, length.out = 6))
    fq <- function(x) list(f = 0.5 * sum(x * (H %*% x)), g = as.numeric(H %*% x))
    r2 <- bigIRT:::bigIRT_laplace_lbfgs(rep(3, 6), fq, maxit = 500L, gain_tol = 1e-8)
    expect_identical(r2$terminate$what, "gain")
  })
}
