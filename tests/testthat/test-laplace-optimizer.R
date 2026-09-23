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
    optimizer = "fisher", ...){
    set.seed(seed)
    args <- list(Nsubs = ns, Nitems = ni, Nscales = K, AMean = 1.2, ASD = .3, BSD = 1,
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

  test_that("without a usable metric the Fisher path hands over to mize", {
    ## An information matrix too dense to hold is refused, and then there is no
    ## reliable predicted gain either, so the fit must run the mize path with
    ## its polish rather than an unshaped L-BFGS stopping on a number it
    ## cannot estimate.
    old <- options(bigIRT.fisher_max_nnz = 1)
    on.exit(options(old))
    f <- opt_fit(pl = 2, ns = 600, ni = 12)
    expect_false(identical(f$laplaceStatus$optimizer_terminate, "gain"))
    expect_gt(f$laplaceStatus$polish_steps, 0L)
    expect_true(isTRUE(f$laplaceStatus$converged))
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
}
