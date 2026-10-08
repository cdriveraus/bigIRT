## Signed (identity-link) loadings: fitIRT(signedLoadings = ).
##
## A free loading is normally softplus(raw), which keeps it positive; a signed
## one is raw itself, so its first derivative is one and its second zero, and
## every kernel that forms a loading or its derivative is handed a per-loading
## flag saying which (src/laplace_backend.cpp, agq_backend.cpp,
## laplace2_backend.cpp, fisher_blocks.cpp, and their R mirrors). What is
## asserted, each able to fail on its own:
##
## 1. the analytic gradient of each objective the Laplace path can end on --
##    Laplace, fixed-node quadrature, second-order Laplace -- matches central
##    differences at 1e-6, for the 2PL, 3PL and 4PL at two and three
##    dimensions, at a point where signed loadings are negative. A kernel that
##    took a signed loading's slope from softplus, or its value from one link
##    and its derivative from the other, fails here. The 2PL alone would prove
##    nothing about the asymptote slots (test-laplace-direct-gradients.R), so
##    all three run;
## 2. the second derivative used by the fast certificate's item blocks, the
##    Fisher row derivatives, and the C++ metric against the R one all treat
##    the identity link as the identity;
## 3. the prior and empirical Bayes keep signed loadings off the
##    discrimination mean and dispersion;
## 4. signed cross-loadings are recovered with their signs, positive ones
##    cannot go negative, the fit is certified, and the reported standard
##    errors carry no softplus Jacobian;
## 5. NULL and an all-FALSE matrix are the same fit.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## K dimensions of `per` items each. Every dimension's first three items
  ## also load on the next dimension, at -0.6, 0.5 and -0.4; every other cross
  ## loading is a fixed zero, so the rotation is pinned. `L` frees exactly the
  ## generating pattern.
  signed_sim <- function(seed, K, pl, Np = 300, per = 8){
    set.seed(seed)
    ni <- per * K
    prim <- rep(seq_len(K), each = per)
    A <- matrix(0, ni, K)
    A[cbind(seq_len(ni), prim)] <- exp(stats::rnorm(ni, log(1.2), .2))
    for(k in seq_len(K)){
      rows <- which(prim == k)[1:3]
      A[cbind(rows, k %% K + 1L)] <- c(-0.6, 0.5, -0.4)
    }
    s <- simIRT(Nsubs = Np, Nitems = ni, Nscales = K, NitemsAnswered = per, mirt = TRUE,
      loadings = A, primaryScale = prim, BSD = 1,
      logitCMean = if(pl >= 3) -1.5 else -20, logitCSD = if(pl >= 3) .3 else 0,
      logitDMean = if(pl >= 4) 2.5 else 20, logitDSD = if(pl >= 4) .3 else 0)
    L <- ifelse(A != 0, NA_real_, 0)
    dimnames(L) <- list(rownames(s$A), sort(unique(as.character(s$dat$Scale))))
    list(sim = s, A = A, L = L, prim = prim)
  }

  ## Prepared data from a two-iteration fit (the fit itself is unused), and
  ## the direct objective at a point away from the start where the signed
  ## loadings are set to values of both signs.
  signed_evaluator <- function(ss, pl, signed = "cross") {
    f <- suppressWarnings(fitIRT(ss$sim$dat, pl = pl, cores = 1, priors = TRUE,
      ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace", laplaceOuterIter = 2, verbose = 0, plot = FALSE,
      keepInternals = TRUE, laplaceRefine = "none", laplaceCertify = FALSE,
      loadings = ss$L, signedLoadings = signed))
    sdat <- f$internals$sdat
    st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
    ilay <- lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))]
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, layout = ilay)
    ctx$grain_size <- bigIRT:::bigIRT_laplace_subject_grain(sdat$Nsubs, 1L)
    ctx$direct_layout <- lay
    ctx$estimateAbilityCorr <- FALSE
    ctx$corr_paramization <- "normalized_chol"
    st$laplaceCorrParam <- "normalized_chol"
    par <- bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    set.seed(97); par <- par + stats::rnorm(length(par), 0, 0.15)
    sgn <- bigIRT:::bigIRT_laplace_signed_A(sdat) == 1L
    signed_slots <- lay$A[sgn]
    par[signed_slots] <- rep(c(-0.6, 0.45, -0.35), length.out = length(signed_slots))
    ev <- function(v, cx = ctx) bigIRT:::bigIRT_laplace_direct_objective(par = v, state = st,
      sdat = sdat, prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6,
      max_attempts = 8L, max_iter = 4000L, tol = 1e-12, keep_covariance = TRUE, context = cx)
    list(layout = lay, par = par, sdat = sdat, ctx = ctx, ev = ev, state = st,
      signed = sgn, signed_slots = signed_slots, primary_slots = lay$A[!sgn])
  }

  slot_relerr <- function(f, par, slots, h = 1e-5) {
    g <- f(par)$approx_grad
    fd <- vapply(slots, function(k) {
      up <- par; up[k] <- up[k] + h; dn <- par; dn[k] <- dn[k] - h
      (f(up)$value - f(dn)$value) / (2 * h)
    }, numeric(1))
    max(abs(g[slots] - fd) / pmax(1, abs(fd)))
  }

  test_that("gradients with negative signed loadings match finite differences (Laplace, quadrature, second-order; 2PL to 4PL; K = 2, 3)", {
    for(K in 2:3) for(pl in 2:4) {
      ss <- signed_sim(7300 + 10 * K + pl, K, pl)
      d <- signed_evaluator(ss, pl)
      lab <- function(obj, b) sprintf("%s K=%d pl=%d %s", obj, K, pl, b)
      ## The point being tested has negative loadings, and the kernels see them.
      expect_equal(length(d$signed_slots), 3L * K)
      lap <- d$ev(d$par)
      re <- bigIRT:::bigIRT_laplace_row_effective(state = lap$state, sdat = d$sdat,
        thetaBase = lap$posterior$theta_mode, rows = d$ctx$train_rows, context = d$ctx$row_context)
      expect_lt(min(re$loadings), -0.3)
      L <- d$layout
      slots <- list(signed = d$signed_slots, primary = head(d$primary_slots, 2L),
        B = head(L$B, 2L), C = head(L$C, 2L), D = head(L$D, 2L), A_mean = L$A_mean)
      slots <- slots[lengths(slots) > 0L]
      expect_true(all(c("signed", "A_mean") %in% names(slots)))
      if(pl >= 3) expect_true("C" %in% names(slots))
      if(pl >= 4) expect_true("D" %in% names(slots))
      ## Laplace.
      for(b in names(slots))
        expect_lt(slot_relerr(d$ev, d$par, slots[[b]]), 1e-6, label = lab("laplace", b))
      ## Quadrature with the nodes held where the Laplace posterior sits, the
      ## objective whose Fisher-identity gradient is exact (test-agq.R).
      cq <- d$ctx; cq$agq <- bigIRT:::bigIRT_agq_rules(K, nodes = if(K == 2) 7L else 5L)
      cq$agq_centre <- lap$posterior
      fq <- function(v) d$ev(v, cq)
      for(b in names(slots))
        expect_lt(slot_relerr(fq, d$par, slots[[b]]), 1e-6, label = lab("agq", b))
      ## Second-order Laplace.
      c2 <- d$ctx; c2$laplace2 <- TRUE
      f2 <- function(v) d$ev(v, c2)
      for(b in names(slots))
        expect_lt(slot_relerr(f2, d$par, slots[[b]]), 1e-6, label = lab("laplace2", b))
    }
  })

  test_that("an item covariate on discrimination adds to a signed loading's parameter, gradients exact", {
    ## One shared coefficient reaches softplus and signed loadings alike, so
    ## its gradient mixes the two slopes; each objective is checked.
    ss <- signed_sim(7406, 2L, 3L)
    ip <- matrix(stats::rnorm(nrow(ss$A)), ncol = 1)
    dt <- data.table::as.data.table(ss$sim$dat)
    dt$V1 <- ip[as.integer(as.character(dt$Item)), 1]
    f <- suppressWarnings(fitIRT(dt, pl = 3, cores = 1, priors = TRUE, ebayes = FALSE,
      dropPerfectScores = FALSE, normalise = FALSE, marginalApprox = "laplace",
      laplaceOuterIter = 2, verbose = 0, keepInternals = TRUE, laplaceRefine = "none",
      laplaceCertify = FALSE, loadings = ss$L, signedLoadings = "cross",
      AitemPreds = "V1", betaScale = 2))
    sdat <- f$internals$sdat
    st <- bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- bigIRT:::bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat,
      layout = lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))])
    ctx$grain_size <- 64L; ctx$direct_layout <- lay; ctx$estimateAbilityCorr <- FALSE
    ctx$corr_paramization <- "normalized_chol"; st$laplaceCorrParam <- "normalized_chol"
    par <- bigIRT:::bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    set.seed(96); par <- par + stats::rnorm(length(par), 0, 0.15)
    sg <- lay$A[bigIRT:::bigIRT_laplace_signed_A(sdat) == 1L]
    par[sg] <- rep(c(-0.6, 0.45, -0.35), length.out = length(sg))
    par[lay$A_beta] <- 0.3 / attr(lay, "par_scale")[lay$A_beta]
    expect_length(lay$A_beta, 1L)
    ev <- function(v, cx = ctx) bigIRT:::bigIRT_laplace_direct_objective(par = v, state = st,
      sdat = sdat, prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6,
      max_attempts = 8L, max_iter = 4000L, tol = 1e-12, keep_covariance = TRUE, context = cx)
    lap <- ev(par)
    ## The covariate term really is inside the signed loadings: a signed
    ## row's loading is its parameter plus 0.3 x V1, not a softplus of it.
    re <- bigIRT:::bigIRT_laplace_row_effective(state = lap$state, sdat = sdat,
      thetaBase = lap$posterior$theta_mode, rows = ctx$train_rows, context = ctx$row_context)
    rc <- ctx$row_context
    q1 <- which(bigIRT:::bigIRT_laplace_signed_A(sdat) == 1L)[1]
    hit <- which(apply(rc$A_ref == q1, 1, any))[1]
    k1 <- which(rc$A_ref[hit, ] == q1)
    expect_equal(re$loadings[hit, k1], lap$state$invspApars[q1] + 0.3 * rc$A_pred[hit, 1], tolerance = 1e-12)
    cq <- ctx; cq$agq <- bigIRT:::bigIRT_agq_rules(2L, nodes = 7L); cq$agq_centre <- lap$posterior
    c2 <- ctx; c2$laplace2 <- TRUE
    for(o in list(list("laplace", ctx), list("agq", cq), list("laplace2", c2))){
      fo <- function(v) ev(v, o[[2]])
      expect_lt(slot_relerr(fo, par, lay$A_beta), 1e-6, label = paste(o[[1]], "A_beta"))
      expect_lt(slot_relerr(fo, par, sg), 1e-6, label = paste(o[[1]], "signed"))
    }
  })

  test_that("the fast certificate's item blocks treat a signed loading's second derivative as zero", {
    ## The quadrature kernel's item blocks use the link's second derivative,
    ## which is softplus'' for a positive loading and zero for a signed one.
    ## Against the differenced fixed-node Hessian, at a negative cross-loading.
    ss <- signed_sim(7401, 2L, 3L, Np = 500)
    d <- signed_evaluator(ss, 3L)
    lap <- d$ev(d$par)
    ctx <- d$ctx; ctx$agq <- bigIRT:::bigIRT_agq_rules(2L, nodes = 7L); ctx$agq_centre <- lap$posterior
    cb <- ctx; cb$agq_blocks <- TRUE
    blk <- d$ev(d$par, cb)$item_fg$item_blocks
    K <- 2L; lay <- d$layout; rc <- d$ctx$row_context
    prior_sd <- c(B = d$sdat$BSDx, A = d$sdat$invspASD, C = d$sdat$logitCSD)
    ## Items 1 and 9 carry a negative signed cross-loading (-0.6 on the other
    ## dimension); item 4 none.
    worst <- 0
    for(j in c(1L, 9L, 4L)){
      r <- match(j, rc$items)
      sc <- c(B = lay$B[rc$B_ref[r]],
        setNames(vapply(seq_len(K), function(k) if(rc$A_ref[r, k] > 0) lay$A[rc$A_ref[r, k]]
          else NA_integer_, 1L), paste0("A", seq_len(K))),
        C = lay$C[rc$C_ref[r]])
      ## Slots B, A1..AK, C are the kernel's first K + 2 (D is last).
      keep <- which(!is.na(sc)); ix <- sc[keep]
      if(j != 4L) expect_true(any(d$par[ix[grepl("^A", names(ix))]] < 0))
      Lb <- matrix(blk[, , j], K + 3)[keep, keep, drop = FALSE] -
        diag(1 / prior_sd[substr(names(ix), 1, 1)]^2, length(ix))
      H <- matrix(NA_real_, length(ix), length(ix))
      for(a in seq_along(ix)){
        up <- d$par; up[ix[a]] <- up[ix[a]] + 1e-4; dn <- d$par; dn[ix[a]] <- dn[ix[a]] - 1e-4
        H[, a] <- ((d$ev(up, ctx)$approx_grad - d$ev(dn, ctx)$approx_grad) / 2e-4)[ix]
      }
      worst <- max(worst, max(abs(Lb - (H + t(H)) / 2)) / max(abs(H)))
    }
    expect_lt(worst, 1e-6)
  })

  test_that("Fisher row derivatives and the C++ metric use slope one for a signed loading", {
    ss <- signed_sim(7402, 2L, 3L, Np = 400)
    f <- suppressWarnings(fitIRT(ss$sim$dat, pl = 3, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, loadings = ss$L, signedLoadings = "cross",
      laplaceCertify = FALSE))
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    sgn <- bigIRT:::bigIRT_laplace_signed_A(d$sdat) == 1L
    expect_true(any(d$state$invspApars[sgn] < -0.2))
    ## Row derivatives against differences of the row probability itself.
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
    for(j in c(d$layout$A[sgn], head(d$layout$A[!sgn], 2L))){
      xp <- d$par; xm <- d$par
      xp[j] <- xp[j] + h / ps[j]; xm[j] <- xm[j] - h / ps[j]
      fd <- (rowp(xp) - rowp(xm)) / (2 * h) * w
      expect_lt(max(abs(as.numeric(D$V[, j]) - fd)) / max(abs(fd)), 1e-5)
    }
    ## The C++ dense and block-diagonal metrics against the R assemblies.
    gfun <- function(p) -bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
      sdat = d$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE, context = d$context,
      tol = 1e-10, max_iter = 200L)$approx_grad
    g0 <- gfun(d$par)
    n <- length(d$par)
    set.seed(3); V <- matrix(stats::rnorm(n * 5), n, 5)
    for(kind in c("dense", "blockdiag")){
      solve_with <- function(impl){
        if(kind == "dense"){
          old <- options(bigIRT.dense_impl = impl); on.exit(options(old))
          bigIRT:::bigIRT_laplace_fisher_solver(d$eval, d$par, g0, d$sdat, d$context, d$layout, gfun = gfun)
        } else {
          old <- options(bigIRT.blockdiag_impl = impl); on.exit(options(old))
          bigIRT:::bigIRT_laplace_blockdiag_solver(d$eval, d$par, g0, d$sdat, d$context, d$layout, gfun = gfun)
        }
      }
      mc <- solve_with("cpp"); mr <- solve_with("R")
      rel <- max(abs(apply(V, 2, mc) - apply(V, 2, mr))) / max(abs(apply(V, 2, mr)))
      expect_lt(rel, 1e-8, label = kind)
    }
  })

  test_that("a signed loading's prior is centred at zero and does not pull on the A mean", {
    ss <- signed_sim(7403, 2L, 2L)
    d <- signed_evaluator(ss, 2L)
    lay <- d$layout; sdat <- d$sdat; sg <- d$signed
    expect_gt(length(lay$A_mean), 0L)
    st <- bigIRT:::bigIRT_laplace_unpack_direct_state(d$par, d$state, sdat, layout = lay)
    st2 <- st; st2$invspApars[sg] <- st$invspApars[sg] + 0.7
    p1 <- bigIRT:::bigIRT_laplace_item_prior(st, sdat, layout = lay)
    p2 <- bigIRT:::bigIRT_laplace_item_prior(st2, sdat, layout = lay)
    ## Moving the signed loadings changes the prior by their N(0, invspASD)
    ## terms exactly, and leaves the A mean's gradient alone.
    expect_equal(p2$value - p1$value,
      sum(stats::dnorm(st2$invspApars[sg], 0, sdat$invspASD, log = TRUE) -
          stats::dnorm(st$invspApars[sg], 0, sdat$invspASD, log = TRUE)), tolerance = 1e-12)
    expect_equal(p2$grad[lay$A_mean], p1$grad[lay$A_mean], tolerance = 1e-12)
    expect_false(isTRUE(all.equal(st$invspAMean, 0)))
    ## The prior curvature the optimiser's metric uses is this prior's.
    n <- bigIRT:::bigIRT_laplace_layout_size(lay)
    Pr <- as.matrix(bigIRT:::bigIRT_laplace_prior_curvature(sdat, lay, n, sdat$Nsubs,
      diag(1, sdat$Nscales)))
    pg <- function(par) bigIRT:::bigIRT_laplace_item_prior(
      bigIRT:::bigIRT_laplace_unpack_direct_state(par, d$state, sdat, layout = lay),
      sdat, layout = lay)$grad
    ps <- attr(lay, "par_scale")
    for(j in c(lay$A_mean, lay$A[sg][1], lay$A[!sg][1])){
      up <- d$par; up[j] <- up[j] + 1e-5 / ps[j]; dn <- d$par; dn[j] <- dn[j] - 1e-5 / ps[j]
      col <- -(pg(up) - pg(dn)) / 2e-5
      idx <- unlist(lay[c("A", "A_mean")])
      expect_equal(Pr[idx, j], col[idx], tolerance = 1e-6)
    }
  })

  ## The recovery fixture: three dimensions, cross-loadings drawn by simIRT
  ## (N(0, crossLoadingSD), so of both signs), the generating zero pattern
  ## fixed and everything else free.
  rec <- local({
    set.seed(11)
    K <- 3; ni <- 30
    sim <- simIRT(Nsubs = 1500, Nitems = ni, Nscales = K, mirt = TRUE,
      AMean = 1.2, ASD = .2, crossLoadingSD = .5, loadingSparsity = .4,
      BSD = 1, logitCMean = -20, logitCSD = 0)
    A <- sim$A
    L <- ifelse(A != 0, NA_real_, 0)
    dimnames(L) <- list(rownames(A), sort(unique(as.character(sim$dat$Scale))))
    cross <- A != 0; cross[cbind(seq_len(ni), sim$primaryScale)] <- FALSE
    fs <- fitIRT(sim$dat, pl = 2, cores = 2, loadings = L, signedLoadings = "cross",
      marginalApprox = "laplace", keepInternals = TRUE)
    fp <- fitIRT(sim$dat, pl = 2, cores = 2, loadings = L, marginalApprox = "laplace")
    list(sim = sim, A = A, L = L, cross = cross, fs = fs, fp = fp)
  })

  test_that("signed cross-loadings are recovered with their signs; softplus ones cannot go negative", {
    A <- rec$A; cross <- rec$cross
    ## The truth has clearly negative cross-loadings to recover.
    expect_gt(sum(A[cross] < -0.3), 2L)
    es <- as.matrix(rec$fs$pars$A); ep <- as.matrix(rec$fp$pars$A)
    expect_gt(stats::cor(es[cross], A[cross]), 0.8)
    big <- cross & abs(A) > 0.3
    expect_true(all(sign(es[big]) == sign(A[big])))
    ## Primary loadings stay softplus-positive.
    prim <- cbind(seq_len(nrow(A)), rec$sim$primaryScale)
    expect_true(all(es[prim] > 0))
    ## Under the default every free cross-loading is a softplus, so the
    ## negative ones are pinned above zero and the signed fit beats it.
    expect_true(all(ep[cross] > 0))
    expect_gt(rec$fs$optim$logLik, rec$fp$optim$logLik + 10)
    ## The signed fit started its signed loadings at zero and moved them.
    expect_true(all(abs(rec$fs$pars$invspApars[bigIRT:::bigIRT_laplace_signed_A(rec$fs$prepared) == 1L]) > 1e-4))
  })

  test_that("a signed fit is certified, and its loading errors carry no softplus Jacobian", {
    f <- rec$fs; cross <- rec$cross
    expect_equal(f$laplaceStatus$certificate$status, "certified")
    expect_true(f$laplaceStatus$converged)
    ## A signed loading is the identity of its coordinate: never saturated,
    ## however far negative, where a softplus at -12 would be.
    expect_length(f$laplaceStatus$certificate$saturated, 0L)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f)
    st <- d$state; q <- which(bigIRT:::bigIRT_laplace_signed_A(d$sdat) == 1L)[1]
    st$invspApars[q] <- -12
    sl <- bigIRT:::bigIRT_transform_slopes(st, d$sdat, d$layout, d$context)
    expect_equal(sl[d$layout$A[q]], 1)
    ii <- itemInformation(f, method = "exact")
    expect_false(is.null(ii$se_loadings))
    td <- suppressMessages(tidyIRT(f, conf.int = TRUE))
    xr <- td[grepl("^A_", td$term), ]
    expect_equal(nrow(xr), sum(cross))
    expect_true(all(is.finite(xr$std.error)))
    expect_true(all(xr$std.error > 0))
    j <- match(xr$item, as.character(f$itemPars$Item))
    k <- match(sub("^A_", "", xr$term), colnames(f$pars$A))
    ## jac = 1: the reported error is the free parameter's own.
    expect_equal(xr$std.error, ii$se_loadings[cbind(j, k)])
    expect_equal(xr$estimate, as.matrix(f$pars$A)[cbind(j, k)])
    ## Signed intervals are not clipped at zero.
    expect_true(any(xr$conf.high < 0))
    ## The primary loadings are softplus, so theirs is the raw error times
    ## logistic(raw) = 1 - exp(-A).
    ta <- td[td$term == "A", ]
    ja <- match(ta$item, as.character(f$itemPars$Item))
    own <- rec$sim$primaryScale[ja]
    expect_true(all(is.finite(ta$std.error)))
    expect_equal(ta$std.error, ii$se_loadings[cbind(ja, own)] * (1 - exp(-ta$estimate)))
  })

  test_that("reverse-keyed items at one dimension: signed primaries, empirical Bayes, jac = 1", {
    set.seed(21); N <- 1500; ni <- 20
    a <- c(rep(1.2, 15), rep(-1, 5)) * exp(stats::rnorm(ni, 0, .15)); b <- stats::rnorm(ni)
    th <- stats::rnorm(N)
    dat <- data.frame(id = rep(seq_len(N), ni), Item = rep(seq_len(ni), each = N), Scale = 1L)
    dat$score <- stats::rbinom(nrow(dat), 1, stats::plogis(a[dat$Item] * th[dat$id] - b[dat$Item]))
    S <- matrix(rep(c(FALSE, TRUE), c(15, 5)), ni, 1, dimnames = list(as.character(seq_len(ni)), "1"))
    ## ebayes is on by default: with signed loadings in play it must leave
    ## them out of the discrimination update, which the next test checks.
    f <- fitIRT(dat, pl = 2, cores = 2, marginalApprox = "laplace", signedLoadings = S,
      keepInternals = TRUE)
    A <- f$itemPars$A[match(as.character(seq_len(ni)), as.character(f$itemPars$Item))]
    expect_true(all(A[16:20] < 0))
    expect_true(all(A[1:15] > 0))
    expect_gt(stats::cor(A, a), 0.95)
    expect_equal(f$laplaceStatus$certificate$status, "certified")
    ii <- itemInformation(f, method = "exact")
    td <- suppressMessages(tidyIRT(f, conf.int = TRUE))
    ta <- td[td$term == "A", ]
    jj <- match(ta$item, as.character(f$itemPars$Item))
    se_raw <- ii$se[jj, "A"]
    signed_item <- ta$item %in% as.character(16:20)
    expect_equal(ta$std.error[signed_item], se_raw[signed_item])
    expect_equal(ta$std.error[!signed_item], se_raw[!signed_item] * (1 - exp(-ta$estimate[!signed_item])))
    expect_true(all(ta$conf.high[signed_item] < 0))
  })

  test_that("empirical Bayes leaves signed loadings out of the discrimination update", {
    set.seed(23); N <- 800; ni <- 16
    a <- c(rep(1.2, 12), rep(-1, 4)) * exp(stats::rnorm(ni, 0, .2)); b <- stats::rnorm(ni)
    th <- stats::rnorm(N)
    dat <- data.frame(id = rep(seq_len(N), ni), Item = rep(seq_len(ni), each = N), Scale = 1L)
    dat$score <- stats::rbinom(nrow(dat), 1, stats::plogis(a[dat$Item] * th[dat$id] - b[dat$Item]))
    S <- matrix(rep(c(FALSE, TRUE), c(12, 4)), ni, 1, dimnames = list(as.character(seq_len(ni)), "1"))
    f <- suppressWarnings(fitIRT(dat, pl = 2, cores = 1, marginalApprox = "laplace",
      signedLoadings = S, ebayes = FALSE, keepInternals = TRUE))
    sdat <- f$internals$sdat
    sg <- bigIRT:::bigIRT_laplace_signed_A(sdat) == 1L
    expect_equal(sum(sg), 4L)
    rows <- bigIRT:::bigIRT_laplace_row_context(sdat, rows = seq_len(sdat$Nobs))
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, row_context = rows)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    post <- d$eval$posterior; theta <- post$theta_mode
    st1 <- d$state
    st2 <- st1; st2$invspApars[sg] <- st1$invspApars[sg] + c(2, -1.5, 3, -2.5)
    eff <- function(st) bigIRT:::bigIRT_laplace_row_effective(state = st, sdat = sdat,
      thetaBase = theta, rows = rows$rows, context = rows)
    ## The moment rule: moving the signed loadings, whose rows touch nothing
    ## else at one dimension, leaves the A update exactly where it was, and
    ## the mean it sets is the softplus loadings' alone.
    m1 <- bigIRT:::bigIRT_laplace_eb_update(state = st1, sdat = sdat, context = ctx,
      row_effective = eff(st1), row_context = rows, thetaBase = theta)
    m2 <- bigIRT:::bigIRT_laplace_eb_update(state = st2, sdat = sdat, context = ctx,
      row_effective = eff(st2), row_context = rows, thetaBase = theta)
    expect_true("invspASD" %in% m1$changed)
    expect_identical(m1$sdat$invspASD, m2$sdat$invspASD)
    expect_identical(m1$sdat$invspAMeandat, m2$sdat$invspAMeandat)
    expect_equal(m1$sdat$invspAMeandat, mean(st1$invspApars[!sg]))
    ## The Laplace rule: the profile is over the softplus estimates, and an
    ## item with a signed loading holds its A precision, so the slope of the
    ## objective in log tau_A does not see the signed loadings at all.
    p1 <- bigIRT:::bigIRT_laplace_hyper_pieces(st1, sdat, ctx, eff(st1), rows, post, theta)
    p2 <- bigIRT:::bigIRT_laplace_hyper_pieces(st2, sdat, ctx, eff(st2), rows, post, theta)
    expect_equal(p1$active, c("B", "A"))
    expect_equal(p1$est$A, as.numeric(st1$invspApars[!sg]))
    expect_equal(sum(p1$signed_item), 4L)
    dA <- function(p){ h <- 1e-4; lt <- log(c(1.1, 0.4))
      (bigIRT:::bigIRT_laplace_hyper_objective(lt + c(0, h), p) -
       bigIRT:::bigIRT_laplace_hyper_objective(lt - c(0, h), p)) / (2 * h) }
    expect_gt(abs(dA(p1)), 1e-2)
    expect_equal(dA(p1), dA(p2), tolerance = 1e-6)
    u <- bigIRT:::bigIRT_laplace_hyper_update(st1, sdat, ctx, eff(st1), rows, post, theta)
    expect_equal(u$sdat$invspAMeandat, mean(st1$invspApars[!sg]))
  })

  test_that("signedLoadings = NULL and an all-FALSE matrix are the same fit", {
    ss <- signed_sim(7404, 2L, 3L, Np = 400)
    args <- list(dat = ss$sim$dat, pl = 3, cores = 1, marginalApprox = "laplace", loadings = ss$L)
    f0 <- do.call(fitIRT, args)
    S0 <- matrix(FALSE, nrow(ss$L), ncol(ss$L), dimnames = dimnames(ss$L))
    expect_no_warning(f1 <- do.call(fitIRT, c(args, list(signedLoadings = S0))), message = "unrecognised")
    expect_null(f1$prepared$signedA)
    expect_identical(f1$itemPars, f0$itemPars)
    expect_identical(f1$optim$logLik, f0$optim$logLik)
    expect_identical(f1$laplaceStatus$certificate$gap, f0$laplaceStatus$certificate$gap)
    f2 <- do.call(fitIRT, c(args, list(signedLoadings = FALSE)))
    expect_identical(f2$itemPars, f0$itemPars)
  })

  test_that("signedLoadings is refused where it cannot apply, and checked where it can", {
    ss <- signed_sim(7405, 2L, 2L, Np = 150)
    dat <- ss$sim$dat; L <- ss$L
    expect_error(fitIRT(dat, pl = 2, loadings = L, signedLoadings = "cross", marginalApprox = "none"),
      "marginalApprox = \"laplace\"")
    expect_error(fitIRT(dat, pl = 1, marginalApprox = "laplace", signedLoadings = "cross"),
      "pl >= 2")
    S <- matrix(FALSE, nrow(L), ncol(L), dimnames = dimnames(L))
    S[which(!is.na(L) & L == 0)[1]] <- TRUE
    expect_error(fitIRT(dat, pl = 2, marginalApprox = "laplace", loadings = L, signedLoadings = S),
      "fixed loading")
    S2 <- matrix(FALSE, nrow(L), ncol(L), dimnames = dimnames(L)); S2[1, 1] <- NA
    expect_error(fitIRT(dat, pl = 2, marginalApprox = "laplace", loadings = L, signedLoadings = S2),
      "TRUE or FALSE")
    expect_error(fitIRT(dat, pl = 2, marginalApprox = "laplace", loadings = L, signedLoadings = "all"),
      "must be NULL, FALSE")
    ## Aligned by name, as `loadings` is: a row-permuted matrix means the same.
    Sc <- is.na(L); Sc[cbind(seq_len(nrow(L)), ss$prim)] <- FALSE
    perm <- rev(seq_len(nrow(Sc)))
    fa <- fitIRT(dat, pl = 2, cores = 1, marginalApprox = "laplace", loadings = L,
      signedLoadings = Sc[perm, ], laplaceCertify = FALSE)
    fb <- fitIRT(dat, pl = 2, cores = 1, marginalApprox = "laplace", loadings = L,
      signedLoadings = "cross", laplaceCertify = FALSE)
    expect_identical(fa$prepared$signedA, fb$prepared$signedA)
    expect_identical(fa$itemPars, fb$itemPars)
    ## "cross" with the default one-hot loadings has nothing to sign, and says so.
    expect_warning(fitIRT(dat, pl = 2, cores = 1, marginalApprox = "laplace",
      signedLoadings = "cross", laplaceCertify = FALSE), "no free cross-loadings")
  })
}
