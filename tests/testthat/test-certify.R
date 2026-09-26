## The convergence certificate (R/certify.R).
##
## The first half is ctsem's matrix tests for the same arithmetic
## (tests/testthat/test-backend-optimgap.R, 2026-09-25), copied with the
## arithmetic: the quantity is a function of a Hessian and a gradient, so a
## constructed pair says whether it is right, and a constructed pair is the
## only way to put a genuinely flat or genuinely negative direction in front of
## it on demand. The second half is bigIRT's own: the noise band a differenced
## Hessian needs, the block form, the transform slopes, and fits.

test_that("the gap is the predicted improvement, exactly, for a quadratic", {
  H <- diag(c(4, 25))
  g <- c(2, 5)
  out <- bigIRT:::bigIRT_optim_gap(-H, g)
  expect_true(out$ok)
  expect_equal(out$gap, 0.5 * (2^2 / 4 + 5^2 / 25))
  expect_equal(out$lambda, sqrt(2 * out$gap))
  expect_equal(out$step, c(2 / 4, 5 / 25))
  expect_equal(out$ntrusted, 2L)
  expect_equal(out$nflat, 0L)
  expect_equal(out$nnegative, 0L)
})

test_that("the gap is invariant to rescaling a parameter, and the gradient is not", {
  H <- matrix(c(4, 1, 1, 25), 2, 2)
  g <- c(2, 5)
  A <- diag(c(1, 1000))
  Hs <- solve(A) %*% H %*% solve(A)
  gs <- as.numeric(solve(A) %*% g)
  bare <- bigIRT:::bigIRT_optim_gap(-H, g)
  scaled <- bigIRT:::bigIRT_optim_gap(-Hs, gs)
  expect_equal(scaled$gap, bare$gap)
  expect_equal(scaled$lambda, bare$lambda)
  expect_false(isTRUE(all.equal(max(abs(gs)), max(abs(g)))))
})

test_that("a flat direction is excluded from the gap and its gradient kept", {
  H <- diag(c(4, 0))
  g <- c(2, 7)
  out <- bigIRT:::bigIRT_optim_gap(-H, g)
  expect_equal(out$ntrusted, 1L)
  expect_equal(out$nflat, 1L)
  expect_equal(out$gap, 0.5 * 2^2 / 4)
  expect_equal(out$residual, c(0, 7))
  expect_equal(out$residual_norm, 7)
  expect_true(all(is.finite(out$step)))
  expect_equal(out$step[2L], 0)
})

test_that("negative curvature is counted and reported as not a maximum", {
  out <- bigIRT:::bigIRT_optim_gap(-diag(c(4, -9)), c(1, 1))
  expect_equal(out$nnegative, 1L)
  verdict <- bigIRT:::bigIRT_certify_verdict(out, probe = list(gain = 0), tolerance = 0.01)
  expect_equal(verdict$status, "notmaximum")
  expect_false(verdict$certified)
  expect_match(verdict$reason, "negative curvature")
})

test_that("a small gap with a live flat direction is not certified", {
  out <- bigIRT:::bigIRT_optim_gap(-diag(c(4, 0)), c(1e-8, 7))
  expect_lt(out$gap, 1e-10)
  verdict <- bigIRT:::bigIRT_certify_verdict(out, probe = list(gain = 1.4), tolerance = 0.01)
  expect_equal(verdict$status, "notstationary")
  expect_match(verdict$reason, "no curvature")
  ## The same point with nothing to be had along that direction is certified:
  ## a flat direction is not by itself a failure.
  expect_equal(bigIRT:::bigIRT_certify_verdict(out, probe = list(gain = 0),
    tolerance = 0.01)$status, "certified")
})

test_that("a gap above tolerance says how far, in nats", {
  out <- bigIRT:::bigIRT_optim_gap(-diag(c(4, 25)), c(2, 5))
  verdict <- bigIRT:::bigIRT_certify_verdict(out, probe = list(gain = 0), tolerance = 0.01)
  expect_equal(verdict$status, "suboptimal")
  expect_match(verdict$reason, "nats above this estimate")
  expect_false(verdict$certified)
})

test_that("the probe reports the best actual gain, and zero when there is none", {
  rising <- function(x) 3 * x[2L]
  found <- bigIRT:::bigIRT_optim_gap_probe(rising, at = c(0, 0), direction = c(0, 1), value = 0)
  expect_equal(found$gain, 12)
  expect_equal(found$length, 4)
  falling <- function(x) -3 * x[2L]
  expect_equal(bigIRT:::bigIRT_optim_gap_probe(falling, c(0, 0), c(0, 1), 0)$gain, 0)
  expect_equal(bigIRT:::bigIRT_optim_gap_probe(rising, c(0, 0), c(0, 0), 0)$gain, 0)
  broken <- function(x) stop("no")
  expect_equal(bigIRT:::bigIRT_optim_gap_probe(broken, c(0, 0), c(0, 1), 0)$gain, 0)
})

test_that("a hessian that cannot be used says so rather than certifying", {
  expect_false(bigIRT:::bigIRT_optim_gap(NULL, 1)$ok)
  expect_false(bigIRT:::bigIRT_optim_gap(matrix(NA_real_, 2, 2), c(1, 1))$ok)
  expect_false(bigIRT:::bigIRT_optim_gap(-diag(2), c(1, 1, 1))$ok)
  verdict <- bigIRT:::bigIRT_certify_verdict(bigIRT:::bigIRT_optim_gap(NULL, 1))
  expect_equal(verdict$status, "unknown")
  expect_false(verdict$certified)
})

test_that("the optimiser's rule and the certificate estimate the same thing", {
  ## L-BFGS's predicted gain is 1/2 g'Bg; with B exact it is the gap to the
  ## last bit, which is why the certificate needs no second tolerance.
  H <- matrix(c(4, 1, 1, 25), 2, 2)
  g <- c(2, 5)
  expect_equal(0.5 * as.numeric(t(g) %*% solve(H) %*% g),
    bigIRT:::bigIRT_optim_gap(-H, g)$gap)
})

test_that("only the exact quantity is invariant, which is why only it certifies", {
  ## An inverse-Hessian approximation carried over from before a rescaling is
  ## not invariant; the exact gap is. So the proxy can stop an optimiser and
  ## cannot certify a fit.
  H <- matrix(c(4, 1, 1, 25), 2, 2)
  g <- c(2, 5)
  A <- diag(c(1, 1000))
  gs <- as.numeric(solve(A) %*% g)
  stale <- 0.5 * as.numeric(t(gs) %*% solve(H) %*% gs)
  expect_false(isTRUE(all.equal(stale, bigIRT:::bigIRT_optim_gap(-H, g)$gap)))
})

test_that("lambda is a displacement in standard errors", {
  information <- matrix(4, 1, 1)          # se = 1/2
  g <- 0.1 * 4 * 0.5                      # a gradient 0.1 se from the optimum
  out <- bigIRT:::bigIRT_optim_gap(-information, g)
  expect_equal(out$lambda, 0.1)
  expect_equal(out$gap, 0.005)
})

test_that("stopping is not certifying: the optimiser's verdict is not consulted", {
  expect_false("converged" %in% names(formals(bigIRT:::bigIRT_certify_verdict)))
  short <- bigIRT:::bigIRT_optim_gap(-diag(c(4, 25)), c(2, 5))
  expect_equal(bigIRT:::bigIRT_certify_verdict(short, probe = list(gain = 0),
    tolerance = 1e-6)$status, "suboptimal")
})

## --- bigIRT's own ---------------------------------------------------------------

test_that("an eigenvalue inside the differencing noise is flat, not a saddle", {
  ## A true curvature of 1e-5 differenced with error 1e-4 can come out
  ## negative. Without the band that is a saddle and the fit is condemned;
  ## with it the direction is flat and is probed instead.
  H <- -diag(c(100, -2e-5))
  bare <- bigIRT:::bigIRT_optim_gap(H, c(1, 0.01))
  expect_equal(bare$nnegative, 1L)
  banded <- bigIRT:::bigIRT_optim_gap(H, c(1, 0.01), noise = 1e-4)
  expect_equal(banded$nnegative, 0L)
  expect_equal(banded$nflat, 1L)
  expect_equal(banded$residual, c(0, 0.01))
  ## And a curvature well outside the band is still counted.
  expect_equal(bigIRT:::bigIRT_optim_gap(-diag(c(100, -1)), c(1, 1), noise = 1e-4)$nnegative, 1L)
  ## noise = 0 is ctsem's split exactly.
  expect_identical(bigIRT:::bigIRT_optim_gap(H, c(1, 0.01), noise = 0), bare)
})

test_that("with no coupling between items the block form is the dense gap", {
  ## Items meet only through the shared coordinate; set their direct coupling
  ## to zero and the Schur route must reproduce the dense arithmetic exactly.
  set.seed(1)
  P <- 3L; ni <- 4L; m <- 2L; n <- P * ni + m
  I <- matrix(0, n, n)
  blocks <- lapply(seq_len(ni), function(j) (j - 1L) * P + seq_len(P))
  shared <- P * ni + seq_len(m)
  for(b in blocks){ X <- matrix(rnorm(P * P), P); I[b, b] <- crossprod(X) + diag(P) }
  X <- matrix(rnorm(m * m), m); I[shared, shared] <- crossprod(X) + diag(m) * 20
  for(b in blocks){ B <- matrix(rnorm(P * m, 0, 0.3), P); I[b, shared] <- B; I[shared, b] <- t(B) }
  g <- rnorm(n)
  H <- -I
  dense <- bigIRT:::bigIRT_optim_gap(H, g)
  blk <- bigIRT:::bigIRT_optim_gap_blocks(lapply(blocks, function(b) H[b, b]),
    lapply(blocks, function(b) H[b, shared, drop = FALSE]), H[shared, shared],
    blocks, shared, g)
  expect_true(blk$ok)
  expect_equal(blk$gap, dense$gap, tolerance = 1e-10)
  expect_equal(blk$ntrusted, n)
  expect_equal(blk$nnegative, 0L)
  ## Inertia by Sylvester: a negative item block, or a shared block made
  ## indefinite by its coupling, is counted either way.
  H2 <- H; H2[blocks[[2]], blocks[[2]]] <- -H2[blocks[[2]], blocks[[2]]]
  blk2 <- bigIRT:::bigIRT_optim_gap_blocks(lapply(blocks, function(b) H2[b, b]),
    lapply(blocks, function(b) H2[b, shared, drop = FALSE]), H2[shared, shared],
    blocks, shared, g)
  expect_equal(blk2$nnegative, bigIRT:::bigIRT_optim_gap(H2, g)$nnegative)
})

test_that("transform slopes are one in the live range and vanish in the tails", {
  ## Each link's derivative relative to its value at zero: tanh for the
  ## correlation, softplus for discrimination, logistic/2 for the asymptotes.
  expect_equal(1 - tanh(20)^2, 0)                    # the recorded failure
  sl <- function(x) 2 * stats::plogis(x)             # softplus, relative
  expect_equal(sl(0), 1)
  expect_lt(sl(-8), 1e-3)
  cc <- bigIRT:::cfunc(c(0, -9)); expect_equal(8 * cc[1] * (1 - 2 * cc[1]), 1)
  expect_lt(8 * cc[2] * (1 - 2 * cc[2]), 1e-3)
  ## The correlation slope as the certificate measures it, through the
  ## package's own constructor, for both parameterisations.
  num <- function(p, param, h = 1e-6){
    up <- bigIRT:::bigIRT_laplace_corr_from_packed(p + h, 2L, paramization = param)
    dn <- bigIRT:::bigIRT_laplace_corr_from_packed(p - h, 2L, paramization = param)
    max(abs(up - dn)) / (2 * h)
  }
  expect_equal(num(0, "stan_corsqrt"), 1, tolerance = 1e-8)
  expect_equal(num(3, "stan_corsqrt"), 1 - tanh(3)^2, tolerance = 1e-6)
  expect_equal(num(0, "normalized_chol"), 1, tolerance = 1e-8)
  expect_lt(num(40, "normalized_chol"), 1e-3)
})

test_that("suboptimal outranks saturated, and a gaining pull-back outranks both", {
  gap <- list(ok = TRUE, gap = 5, nnegative = 0L)
  expect_equal(bigIRT:::bigIRT_certify_verdict(gap, tolerance = 1e-3,
    saturated = "corr[1]", overshoot = list(gain = 0, moved = character(0)))$status, "suboptimal")
  gap$gap <- 1e-6
  expect_equal(bigIRT:::bigIRT_certify_verdict(gap, tolerance = 1e-3,
    saturated = "corr[1]", overshoot = list(gain = 0, moved = character(0)))$status, "saturated")
  expect_equal(bigIRT:::bigIRT_certify_verdict(gap, tolerance = 1e-3,
    saturated = "corr[1]", overshoot = list(gain = 3, moved = "corr[1]"))$status, "notmaximum")
  expect_match(bigIRT:::bigIRT_certify_verdict(gap, tolerance = 1e-3,
    saturated = "corr[1]", overshoot = list(gain = 3, moved = "corr[1]"))$reason, "corr\\[1\\]")
  ## What `converged` becomes.
  conv <- vapply(c("certified", "saturated", "suboptimal", "notstationary",
    "notmaximum", "skipped", "unknown"), bigIRT:::bigIRT_certificate_converged, logical(1))
  expect_equal(unname(conv), c(TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA))
})

test_that("the pull-back finds a gain towards the live range, and none at a boundary", {
  layout <- bigIRT:::bigIRT_laplace_layout_with_size(list(B = 1:2, corr = 3L))
  par <- c(0.1, -0.2, 20)
  ## An objective that prefers the correlation's raw value near 0.7.
  f <- function(p) -(tanh(p[3]) - 0.6)^2 * 100
  pb <- bigIRT:::bigIRT_certify_pullback(f, par, 3L, layout, f(par))
  expect_gt(pb$gain, 1)
  expect_equal(pb$moved, "corr[1]")
  ## One that prefers the bound: pulling back only loses.
  g <- function(p) tanh(p[3]) * 100
  expect_equal(bigIRT:::bigIRT_certify_pullback(g, par, 3L, layout, g(par))$gain, 0)
})

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## Drive the optimiser directly from a chosen starting state, which is the
  ## only way to put a transform in its flat tail on purpose: fitIRT clamps a
  ## starting correlation at 0.999999.
  ## The certificate is what these tests are about, so they ask for it: the
  ## default fit carries none.
  direct_fit <- function(sim, pl, ecorr = FALSE, priors = TRUE, modify = identity,
                         certify = list(route = "fast", tolerance = 1e-3), tol = 1e-4){
    f <- suppressWarnings(fitIRT(sim$dat, pl = pl, cores = 1, priors = priors,
      ebayes = FALSE, dropPerfectScores = FALSE, normalise = FALSE,
      marginalApprox = "laplace", laplaceOuterIter = 2, verbose = 0,
      keepInternals = TRUE, estimateAbilityCorr = ecorr, laplaceCertify = FALSE))
    sdat <- f$internals$sdat
    st <- modify(bigIRT:::bigIRT_laplace_initial_state(sdat, eps = 1e-6,
      corr_paramization = "stan_corsqrt"))
    pp <- bigIRT:::bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    bigIRT:::bigIRT_laplace_optimize_direct(state = st, sdat = sdat, prior_precision = pp,
      niter = 200L, tol = tol, jitter = 1e-6, person_tol = 1e-6, keep_covariance = TRUE,
      cores = 1L, estimateAbilityCorr = ecorr, corr_paramization = "stan_corsqrt",
      certify = certify)
  }

  test_that("a correlation parked at the bound with a zero gradient is not certified", {
    ## The recorded failure. Started at raw 20, tanh is exactly 1 and its
    ## derivative exactly 0, so the gradient in that coordinate is exactly
    ## zero, the optimiser cannot move it, and its own rule reports
    ## convergence at rho = 1 on data generated at 0.6. Asserted first, so
    ## this test cannot pass by the failure not reproducing.
    set.seed(77)
    s <- simIRT(Nsubs = 800, Nitems = 10, Nscales = 2, AMean = 1, ASD = .3, BMean = 0,
      BSD = 1, AbilityCorr = matrix(c(1, .6, .6, 1), 2), logitCMean = -20, logitCSD = 0)
    d <- direct_fit(s, 2L, ecorr = TRUE, modify = function(st){ st$AbilityCorrPars <- 20; st })
    expect_true(d$optim$terminate$what %in% c("gain", "noise_floor"))
    expect_equal(d$state$AbilityCorr[1, 2], 1)
    expect_equal(d$optim$grad[d$optim$layout$corr], 0)
    ce <- d$optim$certificate
    expect_equal(ce$status, "notmaximum")
    expect_false(isTRUE(ce$converged))
    expect_equal(ce$saturated, "corr[1]")
    expect_gt(ce$overshoot_gain, 1)
    ## The same data from an ordinary start is certified, by either route.
    ok <- direct_fit(s, 2L, ecorr = TRUE)$optim$certificate
    expect_equal(ok$status, "certified")
    expect_equal(ok$route, "fast")
    ex <- direct_fit(s, 2L, ecorr = TRUE,
      certify = list(route = "exact", tolerance = 1e-3))$optim$certificate
    expect_equal(ex$status, "certified")
    expect_equal(ex$route, "dense")
  })

  test_that("an asymptote on its bound, where the data put it, is a boundary estimate", {
    ## 2PL data fitted as a 3PL without priors, guessing started deep in the
    ## lower tail: the transform is flat there, and pulling it back only
    ## loses, so the fit is a maximum with coordinates it cannot certify.
    set.seed(5)
    s <- simIRT(Nsubs = 800, Nitems = 12, Nscales = 1, AMean = 1.2, ASD = .3, BMean = 0,
      BSD = 1, logitCMean = -20, logitCSD = 0)
    d <- direct_fit(s, 3L, priors = FALSE, modify = function(st){ st$logitCpars[] <- -30; st })
    ce <- d$optim$certificate
    expect_equal(ce$status, "saturated")
    expect_true(isTRUE(ce$converged))
    expect_equal(length(ce$saturated), 12L)
    expect_true(all(grepl("^C\\[", ce$saturated)))
    expect_lt(ce$overshoot_gain, 1e-3)
  })

  ## The exact route is what the next tests check against a hand-built
  ## Hessian, so the fixture asks for it by name; the fast route is compared
  ## with it further down.
  fit3 <- local({
    set.seed(3)
    s <- simIRT(Nsubs = 800, Nitems = 15, Nscales = 1, AMean = 1.2, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -1.4, logitCSD = .25)
    list(sim = s, fit = fitIRT(s$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      ebayes = FALSE, keepInternals = TRUE, dropPerfectScores = FALSE,
      laplaceCertify = "exact"))
  })

  test_that("the certificate's slopes find a flat transform and pass a live one", {
    d <- bigIRT:::bigIRT_direct_eval_from_fit(fit3$fit)
    lay <- d$layout; st <- d$state
    st$logitCpars[2] <- -12; st$invspApars[3] <- -9
    sl <- bigIRT:::bigIRT_transform_slopes(st, d$sdat, lay, d$context)
    expect_lt(sl[lay$C[2]], 1e-3)
    expect_lt(sl[lay$A[3]], 1e-3)
    expect_gt(min(sl[setdiff(c(lay$A, lay$C), c(lay$C[2], lay$A[3]))]), 1e-2)
    expect_equal(sl[lay$B], rep(1, length(lay$B)))
  })

  test_that("a converged refined fit is certified on the curvature of its own objective", {
    f <- fit3$fit
    ce <- f$laplaceStatus$certificate
    expect_equal(f$laplaceStatus$marginal, "agq")
    expect_equal(ce$marginal, "agq")
    expect_equal(ce$status, "certified")
    expect_true(f$laplaceStatus$converged)
    expect_equal(f$laplaceStatus$reason, "certified")
    expect_lt(ce$gap, ce$tolerance)
    expect_equal(ce$tolerance, 10 * 1e-4)
    expect_equal(ce$nnegative, 0L)
    expect_equal(ce$route, "dense")
    ## The gap recomputed independently: every person mode re-solved and the
    ## nodes re-centred at each perturbed point (the certificate holds them),
    ## which agrees to the quadrature error.
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    gf <- function(p) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
      sdat = d$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = d$context, tol = 1e-10, max_iter = 500L)$approx_grad
    n <- length(d$par); H <- matrix(0, n, n)
    for(k in seq_len(n)){
      up <- d$par; up[k] <- up[k] + 1e-4; dn <- d$par; dn[k] <- dn[k] - 1e-4
      H[, k] <- (gf(up) - gf(dn)) / 2e-4
    }
    g <- gf(d$par)
    expect_equal(ce$gap, 0.5 * sum(g * solve(-(H + t(H)) / 2, g)), tolerance = 1e-2)
    ## The marginal item covariance is at least the conditional one: a block
    ## of the inverse dominates the inverse of the block. itemInformation's
    ## differenced method inverts the block alone.
    ih <- itemInformation(f, method = "hessian")
    se_cert <- sqrt(apply(ce$covariance$items, 3, diag))
    expect_true(all(t(se_cert) >= ih$se[, rownames(se_cert)] * (1 - 1e-3)))
  })

  test_that("standard errors come from the exact covariance the certificate kept", {
    f <- fit3$fit
    ii <- itemInformation(f)
    expect_equal(ii$method, "exact")
    V <- f$laplaceStatus$certificate$covariance$items
    expect_equal(ii$se, t(sqrt(apply(V, 3, diag))), check.attributes = FALSE)
    ## Computed afresh it is the same matrix, and without internals the stored
    ## one still serves; with neither, it says what is missing.
    f2 <- f; f2$laplaceStatus$certificate$covariance <- NULL
    expect_equal(itemInformation(f2, method = "exact")$se, ii$se, tolerance = 1e-8)
    f3 <- f; f3$internals <- NULL
    expect_equal(itemInformation(f3)$se, ii$se)
    f4 <- f2; f4$internals <- NULL
    expect_error(itemInformation(f4, method = "exact"), "keepInternals")
    ## summary(), vcov() and tidyIRT() follow it.
    expect_equal(summary(f)$se_method, "exact")
    expect_equal(vcov(f), V)
    td <- suppressMessages(tidyIRT(f, conf.int = TRUE))
    b <- td[td$term == "B", ]
    expect_equal(b$std.error, ii$se[match(b$item, as.character(f$itemPars$Item)), "B"])
  })

  test_that("a fit stopped early is not certified, and says how far it is", {
    ## The exact route, by name: on an unrefined fit it is the one that
    ## measures the fitted (Laplace) objective's own curvature, and this checks
    ## its wording. The fast route is checked below for the verdict alone.
    f <- suppressWarnings(fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace",
      cores = 1L, ebayes = FALSE, dropPerfectScores = FALSE, laplaceOuterIter = 3,
      laplaceRefine = "none", laplaceCertify = "exact"))
    ce <- f$laplaceStatus$certificate
    expect_equal(ce$status, "suboptimal")
    expect_false(f$laplaceStatus$converged)
    expect_equal(f$laplaceStatus$reason, "suboptimal")
    expect_gt(ce$gap, 1e-3)
    expect_warning(fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      ebayes = FALSE, dropPerfectScores = FALSE, laplaceOuterIter = 3,
      laplaceRefine = "none", laplaceCertify = "exact"), "not certified as converged \\(suboptimal\\)")
    ## The fast route catches the same stop. Its item blocks there are the
    ## quadrature marginal's at the Laplace modes, not the Laplace objective's,
    ## and three iterations from the start that curvature is indefinite, so it
    ## says "not a maximum" rather than how far: both are true, and neither
    ## certifies.
    ff <- suppressWarnings(fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace",
      cores = 1L, ebayes = FALSE, dropPerfectScores = FALSE, laplaceOuterIter = 3,
      laplaceRefine = "none", laplaceCertify = TRUE))
    expect_false(ff$laplaceStatus$converged)
    expect_true(ff$laplaceStatus$certificate$status %in% c("suboptimal", "notmaximum"))
    ## And when it says "not a maximum" it says by whose curvature, and what to
    ## ask for instead.
    if(identical(ff$laplaceStatus$certificate$status, "notmaximum"))
      expect_match(ff$laplaceStatus$certificate$reason, "laplaceCertify = \"exact\"", fixed = TRUE)
  })

  test_that("a certificate over budget is skipped, says so, and leaves the optimiser's verdict", {
    expect_message(f <- fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace",
      cores = 1L, ebayes = FALSE, dropPerfectScores = FALSE, laplaceCertify = 1e-6),
      "not certified: the exact curvature would need")
    ce <- f$laplaceStatus$certificate
    expect_equal(ce$status, "skipped")
    expect_true(is.na(ce$converged))
    expect_equal(f$laplaceStatus$reason, "predicted_gain")
    expect_true(f$laplaceStatus$converged)
    expect_lt(ce$evaluations, 5L)
    ## FALSE turns it off altogether.
    off <- fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      ebayes = FALSE, dropPerfectScores = FALSE, laplaceCertify = FALSE)
    expect_null(off$laplaceStatus$certificate)
    expect_error(fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      laplaceCertify = "yes"), "laplaceCertify must be")
  })

  test_that("by default a fit is certified by the fast route, and its errors come from it", {
    ## The default: `converged` follows the fast certificate, and the default
    ## standard errors are its curvature's. FALSE, tested above, restores the
    ## optimiser's verdict and the profile errors.
    f <- fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      ebayes = FALSE, dropPerfectScores = FALSE, keepInternals = TRUE)
    ce <- f$laplaceStatus$certificate
    expect_equal(ce$route, "fast")
    expect_equal(ce$status, "certified")
    expect_equal(f$laplaceStatus$reason, "certified")
    expect_true(f$laplaceStatus$converged)
    expect_equal(summary(f)$se_method, "exact")
    expect_equal(itemInformation(f)$method, "exact")
    expect_equal(itemInformation(f)$se, t(sqrt(apply(ce$covariance$items, 3, diag))),
      check.attributes = FALSE)
  })

  ## The fast route's item blocks come from the quadrature kernel by the Louis
  ## identity rather than by differencing, so they are checked against
  ## differencing: with the nodes held fixed the two must agree to the
  ## difference quotient's own error, on every slot a 3PL and a 4PL free, and
  ## on items loading on either of two dimensions.
  louis_vs_fd <- function(sim, pl, ecorr = FALSE, items){
    f <- suppressWarnings(fitIRT(sim$dat, pl = pl, marginalApprox = "laplace", cores = 1L,
      keepInternals = TRUE, dropPerfectScores = FALSE, ebayes = FALSE,
      estimateAbilityCorr = ecorr, laplaceRefine = "agq"))
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    ctx <- d$context; ctx$agq_centre <- d$eval$posterior
    ev <- function(p, cx) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
      sdat = d$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE, context = cx,
      tol = 1e-10, max_iter = 500L)
    cb <- ctx; cb$agq_blocks <- TRUE
    blk <- ev(d$par, cb)$item_fg$item_blocks
    K <- d$sdat$Nscales; lay <- d$layout; rc <- d$context$row_context
    prior_sd <- c(B = d$sdat$BSDx, A = d$sdat$invspASD, C = d$sdat$logitCSD, D = d$sdat$logitDSD)
    worst <- 0; slots_seen <- character(0)
    for(j in items){
      r <- match(j, rc$items)
      sc <- c(B = lay$B[rc$B_ref[r]],
        setNames(vapply(seq_len(K), function(k) if(rc$A_ref[r, k] > 0) lay$A[rc$A_ref[r, k]]
          else NA_integer_, 1L), paste0("A", seq_len(K))),
        C = if(length(lay$C)) lay$C[rc$C_ref[r]] else NA, D = if(length(lay$D)) lay$D[rc$D_ref[r]] else NA)
      keep <- which(!is.na(sc)); ix <- sc[keep]
      L <- matrix(blk[, , j], K + 3)[keep, keep, drop = FALSE] -
        diag(1 / prior_sd[substr(names(ix), 1, 1)]^2, length(ix))
      H <- matrix(NA_real_, length(ix), length(ix))
      for(a in seq_along(ix)){
        up <- d$par; up[ix[a]] <- up[ix[a]] + 1e-4; dn <- d$par; dn[ix[a]] <- dn[ix[a]] - 1e-4
        H[, a] <- ((ev(up, ctx)$approx_grad - ev(dn, ctx)$approx_grad) / 2e-4)[ix]
      }
      worst <- max(worst, max(abs(L - (H + t(H)) / 2)) / max(abs(H)))
      slots_seen <- union(slots_seen, names(ix))
    }
    list(worst = worst, slots = slots_seen)
  }

  test_that("the kernel's item blocks are the fixed-node Hessian's (3PL and 4PL, K = 1 and 2)", {
    r3 <- louis_vs_fd(fit3$sim, 3L, items = c(1, 8))
    expect_lt(r3$worst, 1e-6)
    expect_setequal(r3$slots, c("B", "A1", "C"))
    set.seed(41)
    s4 <- simIRT(Nsubs = 900L, Nitems = 12L, Nscales = 1, AMean = 1.1, ASD = .25, BMean = 0,
      BSD = 1, logitCMean = -1.6, logitCSD = .2, logitDMean = 0.6, logitDSD = .15)
    r4 <- louis_vs_fd(s4, 4L, items = c(2, 11))
    expect_lt(r4$worst, 1e-6)
    expect_setequal(r4$slots, c("B", "A1", "C", "D"))
    set.seed(77)
    s2 <- simIRT(Nsubs = 700, Nitems = 8, Nscales = 2, AMean = 1.2, ASD = .3, BMean = 0,
      BSD = 1, AbilityCorr = matrix(c(1, .5, .5, 1), 2), logitCMean = -1.4, logitCSD = .25)
    r2 <- louis_vs_fd(s2, 3L, ecorr = TRUE, items = c(3, 12))
    expect_lt(r2$worst, 1e-6)
    expect_setequal(r2$slots, c("B", "A1", "A2", "C"))
  })

  test_that("the fast certificate agrees with the exact one, for a few evaluations", {
    f <- fitIRT(fit3$sim$dat, pl = 3L, marginalApprox = "laplace", cores = 1L,
      ebayes = FALSE, keepInternals = TRUE, dropPerfectScores = FALSE, laplaceCertify = TRUE)
    fast <- f$laplaceStatus$certificate; ex <- fit3$fit$laplaceStatus$certificate
    expect_equal(fast$route, "fast")
    expect_equal(fast$status, ex$status)
    expect_true(f$laplaceStatus$converged)
    ## Only the item-item coupling through shared persons is dropped; on the
    ## coverage study's fits that moved the gap by a median 2 to 20 per cent.
    expect_lt(abs(log(fast$gap / ex$gap)), log(2))
    se_f <- sqrt(apply(fast$covariance$items, 3, diag))
    se_e <- sqrt(apply(ex$covariance$items, 3, diag))
    expect_lt(max(abs(se_f / se_e - 1)), 0.05)
    ## The blocks in one pass, the item means from the prior alone: the
    ## fitted value, its re-centred twin and the pass, and nothing per item.
    expect_lte(fast$evaluations, 4L)
    expect_gt(ex$evaluations, 2L * length(f$optim$par))
  })
}
