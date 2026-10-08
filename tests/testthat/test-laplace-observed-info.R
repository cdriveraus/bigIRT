## The Laplace curvature at the person mode is the observed Hessian.
##
## Fisher information was used until September 2026, then briefly selectable,
## and is now neither: it is still computed, but only for the Newton step,
## where its guaranteed positive definiteness is what makes the mode solve
## robust. These tests assert the properties that choice has to satisfy,
## rather than comparing two settings.
##
## The gradient consequence -- the item gradient is the true total derivative
## only with the observed curvature -- is checked in
## test-laplace-direct-gradients.R, since that is where the finite-difference
## harness lives.

sig <- function(x) 1 / (1 + exp(-x))

test_that("wl is the observed curvature, and w is Fisher information", {
  set.seed(2)
  for (i in 1:200) {
    eta <- rnorm(1, 0, 3); cc <- runif(1, 0, 0.45); dd <- runif(1, 0.6, 1)
    p <- cc + (dd - cc) * sig(eta)
    r1 <- bigIRT:::bigIRT_laplace_row_terms_debug(1, eta, cc, dd)
    r0 <- bigIRT:::bigIRT_laplace_row_terms_debug(0, eta, cc, dd)
    ## the Laplace curvature is minus the second derivative of the row
    ## log-likelihood, which the struct already carried
    expect_equal(r1$wl, -r1$dgrad_eta_deta, tolerance = 1e-12)
    ## and the Newton-step curvature is its average over the response
    expect_equal(r1$w, p * r1$wl + (1 - p) * r0$wl, tolerance = 1e-10)
  }
})

test_that("the two curvatures coincide for the 2PL and differ for the 3PL", {
  ## The canonical link is the whole reason this distinction stayed invisible:
  ## for the 2PL the second derivative of the log-likelihood contains no y, so
  ## any test run on 2PL data cannot tell the two apart.
  set.seed(3)
  same <- diff3 <- numeric(0)
  for (i in 1:200) {
    y <- rbinom(1, 1, 0.5); eta <- rnorm(1, 0, 3)
    t2 <- bigIRT:::bigIRT_laplace_row_terms_debug(y, eta, 0, 1)
    t3 <- bigIRT:::bigIRT_laplace_row_terms_debug(y, eta, 0.25, 1)
    same <- c(same, abs(t2$wl - t2$w))
    diff3 <- c(diff3, abs(t3$wl - t3$w))
  }
  expect_lt(max(same), 1e-12)
  expect_gt(max(diff3), 0.01)
})

test_that("the observed curvature derivatives match finite differences", {
  ## These three are the terms the item gradient needs and that Fisher never
  ## required, so nothing older exercises them.
  h <- 1e-5
  wl <- function(y, eta, cc, dd)
    bigIRT:::bigIRT_laplace_row_terms_debug(y, eta, cc, dd)$wl
  set.seed(4)
  e_eta <- e_c <- e_d <- 0
  for (i in 1:300) {
    y <- rbinom(1, 1, 0.5); eta <- rnorm(1, 0, 2.5)
    cc <- runif(1, 0.02, 0.4); dd <- runif(1, 0.65, 0.98)
    rt <- bigIRT:::bigIRT_laplace_row_terms_debug(y, eta, cc, dd)
    rel <- function(a, b) abs(a - b) / max(1, abs(a))
    e_eta <- max(e_eta, rel(rt$dwl_deta,
      (wl(y, eta + h, cc, dd) - wl(y, eta - h, cc, dd)) / (2 * h)))
    e_c <- max(e_c, rel(rt$dwl_dc,
      (wl(y, eta, cc + h, dd) - wl(y, eta, cc - h, dd)) / (2 * h)))
    e_d <- max(e_d, rel(rt$dwl_dd,
      (wl(y, eta, cc, dd + h) - wl(y, eta, cc, dd - h)) / (2 * h)))
  }
  expect_lt(e_eta, 1e-6)
  expect_lt(e_c, 1e-6)
  expect_lt(e_d, 1e-6)
})

test_that("the curvature is invariant to an exact rewrite of the likelihood", {
  ## An incorrect response satisfies log(1-p) = log(1-c) + log(1-sigma), so
  ## writing it with c = 0 plus a constant leaves the integrand untouched. A
  ## curvature that is a property of the integrand cannot notice. Fisher
  ## information does, because it is a property of the assumed model, and
  ## that is the cleanest demonstration that it was the wrong object here.
  set.seed(5)
  eta <- rnorm(40, 0, 2); cc <- 0.25
  sum_over <- function(field, c_used) sum(vapply(eta, function(e)
    bigIRT:::bigIRT_laplace_row_terms_debug(0, e, c_used, 1)[[field]], numeric(1)))
  expect_equal(sum_over("ll", cc), sum_over("ll", 0) + length(eta) * log(1 - cc),
               tolerance = 1e-10)
  expect_equal(sum_over("wl", cc), sum_over("wl", 0), tolerance = 1e-10)
  expect_gt(abs(sum_over("w", cc) - sum_over("w", 0)), 0.5)
})

person_sds <- function(f) {
  cv <- f$personPosterior$covariance
  sqrt(vapply(seq_len(dim(cv)[3]), function(i) cv[1, 1, i], numeric(1)))
}

fit_pl <- function(d, pl) {
  fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
         pl = pl, marginalApprox = "laplace", cores = 1L, verbose = 0L, priors = TRUE, ebayes = FALSE, normalise = FALSE,
         dropPerfectScores = FALSE, laplaceKeepCovariance = TRUE)
}

test_that("no person is reported less certain than the prior", {
  skip_on_cran()
  ## The clipping invariant: H is the prior precision plus something positive
  ## semidefinite, so the posterior SD cannot exceed the prior SD.
  ##
  ## This fails against the unguarded observed curvature: on this seed one
  ## person in 800 came back at 2.72 against a prior SD of 1, where
  ## quadrature said 0.81 and Fisher said 0.50. The clipping that enforces it
  ## is continuous, unlike the switch to Fisher it replaced -- scanning one
  ## item difficulty across a crossing, the switch stepped 0.8 nats, 1765x
  ## the adjacent-point change, while clipping passed through at 0.0005.
  set.seed(103)
  s <- simIRT(Nsubs = 800, Nitems = 40, Nscales = 1, NitemsAnswered = 25L,
              AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -1.4, logitCSD = .2, logitDMean = 2.5, logitDSD = .2)
  f <- fit_pl(s$dat, 4L)
  prior_sd <- as.numeric(f$abilityPrior$sd)
  expect_lte(max(person_sds(f)), prior_sd * (1 + 1e-6))
  expect_true(all(person_sds(f) > 0))
})

test_that("partial curvature clipping stays incidental and is counted", {
  skip_on_cran()
  ## Full clipping leaves log|H| exactly constant in the item parameters, so
  ## zeroing its gradient contribution is exact -- always the case at K = 1.
  ## Partial clipping needs K > 1 and makes that zeroing an approximation.
  ##
  ## It does occur: a few person-evaluations per fit at three dimensions. A
  ## scan of converged fits said zero, which was misleading, because the
  ## optimiser passes through places the converged fit never sits at. It is
  ## nonetheless harmless at that rate -- with clipping active for 2 of 300
  ## persons the item gradient still matched central differences at 1.7e-7,
  ## against 1e-7 unclipped -- so what matters is that it stays incidental.
  ##
  ## The bar here is the same one fitIRT warns at: more partial clips than
  ## there are persons would mean at least one per person across the fit,
  ## orders above anything measured.
  set.seed(6)
  s <- simIRT(Nsubs = 400, Nitems = 60, Nscales = 2, NitemsAnswered = c(12L, 12L),
              AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -1.4, logitCSD = .25, logitDMean = 2.5, logitDSD = .25)
  bigIRT:::bigIRT_laplace_clip_counts(reset = TRUE)
  f <- fit_pl(s$dat, 4L)
  cnt <- bigIRT:::bigIRT_laplace_clip_counts()
  expect_true(is.finite(cnt[["partial"]]))
  expect_lt(cnt[["partial"]], 400L)
  ## and the invariant still holds however much clipping happened.
  ## person_sds reads the first latent dimension, so bound it by that
  ## dimension's prior SD -- with two scales, abilityPrior$sd is a vector.
  expect_lte(max(person_sds(f)), as.numeric(f$abilityPrior$sd)[1] * (1 + 1e-6))
})
