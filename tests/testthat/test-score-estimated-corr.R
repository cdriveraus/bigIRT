## Score tests at a fit whose latent correlation was estimated. Until
## October 2026, fit$internals$priorPrecision (kept with keepInternals = TRUE)
## held the prior the last pass STARTED from -- the identity correlation --
## and the score tests and heldoutLogLik read it before fit$prepared. The
## person posteriors behind every score were then those of uncorrelated
## dimensions: the ability test for a group gap read 33.9 where the
## likelihood ratio was 18.8. Without keepInternals the estimate was used,
## so the two routes disagreed on the same fit. Checked here against the
## likelihood ratio, the quantity the score test approximates.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  test_that("score tests use the estimated correlation, and agree with the likelihood ratio", {
    skip_on_cran()
    set.seed(7); N <- 3000L; J <- 10L
    R <- matrix(.7, 3, 3); diag(R) <- 1
    g <- stats::rbinom(N, 1, .5)
    th <- matrix(stats::rnorm(3 * N), N) %*% chol(R) + .2 * g
    sub <- rep(c("A", "B", "C"), each = J)
    a <- stats::runif(3 * J, .9, 1.6); b <- stats::rnorm(3 * J, 0, .8)
    ii <- rep(seq_len(N), 3 * J); jj <- rep(seq_len(3 * J), each = N)
    d <- data.table::data.table(id = ii, Item = jj, Scale = sub[jj], g = g[ii],
      score = stats::rbinom(N * 3 * J, 1, stats::plogis(a[jj] * th[cbind(ii, match(sub[jj], c("A", "B", "C")))] - b[jj])))
    fit <- function(pp, ki) suppressWarnings(fitIRT(d, pl = 2, personPreds = pp, normalise = FALSE,
      estimateAbilityCorr = TRUE, cores = 2, laplaceRefine = "agq", keepInternals = ki))
    f0k <- fit(character(), TRUE); f0 <- fit(character(), FALSE); f1 <- fit("g", FALSE)
    ## The stored prior is the estimate's.
    Rk <- stats::cov2cor(solve(matrix(f0k$internals$priorPrecision[, , 1], 3, 3)))
    expect_lt(max(abs(Rk - f0k$prepared$AbilityCorr)), 1e-6)
    expect_gt(min(f0k$prepared$AbilityCorr[upper.tri(Rk)]), .5)
    gv <- unique(d[, list(id, g)]); cv <- data.frame(id = as.character(gv$id), g = factor(gv$g))
    lm <- function(f) suppressWarnings(scoreTest(scoreContributions(f), cv, parameters = "Ability",
      form = "factor"))$tests$LM[1]
    lmk <- lm(f0k); lm0 <- lm(f0)
    expect_equal(lmk, lm0, tolerance = 1e-6)
    lr <- 2 * (as.numeric(logLik(f1)) - as.numeric(logLik(f0)))
    expect_gt(lr, 10)
    expect_lt(abs(lm0 / lr - 1), .1)
  })
}
