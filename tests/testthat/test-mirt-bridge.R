## Coercion to mirt, and the log-likelihood scale it exposed.
##
## The coercion is only worth having if it is exactly right, because a fit
## carrying subtly wrong parameters still produces diagnostics that look
## entirely reasonable. So the trace lines are checked directly rather than the
## log-likelihood, which would confound a translation error with the difference
## between Laplace and quadrature.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  mk_fit <- function(pl = 2L, K = 1L, ni = 20L, ns = 900L, seed = 3){
    set.seed(seed)
    s <- simIRT(Nsubs = ns, Nitems = ni, Nscales = K, NitemsAnswered = rep(ni %/% K, K),
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = if(pl >= 3) -1.4 else -20, logitCSD = if(pl >= 3) .2 else 0,
      logitDMean = if(pl >= 4) 2.5 else 20, logitDSD = if(pl >= 4) .2 else 0)
    fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale", pl = pl,
      marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE, priors = TRUE,
      ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      keepInternals = TRUE, laplaceKeepCovariance = TRUE)
  }

  test_that("coerced response curves match bigIRT's own, for every model type", {
    skip_if_not_installed("mirt")
    for(cfg in list(list(pl = 2L, K = 1L), list(pl = 3L, K = 1L),
                    list(pl = 4L, K = 1L), list(pl = 2L, K = 2L))){
      f <- mk_fit(cfg$pl, cfg$K)
      ck <- checkMirtBridge(f)
      ## An earlier boundary nudge of 1e-4 on the guessing and upper asymptote
      ## moved every 2PL and 3PL trace line by exactly that, and read as a
      ## parameterisation error. The bound has to sit well below it.
      expect_true(ck$agrees,
        info = sprintf("pl=%d K=%d max|dP|=%.2e", cfg$pl, cfg$K, ck$max_abs_difference))
      expect_lt(ck$max_abs_difference, 1e-6)
    }
  })

  test_that("mirt's diagnostics run through the coerced object", {
    skip_if_not_installed("mirt")
    f <- mk_fit(ni = 15L, ns = 700L)
    m <- as.mirt(f)
    expect_s4_class(m, "SingleGroupClass")
    fit <- mirt::itemfit(m, na.rm = TRUE)
    expect_equal(nrow(fit), as.integer(f$dat$Nitems))
    expect_true(all(is.finite(fit$S_X2)))
    ## Correctly specified data, so most items should not be flagged.
    expect_gt(mean(fit$p.S_X2 > 0.05, na.rm = TRUE), 0.7)
  })

  test_that("a wide matrix that cannot fit is refused rather than attempted", {
    f <- mk_fit(ni = 15L, ns = 700L)
    expect_error(bigIRT:::bigIRT_wide_responses(f, max_cells = 100),
      "bigIRT exists to avoid")
  })

  test_that("logLik is a likelihood, not the optimiser objective", {
    ## The objective omits the person prior's normalising constant and, with
    ## priors on, carries the item prior density. Both are constant in the
    ## parameters and so invisible to the fit, but they put the reported value
    ## 6.5 per cent adrift of the marginal likelihood -- which would have made
    ## every AIC wrong and every comparison against other software misleading.
    ## Checked here against dense Gauss-Hermite quadrature at the same
    ## parameters; what remains is the Laplace approximation itself.
    f <- mk_fit(ni = 20L, ns = 900L)
    W <- bigIRT:::bigIRT_wide_responses(f)
    ip <- as.data.frame(f$itemPars)
    A <- as.numeric(ip$A); B <- as.numeric(ip$B)
    nodes <- seq(-6, 6, length.out = 201L)
    w <- stats::dnorm(nodes); w <- w / sum(w)
    P <- 1 / (1 + exp(-(outer(nodes, A) -
      matrix(B, nrow = length(nodes), ncol = length(B), byrow = TRUE))))
    quad <- 0
    for(i in seq_len(nrow(W))){
      y <- W[i, ]; ok <- !is.na(y)
      lp <- colSums(t(log(P[, ok, drop = FALSE])) * y[ok] +
                    t(log(1 - P[, ok, drop = FALSE])) * (1 - y[ok]))
      quad <- quad + log(sum(exp(lp - max(lp)) * w)) + max(lp)
    }
    ll <- as.numeric(logLik(f))
    expect_lt(abs(ll - quad) / abs(quad), 0.005)
    ## And it must differ from the raw objective, or the correction was dropped.
    expect_gt(abs(ll - as.numeric(f$optim$logLik)), 1)
  })
}
