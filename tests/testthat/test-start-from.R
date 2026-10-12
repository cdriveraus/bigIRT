## fitIRT(startFrom = ): a warm start from a previous fit's estimates, matched
## by label. Only the starting point changes, so a warm-started fit must reach
## the optimum a cold start reaches -- for the same model, and for a bigger one
## that adds DIF terms, frees asymptotes, or splits one dimension into
## correlated ones -- and from its own estimates it must get there in far
## fewer evaluations. improveModel() refits every batch this way: on Mindsteps
## a cold-started 3-D 4PL with four DIF terms added stopped 16,551 held-out
## nats short.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  sim_start <- function(seed = 7, N = 1500, J = 18){
    set.seed(seed)
    g <- stats::rbinom(N, 1, .5)
    sub <- rep(c("s1", "s2", "s3"), length.out = J)
    Sig <- matrix(.7, 3, 3); diag(Sig) <- 1
    th <- matrix(stats::rnorm(N * 3), N) %*% chol(Sig)
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    cc <- ifelse(seq_len(J) %% 4 == 0, .2, 0)
    dif <- rep(0, J); dif[1:2] <- .6
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    k <- match(sub, c("s1", "s2", "s3"))
    eta <- a[jj] * th[cbind(ii, k[jj])] - b[jj] + dif[jj] * g[ii]
    d <- data.frame(id = ii, Item = jj, Scale = 1, sub = sub[jj], g = g[ii],
      score = stats::rbinom(N * J, 1, cc[jj] + (1 - cc[jj]) * stats::plogis(eta)))
    d$dif_g <- d$g * (d$Item %in% 1:2)
    d
  }
  fit_q <- function(d, ...) suppressWarnings(fitIRT(d, cores = 1, verbose = 0, laplaceCertify = FALSE,
    normalise = FALSE, laplaceRefine = "none", ...))

  test_that("a fit started from its own estimates stays at its optimum, in far fewer evaluations", {
    skip_on_cran()
    d <- sim_start()
    f0 <- fit_q(d, pl = 2, personPreds = "g")
    f1 <- fit_q(d, pl = 2, personPreds = "g", startFrom = f0)
    expect_equal(f1$optim$logLik, f0$optim$logLik, tolerance = 1e-6)
    expect_equal(f1$itemPars$B, f0$itemPars$B, tolerance = 1e-3)
    expect_lt(f1$optim$target_evals, f0$optim$target_evals / 3)
  })

  test_that("a bigger model started from a smaller one reaches the optimum a cold start reaches", {
    skip_on_cran()
    d <- sim_start()
    small <- fit_q(d, pl = 2, personPreds = "g")
    ## DIF terms added.
    cold <- fit_q(d, pl = 2, personPreds = "g", BitemPreds = "dif_g", itemSpecificBetas = TRUE)
    warm <- fit_q(d, pl = 2, personPreds = "g", BitemPreds = "dif_g", itemSpecificBetas = TRUE, startFrom = small)
    expect_equal(warm$optim$logLik, cold$optim$logLik, tolerance = 1e-5)
    expect_equal(warm$pars$Bbeta, cold$pars$Bbeta, tolerance = 1e-2)
    ## Asymptotes freed.
    cold3 <- fit_q(d, pl = 3, personPreds = "g")
    warm3 <- fit_q(d, pl = 3, personPreds = "g", startFrom = small)
    expect_equal(warm3$optim$logLik, cold3$optim$logLik, tolerance = 1e-5)
    expect_equal(warm3$itemPars$C, cold3$itemPars$C, tolerance = 2e-2)
    ## One dimension split into three correlated ones.
    cold_k <- fit_q(d, pl = 2, personPreds = "g", scale = "sub", estimateAbilityCorr = TRUE)
    warm_k <- fit_q(d, pl = 2, personPreds = "g", scale = "sub", estimateAbilityCorr = TRUE, startFrom = small)
    expect_equal(warm_k$optim$logLik, cold_k$optim$logLik, tolerance = 1e-5)
    expect_equal(warm_k$abilityPrior$corr, cold_k$abilityPrior$corr, tolerance = 1e-2)
    expect_gt(min(cold_k$abilityPrior$corr[lower.tri(cold_k$abilityPrior$corr)]), .5)
    ## Fixed correlations stay as given, though the previous fit estimated them.
    fixed_k <- fit_q(d, pl = 2, personPreds = "g", scale = "sub", startFrom = cold_k)
    expect_equal(as.numeric(fixed_k$prepared$AbilityCorr), as.numeric(diag(3)))
  })
}
