## How improveModel chooses between alternatives that both pass the gate
## (bigIRT_modify_prefer): the one with fewer terms, unless a larger one beats
## it on the held-out persons by the gate itself. Chosen by the larger gain
## alone, near ties went to noise: with a gap specific to one subdomain at
## 20,000 simulated persons, DIF on all 20 of its items beat the 2-term effect
## by dimension in 9 of 50 runs, by 0.02 to 2.7 nats out of about 100.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## A batch's held-out evaluation: per-person log-likelihoods.
  ho <- function(x){ names(x) <- paste0("p", seq_along(x)); list(loglik = sum(x), by_person = x) }

  test_that("of two passing alternatives, the smaller is kept unless the larger beats it by the gate", {
    set.seed(3); n <- 2000
    cur <- ho(stats::rnorm(n, -40))
    small <- ho(cur$by_person + stats::rnorm(n, .05, .2))
    ## Over the smaller: exactly 1 nat more (z about .1), or 100 (z about 11).
    noise <- function(total){ e <- stats::rnorm(n, 0, .2); e - mean(e) + total / n }
    tie <- ho(small$by_person + noise(1))
    clear <- ho(small$by_person + noise(100))
    alt <- function(h, terms) list(tr = list(h = h, gain = bigIRT:::bigIRT_modify_gain(cur, h)$gain), terms = terms)
    a_small <- alt(small, 2); a_tie <- alt(tie, 20); a_clear <- alt(clear, 20)
    expect_gt(a_tie$tr$gain, a_small$tr$gain)
    pref <- function(best, cand) bigIRT:::bigIRT_modify_prefer(best, cand, minGain = 0, minZ = stats::qnorm(.95))
    ## A near tie: the smaller, whichever was evaluated first.
    expect_false(pref(a_small, a_tie)$cand)
    expect_true(pref(a_tie, a_small)$cand)
    ## A clear win: the larger, whichever was evaluated first.
    expect_true(pref(a_small, a_clear)$cand)
    expect_false(pref(a_clear, a_small)$cand)
    expect_gt(pref(a_small, a_clear)$z, 1.645)
    ## Equal size: the larger gain.
    expect_true(pref(alt(small, 20), a_tie)$cand)
    expect_true(is.na(pref(alt(small, 20), a_tie)$z))
  })

  ## A batch fit's warnings were suppressed, so a fit that stopped short (an
  ## iteration limit here; unresolved person modes on simulated data, where
  ## such fits ended up to 33,000 nats below a normal one) looked like a
  ## batch that did not help. They are kept in the path and the result.
  test_that("a batch fit's warnings are kept, not dropped", {
    skip_on_cran()
    set.seed(5); N <- 600; J <- 12
    g <- stats::rbinom(N, 1, .5); th <- stats::rnorm(N) + .8 * g
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii],
      score = stats::rbinom(N * J, 1, stats::plogis(a[jj] * th[ii] - b[jj])))
    r <- suppressWarnings(suppressMessages(improveModel(d, pl = 2, personCovariates = "g", dif = character(),
      asymptotes = character(), nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE,
      fitArgs = list(laplaceRefine = "none", laplaceOuterIter = 2L))))
    expect_true(any(grepl("iteration limit", r$fit_warnings$base)))
    expect_true(any(grepl("iteration limit", r$fit_warnings$final)))
    expect_gte(nrow(r$path), 1L)
    expect_true(all(grepl("iteration limit", r$path$fit_warnings)))
  })

  ## On a resumed run the chosen alternative's fit may be missing from the
  ## checkpoint (it lost under the rule the run was made with, so only its
  ## verdict was kept). The current model is then refitted when it is next
  ## needed, stored under its step, and the run finishes as before.
  test_that("a resumed run refits the current model when its fit is not in the checkpoint", {
    skip_on_cran()
    set.seed(21); N <- 3000; J <- 20
    g <- stats::rbinom(N, 1, .5)
    th <- stats::rnorm(N) + .8 * g
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    dif <- rep(0, J); dif[1:3] <- .8
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii],
      score = stats::rbinom(N * J, 1, stats::plogis(a[jj] * th[ii] - b[jj] + dif[jj] * g[ii])))
    ck <- file.path(tempdir(), "improve_ck_refit")
    unlink(ck, recursive = TRUE)
    run <- function() suppressMessages(improveModel(d, pl = 2, personCovariates = "g", dif = "B",
      asymptotes = character(), nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE, checkpoint = ck))
    r1 <- run()
    acc <- r1$path[r1$path$accepted]
    expect_gte(nrow(acc), 1L)
    last <- file.path(ck, paste0("round", acc$round[nrow(acc)], "_", acc$batch[nrow(acc)], ".rds"))
    x <- readRDS(last); expect_false(is.null(x$value$fit))
    x$value$fit <- NULL; saveRDS(x, last)
    unlink(file.path(ck, "final.rds"))
    r2 <- run()
    expect_identical(r2$spec, r1$spec)
    expect_false(is.null(readRDS(last)$value$fit))
    expect_equal(r2$fit$itemPars$B, r1$fit$itemPars$B, tolerance = 1e-3)
    unlink(ck, recursive = TRUE)
  })
}
