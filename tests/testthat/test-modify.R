## Model modification by score tests (R/modify.R, src/agq_backend.cpp).
##
## Each score is checked against a finite difference of the person's marginal
## log-likelihood computed by brute-force Gauss-Hermite integration, person
## by person: the group-variance score in tau^2 at zero, the scores of a
## fixed lower or upper asymptote, and the residual-covariance product M v.
## The held-out log-likelihood is checked against the same integration. A
## 3PL fit exercises the general second derivative (in the 2PL it has no y).

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  gh_rule <- function(n){
    i <- seq_len(n - 1); J <- matrix(0, n, n); J[cbind(i, i + 1)] <- sqrt(i); J[cbind(i + 1, i)] <- sqrt(i)
    e <- eigen(J, symmetric = TRUE); list(x = e$values, w = e$vectors[1, ]^2)
  }
  ## Exact pieces of a 1-D fit: row parameters, scores, prior.
  exact_setup <- function(f, rows = NULL){
    st <- bigIRT:::bigIRT_score_setup(f, rows = rows)
    re <- bigIRT:::bigIRT_laplace_row_effective(st$state, st$sdat, thetaBase = matrix(0, st$N, 1),
      rows = st$rc$rows, context = st$rc)
    list(st = st, a = re$loadings[, 1], b = re$b_row, c = re$c_row, d = re$d_row,
      y = st$sdat$score[st$rc$rows], pid = st$rc$ids, itm = st$rc$items,
      m = st$args$prior_mean[, 1], sd = 1 / sqrt(as.numeric(st$prior_prec)[1]))
  }
  logp <- function(ex, i, G1, G2 = NULL, tau2 = 0, S = integer(0), j0 = 0L, dc = 0, dd = 0){
    r <- which(ex$pid == i); tq <- ex$m[i] + ex$sd * G1$x
    uq <- if(tau2 > 0) sqrt(tau2) * G2$x else 0; wu <- if(tau2 > 0) G2$w else 1
    tot <- 0
    for(k in seq_along(uq)){
      ll <- rep(0, length(tq))
      for(rr in r){
        cc <- ex$c[rr] + (ex$itm[rr] == j0) * dc; dd0 <- ex$d[rr] - (ex$itm[rr] == j0) * dd
        p <- cc + (dd0 - cc) * stats::plogis(ex$a[rr] * tq - ex$b[rr] + (ex$itm[rr] %in% S) * uq[k])
        ll <- ll + if(ex$y[rr] == 1) log(p) else log(1 - p)
      }
      tot <- tot + wu[k] * sum(G1$w * exp(ll))
    }
    log(tot)
  }
  sim3 <- function(seed, N = 250, J = 7){
    set.seed(seed)
    th <- stats::rnorm(N); a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    cc <- rep(c(.2, 0, .15, .1), length.out = J)
    d <- data.frame(id = rep(1:N, J), Item = rep(1:J, each = N), Scale = 1)
    d$score <- stats::rbinom(nrow(d), 1, cc[d$Item] + (1 - cc[d$Item]) * stats::plogis(a[d$Item] * th[d$id] - b[d$Item]))
    d
  }

  test_that("group-variance scores match finite differences of the exact marginal (2PL and 3PL)", {
    skip_on_cran()
    d <- sim3(9)
    G1 <- gh_rule(80); G2 <- gh_rule(40)
    for(pl in 2:3){
      f <- suppressWarnings(fitIRT(d, pl = pl, laplaceRefine = "agq", cores = 1, verbose = 0,
        laplaceCertify = FALSE, normalise = FALSE))
      ex <- exact_setup(f)
      S <- 1:4; soi <- integer(7); soi[S] <- 1L
      vc <- bigIRT:::bigIRT_modify_vc_scores(f, soi, nodes = 41L)[, 1]
      eps <- 1e-6
      fd <- vapply(1:30, function(i) (logp(ex, i, G1, G2, eps, S) - logp(ex, i, G1)) / eps, 0)
      expect_lt(max(abs(vc[1:30] - fd)), 1e-4)
      expect_gt(max(abs(fd)), .1)
    }
  })

  test_that("asymptote scores are exact from the leave-one-out probability", {
    skip_on_cran()
    d <- sim3(9)
    G1 <- gh_rule(80)
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    ex <- exact_setup(f)
    sc <- scoreContributions(f, nodes = 41L)
    R <- sc$rows[item == 3]
    for(side in c("lower", "upper")){
      s_an <- if(side == "lower") (R$y - R$ppred) / R$ppred else -(R$y - R$ppred) / (1 - R$ppred)
      e2 <- 1e-6
      fd <- vapply(R$person[1:30], function(i) (logp(ex, i, G1, j0 = 3L, dc = if(side == "lower") e2 else 0,
        dd = if(side == "upper") e2 else 0) - logp(ex, i, G1)) / e2, 0)
      expect_lt(max(abs(s_an[1:30] - fd)), 1e-4)
    }
    ## And the test: items with a fixed asymptote get one-sided tests.
    at <- bigIRT:::bigIRT_modify_asymptotes(f, sc, minN = 10L)
    expect_identical(sort(unique(at$side)), c("lower", "upper"))
    expect_true(all(at$p >= 0 & at$p <= 1))
  })

  test_that("the residual-covariance product M v matches M built by integration", {
    skip_on_cran()
    d <- sim3(4); d <- d[stats::runif(nrow(d)) > .2, ]
    f <- suppressWarnings(fitIRT(d, pl = 3, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    ex <- exact_setup(f); st <- ex$st
    rule <- bigIRT:::bigIRT_score_rule(f, 1L, 41L)
    G1 <- gh_rule(80); J <- 7
    M <- matrix(0, J, J)
    for(i in sort(unique(ex$pid))){
      r <- which(ex$pid == i); tq <- ex$m[i] + ex$sd * G1$x
      gq <- hq <- matrix(0, length(tq), length(r)); ll <- rep(0, length(tq))
      for(k in seq_along(r)){ rr <- r[k]
        s <- stats::plogis(ex$a[rr] * tq - ex$b[rr]); u <- ex$d[rr] - ex$c[rr]
        p <- ex$c[rr] + u * s; p1 <- u * s * (1 - s); p2 <- p1 * (1 - 2 * s); yy <- ex$y[rr]
        gq[, k] <- (yy - p) * p1 / (p * (1 - p))
        hq[, k] <- -p1^2 / (p * (1 - p)) - (yy - p) * (1 - 2 * p) * p1^2 / (p * (1 - p))^2 + (yy - p) * p2 / (p * (1 - p))
        ll <- ll + if(yy == 1) log(p) else log(1 - p) }
      w <- G1$w * exp(ll); w <- w / sum(w)
      M[ex$itm[r], ex$itm[r]] <- M[ex$itm[r], ex$itm[r]] + crossprod(gq * w, gq) + diag(colSums(hq * w), length(r))
    }
    set.seed(1); V <- matrix(stats::rnorm(J * 2), J, 2)
    mv <- bigIRT:::bigIRT_modify_mv(st, rule, V, rep(1, J))
    expect_lt(max(abs(mv - M %*% V)), 1e-6)
    ## One direction's product is the group-variance score of a set: 1'M1/2.
    one <- matrix(as.numeric(1:J %in% c(1, 3, 4)), J, 1)
    soi <- integer(J); soi[c(1, 3, 4)] <- 1L
    expect_equal(sum(one * bigIRT:::bigIRT_modify_mv(st, rule, one, rep(1, J))) / 2,
      sum(bigIRT:::bigIRT_modify_vc_scores(f, soi, nodes = 41L)), tolerance = 1e-8)
  })

  ## Overlapping sets (a subdomain and a competence area within it) are
  ## tested as if each were alone. One pass once let the last set take every
  ## shared item.
  test_that("overlapping item sets are each tested whole", {
    skip_on_cran()
    d <- sim3(9)
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    sc <- scoreContributions(f)
    both <- bigIRT:::bigIRT_modify_groups(f, sc, list(A = as.character(1:4), B = as.character(3:6)))
    a <- bigIRT:::bigIRT_modify_groups(f, sc, list(A = as.character(1:4)))
    b <- bigIRT:::bigIRT_modify_groups(f, sc, list(B = as.character(3:6)))
    expect_equal(both$z, c(a$z, b$z), tolerance = 1e-10)
  })

  test_that("heldoutLogLik is the held-out persons' exact marginal log-likelihood", {
    skip_on_cran()
    d <- sim3(9)
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE, trainingRows = which(d$id > 60)))
    h <- heldoutLogLik(f, nodes = 41L)
    expect_identical(h$persons, 60L)
    ex <- exact_setup(f, rows = which(f$prepared$trainingLogical == 0))
    G1 <- gh_rule(80)
    exact <- sum(vapply(sort(unique(ex$pid)), function(i) logp(ex, i, G1), 0))
    expect_equal(h$loglik, exact, tolerance = 1e-7)
    expect_equal(sum(h$by_person), h$loglik)
  })

  ## improveModel on a small simulation: ability depends on g, items 1-3
  ## have DIF along g, items 11-15 share a testlet effect, w does nothing. It
  ## should add g to the ability model, DIF for items 1-3 only, and a group
  ## dimension for items 11-15 found from the scores alone, and leave w out.
  test_that("improveModel finds ability covariates, DIF items and a testlet, and leaves a null covariate", {
    skip_on_cran()
    set.seed(21); N <- 3000; J <- 20
    g <- stats::rbinom(N, 1, .5); w <- stats::rnorm(N)
    th <- stats::rnorm(N) + .8 * g; u <- stats::rnorm(N, 0, .9)
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    dif <- rep(0, J); dif[1:3] <- .8
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    eta <- a[jj] * th[ii] - b[jj] + dif[jj] * g[ii] + (jj %in% 11:15) * u[ii]
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii], w = w[ii],
      score = stats::rbinom(N * J, 1, stats::plogis(eta)))
    r <- suppressMessages(improveModel(d, pl = 2, personCovariates = c("g", "w"), dif = "B",
      asymptotes = character(), nsimDim = 9L, maxRounds = 2L, cores = 1, verbose = FALSE))
    s <- r$spec
    expect_identical(vapply(s$abil, `[[`, "", "covariate"), "g")
    expect_setequal(unlist(lapply(s$dif, `[[`, "items")), as.character(1:3))
    expect_length(s$groups, 1L)
    expect_setequal(s$groups[[1]]$items, as.character(11:15))
    expect_true(all(r$path$accepted[r$path$kind %in% c("ability", "dif", "groups")]))
    ## Kinds are judged in turn: DIF against the model that already has g's
    ## effect on ability, not against the base (where DIF would absorb it).
    p1 <- r$path[r$path$round == 1]
    expect_equal(p1$heldout_before[p1$kind == "dif"], p1$heldout_after[p1$kind == "ability"])
  })

  ## A checkpoint directory is reused only under the same settings. The test
  ## plants a wrong value in a stored step: a rerun with the same settings
  ## must return it (the step was reused, not recomputed), one with different
  ## settings must not.
  test_that("improveModel resumes from a checkpoint, and only under the same settings", {
    skip_on_cran()
    set.seed(5); N <- 1500; J <- 10
    th <- stats::rnorm(N); a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    cc <- c(.3, .3, rep(0, J - 2))
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1,
      score = stats::rbinom(N * J, 1, cc[jj] + (1 - cc[jj]) * stats::plogis(a[jj] * th[ii] - b[jj])))
    ck <- file.path(tempdir(), "improve_ck")
    unlink(ck, recursive = TRUE)
    run <- function(...) suppressMessages(improveModel(d, pl = 2, dif = character(), asymptotes = "lower",
      nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE, checkpoint = ck, ...))
    r1 <- run()
    expect_gt(nrow(r1$path), 0L)
    expect_true(all(file.exists(file.path(ck, c("base.rds", "round1_screens.rds", "final.rds")))))
    f <- file.path(ck, "base.rds"); x <- readRDS(f)
    x$value$h$loglik <- x$value$h$loglik + 1000
    saveRDS(x, f, compress = FALSE)
    r2 <- run()
    expect_equal(r2$path$heldout_before[1], r1$path$heldout_before[1] + 1000)
    expect_identical(r2$spec, r1$spec)
    r3 <- run(minAsymptote = .04)
    expect_equal(r3$path$heldout_before[1], r1$path$heldout_before[1])
    unlink(ck, recursive = TRUE)
  })
}
