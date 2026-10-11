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
  logp <- function(ex, i, G1, G2 = NULL, tau2 = 0, S = integer(0), j0 = 0L, dc = 0, dd = 0, dh2 = 0, dh3 = 0){
    r <- which(ex$pid == i); tq <- ex$m[i] + ex$sd * G1$x
    uq <- if(tau2 > 0) sqrt(tau2) * G2$x else 0; wu <- if(tau2 > 0) G2$w else 1
    tot <- 0
    for(k in seq_along(uq)){
      ll <- rep(0, length(tq))
      for(rr in r){
        cc <- ex$c[rr] + (ex$itm[rr] == j0) * dc; dd0 <- ex$d[rr] - (ex$itm[rr] == j0) * dd
        ## Shape terms on item j0: Hermite polynomials of the ability.
        sh <- (ex$itm[rr] == j0) * (dh2 * (tq^2 - 1) + dh3 * (tq^3 - 3 * tq))
        p <- cc + (dd0 - cc) * stats::plogis(ex$a[rr] * tq - ex$b[rr] + (ex$itm[rr] %in% S) * uq[k] + sh)
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

  ## With the lower asymptote free (a 3PL fit), the upper asymptote's score
  ## depends on the fitted c: dpi/dd = (pi - c)/(d - c). The test statistic
  ## built from finite-difference scores of the exact marginal must equal the
  ## function's (an earlier version used c = 0 there). Also: a fit with every
  ## item fixed has no item slots and must still be testable.
  test_that("upper-asymptote tests use the fitted lower asymptote, and work with every item fixed", {
    skip_on_cran()
    d <- sim3(9)
    G1 <- gh_rule(80)
    f <- suppressWarnings(fitIRT(d, pl = 3, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    ex <- exact_setup(f)
    sc <- scoreContributions(f, nodes = 41L)
    at <- bigIRT:::bigIRT_modify_asymptotes(f, sc, sides = "upper", minN = 10L)
    R <- sc$rows; j <- 3L; u <- which(R$item == j)
    e2 <- 1e-6
    s_fd <- vapply(R$person[u], function(i) (logp(ex, i, G1, j0 = j, dd = e2) - logp(ex, i, G1)) / e2, 0)
    expect_gt(f$itemPars$C[j], .03)
    S <- as.matrix(R[u, names(sc$slots), with = FALSE])
    Sa <- cbind(S[, which(sc$item_free[j, ]), drop = FALSE], s_fd)
    info <- bigIRT:::bigIRT_score_expected_info(Sa, R$y[u], R$ppred[u])
    pc <- bigIRT:::bigIRT_score_lm_pieces(matrix(1, length(u), 1), matrix(1, length(u), 1), Sa, info,
      ncol(Sa), seq_len(ncol(Sa) - 1L))
    expect_equal(at[item == sc$item_labels[j], z], pc$Ustar / sqrt(as.numeric(pc$V)), tolerance = 1e-3)
    ## Every item fixed.
    ip <- f$itemPars
    f0 <- suppressWarnings(fitIRT(d, pl = 2, itemDat = data.frame(Item = ip$Item, A = ip$A, B = ip$B, C = 0),
      cores = 1, verbose = 0, laplaceCertify = FALSE, normalise = FALSE, laplaceRefine = "agq"))
    a0 <- bigIRT:::bigIRT_modify_asymptotes(f0, scoreContributions(f0), minN = 10L)
    expect_identical(nrow(a0), 14L)
    expect_true(all(a0$identified > .999))
  })

  ## The item-shape scores: each response's score in a Hermite term added to
  ## its item's linear predictor, from the kernel's shape output, against
  ## finite differences of the exact marginal (2PL and 3PL; a person answers
  ## each item once, so the person's derivative is the row's score).
  test_that("item-shape scores match finite differences of the exact marginal", {
    skip_on_cran()
    d <- sim3(9)
    G1 <- gh_rule(80)
    for(pl in 2:3){
      f <- suppressWarnings(fitIRT(d, pl = pl, laplaceRefine = "agq", cores = 1, verbose = 0,
        laplaceCertify = FALSE, normalise = FALSE))
      ex <- exact_setup(f); st <- ex$st
      rule <- bigIRT:::bigIRT_score_rule(f, 1L, 41L)
      slots <- bigIRT:::bigIRT_score_free_slots(st$sdat)
      aq <- do.call(bigIRT:::bigIRT_agq_fg_cpp_impl, c(st$args, list(theta_mode = st$posterior$theta_mode,
        precision = st$posterior$precision, rule = rule, row_out = 1L, slot_out = as.integer(slots), shape_out = 1L)))
      g0 <- aq$row_scores[, length(slots) + 1L]
      he2 <- aq$shape[, 2] - g0; he3 <- aq$shape[, 3] - 3 * aq$shape[, 1]
      j0 <- 4L; u <- which(st$rc$items == j0)[1:25]; e2 <- 1e-6
      fd2 <- vapply(st$rc$ids[u], function(i) (logp(ex, i, G1, j0 = j0, dh2 = e2) - logp(ex, i, G1)) / e2, 0)
      fd3 <- vapply(st$rc$ids[u], function(i) (logp(ex, i, G1, j0 = j0, dh3 = e2) - logp(ex, i, G1)) / e2, 0)
      expect_lt(max(abs(he2[u] - fd2)), 1e-4)
      expect_lt(max(abs(he3[u] - fd3)), 1e-4)
      expect_gt(max(abs(fd3)), .05)
    }
  })

  ## The shape screen flags an item whose curve bends where a logistic cannot
  ## and leaves the logistic items alone.
  test_that("the item-shape screen finds a bent item", {
    skip_on_cran()
    set.seed(31); N <- 3000; J <- 10
    th <- stats::rnorm(N); a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .7)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    eta <- a[jj] * th[ii] - b[jj] + (jj == 1L) * 1.2 * (th[ii]^2 - 1)
    d <- data.frame(id = ii, Item = jj, Scale = 1, score = stats::rbinom(N * J, 1, stats::plogis(eta)))
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    sh <- bigIRT:::bigIRT_modify_shape(f)
    expect_equal(nrow(sh), J)
    expect_lt(sh[item == "1", p], 1e-10)
    expect_gt(sh[item == "1", dP], .1)
    expect_lt(mean(sh[item != "1", p] < .05), .35)
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

  ## The one-step size of a group dimension must not shrink when persons give
  ## many responses to the set. Truth: each of three 60-item sets carries a
  ## person effect with SD 0.5 on the linear predictor (slopes 1); every
  ## person answers 40 items in each. Dividing the score by its observed
  ## outer product gave about 0.2 here (0.09 at 100 items a set); dividing by
  ## the null information recovers the truth.
  test_that("the group screen's one-step SD is not attenuated by many responses per person", {
    skip_on_cran()
    set.seed(12); N <- 1500L; S <- 3L; Js <- 60L; k <- 40L
    sub <- rep(seq_len(S), each = Js); b <- stats::rnorm(S * Js, 0, 1)
    th <- stats::rnorm(N); u <- matrix(stats::rnorm(N * S, 0, .5), N, S)
    d <- data.table::rbindlist(lapply(seq_len(N), function(i)
      data.table::data.table(id = i, Item = unlist(lapply(seq_len(S), function(s) sample(which(sub == s), k))))))
    d[, set := sub[Item]]
    d[, score := as.integer(stats::runif(.N) < stats::plogis(th[id] + u[cbind(id, set)] - b[Item]))]
    d[, `:=`(Scale = 1L, set = paste0("s", set))]
    f <- suppressWarnings(fitIRT(d, pl = 2, cores = 1, verbose = 0, laplaceCertify = FALSE,
      laplaceRefine = "agq", normalise = FALSE))
    mt <- modificationTests(f, d, itemSets = "set", dif = character(), asymptotes = character(),
      exploratory = FALSE)
    sdh <- sqrt(mt$groups$tau2)
    expect_length(sdh, 3L)
    expect_true(all(sdh > .38 & sdh < .7))
    expect_true(all(mt$groups$z > 5))
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
    ## g's impact came in with the ability batch, so no impact batch is
    ## refitted (judged at the round's start, it refitted the same model).
    expect_false(any(r$path$kind == "impact"))
  })

  ## A kind rejected in a round is not tried again when only kinds before it
  ## were kept: it was already judged against them. Here the round keeps g on
  ## ability and rejects a batch of weak lower asymptotes; a second round
  ## would refit the same batch (the previous build did).
  test_that("improveModel does not retry a kind whose verdict cannot have changed", {
    skip_on_cran()
    set.seed(1); N <- 1500; J <- 10
    g <- stats::rbinom(N, 1, .5)
    th <- stats::rnorm(N) + .8 * g; a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    cc <- c(.12, .12, rep(0, J - 2))
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii],
      score = stats::rbinom(N * J, 1, cc[jj] + (1 - cc[jj]) * stats::plogis(a[jj] * th[ii] - b[jj])))
    ## alpha = .5 so that weak asymptotes become candidates (and are then
    ## rejected by the held-out gate).
    r <- suppressMessages(improveModel(d, pl = 2, personCovariates = "g", dif = character(), asymptotes = "lower",
      alpha = .5, minAsymptote = 0, nsimDim = 0L, maxRounds = 2L, cores = 1, verbose = FALSE))
    p <- r$path
    ## (Round 1 tries the screened asymptotes and the pooled alternative.)
    expect_identical(unique(p$kind[p$round == 1]), c("ability", "asymptotes"))
    expect_true(p$accepted[p$round == 1 & p$kind == "ability"])
    expect_false(any(p$accepted[p$round == 1 & p$kind == "asymptotes"]))
    expect_false(any(p$round == 2 & p$kind == "asymptotes"))
  })

  ## When a named set and a set found from the scores overlap and both pass,
  ## the named one is tried, even with the smaller z; a set from the scores
  ## that overlaps nothing is still tried.
  test_that("named item sets take precedence over overlapping exploratory sets", {
    gr <- data.table::data.table(set = c("topic=T", "eigen1+", "eigen2+", "topic=A"),
      z = c(5, 7, 6, 1), p = c(1e-7, 1e-3, 1e-3, .2), tau2 = c(.3, .4, .3, .2),
      source = c("topic", "eigen", "eigen", "topic"))
    gr[, p_fdr := p]
    sets <- list(`topic=T` = as.character(11:15), `eigen1+` = as.character(12:14),
      `eigen2+` = as.character(21:24), `topic=A` = as.character(1:10))
    k <- bigIRT:::bigIRT_modify_pick_groups(gr, sets, used = character(), slots = 2L, alpha = .05, minGroupSD = .3)
    expect_identical(vapply(k, `[[`, "", "name"), c("topic=T", "eigen2+"))
  })

  ## Group dimensions go in one per round. On this data set (the known-
  ## structure simulation's seed 810110: a testlet on items 11-15, nothing on
  ## 16-30) the topic sets offered as candidates were both added in one batch
  ## once the size estimate stopped shrinking: items 16-30 look dependent until
  ## the testlet is modelled. Added one at a time, the testlet goes in alone.
  test_that("improveModel adds group dimensions one per round and leaves a set that only looked dependent", {
    skip_on_cran()
    set.seed(810110); N <- 3000; J <- 30
    g <- stats::rbinom(N, 1, .5); z <- stats::runif(N); w <- stats::rnorm(N)
    th <- stats::rnorm(N) + .8 * g + .8 * (z > .5)
    u <- stats::rnorm(N, 0, .8)
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    cc <- rep(0, J); dif <- rep(0, J); tl <- rep(FALSE, J)
    cc[6:9] <- .25; dif[1:3] <- .7; tl[11:15] <- TRUE
    topic <- ifelse(1:J <= 10, "A", ifelse(1:J <= 15, "T", "B"))
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    eta <- a[jj] * th[ii] - b[jj] + dif[jj] * g[ii] + tl[jj] * u[ii]
    d <- data.table::data.table(id = ii, Item = jj, Scale = 1L, g = g[ii], z = z[ii], w = w[ii], topic = topic[jj],
      score = stats::rbinom(N * J, 1, cc[jj] + (1 - cc[jj]) * stats::plogis(eta)))
    r <- suppressMessages(improveModel(d, pl = 2, personCovariates = c("g", "z", "w"), itemSets = "topic",
      dif = "B", asymptotes = "lower", nsimDim = 0L, maxRounds = 3L, cores = 1, verbose = FALSE, partitions = FALSE))
    expect_true(all(r$path$terms[r$path$batch == "groups"] <= 1L))
    expect_length(r$spec$groups, 1L)
    expect_setequal(r$spec$groups[[1]]$items, as.character(11:15))
  })

  ## DIF along a response covariate whose effect on ability the model lacks:
  ## the covariate's impact goes in first, as its own batch, then its DIF, in
  ## a batch of its own. Truth: ability is higher on responses with ctx = 1
  ## (a within-person effect) and items 1-3 also have DIF along ctx.
  test_that("improveModel adds a DIF covariate's missing impact first, then its DIF per covariate", {
    skip_on_cran()
    set.seed(41); N <- 3000; J <- 20
    th <- stats::rnorm(N); a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    ctx <- stats::rbinom(N * J, 1, .5)
    dif <- rep(0, J); dif[1:3] <- .8
    eta <- a[jj] * (th[ii] + .6 * ctx) - b[jj] + dif[jj] * ctx
    d <- data.frame(id = ii, Item = jj, Scale = 1, ctx = ctx, score = stats::rbinom(N * J, 1, stats::plogis(eta)))
    r <- suppressMessages(improveModel(d, pl = 2, responseCovariates = "ctx", dif = "B",
      asymptotes = character(), nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE))
    p <- r$path
    expect_true(p$accepted[p$kind == "impact"])
    expect_identical(r$spec$impact, "ctx")
    expect_identical(p$batch[p$kind == "dif"], "dif-ctx-B")
    expect_true(all(as.character(1:3) %in% unlist(lapply(r$spec$dif, `[[`, "items"))))
    expect_lte(length(unlist(lapply(r$spec$dif, `[[`, "items"))), 5L)
  })

  ## A 0/1 response covariate that is rarely 1 (a repeated attempt, 6.5 per
  ## cent of Mindsteps responses) has equal 10th and 90th percentiles. Read as
  ## continuous, its DIF effect contrast was zero for every item, so no item
  ## could pass the effect floor however strong its DIF: on Mindsteps 434
  ## items were significant and none was kept. Both routes are checked: the
  ## screen treats it as a factor, and scoreTest given it as a number
  ## contrasts its range.
  test_that("a rare 0/1 response covariate's DIF has a real effect size", {
    skip_on_cran()
    set.seed(43); N <- 3000; J <- 20
    th <- stats::rnorm(N); a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    rep1 <- stats::rbinom(N * J, 1, .07)
    dif <- rep(0, J); dif[1:3] <- 1
    eta <- a[jj] * th[ii] - b[jj] + dif[jj] * rep1
    d <- data.frame(id = ii, Item = jj, Scale = 1, rep1 = rep1, score = stats::rbinom(N * J, 1, stats::plogis(eta)))
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0, laplaceCertify = FALSE,
      normalise = FALSE))
    mt <- modificationTests(f, d, responseCovariates = "rep1", dif = "B", asymptotes = character(),
      exploratory = FALSE, cores = 1)
    x <- mt$dif[item %in% as.character(1:3)]
    expect_true(all(x$kind == "factor"))
    expect_true(all(x$p_fdr < .05))
    expect_true(all(x$dP_abs > .03))
    r <- suppressWarnings(scoreTest(f, data.frame(rep1 = d$rep1), parameters = "B", form = "linear", effects = TRUE))
    e <- r$effects[item %in% as.character(1:3)]
    expect_true(all(e$dP > .03))
    expect_match(e$contrast[1], "maximum vs minimum")
  })

  ## Asymptotes: the screened selection and the pooled alternative (free on
  ## every item) are both tried, and the better of those that pass is kept.
  test_that("improveModel weighs pooled asymptotes against the screened selection", {
    skip_on_cran()
    set.seed(43); N <- 3000; J <- 15
    th <- stats::rnorm(N); a <- stats::runif(J, .9, 1.8); b <- stats::rnorm(J, 0, .8)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1,
      score = stats::rbinom(N * J, 1, .15 + .85 * stats::plogis(a[jj] * th[ii] - b[jj])))
    r <- suppressMessages(improveModel(d, pl = 2, dif = character(), asymptotes = "lower",
      nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE))
    p <- r$path[kind == "asymptotes"]
    expect_true("asymptotes-pooled-lower" %in% p$batch)
    if(any(p$accepted)){
      pass <- p[!is.na(z) & z >= stats::qnorm(.95)]
      expect_identical(p$batch[p$accepted], pass$batch[which.max(pass$gain)])
    }
  })

  ## Item sets that cover every item: the partition into correlated
  ## dimensions is offered alongside a single group factor, and with three
  ## subdomains correlated .5 it is the one kept.
  test_that("improveModel offers a partition of the items as correlated dimensions", {
    skip_on_cran()
    set.seed(47); N <- 3000; S <- 3L; Js <- 8L; J <- S * Js
    R <- matrix(.5, S, S); diag(R) <- 1
    th <- matrix(stats::rnorm(N * S), N, S) %*% chol(R)
    sub <- rep(seq_len(S), each = Js); a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, sub = paste0("s", sub[jj]),
      score = stats::rbinom(N * J, 1, stats::plogis(a[jj] * th[cbind(ii, sub[jj])] - b[jj])))
    r <- suppressMessages(improveModel(d, pl = 2, itemSets = "sub", dif = character(), asymptotes = character(),
      nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE))
    expect_true("partition-sub" %in% r$path$batch)
    expect_identical(r$spec$partition, "sub")
  })

  ## Sets found from the scores are chosen on one half of the persons and
  ## tested on the other, so their test is valid and their size unbiased.
  test_that("exploratory sets are tested on the half of the persons not used to find them", {
    skip_on_cran()
    set.seed(53); N <- 2000; J <- 20
    th <- stats::rnorm(N); u <- stats::rnorm(N, 0, .9)
    a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1,
      score = stats::rbinom(N * J, 1, stats::plogis(a[jj] * th[ii] - b[jj] + (jj %in% 11:15) * u[ii])))
    f <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = FALSE))
    mt <- modificationTests(f, d, dif = character(), asymptotes = character(), nsimDim = 5L)
    ge <- mt$groups[source == "eigen"]
    expect_gt(nrow(ge), 0L)
    expect_true(all(ge$persons < .6 * N & ge$persons > .4 * N))
    best <- ge[which.max(z)]
    expect_setequal(attr(mt$groups, "sets")[[best$set]], as.character(11:15))
    expect_gt(best$z, 4)
  })

  ## A failed step is not stored: a rerun must try it again, not reuse the
  ## failure (a 76-minute refit of a 20-million-response bank once failed and
  ## was cached).
  test_that("the checkpoint does not store a failed step", {
    ck <- file.path(tempdir(), "improve_ck_fail")
    unlink(ck, recursive = TRUE)
    step <- bigIRT:::bigIRT_modify_checkpoint(ck, list(a = 1))
    n <- 0L
    bad <- function(){ n <<- n + 1L; list(gain = NA_real_, error = "fit failed") }
    good <- function(){ n <<- n + 1L; list(gain = 2) }
    step("round1_dif", list(), bad); step("round1_dif", list(), bad)
    expect_identical(n, 2L)
    expect_false(file.exists(file.path(ck, "round1_dif.rds")))
    step("round1_dif", list(), good); step("round1_dif", list(), good)
    expect_identical(n, 3L)
    unlink(ck, recursive = TRUE)
  })

  ## A checkpoint directory is reused only under the same settings. The test
  ## plants a wrong value in a stored step: a rerun with the same settings
  ## must return it (the step was reused, not recomputed), one with different
  ## settings must not.
  test_that("improveModel resumes from a checkpoint, and only under the same settings", {
    skip_on_cran()
    set.seed(5); N <- 1500; J <- 10
    g <- stats::rbinom(N, 1, .5)
    th <- stats::rnorm(N) + .8 * g; a <- stats::runif(J, .9, 1.6); b <- stats::rnorm(J, 0, .8)
    cc <- c(.3, .3, rep(0, J - 2))
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii],
      score = stats::rbinom(N * J, 1, cc[jj] + (1 - cc[jj]) * stats::plogis(a[jj] * th[ii] - b[jj])))
    ck <- file.path(tempdir(), "improve_ck")
    unlink(ck, recursive = TRUE)
    run <- function(...) suppressMessages(improveModel(d, pl = 2, personCovariates = "g", dif = character(),
      asymptotes = "lower", nsimDim = 0L, maxRounds = 1L, cores = 1, verbose = FALSE, checkpoint = ck, ...))
    r1 <- run()
    expect_gt(nrow(r1$path), 0L)
    ## Perfect scorers are kept by default (dropping all-correct persons biases
    ## the upper-asymptote screen); this design has some.
    expect_gt(sum(tapply(d$score, d$id, mean) %in% c(0, 1)), 0)
    expect_identical(as.integer(r1$base_fit$prepared$Nsubs), length(unique(d$id)))
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

  ## The checkpoint kept the fit of every batch that passed; on Mindsteps a
  ## fit is 13 GB and a round can accept a batch per DIF covariate, more than
  ## the disk held. Only the latest accepted fit is needed to resume, so the
  ## others keep their verdicts and drop their fits -- and a resumed run must
  ## still give the same result.
  test_that("the checkpoint keeps only the current model's fit, and a resumed run gives the same result", {
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
    ck <- file.path(tempdir(), "improve_ck_slim")
    unlink(ck, recursive = TRUE)
    run <- function() suppressMessages(improveModel(d, pl = 2, personCovariates = c("g", "w"), dif = "B",
      asymptotes = character(), nsimDim = 9L, maxRounds = 2L, cores = 1, verbose = FALSE, checkpoint = ck))
    r1 <- run()
    acc <- r1$path[r1$path$accepted]
    expect_gte(nrow(acc), 2L)
    files <- file.path(ck, paste0("round", acc$round, "_", acc$batch, ".rds"))
    has_fit <- vapply(files, function(f) !is.null(readRDS(f)$value$fit), TRUE)
    expect_identical(unname(has_fit), c(rep(FALSE, nrow(acc) - 1L), TRUE))
    expect_false(is.null(readRDS(file.path(ck, "base.rds"))$value$fit))
    r2 <- run()
    expect_identical(r2$spec, r1$spec)
    expect_equal(r2$path, r1$path)
    expect_equal(r2$fit$itemPars, r1$fit$itemPars)
    unlink(ck, recursive = TRUE)
  })
}
