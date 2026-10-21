## Score contributions (R/scores.R, the per-row output of src/agq_backend.cpp).
##
## The property scoreTest() rests on is that a response's score IS that
## person's marginal score in the item's parameters: summed over the person's
## rows on the item, it must equal the derivative of the person's own log
## marginal likelihood. So these tests difference each person's quadrature
## value (`value_by_subj`, nodes held at the person's Laplace mode and
## precision, where the Fisher identity is exact) and compare person by
## person, not in total -- a row attributed to the wrong person, or a slot to
## the wrong item, would survive a summed check and fail this one. The
## information is checked the same way, against the differenced scores.
##
## Every case has free asymptotes (a 2PL hides curvature errors; see
## test-laplace-direct-gradients.R), covariates on ability and on an item
## parameter, and in two dimensions persons with one or both abilities fixed.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  score_case <- function(dat, pl, args = list(), nodes = 5L){
    f <- suppressWarnings(do.call(fitIRT, c(list(dat = dat, pl = pl, cores = 1,
      dropPerfectScores = FALSE, laplaceOuterIter = 3, laplaceCertify = FALSE,
      laplaceRefine = "none", verbose = 0), args)))
    st <- bigIRT:::bigIRT_score_setup(f, cores = 1L)
    rule <- bigIRT:::bigIRT_score_rule(f, st$K, nodes = nodes)
    slots <- bigIRT:::bigIRT_score_free_slots(st$sdat)
    run <- function(args, out = 0L) do.call(bigIRT:::bigIRT_agq_fg_cpp_impl, c(args, list(
      theta_mode = st$posterior$theta_mode, precision = st$posterior$precision,
      rule = rule, want_person = TRUE, row_out = out, slot_out = as.integer(slots))))
    list(fit = f, st = st, rule = rule, slots = slots, run = run,
      sc = scoreContributions(f, nodes = nodes))
  }

  ## Per-person central difference of the fixed-node marginal in one entry of
  ## one argument of the kernel.
  person_fd <- function(cs, name, idx, h = 1e-5){
    up <- cs$st$args; dn <- cs$st$args
    up[[name]][idx] <- up[[name]][idx] + h; dn[[name]][idx] <- dn[[name]][idx] - h
    (cs$run(up)$value_by_subj - cs$run(dn)$value_by_subj) / (2 * h)
  }
  per_person <- function(cs, v, rows = TRUE){
    out <- numeric(cs$st$N)
    s <- rowsum(v[rows], cs$sc$rows$person[rows], reorder = TRUE)
    out[as.integer(rownames(s))] <- s[, 1]
    out
  }
  relerr <- function(a, b) max(abs(a - b) / pmax(1, abs(b)))

  ## Free-parameter index of item j's slot, NA if fixed.
  free_index <- function(sdat, j, slot){
    K <- as.integer(sdat$Nscales)
    switch(substr(slot, 1, 1),
      B = if(sdat$fixedB[j] == 1L) NA else sdat$freeBref[j],
      A = { k <- if(slot == "A") 1L else as.integer(substring(slot, 2)); cell <- (j - 1L) * K + k
            if(sdat$fixedAlog[cell] == 1L) NA else sdat$freeAref[cell] },
      C = if(sdat$fixedClogit[j] == 1L) NA else sdat$freeCref[j],
      D = if(sdat$fixedDlogit[j] == 1L) NA else sdat$freeDref[j])
  }
  arg_of <- c(B = "Bpars", A = "invspApars", C = "logitCpars", D = "logitDpars")

  check_item_scores <- function(cs, items, label){
    sd <- cs$st$sdat; R <- cs$sc$rows
    for(j in items) for(sl in names(cs$slots)){
      ix <- free_index(sd, j, sl)
      if(is.na(ix)) next
      fd <- person_fd(cs, arg_of[[substr(sl, 1, 1)]], ix)
      an <- per_person(cs, R[[sl]], R$item == j)
      expect_lt(relerr(an, fd), 1e-6, label = sprintf("%s item %d slot %s", label, j, sl))
      ## Everyone who did not answer the item scores zero, and differences to it.
      out <- !(seq_len(cs$st$N) %in% R$person[R$item == j])
      if(any(out)) expect_lt(max(abs(fd[out])), 1e-8)
    }
  }

  check_item_info <- function(cs, items, label, h = 1e-5){
    sd <- cs$st$sdat; R <- cs$sc$rows; sl <- names(cs$slots)
    for(j in items) for(a in sl){
      ix <- free_index(sd, j, a)
      if(is.na(ix)) next
      nm <- arg_of[[substr(a, 1, 1)]]
      up <- cs$st$args; dn <- cs$st$args
      up[[nm]][ix] <- up[[nm]][ix] + h; dn[[nm]][ix] <- dn[[nm]][ix] - h
      su <- cs$run(up, 1L)$row_scores; sdn <- cs$run(dn, 1L)$row_scores
      rows <- which(R$item == j)
      for(b in sl){
        if(is.na(free_index(sd, j, b))) next
        cb <- match(b, sl)
        fd <- -(su[rows, cb] - sdn[rows, cb]) / (2 * h)
        ab <- sort(match(c(a, b), sl))
        an <- R[[paste0("I.", sl[ab[1]], ".", sl[ab[2]])]][rows]
        expect_lt(relerr(an, fd), 1e-6, label = sprintf("%s item %d info %s,%s", label, j, a, b))
      }
    }
  }

  test_that("per-person scores are the derivatives of each person's marginal (1D 4PL)", {
    skip_on_cran()
    set.seed(11)
    sim <- simIRT(Nsubs = 160, Nitems = 12, Nscales = 1, AMean = 1.2, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3, logitDMean = 2, logitDSD = .3)
    d <- sim$dat
    d[, x := stats::rnorm(1), by = id]
    d[, w := stats::rnorm(.N)]
    cs <- score_case(d, pl = 4, list(personPreds = "x", BitemPreds = "w",
      estMeans = c("ability", "A", "C", "D")))
    check_item_scores(cs, c(1L, 5L, 12L), "1D 4PL")
    check_item_info(cs, c(2L, 9L), "1D 4PL")

    ## The ability coefficient: through the likelihood, each row's linear
    ## predictor score times its loading and covariate value.
    R <- cs$sc$rows; rc <- cs$st$rc
    eff <- bigIRT:::bigIRT_laplace_row_effective(cs$st$state, cs$st$sdat,
      thetaBase = cs$st$posterior$theta_mode, rows = rc$rows, context = rc)
    fd <- person_fd(cs, "Abilitybeta", 1L)
    an <- per_person(cs, R$eta * eff$loadings[, 1] * rc$person_pred[, 1])
    expect_lt(relerr(an, fd), 1e-6, label = "ability coefficient")
    ## The shared item covariate coefficient on B: the B score times w.
    fd <- person_fd(cs, "Bbeta", 1L)
    an <- per_person(cs, R$B * rc$B_pred[, 1])
    expect_lt(relerr(an, fd), 1e-6, label = "item covariate coefficient")
    ## The ability mean.
    fd <- person_fd(cs, "prior_mean", seq_len(cs$st$N))
    ## (prior_mean is N x 1: perturbing every person's entry perturbs the mean.)
    expect_lt(relerr(cs$sc$persons$mu, fd), 1e-6, label = "ability mean")
  })

  test_that("scores and information are right in 2D with abilities partly fixed", {
    skip_on_cran()
    set.seed(12)
    sim <- simIRT(Nsubs = 150, Nitems = 16, Nscales = 2, NitemsAnswered = 16, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d <- sim$dat
    d[, x := stats::rnorm(1), by = id]
    pd <- data.frame(id = unique(d$id)[1:50], a = stats::rnorm(50), b = c(rep(NA, 40), stats::rnorm(10)))
    names(pd)[2:3] <- sort(unique(as.character(d$Scale)))
    cs <- score_case(d, pl = 3, list(personDat = pd, personPreds = "x",
      estMeans = c("ability", "A", "C")))
    expect_true(all(c("A1", "A2", "C") %in% names(cs$slots)))
    items <- c(1L, 8L, 9L, 16L)
    check_item_scores(cs, items, "2D 3PL fixed")
    check_item_info(cs, c(1L, 16L), "2D 3PL fixed")
    ## Rows of a person with both abilities fixed carry no posterior
    ## uncertainty, so their information is the row's own curvature with no
    ## Louis correction: still checked above, against the differences.
  })

  test_that("per-row scores sum to the kernel's gradient in each free parameter", {
    skip_on_cran()
    set.seed(13)
    sim <- simIRT(Nsubs = 200, Nitems = 10, Nscales = 1, AMean = 1, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    cs <- score_case(sim$dat, pl = 3)
    g <- cs$run(cs$st$args)
    R <- cs$sc$rows; sd <- cs$st$sdat
    for(sl in names(cs$slots)){
      ix <- vapply(R$item, function(j) free_index(sd, j, sl), numeric(1))
      tot <- rowsum(R[[sl]][!is.na(ix)], ix[!is.na(ix)], reorder = TRUE)[, 1]
      ref <- g[[paste0("grad_", substr(sl, 1, 1))]]
      expect_equal(as.numeric(tot), as.numeric(ref)[as.integer(names(tot))], tolerance = 1e-10)
    }
  })

  test_that("one node gives the plug-in score at the posterior mode", {
    skip_on_cran()
    set.seed(14)
    sim <- simIRT(Nsubs = 120, Nitems = 10, Nscales = 1, AMean = 1, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -10, logitCSD = 0)
    f <- suppressWarnings(fitIRT(sim$dat, pl = 2, cores = 1, verbose = 0, laplaceCertify = FALSE))
    sc1 <- scoreContributions(f, nodes = 1L)
    R <- sc1$rows
    th <- unname(f$personPosterior$mode[R$person, 1])
    a <- unname(f$pars$A[R$item, 1]); b <- unname(f$pars$B[R$item, 1])
    p <- stats::plogis(a * th - b)
    expect_equal(R$eta, R$y - p, tolerance = 1e-6)
    expect_equal(R$B, -(R$y - p), tolerance = 1e-6)
    ## The raw loading coordinate carries the softplus slope, 1 - exp(-a).
    expect_equal(R$A, (R$y - p) * th * (1 - exp(-a)), tolerance = 1e-6)
  })

  test_that("a normalised fit is refused rather than scored on a mixed metric", {
    skip_on_cran()
    set.seed(15)
    sim <- simIRT(Nsubs = 150, Nitems = 10, Nscales = 1, AMean = 1, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -10, logitCSD = 0, AbilitySD = 2, AbilityMean = 1)
    f <- suppressWarnings(fitIRT(sim$dat, pl = 2, cores = 1, verbose = 0,
      laplaceCertify = FALSE, normalise = TRUE))
    expect_error(scoreContributions(f), "normalise")
  })
}

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## Purification re-estimates the ability side with the items held. On the
  ## full data that optimum is the fit's own (a quadrature-refined fit sits at
  ## the optimum of the same marginal), so re-estimating from a perturbed start
  ## must return the fitted coefficients and mean. A Newton step built on the
  ## wrong score or information would not get back.
  test_that("re-estimating the ability side with items held returns the fit's own", {
    skip_on_cran()
    set.seed(21)
    N <- 600; J <- 15
    z <- stats::runif(N); x <- stats::rnorm(N)
    th <- stats::rnorm(N) + .8 * (z > .5) + .3 * x
    P <- stats::plogis(matrix(stats::rlnorm(J, 0, .2), N, J, byrow = TRUE) * th -
      matrix(stats::rnorm(J), N, J, byrow = TRUE))
    d <- data.table::data.table(id = rep(seq_len(N), J), Item = rep(seq_len(J), each = N), Scale = 1L,
      score = stats::rbinom(N * J, 1, P))
    d[, `:=`(zz = z[id], x = x[id])]
    f <- suppressWarnings(fitIRT(d, pl = 2, personPreds = c("zz", "x"), estMeans = c("ability", "A"),
      laplaceRefine = "agq", cores = 1, verbose = 0, laplaceCertify = FALSE, laplaceTol = 1e-9))
    st <- bigIRT:::bigIRT_score_setup(f)
    rule <- bigIRT:::bigIRT_score_rule(f, 1L)
    st0 <- st
    st$args$Abilitybeta <- st$args$Abilitybeta + c(.3, -.2)
    st$args$prior_mean <- st$args$prior_mean + .25
    st$posterior <- do.call(bigIRT:::bigIRT_laplace_person_step_block_cpp_impl, c(st$args, list(
      theta_init = st$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L, max_iter = 100L,
      tol = 1e-10, keep_covariance = FALSE)))
    st1 <- bigIRT:::bigIRT_score_refit_ability(st, rule)
    expect_lt(max(abs(st1$args$Abilitybeta - st0$args$Abilitybeta)), 1e-4)
    expect_lt(max(abs(st1$args$prior_mean - st0$args$prior_mean)), 1e-4)
  })

  ## Four items with strong DIF in one direction contaminate the rest: the
  ## clean items' one-step slopes shift the other way. Anchoring on the
  ## unflagged items must pull them back towards zero and keep the DIF items.
  test_that("purification removes contamination from the clean items", {
    skip_on_cran()
    set.seed(1)
    N <- 1500; J <- 20
    a <- stats::rlnorm(J, 0, .2); b <- stats::rnorm(J)
    z <- stats::runif(N)
    B <- matrix(b, N, J, byrow = TRUE); B[, 1:4] <- B[, 1:4] - 1.2 * (z - .5)
    P <- stats::plogis(matrix(a, N, J, byrow = TRUE) * (stats::rnorm(N) + (z > .5)) - B)
    d <- data.frame(id = rep(1:N, J), Item = rep(1:J, each = N), Scale = 1, score = stats::rbinom(N * J, 1, P))
    Bz <- splines::ns(z, df = 3); d[c("z1", "z2", "z3")] <- Bz[d$id, ]
    f <- suppressWarnings(fitIRT(d, pl = 2, personPreds = c("z1", "z2", "z3"), laplaceRefine = "agq",
      cores = 1, verbose = 0, laplaceCertify = FALSE))
    cv <- data.frame(id = rownames(f$pars$Ability)); cv$z <- z[as.integer(cv$id)]
    t0 <- scoreTest(f, cv, parameters = "B")
    t1 <- scoreTest(f, cv, parameters = "B", purify = TRUE)
    clean <- as.character(5:20)
    e0 <- t0$coefficients[item %in% clean, mean(estimate)]
    e1 <- t1$coefficients[item %in% clean, mean(estimate)]
    expect_gt(e0, .1)
    expect_lt(abs(e1), abs(e0) / 3)
    expect_true(all(as.character(1:4) %in% t1$tests[anchor == FALSE, item]))
    expect_lt(t1$tests[item %in% clean, sum(p_fdr < .05)], t0$tests[item %in% clean, sum(p_fdr < .05)])
    ## Contributions computed once give the same tests, and another fit's
    ## are refused.
    sc <- scoreContributions(f)
    t2 <- scoreTest(f, cv, parameters = "B", purify = TRUE, contributions = sc)
    expect_equal(as.data.frame(t2$tests), as.data.frame(t1$tests))
    expect_equal(as.data.frame(t2$coefficients), as.data.frame(t1$coefficients))
    f2 <- f; f2$pars$B <- f2$pars$B + .01
    expect_error(scoreTest(f2, cv, parameters = "B", purify = TRUE, contributions = sc), "do not belong")
    expect_error(scoreTest(f, cv, parameters = "B", contributions = scoreContributions(f, exclude = "1")), "exclude")
  })
}

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  ## The expected information rests on an identity: a response's marginal
  ## score in its item's parameters is the score of the Bernoulli
  ## p(y_r | y_-r), whose probability is the row's leave-one-out predictive
  ## `ppred`. So s = (y - ppred) / (ppred (1 - ppred)) * d ppred / d psi, with
  ## the derivative differenced through the kernel at fixed nodes. A wrong
  ## `ppred` (the full posterior's mean, say) fails this by orders of
  ## magnitude.
  test_that("each row's score is the score of its leave-one-out Bernoulli (3PL)", {
    skip_on_cran()
    set.seed(31)
    sim <- simIRT(Nsubs = 150, Nitems = 12, Nscales = 1, AMean = 1.2, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    cs <- score_case(sim$dat, pl = 3)
    R <- cs$sc$rows; sd <- cs$st$sdat; h <- 1e-5
    for(j in c(2L, 7L)) for(sl in c("B", "A", "C")){
      ix <- free_index(sd, j, sl); nm <- arg_of[[sl]]
      up <- cs$st$args; dn <- cs$st$args
      up[[nm]][ix] <- up[[nm]][ix] + h; dn[[nm]][ix] <- dn[[nm]][ix] - h
      k <- length(cs$slots) + 2L
      dp <- (cs$run(up, 1L)$row_scores[, k] - cs$run(dn, 1L)$row_scores[, k]) / (2 * h)
      rows <- which(R$item == j)
      pp <- R$ppred[rows]; y <- R$y[rows]
      an <- (y - pp) / (pp * (1 - pp)) * dp[rows]
      expect_lt(relerr(R[[sl]][rows], an), 1e-6, label = sprintf("item %d slot %s", j, sl))
    }
  })
}

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  ## Purification holds items' responses out of the posterior and scores them
  ## against the rest with their own likelihood added. Three properties pin it:
  ## a held-out row's score is the score of its Bernoulli given the other
  ## responses (probability ppred, differenced through the kernel); its
  ## information is its score differenced; and holding rows out leaves every
  ## other row's score exactly as if those rows did not exist.
  test_that("held-out rows are scored against the rest, and the rest ignore them (3PL)", {
    skip_on_cran()
    set.seed(41)
    sim <- simIRT(Nsubs = 150, Nitems = 12, Nscales = 1, AMean = 1.2, ASD = .3,
      BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    cs <- score_case(sim$dat, pl = 3)
    st <- cs$st; sd <- st$sdat; h <- 1e-5
    held_items <- c(3L, 8L)
    hold <- as.integer(st$rc$items %in% held_items)
    post_args <- bigIRT:::bigIRT_score_args_rows(st$args, hold == 0L)
    post <- do.call(bigIRT:::bigIRT_laplace_person_step_block_cpp_impl, c(post_args, list(
      theta_init = st$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L, max_iter = 200L,
      tol = 1e-12, keep_covariance = FALSE)))
    runh <- function(a, out = 2L) do.call(bigIRT:::bigIRT_agq_fg_cpp_impl, c(a, list(
      theta_mode = post$theta_mode, precision = post$precision, rule = cs$rule, want_person = TRUE,
      row_out = out, slot_out = as.integer(cs$slots), row_hold = hold)))
    r0 <- runh(st$args)
    k <- length(cs$slots) + 2L
    for(j in held_items) for(sl in c("B", "A", "C")){
      ix <- free_index(sd, j, sl); nm <- arg_of[[sl]]
      up <- st$args; dn <- st$args
      up[[nm]][ix] <- up[[nm]][ix] + h; dn[[nm]][ix] <- dn[[nm]][ix] - h
      ru <- runh(up, 1L)$row_scores; rd <- runh(dn, 1L)$row_scores
      rows <- which(st$rc$items == j)
      y <- st$args$score[rows]; pp <- r0$row_scores[rows, k]
      dp <- (ru[rows, k] - rd[rows, k]) / (2 * h)
      cb <- match(sl, names(cs$slots))
      expect_lt(relerr(r0$row_scores[rows, cb], (y - pp) / (pp * (1 - pp)) * dp), 1e-6,
        label = sprintf("held item %d slot %s score", j, sl))
      fd_info <- -(ru[rows, cb] - rd[rows, cb]) / (2 * h)
      expect_lt(relerr(r0$row_info[rows, bigIRT:::bigIRT_score_pack_index(cb, cb)], fd_info), 1e-6,
        label = sprintf("held item %d slot %s information", j, sl))
    }
    ## The rest, against a pass on the remaining rows alone.
    keep <- hold == 0L
    r1 <- do.call(bigIRT:::bigIRT_agq_fg_cpp_impl, c(post_args, list(
      theta_mode = post$theta_mode, precision = post$precision, rule = cs$rule, want_person = TRUE,
      row_out = 2L, slot_out = as.integer(cs$slots))))
    expect_equal(r0$row_scores[keep, ], r1$row_scores, tolerance = 1e-12)
    expect_equal(r0$row_info[keep, ], r1$row_info, tolerance = 1e-12)
    expect_equal(r0$value, r1$value, tolerance = 1e-12)
  })
}

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  ## The within-person path of the ability re-estimation scores each person
  ## by summing their responses' linear-predictor scores times loading times
  ## covariate value. A fit with a predictor that varies within person sits at
  ## the optimum of the same objective, so from a perturbed start the
  ## re-estimation must come back to the fit's own coefficients. Wrong
  ## loadings, a covariate taken at its person mean, or a fixed ability left
  ## in the sum would all stop it short.
  test_that("ability re-estimation with a within-person predictor returns the fit's own", {
    skip_on_cran()
    set.seed(22)
    N <- 500; J <- 30; per <- 3; ni <- 8
    age0 <- stats::runif(N, 8, 12)
    a <- stats::rlnorm(J, 0, .2); b <- stats::rnorm(J)
    th0 <- stats::rnorm(N)
    d <- data.table::rbindlist(lapply(seq_len(N), function(i){
      s <- rep(seq_len(per), each = ni); age <- age0[i] + (s - 1) * .5
      it <- sample.int(J, per * ni)
      th <- th0[i] + .4 * (age - 10)
      data.table::data.table(id = i, Item = it, Scale = 1L, age = age,
        score = stats::rbinom(per * ni, 1, stats::plogis(a[it] * th - b[it])))
    }))
    d[, agec := age - 10]
    f <- suppressWarnings(fitIRT(d, pl = 2, personPreds = "agec", estMeans = c("ability", "A"),
      laplaceRefine = "agq", cores = 1, verbose = 0, laplaceCertify = FALSE, laplaceTol = 1e-9))
    st <- bigIRT:::bigIRT_score_setup(f)
    expect_true(any(bigIRT:::bigIRT_laplace_person_pred_varies(st$args$person_pred, st$args$id)))
    rule <- bigIRT:::bigIRT_score_rule(f, 1L)
    st0 <- st
    st$args$Abilitybeta <- st$args$Abilitybeta + .2; st$args_post$Abilitybeta <- st$args$Abilitybeta
    st$args$prior_mean <- st$args$prior_mean - .2; st$args_post$prior_mean <- st$args$prior_mean
    st$posterior <- do.call(bigIRT:::bigIRT_laplace_person_step_block_cpp_impl, c(st$args_post, list(
      theta_init = st$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L, max_iter = 100L,
      tol = 1e-10, keep_covariance = FALSE)))
    st1 <- bigIRT:::bigIRT_score_refit_ability(st, rule, maxit = 200L, tol = 1e-9)
    expect_lt(max(abs(st1$args$Abilitybeta - st0$args$Abilitybeta)), 1e-4)
    expect_lt(max(abs(st1$args$prior_mean - st0$args$prior_mean)), 1e-4)
  })

  ## A covariate related to ability but missing from the fit: tested as the
  ## fit stands, impact reads as DIF in every item; adjusted after the fact,
  ## it does not, and the group difference the adjustment estimates matches a
  ## refit with the covariate. The two sit on different metrics (the fit
  ## without the covariate scales the total ability variance to one, the
  ## refit the residual), so they are compared as the group difference over
  ## the residual SD, in which the metric cancels.
  test_that("post-hoc adjustment for a covariate the ability model lacks", {
    skip_on_cran()
    set.seed(23)
    N <- 3000; J <- 20
    z <- stats::runif(N); a <- stats::rlnorm(J, 0, .2); b <- stats::rnorm(J)
    th <- stats::rnorm(N) + (z > .5)
    P <- stats::plogis(matrix(a, N, J, byrow = TRUE) * th - matrix(b, N, J, byrow = TRUE))
    d <- data.frame(id = rep(1:N, J), Item = rep(1:J, each = N), Scale = 1,
      score = stats::rbinom(N * J, 1, P))
    f0 <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE))
    cv <- data.frame(id = rownames(f0$pars$Ability)); cv$z <- z[as.integer(cv$id)]
    t_none <- suppressWarnings(scoreTest(f0, cv, parameters = "B", adjust = "none"))$tests
    r_adj <- scoreTest(f0, cv, parameters = "B"); t_adj <- r_adj$tests
    expect_true(all(t_adj$ability_adjusted)); expect_false(any(t_none$ability_adjusted))
    expect_gt(mean(t_none$p < .05), .3)
    expect_warning(scoreTest(f0, cv, parameters = "B", adjust = "none"), "related to ability")
    ## The group difference implied by the adjusted ability regression, against
    ## a refit with the same basis in the ability model.
    B <- bigIRT:::bigIRT_score_adjust_basis(cv$z, "person", scoreContributions(f0), 3L, "z")
    sc <- bigIRT:::bigIRT_score_compute(f0, add_preds = B, adjust_method = "fast")
    ## The fast route (the default) and the full joint re-estimation solve the
    ## same problem here, the fit having no coefficients of its own to hold.
    sc_full <- bigIRT:::bigIRT_score_compute(f0, add_preds = B, adjust_method = "full")
    expect_identical(c(sc$adjust_method, sc_full$adjust_method), c("fast", "full"))
    expect_lt(sc$refit_iter, sc_full$refit_iter)
    expect_equal(sc$ability_beta, sc_full$ability_beta, tolerance = 1e-4)
    expect_equal(sc$ability_sd, sc_full$ability_sd, tolerance = 1e-5)
    zp <- cv$z
    Bp <- splines::ns(zp, df = 3); Bp <- sweep(Bp, 2, colMeans(Bp))
    gap_adj <- diff(tapply(as.numeric(Bp %*% sc$ability_beta[1, ]), zp > .5, mean)) / sc$ability_sd[1]
    d[c("b1", "b2", "b3")] <- Bp[match(d$id, as.integer(cv$id)), ]
    f1 <- suppressWarnings(fitIRT(d, pl = 2, personPreds = c("b1", "b2", "b3"), laplaceRefine = "agq",
      cores = 1, verbose = 0, laplaceCertify = FALSE))
    gap_fit <- diff(tapply(as.numeric(Bp %*% f1$pars$Abilitybeta[1, ]), zp > .5, mean))
    expect_lt(abs(gap_adj - gap_fit), .05)
    ## 1 SD of impact on a step is a quarter of the total variance the fit
    ## without it scaled to one, so the residual SD should come out near
    ## sqrt(1 / 1.25) = .894.
    expect_lt(abs(sc$ability_sd[1] - .894), .03)
    ## The result records what the adjustment estimated: the same model.
    ra <- r_adj$adjustment
    expect_true(all(ra$converged)); expect_equal(ra$term, colnames(B))
    expect_equal(ra$estimate, as.numeric(sc$ability_beta[1, colnames(B)]), tolerance = 1e-8)
    expect_equal(ra$ability_sd[1], sc$ability_sd[1], tolerance = 1e-8)
    expect_equal(ra$fit_ability_sd[1], 1, tolerance = 1e-6)
    ## And the tests themselves: the adjustment's against the refit's. (This
    ## dataset happens to put 4 of 20 clean items under .05 in both, which is
    ## why the comparison is against the refit, not against .05.)
    t_ref <- suppressWarnings(scoreTest(f1, cv, parameters = "B", adjust = "none"))$tests
    expect_gt(cor(t_adj$p, t_ref$p[match(t_adj$item, t_ref$item)]), .99)
    expect_identical(ra$method[1], "fast")
    t_full <- scoreTest(f0, cv, parameters = "B", adjustMethod = "full")$tests
    expect_lt(max(abs(t_adj$p - t_full$p[match(t_adj$item, t_full$item)])), 1e-3)
  })

  ## The fast route holds the fit's own coefficients, which here vary within
  ## person; the full route re-estimates them with the added ones. Holding
  ## them changes little when the added covariate is unrelated to them. A
  ## covariate given per response has no person-level Newton step, so it
  ## takes the full route.
  test_that("the fast adjustment holds the fit's coefficients; per-response covariates go full", {
    skip_on_cran()
    set.seed(53)
    N <- 2000; J <- 40; per <- 20
    z <- stats::rbinom(N, 1, .5); th <- stats::rnorm(N) + z
    a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    jj <- as.vector(replicate(N, sample.int(J, per))); ii <- rep(seq_len(N), each = per)
    ctx <- stats::rbinom(N * per, 1, .5); w <- stats::rnorm(N * per)
    y <- stats::rbinom(N * per, 1, stats::plogis(a[jj] * (th[ii] + .3 * ctx) - b[jj]))
    d <- data.frame(id = ii, Item = jj, Scale = 1, score = y, ctx = ctx)
    d$ctx_bar <- stats::ave(ctx, ii); d$ctx_dev <- ctx - d$ctx_bar
    f0 <- suppressWarnings(fitIRT(d, pl = 2, personPreds = c("ctx_bar", "ctx_dev"), laplaceRefine = "agq",
      cores = 1, verbose = 0, laplaceCertify = FALSE, dropPerfectScores = FALSE))
    cv <- data.frame(id = rownames(f0$pars$Ability)); cv$z <- factor(z[as.integer(cv$id)])
    rf <- scoreTest(f0, cv, parameters = "B")
    ru <- scoreTest(f0, cv, parameters = "B", adjustMethod = "full")
    expect_identical(c(rf$adjustment$method[1], ru$adjustment$method[1]), c("fast", "full"))
    expect_true(rf$adjustment$converged[1])
    expect_lt(rf$adjustment$iterations[1], ru$adjustment$iterations[1])
    expect_equal(rf$adjustment$estimate, ru$adjustment$estimate, tolerance = 1e-2)
    expect_gt(cor(rf$tests$p, ru$tests$p[match(rf$tests$item, ru$tests$item)]), .999)
    rw <- scoreTest(f0, data.frame(w = w), parameters = "B", form = "linear")
    expect_identical(rw$adjustment$method[1], "full")
  })

  ## With few responses per person most of the SD's information is missing,
  ## and its EM crawls (each change .85 of the last with four responses):
  ## plain EM overran the original 30-iteration cap. The accelerated default
  ## must converge well within it, to the same point.
  test_that("the adjustment's SD re-estimation converges on thin data", {
    skip_on_cran()
    set.seed(31)
    N <- 5000; J <- 100; per <- 4
    z <- stats::runif(N); th <- stats::rnorm(N) + (z > .5)
    a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    jj <- as.vector(replicate(N, sample.int(J, per)))
    ii <- rep(seq_len(N), each = per)
    d <- data.frame(id = ii, Item = jj, Scale = 1,
      score = stats::rbinom(N * per, 1, stats::plogis(a[jj] * th[ii] - b[jj])))
    f0 <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE, dropPerfectScores = FALSE))
    sc <- scoreContributions(f0)
    B <- bigIRT:::bigIRT_score_adjust_basis(z[as.integer(sc$person_labels)], "person", sc, 3L, "z")
    st <- bigIRT:::bigIRT_score_setup(f0, cores = 1L, add_preds = B)
    rule <- bigIRT:::bigIRT_score_rule(f0, st$K, NULL)
    s0 <- bigIRT:::bigIRT_score_refit_ability(st, rule, maxit = 300L, est_sd = TRUE, accelerate = FALSE)
    s1 <- bigIRT:::bigIRT_score_refit_ability(st, rule, est_sd = TRUE)
    expect_true(s0$refit_converged); expect_gt(s0$refit_iter, 30L)
    expect_true(s1$refit_converged); expect_lt(s1$refit_iter, 30L)
    expect_equal(s1$ability_sd, s0$ability_sd, tolerance = 1e-5)
    expect_equal(s1$args$Abilitybeta, s0$args$Abilitybeta, tolerance = 1e-5)
  })

  ## A per-response covariate that depends on the response itself, like time
  ## on the item, added to an ability model whose predictors already vary
  ## within person: outer-product steps far from the curvature, capped at
  ## first. Extrapolating capped steps once sent this to an SD of 59 and
  ## never converged (plain iteration: 26). The acceleration must not lose
  ## to plain iteration, and must land on the same point.
  test_that("the adjustment's acceleration is safe far from the optimum", {
    skip_on_cran()
    set.seed(11)
    N <- 2000; J <- 60; per <- 30
    th <- stats::rnorm(N); a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    jj <- as.vector(replicate(N, sample.int(J, per))); ii <- rep(seq_len(N), each = per)
    ctx <- stats::rbinom(N * per, 1, .5)
    y <- stats::rbinom(N * per, 1, .15 + .85 * stats::plogis(a[jj] * (th[ii] + .3 * ctx) - b[jj]))
    vt <- stats::rnorm(N * per, 3 - .6 * y + .3 * th[ii], .7)
    d <- data.frame(id = ii, Item = jj, Scale = 1, score = y, ctx = ctx)
    d$ctx_bar <- stats::ave(ctx, ii); d$ctx_dev <- ctx - d$ctx_bar
    f0 <- suppressWarnings(fitIRT(d, pl = 2, personPreds = c("ctx_bar", "ctx_dev"), laplaceRefine = "agq",
      cores = 1, verbose = 0, laplaceCertify = FALSE, dropPerfectScores = FALSE))
    sc <- scoreContributions(f0)
    cvv <- bigIRT:::bigIRT_score_covariates(sc, data.frame(vt = vt), "auto")
    B <- bigIRT:::bigIRT_score_adjust_basis(cvv$values$vt, cvv$level, sc, 3L, "vt")
    st <- bigIRT:::bigIRT_score_setup(f0, cores = 1L, add_preds = B)
    rule <- bigIRT:::bigIRT_score_rule(f0, st$K, NULL)
    s0 <- bigIRT:::bigIRT_score_refit_ability(st, rule, maxit = 200L, est_sd = TRUE, accelerate = FALSE)
    s1 <- bigIRT:::bigIRT_score_refit_ability(st, rule, maxit = 200L, est_sd = TRUE)
    expect_true(s0$refit_converged); expect_true(s1$refit_converged)
    expect_lte(s1$refit_iter, s0$refit_iter)
    expect_lt(max(s1$refit_trace[, "sd"]), 2)
    expect_equal(s1$args$Abilitybeta, s0$args$Abilitybeta, tolerance = 1e-5)
    expect_equal(s1$ability_sd, s0$ability_sd, tolerance = 1e-5)
  })

  ## A covariate that takes most of the variance leaves a small residual SD.
  ## The SD's update once added a jitter of 1e-6 to the variance, which put a
  ## floor of 1e-6 / (2 SD^2) under its change: above the tolerance for any
  ## SD below .7, so such an adjustment could never converge.
  test_that("the adjustment converges when the residual SD is small", {
    skip_on_cran()
    set.seed(41)
    N <- 2000; J <- 20
    z <- stats::rbinom(N, 1, .5); th <- stats::rnorm(N) + 3.5 * z
    a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    P <- stats::plogis(matrix(a, N, J, byrow = TRUE) * th - matrix(b, N, J, byrow = TRUE))
    d <- data.frame(id = rep(1:N, J), Item = rep(1:J, each = N), Scale = 1,
      score = stats::rbinom(N * J, 1, P))
    f0 <- suppressWarnings(fitIRT(d, pl = 2, laplaceRefine = "agq", cores = 1, verbose = 0,
      laplaceCertify = FALSE))
    cv <- data.frame(id = rownames(f0$pars$Ability)); cv$z <- factor(z[as.integer(cv$id)])
    ra <- scoreTest(f0, cv, parameters = "B")$adjustment
    expect_lt(ra$ability_sd[1], .7)
    expect_true(ra$converged[1])
  })

  ## Effect sizes against simulation truth. Three of twenty items easier for
  ## the focal group by .8 logits, impact modelled. Against the anchors of
  ## purification, each DIF item's change in the probability of a correct
  ## answer matches the truth at the focal persons' true abilities, clean
  ## items sit near zero, and the focal group's change in expected number
  ## correct over the flagged items matches the true one. Without anchors
  ## the test-level effect is not reported: the impact estimate absorbs it.
  test_that("DIF effect sizes recover simulated truth against the anchors", {
    skip_on_cran()
    set.seed(102)
    N <- 6000; J <- 20
    g <- stats::rbinom(N, 1, .5); th <- stats::rnorm(N) + .8 * g
    a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    shift <- c(.8, .8, .8, rep(0, J - 3))
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    eta0 <- a[jj] * th[ii] - b[jj]
    d <- data.frame(id = ii, Item = jj, Scale = 1, g = g[ii],
      score = stats::rbinom(N * J, 1, stats::plogis(eta0 + shift[jj] * g[ii])))
    f <- suppressWarnings(fitIRT(d, pl = 2, personPreds = "g", laplaceRefine = "agq", cores = 1,
      verbose = 0, laplaceCertify = FALSE, dropPerfectScores = FALSE))
    cv <- data.frame(id = rownames(f$pars$Ability)); cv$g <- factor(g[as.integer(cv$id)])
    r0 <- scoreTest(f, cv, parameters = "B")
    expect_null(r0$dtf); expect_identical(nrow(r0$effects), as.integer(J))
    ## Each response's ability derivative, from one row per item, against the
    ## fitted loadings directly (2PL: a P (1 - P) at the predictive P).
    sc <- scoreContributions(f)
    expect_equal(sc$rows$dpdtheta,
      as.numeric(f$pars$A[sc$rows$Item, 1]) * sc$rows$ppred * (1 - sc$rows$ppred), tolerance = 1e-10)
    ## The fit's own covariate under another name is recognised as already in
    ## the ability model and not added again.
    r2 <- scoreTest(f, data.frame(id = cv$id, other_name = cv$g), parameters = "B", effects = FALSE)
    expect_null(r2$adjustment); expect_false(any(r2$tests$ability_adjusted))
    r <- scoreTest(f, cv, parameters = "B", purify = TRUE)
    foc <- g[ii] == 1
    dP_true <- vapply(seq_len(J), function(j) mean((stats::plogis(eta0 + shift[jj]) -
      stats::plogis(eta0))[foc & jj == j]), 0)
    e <- r$effects[match(as.character(seq_len(J)), item)]
    expect_lt(max(abs(e$dP[1:3] - dP_true[1:3])), .035)
    expect_lt(max(abs(e$dP[-(1:3)])), .035)
    expect_true(all(as.character(1:3) %in% r$tests[p_fdr < .05, item]))
    dT_true <- mean(tapply((stats::plogis(eta0 + shift[jj]) - stats::plogis(eta0))[foc], ii[foc], sum))
    x <- r$dtf[group == "1"]
    expect_lt(abs(x$dT_flagged / dT_true - 1), .15)
    expect_identical(r$dtf[group == "0", c(dT_flagged, dtheta_flagged)], c(0, 0))
    expect_gt(x$dtheta_flagged, 0)
  })

  ## estfun: the person-level matrix holds each person's marginal score per
  ## free item parameter (their responses' scores summed), and its column
  ## sums are the row scores' sums; the row-level one sums to the same.
  test_that("estfun arranges the score contributions for sandwich and strucchange", {
    skip_on_cran(); skip_if_not_installed("sandwich")
    set.seed(61)
    N <- 800; J <- 10
    th <- stats::rnorm(N); a <- stats::runif(J, .8, 1.6); b <- stats::rnorm(J)
    ii <- rep(seq_len(N), J); jj <- rep(seq_len(J), each = N)
    d <- data.frame(id = ii, Item = jj, Scale = 1, score = stats::rbinom(N * J, 1, stats::plogis(a[jj] * th[ii] - b[jj])))
    f <- suppressWarnings(fitIRT(d, pl = 2, cores = 1, verbose = 0, laplaceCertify = FALSE))
    sc <- scoreContributions(f)
    E <- sandwich::estfun(sc)
    expect_identical(dim(E), c(nrow(sc$persons), sum(sc$item_free)))
    expect_identical(rownames(E), sc$person_labels)
    tot <- sc$rows[, list(B = sum(B), A = sum(A)), by = Item]
    expect_equal(unname(E[, paste0(tot$Item, ":B")] |> colSums()), tot$B, tolerance = 1e-10)
    expect_equal(unname(E[, paste0(tot$Item, ":A")] |> colSums()), tot$A, tolerance = 1e-10)
    Er <- sandwich::estfun(sc, level = "row")
    expect_identical(nrow(Er), nrow(sc$rows))
    expect_equal(unname(rowsum(Er, sc$rows$person, reorder = TRUE)),
      unname(E[sort(unique(sc$rows$person)), ]), tolerance = 1e-10)
    E3 <- sandwich::estfun(sc, items = "3", parameters = "B")
    expect_identical(colnames(E3), "3:B")
    skip_if_not_installed("strucchange")
    z <- stats::runif(nrow(E))
    gp <- strucchange::gefp(sc, fit = NULL, order.by = z,
      scores = function(x, ...) sandwich::estfun(x, items = "3"))
    ## The double max. sctest's names "DM", "CvM" belong to its model
    ## interface; on a gefp it takes the functional itself.
    p <- strucchange::sctest(gp, functional = strucchange::maxBB)$p.value
    expect_true(p >= 0 && p <= 1)
  })
}
