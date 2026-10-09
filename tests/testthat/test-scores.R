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
