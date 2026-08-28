## Post-fit inference: item information, convergence blocks, criteria, scoring.
##
## The accuracy numbers asserted here were measured, not chosen. Against the
## full numerical Hessian with person modes re-solved to 1e-8, the closed-form
## profile weight gives standard errors about 6 per cent small at the median
## and 17 per cent at worst, while the differenced method matches the exact
## block-diagonal curvature to machine precision. Both bounds are asserted so
## that a change to either construction has to be deliberate.
##
## The person-mode tolerance is the trap here. Perturbing one item parameter
## barely moves the modes, so at the fitting default of 1e-4 the mode solve
## returns immediately and the difference quotient measures frozen-mode
## curvature instead -- a larger, different quantity that looks plausible.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  fit_small <- function(seed = 4, ni = 25, ns = 900){
    set.seed(seed)
    s <- simIRT(Nsubs = ns, Nitems = ni, Nscales = 1, NitemsAnswered = 15,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, keepInternals = TRUE,
      laplaceKeepCovariance = TRUE)
  }

  test_that("item standard errors are positive, finite and correctly shaped", {
    f <- fit_small()
    ii <- itemInformation(f)
    expect_true(all(c("B", "A") %in% ii$active))
    expect_equal(nrow(ii$se), as.integer(f$dat$Nitems))
    expect_true(all(is.finite(ii$se)))
    expect_true(all(ii$se > 0))
    ## Effective parameters cannot exceed the nominal count, and with priors
    ## this weak they should not be far below it either.
    expect_lte(ii$edf_total, ii$n_items * length(ii$active) + 1e-8)
    expect_gt(ii$edf_total, 0.5 * ii$n_items * length(ii$active))
  })

  test_that("the differenced method matches the exact block-diagonal curvature", {
    f <- fit_small()
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    lay <- d$layout
    gf <- function(p) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
      sdat = d$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = d$context, tol = 1e-8, max_iter = 200L)$approx_grad
    eps <- 1e-4
    items <- 1:4
    ih <- itemInformation(f, method = "hessian")
    for(j in items){
      ix <- c(lay$B[j], lay$A[j])
      Cc <- matrix(NA_real_, 2, 2)
      for(a in 1:2){
        pu <- d$par; pu[ix[a]] <- pu[ix[a]] + eps
        pl <- d$par; pl[ix[a]] <- pl[ix[a]] - eps
        Cc[, a] <- -((gf(pu) - gf(pl)) / (2 * eps))[ix]
      }
      Cc <- 0.5 * (Cc + t(Cc))
      expect_equal(as.numeric(sqrt(diag(solve(Cc)))),
        as.numeric(ih$se[j, c("B", "A")]), tolerance = 1e-6)
    }
  })

  test_that("the profile method stays within its measured accuracy", {
    f <- fit_small()
    ip <- itemInformation(f, method = "profile")
    ih <- itemInformation(f, method = "hessian")
    r <- c(ip$se[, "B"] / ih$se[, "B"], ip$se[, "A"] / ih$se[, "A"])
    ## Measured at about 0.94 median on this design. Bracket it loosely enough
    ## to survive noise but tightly enough that a broken weight fails.
    expect_gt(median(r), 0.85)
    expect_lt(median(r), 1.05)
    expect_true(all(r > 0.6 & r < 1.3))
  })

  test_that("a tight person tolerance changes the differenced curvature", {
    ## Guards the trap directly: if the fitting default were used, this would
    ## return frozen-mode curvature and the two would agree.
    f <- fit_small()
    loose <- itemInformation(f, method = "hessian", person_tol = 1e-3)
    tight <- itemInformation(f, method = "hessian", person_tol = 1e-9)
    expect_false(isTRUE(all.equal(as.numeric(loose$se), as.numeric(tight$se),
      tolerance = 1e-3)))
  })

  test_that("convergence reports blocks, a Newton decrement and a worst parameter", {
    f <- fit_small()
    cc <- checkConvergence(f)
    expect_s3_class(cc, "bigIRT_convergence")
    expect_true(all(c("block", "n", "norm", "share") %in% names(cc$blocks)))
    expect_equal(sum(cc$blocks$n), cc$n_parameters)
    expect_equal(sum(cc$blocks$share), 1, tolerance = 1e-8)
    ## A converged fit has little objective left to gain.
    expect_true(is.finite(cc$newton_decrement))
    expect_gte(cc$newton_decrement, 0)
    if(isTRUE(cc$converged)) expect_lt(cc$worst_parameter_se_units, 0.5)
    expect_output(print(cc), "By block")
  })

  test_that("logLik counts item parameters only and drives AIC and BIC", {
    f <- fit_small()
    ll <- logLik(f)
    ## Person parameters are integrated out of the marginal objective, so the
    ## degrees of freedom must not scale with the number of people.
    expect_lt(attr(ll, "df"), as.integer(f$dat$Nsubs))
    expect_lte(attr(ll, "df"), attr(ll, "nominal_df") + 1e-8)
    expect_equal(attr(ll, "nobs"), as.integer(f$dat$Nsubs))
    expect_equal(AIC(f), -2 * as.numeric(ll) + 2 * attr(ll, "df"), tolerance = 1e-8)
    ic <- IRTic(f)
    expect_true(all(c("AIC", "BIC") %in% ic$criterion))
    ## BIC on responses penalises harder than BIC on persons.
    bp <- ic$value[ic$criterion == "BIC" & ic$n_type == "persons"]
    br <- ic$value[ic$criterion == "BIC" & ic$n_type == "responses"]
    expect_true(all(br > bp))
  })

  test_that("held-out metrics score the excluded rows against a base rate", {
    set.seed(11)
    s <- simIRT(Nsubs = 800, Nitems = 25, Nscales = 1, NitemsAnswered = 15,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    n <- nrow(s$dat)
    set.seed(12); tr <- sort(sample.int(n, floor(0.8 * n)))
    f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, trainingRows = tr,
      dropPerfectScores = FALSE, keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    m <- heldoutMetrics(f)
    expect_true(is.finite(m$log_loss) && m$log_loss > 0)
    expect_true(m$brier > 0 && m$brier < 0.25)
    expect_true(m$auc > 0.5 && m$auc <= 1)
    ## The model uses the person; the base rate does not. It should win.
    expect_lt(m$brier, m$item_base_brier)
  })

  test_that("PSIS-LOO runs and reports usable Pareto k", {
    skip_if_not_installed("loo")
    f <- fit_small()
    l <- looIRT(f, draws = 200L, seed = 1)
    expect_s3_class(l, "loo")
    expect_true(is.finite(l$estimates["elpd_loo", "Estimate"]))
    k <- l$diagnostics$pareto_k
    ## The Laplace posterior should be a decent proposal for a well-behaved 2PL.
    expect_lt(mean(k > 0.7), 0.05)
  })

  test_that("covariate standard errors allow for item parameter uncertainty", {
    ## Inverting the coefficient block alone gives the variance with every item
    ## parameter pinned, which is too small. The Schur complement must widen it.
    ## Over 70 replications the marginal version came out at SE/SD 0.965 with a
    ## z spread of 1.04, so it is calibrated; asserted here only structurally,
    ## because a calibration run is far too slow for a test.
    set.seed(21)
    s <- simIRT(Nsubs = 900, Nitems = 25, Nscales = 1, NitemsAnswered = 15,
      AMean = 1, ASD = .25, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    d <- data.table::as.data.table(s$dat)
    ids <- unique(d$id); x <- stats::rnorm(length(ids))
    names(x) <- as.character(ids)
    d[, xcov := x[as.character(id)]]
    f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      personPreds = "xcov", keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    Vm <- bigIRT:::bigIRT_covariate_vcov(f, marginal = TRUE)
    Vc <- bigIRT:::bigIRT_covariate_vcov(f, marginal = FALSE)
    expect_true(is.matrix(Vm) && all(is.finite(Vm)))
    expect_gt(Vm[1, 1], 0)
    expect_gte(Vm[1, 1], Vc[1, 1])
  })

  test_that("the fit records its gradient block structure", {
    f <- fit_small()
    st <- f$laplaceStatus
    expect_true(is.finite(st$max_abs_grad))
    expect_true(is.data.frame(st$gradient_blocks))
    expect_equal(sum(st$gradient_blocks$share), 1, tolerance = 1e-8)
    expect_false(isTRUE(st$block_imbalance))
  })

  test_that("small aggregating blocks are preconditioned, not left to dominate", {
    ## Regression test for the full-scale failure. A mean hyperparameter is
    ## informed by every item, so its gradient outgrows an item parameter as
    ## the item count rises. At 16,064 items A_mean alone held 78 per cent of
    ## the gradient norm while the fit reported convergence; after scaling it
    ## held 0.003 per cent and the same fit ran faster.
    set.seed(6)
    s <- simIRT(Nsubs = 2500, Nitems = 800, Nscales = 1, NitemsAnswered = 30,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      laplaceTolScale = "per_obs", keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    st <- f$laplaceStatus
    expect_false(isTRUE(st$block_imbalance))
    ## Assert on what the scaling is for -- reaching a stationary point -- and
    ## not on gradient shares. After the Newton polish the item blocks are at
    ## essentially zero, so the untouched mean blocks hold most of a negligible
    ## total; a share test would fail on a perfectly converged fit.
    expect_true(isTRUE(st$converged))
    cc <- checkConvergence(f)
    expect_lt(cc$newton_decrement, 1)
    expect_lt(cc$worst_parameter_se_units, 0.1)
    ## The mean hyperparameters must not carry a large absolute gradient.
    tab <- st$gradient_blocks
    small <- tab[tab$n < 10, ]
    if(nrow(small)) expect_lt(max(small$norm), 1)
  })

  test_that("the parameter scaling is a reparameterisation, not a model change", {
    sdat <- list(Nsubs = 5000, Nitems = 2000, Nobs = 100000, Nscales = 1,
      NpersonPreds = 0L, fixedAbilityMean = 1L)
    lay <- list(B = 1:2000, A = 2001:4000, B_mean = 4001L, A_mean = 4002L,
      ability_beta = integer(0), ability_mean = integer(0), corr = integer(0))
    ps <- bigIRT:::bigIRT_laplace_par_scale(sdat, lay)
    expect_equal(length(ps), 4002L)
    ## Item parameters are the reference and stay at one.
    expect_equal(unique(ps[1:4000]), 1)
    ## Responses per item (50) are far fewer than items (2000), so the mean
    ## blocks must be scaled down.
    expect_lt(ps[4001], 1); expect_lt(ps[4002], 1)
    expect_equal(ps[4001], sqrt((100000 / 2000) / 2000), tolerance = 1e-12)
  })

  test_that("the C++ person row terms match the R reference exactly", {
    ## This was a third of every objective evaluation on fits with person
    ## covariates. The port has to agree to machine precision, not merely
    ## closely, because it feeds the adjoint gradient the whole fit depends on.
    set.seed(31)
    s <- simIRT(Nsubs = 1200, Nitems = 40, Nscales = 2, NitemsAnswered = c(12L, 12L),
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -1.4, logitCSD = .2, logitDMean = 2.5, logitDSD = .2)
    d <- data.table::as.data.table(s$dat)
    ids <- unique(d$id); x <- stats::rnorm(length(ids))
    names(x) <- as.character(ids)
    d[, xcov := x[as.character(id)]]
    f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 4L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      personPreds = "xcov", keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    re <- bigIRT:::bigIRT_laplace_row_effective(state = dd$state, sdat = dd$sdat,
      thetaBase = dd$eval$posterior$theta_mode, rows = dd$context$train_rows,
      context = dd$context$row_context)
    a <- bigIRT:::bigIRT_laplace_person_row_terms(dd$sdat, dd$eval$posterior, re,
      dd$context$row_context, impl = "cpp")
    b <- bigIRT:::bigIRT_laplace_person_row_terms(dd$sdat, dd$eval$posterior, re,
      dd$context$row_context, impl = "R")
    expect_equal(a$score, b$score, tolerance = 1e-12)
    expect_equal(a$slope, b$slope, tolerance = 1e-12)
    expect_equal(dim(a$score), c(as.integer(f$dat$Nsubs), 2L))
  })

  test_that("the kernel's row-effective values match the R construction", {
    ## The block kernel derives the effective discrimination, difficulty,
    ## asymptotes and linear predictor while evaluating the likelihood, and now
    ## hands them back instead of letting R rebuild the same four quantities --
    ## which was a quarter of every objective evaluation on covariate fits.
    ## Two implementations of the same formulas can drift, so this pins them
    ## together on a design that exercises every branch: four parameters, two
    ## dimensions, a person covariate and an estimated latent correlation.
    set.seed(41)
    s <- simIRT(Nsubs = 1500, Nitems = 50, Nscales = 2,
      NitemsAnswered = c(15L, 15L), AMean = 1, ASD = .3, BMean = 0, BSD = 1,
      AbilityMean = 0, AbilitySD = 1, logitCMean = -1.4, logitCSD = .2,
      logitDMean = 2.5, logitDSD = .2)
    d <- data.table::as.data.table(s$dat)
    ids <- unique(d$id); x <- stats::rnorm(length(ids))
    names(x) <- as.character(ids)
    d[, xcov := x[as.character(id)]]
    f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 4L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      personPreds = "xcov", estimateAbilityCorr = TRUE, keepInternals = TRUE,
      laplaceKeepCovariance = TRUE)
    dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    ev <- bigIRT:::bigIRT_laplace_direct_objective(par = dd$par, state = dd$state,
      sdat = dd$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = dd$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = dd$context)
    ker <- ev$item_fg$row_effective
    expect_false(is.null(ker))
    rr <- bigIRT:::bigIRT_laplace_row_effective(state = ev$state, sdat = dd$sdat,
      thetaBase = ev$posterior$theta_mode, rows = dd$context$train_rows,
      context = dd$context$row_context)
    for(nm in c("eta_row", "c_row", "d_row", "loadings"))
      expect_equal(as.numeric(ker[[nm]]), as.numeric(rr[[nm]]), tolerance = 1e-12,
        info = nm)
  })

  test_that("row-effective values are not materialised when nothing needs them", {
    ## Four Nobs-sized buffers are not free at eight million responses, so a fit
    ## with no adjoint gradient path should not pay for them.
    f <- fit_small()
    dd <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    ev <- bigIRT:::bigIRT_laplace_direct_objective(par = dd$par, state = dd$state,
      sdat = dd$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = dd$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = dd$context)
    ## fit_small estimates the item means, so the path does run there; assert on
    ## the switch itself rather than on this particular fit.
    lay <- dd$layout
    needed <- length(lay$ability_beta) > 0L || length(lay$ability_mean) > 0L
    if(!needed) expect_null(ev$item_fg$row_effective)
    else expect_false(is.null(ev$item_fg$row_effective))
  })

  test_that("item-specific covariate rows index their own coefficient", {
    ## `ifelse` is vectorised over its condition, so the scalar test here used
    ## to collapse the index vector to its first element and recycle it: every
    ## response pointed at one item's coefficient. That coefficient then
    ## absorbed every item's gradient and the fit died with a non-finite
    ## difficulty, so item-specific item covariates did not work at all.
    ## The predictor has to vary within item, or each coefficient is confounded
    ## with that item's own difficulty and nothing is identified.
    set.seed(12)
    s <- simIRT(Nsubs = 3000, Nitems = 40, Nscales = 1, NitemsAnswered = 20,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    d <- data.table::as.data.table(s$dat)
    d[, zpred := stats::rnorm(.N)]
    beta_true <- 0.4
    th <- s$Ability[, 1][d$id]
    eta <- s$A[d$Item, 1] * th - (s$B[d$Item, 1] + beta_true * d$zpred)
    d[, score := stats::rbinom(.N, 1, 1 / (1 + exp(-eta)))]
    f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      BitemPreds = "zpred", itemSpecificBetas = TRUE, keepInternals = TRUE,
      laplaceKeepCovariance = TRUE)
    expect_true(is.finite(f$optim$logLik))
    rc <- bigIRT:::bigIRT_laplace_row_context(f$internals$sdat)
    ## The whole point: one coefficient per item, not one for all of them.
    expect_equal(as.integer(rc$B_beta_row), as.integer(rc$B_ref))
    expect_gt(length(unique(rc$B_beta_row)), 1L)
    ## And they must actually estimate the effect, not merely run.
    bb <- as.numeric(f$pars$Bbeta)
    expect_equal(length(bb), as.integer(f$dat$Nitems))
    expect_equal(mean(bb), beta_true, tolerance = 0.1)
  })

  test_that("a shared item covariate does not dominate the gradient", {
    ## With itemSpecificBetas = FALSE -- the default -- there is one coefficient
    ## per predictor and its gradient sums over every response, against an item
    ## parameter's own responses alone. Unscaled it held 92 per cent of the
    ## gradient norm on a 2,000-item fit.
    set.seed(6)
    s <- simIRT(Nsubs = 2500, Nitems = 600, Nscales = 1, NitemsAnswered = 30,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    d <- data.table::as.data.table(s$dat)
    set.seed(7); itm <- unique(d$Item); z <- stats::rnorm(length(itm))
    names(z) <- as.character(itm)
    d[, ipred := z[as.character(Item)]]
    f <- fitIRT(d, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      BitemPreds = "ipred", itemSpecificBetas = FALSE, laplaceTolScale = "per_obs",
      ## No polish: it drives the item blocks to nearly zero, after which the
      ## blocks it does not touch hold most of a negligible total and a share
      ## test says nothing. What is under test is the optimizer's own balance.
      laplacePolish = 0L, keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    tab <- f$laplaceStatus$gradient_blocks
    bb <- tab$share[tab$block == "B_beta"]
    expect_length(bb, 1L)
    expect_lt(bb, 0.4)
    expect_gt(sum(tab$share[tab$block %in% c("A", "B")]), 0.5)
  })

  test_that("every small aggregating block is preconditioned", {
    ## One list, so a block added later is not quietly left out; the covariate
    ## and mean blocks were each found the hard way, one at a time.
    sdat <- list(Nsubs = 5000, Nitems = 2000, Nobs = 100000, Nscales = 1,
      NpersonPreds = 0L, fixedAbilityMean = 1L, itemSpecificBetas = 0L,
      BitemPreds = "z", itemPreds = NULL)
    lay <- list(B = 1:2000, B_mean = 2001L, B_beta = 2002L, A = 2003:4002,
      A_mean = 4003L, ability_beta = integer(0), ability_mean = integer(0),
      corr = integer(0))
    ps <- bigIRT:::bigIRT_laplace_par_scale(sdat, lay)
    expect_equal(unique(ps[c(1:2000, 2003:4002)]), 1)   # item parameters are the reference
    for(nm in c("B_mean", "A_mean", "B_beta"))
      expect_lt(ps[lay[[nm]]], 1, label = nm)
  })

  test_that("empirical Bayes still ends with a full-tolerance polished pass", {
    ## Every empirical-Bayes round runs at a deliberately coarse tolerance,
    ## `ebayesCoarse` times the requested one, because its only job is to feed
    ## the hyperparameter step. The pass after the last update is the one whose
    ## estimates are returned, and it runs at the requested tolerance with the
    ## Newton polish. Settling early used to break out of the loop before it,
    ## returning a fit optimised a hundred times too loosely and unpolished,
    ## which left it just the wrong side of the gradient tolerance.
    set.seed(15)
    s <- simIRT(Nsubs = 2500, Nitems = 100, Nscales = 1, NitemsAnswered = 12L,
      AMean = 1, ASD = .3, BMean = 0, BSD = 0.4, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
    f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = TRUE, normalise = FALSE, dropPerfectScores = FALSE,
      keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    st <- f$laplaceStatus
    expect_true(isTRUE(st$converged))
    ## The final pass must have happened, and been polished.
    expect_gt(st$ebayes_rounds, 0L)
    expect_gt(st$polish_steps, 0L)
    expect_lt(as.numeric(st$last_item_grad_scaled), 1e-4)
  })

  test_that("the Newton polish also cleans the mean hyperparameters", {
    ## The polish stepped item blocks only, so once those were at zero the mean
    ## hyperparameters held whatever was left -- 41 per cent of the remaining
    ## gradient on an empirical-Bayes fit. Each enters through the item prior
    ## alone, so its curvature is exact and costs no data pass.
    f <- fit_small()
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    stp <- bigIRT:::bigIRT_laplace_newton_step(d$eval, d$sdat, d$context, d$layout)
    expect_false(is.null(stp))
    for(nm in c("B_mean", "A_mean")){
      idx <- d$layout[[nm]]
      if(length(idx)) expect_true(all(is.finite(stp[idx])), info = nm)
    }
  })
}
