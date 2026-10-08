## The joint-MAP estimator (marginalApprox = "map", src/map_backend.cpp).
##
## Each property is asserted directly rather than read off a plausible fit:
## the gradient and the item blocks of the Hessian against central
## differences, the objective against a plain-R joint posterior written from
## the model's definition, and the person modes against their own
## stationarity. The tolerances sit two orders above what was measured (the
## gradient agreed to about 1e-7, the blocks to 2e-9) and far below what a
## chain-rule slip produces.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  library(data.table)
  B <- asNamespace("bigIRT")

  ## The joint-MAP objective at a perturbed point, as the optimiser sees it.
  map_evaluator <- function(dat, args = list(), seed = 1, jitter_par = 0.3){
    f <- suppressWarnings(do.call(fitIRT, c(list(dat = dat, cores = 1, priors = TRUE,
      dropPerfectScores = FALSE, marginalApprox = "map", laplaceOuterIter = 2,
      laplaceCertify = FALSE, keepInternals = TRUE), args)))
    sdat <- f$internals$sdat
    st <- B$bigIRT_laplace_initial_state(sdat, eps = 1e-6, corr_paramization = "normalized_chol")
    pp <- B$bigIRT_laplace_prior_mats(sdat, jitter = 1e-6)$precision_array
    lay <- B$bigIRT_laplace_direct_layout(sdat, estimateAbilityCorr = FALSE)
    ctx <- B$bigIRT_laplace_item_context(sdat,
      layout = lay[setdiff(names(lay), c("corr", "ability_mean", "ability_beta"))])
    ctx$grain_size <- 16L
    ctx$direct_layout <- lay
    ctx$corr_paramization <- "normalized_chol"
    ctx$map <- TRUE
    st$laplaceCorrParam <- "normalized_chol"
    set.seed(seed)
    par <- B$bigIRT_laplace_pack_direct_state(st, sdat, layout = lay)
    par <- par + stats::rnorm(length(par), 0, jitter_par)
    ev <- function(v, context = ctx) B$bigIRT_laplace_direct_objective(par = v, state = st,
      sdat = sdat, prior_precision = pp, theta_init = st$AbilityBase, jitter = 1e-6,
      max_attempts = 8L, max_iter = 4000L, tol = 1e-13, keep_covariance = TRUE, context = context)
    list(layout = lay, par = par, eval = ev, sdat = sdat, state = st, ctx = ctx)
  }

  ## Largest relative error of the analytic gradient against central
  ## differences, by parameter block.
  grad_relerr <- function(d, h = 1e-5){
    g <- d$eval(d$par)$approx_grad
    fd <- vapply(seq_along(d$par), function(k){
      up <- d$par; up[k] <- up[k] + h; dn <- d$par; dn[k] <- dn[k] - h
      (d$eval(up)$value - d$eval(dn)$value) / (2 * h)
    }, numeric(1))
    err <- abs(g - fd) / pmax(1, abs(fd))
    vapply(d$layout[lengths(d$layout) > 0], function(i) max(err[i]), numeric(1))
  }

  ## The configurations, one per kind of parameter block and link.
  map_cases <- function(){
    set.seed(11)
    out <- list()
    s1 <- simIRT(Nsubs = 150, Nitems = 20, Nscales = 1, NitemsAnswered = 12, AMean = 1,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -20, logitCSD = 0)
    out$pl2 <- map_evaluator(s1$dat, list(pl = 2))
    s2 <- simIRT(Nsubs = 150, Nitems = 20, Nscales = 1, NitemsAnswered = 12, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    out$pl3 <- map_evaluator(s2$dat, list(pl = 3))
    ## 4PL in two dimensions with free, signed cross-loadings.
    s3 <- simIRT(Nsubs = 200, Nitems = 12, Nscales = 2, NitemsAnswered = 12, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3, logitDMean = 2, logitDSD = .3)
    items <- sort(unique(as.character(s3$dat$Item)))
    scales <- sort(unique(as.character(s3$dat$Scale)))
    own <- s3$dat[!duplicated(Item), setNames(as.character(Scale), as.character(Item))]
    L <- matrix(0, length(items), 2, dimnames = list(items, scales))
    L[cbind(items, own[items])] <- NA
    L[items[1:3], ] <- NA
    out$pl4_signed <- map_evaluator(s3$dat, list(pl = 4, loadings = L, signedLoadings = "cross"))
    ## Person covariates varying within person, shared item covariates on A
    ## and B, and the ability mean estimated.
    s4 <- simIRT(Nsubs = 200, Nitems = 20, Nscales = 2, NitemsAnswered = 14, AMean = 1,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -20, logitCSD = 0)
    d4 <- s4$dat
    d4[, x1 := stats::rnorm(.N)]; d4[, x2 := stats::rnorm(1), by = id]; d4[, ip := stats::rnorm(1), by = Item]
    out$covariates <- map_evaluator(d4, list(pl = 2, personPreds = c("x1", "x2"),
      AitemPreds = "ip", BitemPreds = "ip", estMeans = c("ability", "A"), betaScale = 2))
    ## Item-specific covariate effects on C, with items and persons partly fixed.
    s5 <- simIRT(Nsubs = 150, Nitems = 15, Nscales = 1, NitemsAnswered = 12, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d5 <- s5$dat; d5[, ic := stats::rnorm(.N)]; d5[, x := stats::rnorm(1), by = id]
    idat <- data.frame(Item = unique(d5$Item)[1:3], A = 1.1, B = c(-.5, 0, .5), C = .1, D = 1)
    pdat <- data.frame(id = unique(d5$id)[1:5], s1 = c(-1, -.5, 0, .5, 1))
    names(pdat)[2] <- as.character(unique(d5$Scale))
    ## (A person covariate too: its gradient must skip the fixed abilities.)
    out$fixed <- map_evaluator(d5, list(pl = 3, CitemPreds = "ic", itemSpecificBetas = TRUE,
      itemDat = idat, personDat = pdat, personPreds = "x"))
    ## Item-specific covariate effects on every slot of a 4PL in two
    ## dimensions, cross-loadings free on three items so an item has
    ## coefficients on both loadings; `v` varies within an item and `w` does
    ## not, and only per-row covariate values get the varying one right.
    s6 <- simIRT(Nsubs = 200, Nitems = 10, Nscales = 2, NitemsAnswered = 10, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3, logitDMean = 2, logitDSD = .3)
    d6 <- s6$dat; d6[, v := stats::rnorm(.N, 0, .5)]; d6[, w := stats::rnorm(1, 0, .5), by = Item]
    items6 <- sort(unique(as.character(d6$Item)))
    own6 <- d6[!duplicated(Item), setNames(as.character(Scale), as.character(Item))]
    L6 <- matrix(0, length(items6), 2, dimnames = list(items6, sort(unique(as.character(d6$Scale)))))
    L6[cbind(items6, own6[items6])] <- NA
    L6[items6[1:3], ] <- NA
    out$item_specific <- map_evaluator(d6, list(pl = 4, loadings = L6, signedLoadings = "cross",
      AitemPreds = "v", BitemPreds = c("v", "w"), CitemPreds = "w", DitemPreds = "v",
      itemSpecificBetas = TRUE, betaScale = 2), jitter_par = 0.15)
    out
  }
  cases <- map_cases()

  test_that("the joint-MAP gradient matches central differences in every block", {
    for(nm in names(cases)){
      err <- grad_relerr(cases[[nm]])
      expect_lt(max(err), 1e-5, label = sprintf("%s, worst block %s", nm, names(which.max(err))))
    }
  })

  test_that("the joint-MAP item blocks are the profiled Hessian's, by central differences", {
    ## These feed the certificate's fast route and so itemInformation(method =
    ## "exact"): d2f/dpsi_j2 = sum_r h_r + sum_i X_ij' H_i^-1 X_ij, over the
    ## item's slots and its own covariate coefficients (itemSpecificBetas).
    h <- 1e-4
    for(nm in names(cases)){
      d <- cases[[nm]]
      K <- d$sdat$Nscales; rc <- d$ctx$row_context; lay <- d$layout
      ps <- attr(lay, "par_scale")
      cb <- d$ctx; cb$map_blocks <- TRUE
      arr <- d$eval(d$par, context = cb)$item_fg$item_blocks
      prec <- numeric(length(d$par))
      for(b in list(c("B", "BSDx"), c("A", "invspASD"), c("C", "logitCSD"), c("D", "logitDSD"))){
        idx <- lay[[b[1]]]
        if(length(idx)) prec[idx] <- 1 / as.numeric(d$sdat[[b[2]]])^2
      }
      for(b in c("A_beta", "B_beta", "C_beta", "D_beta"))
        if(length(lay[[b]])) prec[lay[[b]]] <- 1 / as.numeric(d$sdat$betaScale)^2
      first <- match(seq_len(dim(arr)[3]), rc$items)
      worst <- 0; nbeta <- 0L
      for(j in seq_len(min(5L, dim(arr)[3]))){
        r <- first[j]
        sc <- c(if(rc$B_ref[r] > 0) lay$B[rc$B_ref[r]] else NA,
          vapply(seq_len(K), function(k) if(rc$A_ref[r, k] > 0) lay$A[rc$A_ref[r, k]] else NA_integer_, 1L),
          if(length(lay$C) && rc$C_ref[r] > 0) lay$C[rc$C_ref[r]] else NA,
          if(length(lay$D) && rc$D_ref[r] > 0) lay$D[rc$D_ref[r]] else NA,
          B$bigIRT_item_beta_coords(d$sdat, lay, j))
        expect_equal(length(sc), dim(arr)[1])
        ok <- which(!is.na(sc)); co <- sc[ok]
        if(!length(co)) next
        nbeta <- nbeta + sum(ok > K + 3L)
        Hk <- (matrix(arr[, , j], dim(arr)[1])[ok, ok, drop = FALSE] - diag(prec[co], length(co))) *
          outer(ps[co], ps[co])
        Hfd <- vapply(co, function(k){
          up <- d$par; up[k] <- up[k] + h; dn <- d$par; dn[k] <- dn[k] - h
          (d$eval(up)$approx_grad - d$eval(dn)$approx_grad)[co] / (2 * h)
        }, numeric(length(co)))
        Hfd <- matrix(Hfd, length(co))
        Hfd <- (Hfd + t(Hfd)) / 2
        worst <- max(worst, max(abs(Hk - Hfd)) / max(1, max(abs(Hfd))))
      }
      expect_lt(worst, 1e-6, label = nm)
      ## Not vacuously: the item-specific cases compared their coefficients.
      if(isTRUE(d$sdat$itemSpecificBetas == 1L)) expect_gt(nbeta, 0L, label = nm)
    }
  })

  ## The model's joint log posterior, written from its definition and nothing
  ## else: Bernoulli responses with p = c + (d - c) logistic(a'theta - b),
  ## abilities N(0, I) (constants dropped), and the default item priors. The
  ## ability prior's covariance carries the package's jitter (laplaceJitter,
  ## 1e-6), which is worth 5e-5 nats here and is part of the model fitted.
  joint_posterior <- function(fit, dat, pl){
    ii <- match(as.character(dat$Item), rownames(fit$pars$B))
    pp <- match(as.character(dat$id), rownames(fit$pars$Ability))
    A <- as.matrix(fit$pars$A); th <- as.matrix(fit$pars$Ability)
    eta <- rowSums(A[ii, , drop = FALSE] * th[pp, , drop = FALSE]) - as.numeric(fit$pars$B)[ii]
    cc <- as.numeric(fit$pars$C)[ii]; dd <- as.numeric(fit$pars$D)[ii]
    p <- cc + (dd - cc) * stats::plogis(eta)
    out <- sum(dat$score * log(p) + (1 - dat$score) * log(1 - p)) - 0.5 * sum(th^2) / (1 + 1e-6)
    a <- A[A != 0]
    out <- out + sum(stats::dnorm(log(expm1(a)), .542, 1, log = TRUE)) +
      sum(stats::dnorm(as.numeric(fit$pars$B), 0, 2.5, log = TRUE))
    if(pl >= 3) out <- out + sum(stats::dnorm(stats::qlogis(2 * as.numeric(fit$pars$C)), 0, 1, log = TRUE))
    out
  }

  test_that("a joint-MAP fit is the mode of the model's joint posterior", {
    set.seed(2026)
    for(pl in 2:3){
      s <- simIRT(Nsubs = 300, Nitems = 25, Nscales = 1, NitemsAnswered = 20, AMean = 1.2,
        ASD = .3, BMean = 0, BSD = 1, logitCMean = if(pl == 3) -1.5 else -20,
        logitCSD = if(pl == 3) .3 else 0)
      fit <- fitIRT(s$dat, pl = pl, cores = 1, marginalApprox = "map", estMeans = character(0),
        dropPerfectScores = FALSE)
      expect_true(isTRUE(fit$laplaceStatus$converged))
      expect_identical(fit$laplaceStatus$certificate$status, "certified")
      ## The objective is that posterior, at the reported point.
      jp <- joint_posterior(fit, s$dat, pl)
      expect_lt(abs(fit$optim$logLik - jp), 1e-8 * abs(jp))
      ## Every reported ability is its person's mode: moving any one of them
      ## either way lowers the joint posterior. (A shared step, so a single
      ## pair of evaluations covers every person at once.)
      for(dlt in c(-1e-3, 1e-3)){
        f2 <- fit; f2$pars$Ability <- f2$pars$Ability + dlt
        expect_lt(joint_posterior(f2, s$dat, pl), jp)
      }
      ## And the item parameters are where the joint posterior peaks: nudging
      ## them, with the abilities left where they are, loses too. (A step big
      ## enough that the loss, second order, dominates the gradient the
      ## optimiser's tolerance leaves, first order.)
      f3 <- fit; f3$pars$B <- f3$pars$B + 1e-2
      expect_lt(joint_posterior(f3, s$dat, pl), jp)
    }
  })

  test_that("both estimators take the same arguments and return the same structure", {
    set.seed(31)
    s <- simIRT(Nsubs = 250, Nitems = 16, Nscales = 2, NitemsAnswered = 16, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -1.5, logitCSD = .3)
    d <- s$dat; d[, x := stats::rnorm(1), by = id]; d[, ip := stats::rnorm(1), by = Item]
    items <- sort(unique(as.character(d$Item)))
    own <- d[!duplicated(Item), setNames(as.character(Scale), as.character(Item))]
    L <- matrix(0, length(items), 2, dimnames = list(items, sort(unique(as.character(d$Scale)))))
    L[cbind(items, own[items])] <- NA
    L[items[1:2], ] <- NA
    train <- sample(nrow(d), round(.9 * nrow(d)))
    args <- list(dat = d, pl = 3, cores = 1, loadings = L, signedLoadings = "cross",
      personPreds = "x", BitemPreds = "ip", AbilityCorr = matrix(c(1, .3, .3, 1), 2),
      trainingRows = train, dropPerfectScores = FALSE, laplaceKeepCovariance = TRUE,
      laplaceRefine = "none")
    fl <- suppressWarnings(do.call(fitIRT, c(args, list(marginalApprox = "laplace"))))
    fm <- suppressWarnings(do.call(fitIRT, c(args, list(marginalApprox = "map"))))
    expect_identical(fm$backend, "map")
    expect_identical(fl$backend, "laplace")
    expect_setequal(names(fm), names(fl))
    expect_identical(names(fm$itemPars), names(fl$itemPars))
    expect_identical(names(fm$personPars), names(fl$personPars))
    expect_identical(names(fm$personPosterior), names(fl$personPosterior))
    expect_identical(dim(fm$pars$A), dim(fl$pars$A))
    expect_true(all(is.finite(as.matrix(fm$personPars[, -1]))))
    ## The prepared data are the estimator's input, and identical.
    expect_identical(fm$prepared, fl$prepared)
    ## logLik() is the same model's marginal likelihood at the MAP estimates.
    ## With laplaceRefine = "none" that is the Laplace objective, which the
    ## Laplace fit maximised: at the MAP estimates it can be no higher, up to
    ## the optimiser's tolerance. (Compared penalised, as optimised; logLik()
    ## removes the item prior, whose share differs between the two points.)
    expect_identical(fm$laplaceStatus$marginal_approx, "laplace")
    expect_true(is.finite(as.numeric(logLik(fm))))
    expect_lte(fm$laplaceStatus$marginal_logLik, fl$optim$logLik + 1e-2)
  })

  test_that("the output functions work on a joint-MAP fit", {
    set.seed(41)
    s <- simIRT(Nsubs = 300, Nitems = 20, Nscales = 1, NitemsAnswered = 15, AMean = 1.2,
      ASD = .3, BMean = 0, BSD = 1, logitCMean = -20, logitCSD = 0)
    d <- s$dat; d[, x := stats::rnorm(1), by = id]
    train <- sample(nrow(d), round(.9 * nrow(d)))
    fit <- fitIRT(d, pl = 2, cores = 1, marginalApprox = "map", personPreds = "x",
      trainingRows = train, keepInternals = TRUE, laplaceKeepCovariance = TRUE,
      dropPerfectScores = FALSE)
    expect_output(print(fit), "joint MAP")
    expect_output(print(summary(fit)), "joint MAP")
    for(m in c("exact", "profile", "hessian")){
      ii <- itemInformation(fit, method = m)
      expect_true(all(is.finite(ii$se)), label = m)
    }
    ## The exact (certificate) and differenced standard errors describe the
    ## same curvature, the former with the coupling between items kept.
    expect_equal(itemInformation(fit, method = "exact")$se,
      itemInformation(fit, method = "hessian")$se, tolerance = .1)
    td <- tidyIRT(fit, conf.int = TRUE)
    expect_true(all(is.finite(td$std.error)))
    g <- glanceIRT(fit)
    expect_identical(g$backend, "map")
    expect_true(is.finite(g$logLik) && is.finite(g$AIC))
    expect_true(all(is.finite(IRTic(fit)$value)))
    expect_equal(nrow(augmentIRT(fit)), nrow(d))
    expect_s3_class(checkConvergence(fit), "bigIRT_convergence")
    expect_true(all(is.finite(vcov(fit, what = "covariate"))))
    expect_true(all(is.finite(reliability(fit)$empirical)))
    expect_true(is.finite(heldoutMetrics(fit)$log_loss))
    expect_s3_class(testInformation(fit), "bigIRT_information")
    expect_equal(nrow(itemFit(fit)), fit$prepared$Nitems)
    expect_true(nrow(empiricalICC(fit, items = 1:2)) > 0)
    if(requireNamespace("loo", quietly = TRUE))
      expect_s3_class(suppressWarnings(looIRT(fit, draws = 50L, seed = 1)), "loo")
  })

  test_that("old arguments keep working, and unknown ones are refused", {
    set.seed(61)
    s <- simIRT(Nsubs = 60, Nitems = 8, Nscales = 1, NitemsAnswered = 8)
    args <- list(dat = s$dat, pl = 1, marginalApprox = "none", laplaceCertify = FALSE,
      dropPerfectScores = FALSE)
    ## Never changed a result: accepted in silence ("none" too, the old name).
    expect_no_warning(f <- do.call(fitIRT, c(args, list(plot = FALSE, noptimsteps = 3))))
    expect_identical(f$backend, "map")
    ## Configured the Stan path: accepted, and said to be ignored.
    expect_warning(do.call(fitIRT, c(args, list(iter = 10))), "laplaceOuterIter")
    ## Anything else is a mistake, and an error rather than a silent no-op.
    expect_error(do.call(fitIRT, c(args, list(laplaceTolerance = 1e-3))),
      "unused argument: laplaceTolerance")
  })

  test_that("joint MAP refuses estimateAbilityCorr, and Laplace without free abilities falls back to it", {
    set.seed(51)
    s <- simIRT(Nsubs = 100, Nitems = 10, Nscales = 2, NitemsAnswered = 10)
    expect_error(fitIRT(s$dat, pl = 2, marginalApprox = "map", estimateAbilityCorr = TRUE),
      "needs marginalApprox")
    ## Every ability fixed: there is nothing to integrate, and the joint mode
    ## is fitted instead, with a warning saying so.
    s1 <- simIRT(Nsubs = 100, Nitems = 10, Nscales = 1, NitemsAnswered = 10)
    pd <- data.frame(id = unique(s1$dat$id), a = as.numeric(s1$Ability))
    names(pd)[2] <- as.character(unique(s1$dat$Scale))
    expect_warning(f <- fitIRT(s1$dat, pl = 2, personDat = pd, dropPerfectScores = FALSE),
      "no free ability parameters")
    expect_identical(f$backend, "map")
    expect_equal(as.numeric(f$pars$Ability), pd[[2]][match(rownames(f$pars$Ability), pd$id)])
  })
}
