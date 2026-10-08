## Empirical-Bayes hyperparameter updates when some item parameters are fixed
## (anchor items through itemDat, fixed loadings).
##
## Both updates on the Laplace path accumulate per-parameter quantities from
## the response rows, and both used to index them by something other than
## what they summed over. Neither showed while every item parameter was free,
## because then an item's free-parameter index IS the item:
##
## 1. bigIRT_laplace_eb_update (ebayesMethod = "moment") summed information by
##    free-parameter index into a vector of length Nitems. With f parameters
##    of a block fixed the last f entries stayed zero, entered
##    mean(1 / pmax(info, 1e-8)) as 1e8 each, and floored that prior SD
##    whatever the data said.
## 2. bigIRT_laplace_hyper_pieces (ebayesMethod = "laplace") grouped rows by
##    B_ref, the free-difficulty index. Rows of an item with a fixed
##    difficulty fell into group 0 and were dropped from every block, the
##    unused trailing slices were prior-only and each added log tau to the
##    marginal, and a fixed loading or asymptote was integrated over as if it
##    were free.
##
## The reference values are built per ITEM from sdat's fixed flags and the
## free-parameter vectors, never through B_ref, A_ref or freeXref, and are
## checked first against the package on a fit with nothing fixed, where the
## two indexings coincide.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  ## Item-level parameters at K = 1 from the free vectors, by fixed flag.
  ref_item_pars <- function(sdat, st){
    B <- as.numeric(sdat$Bdata); fB <- which(sdat$fixedB == 0L)
    B[fB] <- as.numeric(st$Bpars)
    A <- as.numeric(sdat$Adata); fA <- which(sdat$fixedAlog == 0L)
    A[fA] <- log1p(exp(as.numeric(st$invspApars)))
    C <- as.numeric(sdat$Cdata); fC <- which(sdat$fixedClogit == 0L)
    if(length(fC)) C[fC] <- stats::plogis(as.numeric(st$logitCpars)) * .5
    D <- as.numeric(sdat$Ddata); fD <- which(sdat$fixedDlogit == 0L)
    if(length(fD)) D[fD] <- stats::plogis(as.numeric(st$logitDpars)) * .5 + .5
    list(A = A, B = B, C = C, D = D, free = list(B = fB, A = fA, C = fC, D = fD))
  }

  ref_rows <- function(sdat, st, theta){
    ip <- ref_item_pars(sdat, st)
    item <- as.integer(sdat$item); id <- as.integer(sdat$id)
    th <- as.numeric(theta)[id]
    g <- stats::plogis(ip$A[item] * th - ip$B[item])
    cc <- ip$C[item]; dd <- ip$D[item]
    p <- cc + (dd - cc) * g
    c(ip, list(item = item, id = id, th = th, g = g, cc = cc, dd = dd,
               eta = ip$A[item] * th - ip$B[item],
               s = (dd - cc) * g * (1 - g), r = p * (1 - p)))
  }

  ## d p / d (free raw parameter), per row, for each block.
  ref_v <- function(R) list(B = -R$s, A = R$s * R$th * (1 - exp(-R$A))[R$item],
    C = (1 - R$g) * R$cc * (1 - 2 * R$cc), D = R$g * (2 * R$dd - 1) * (1 - R$dd))

  ## Moment rule: tau^2 = mean((est - mu)^2) - mean(1 / I_j) over free params.
  ref_moment <- function(sdat, st, theta, multiplier, min_sd){
    R <- ref_rows(sdat, st, theta); V <- ref_v(R)
    est <- list(B = st$Bpars, A = st$invspApars, C = st$logitCpars, D = st$logitDpars)
    out <- list()
    for(b in names(est)){
      if(length(R$free[[b]]) < 3L) next
      info <- vapply(R$free[[b]], function(j){ k <- R$item == j; sum(V[[b]][k]^2 / R$r[k]) },
                     numeric(1))
      e <- as.numeric(est[[b]])
      tau2 <- mean((e - mean(e))^2) - mean(1 / info)
      out[[b]] <- c(mean = mean(e), sd = max(sqrt(max(tau2, 0)) * multiplier, min_sd))
    }
    out
  }

  ## Laplace rule: per item, a block over that item's free parameters among
  ## `active`, with prior precision 1/tau^2 on each. A function of log tau.
  ref_hyper_objective <- function(sdat, st, theta, Sig, active){
    R <- ref_rows(sdat, st, theta); V <- ref_v(R)[active]
    wc <- 1 / R$r - R$s^2 * R$A[R$item]^2 * as.numeric(Sig)[R$id] / R$r^2
    est <- list(B = st$Bpars, A = st$invspApars, C = st$logitCpars, D = st$logitDpars)[active]
    blocks <- lapply(seq_len(sdat$Nitems), function(j){
      Fj <- active[vapply(active, function(b) j %in% R$free[[b]], logical(1))]
      if(!length(Fj)) return(NULL)
      k <- which(R$item == j)
      M <- matrix(unlist(lapply(V[Fj], `[`, k)), nrow = length(k))
      list(F = Fj, S = crossprod(M, M * wc[k]))
    })
    function(logtau){
      tau <- setNames(exp(logtau), active)
      pen <- 0
      for(b in active){
        e <- as.numeric(est[[b]])
        pen <- pen - sum((e - mean(e))^2) / (2 * tau[[b]]^2) - length(e) * log(tau[[b]])
      }
      ld <- 0
      for(bl in Filter(Negate(is.null), blocks))
        ld <- ld + as.numeric(determinant(bl$S + diag(1 / tau[bl$F]^2, length(bl$F)),
                                          logarithm = TRUE)$modulus)
      -(pen - 0.5 * ld)
    }
  }

  ## A fit without empirical Bayes, and everything the updates take.
  eb_fixture <- function(dat, pl, itemDat = NA, ...){
    f <- suppressWarnings(fitIRT(dat, pl = pl, cores = 2, marginalApprox = "laplace",
      itemDat = itemDat, ebayes = FALSE, keepInternals = TRUE, ...))
    sdat <- f$internals$sdat
    rows <- bigIRT:::bigIRT_laplace_row_context(sdat, rows = seq_len(sdat$Nobs))
    ctx <- bigIRT:::bigIRT_laplace_item_context(sdat, row_context = rows)
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    post <- d$eval$posterior
    list(f = f, sdat = sdat, rows = rows, ctx = ctx, post = post,
         theta = post$theta_mode, st = d$state,
         eff = function(st) bigIRT:::bigIRT_laplace_row_effective(state = st, sdat = sdat,
           thetaBase = post$theta_mode, rows = rows$rows, context = rows))
  }

  run_moment <- function(fx, st = fx$st, multiplier = 1, min_sd = 0.05)
    bigIRT:::bigIRT_laplace_eb_update(state = st, sdat = fx$sdat, context = fx$ctx,
      row_effective = fx$eff(st), row_context = fx$rows, thetaBase = fx$theta,
      multiplier = multiplier, min_sd = min_sd)

  sd_name <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")
  mean_name <- c(B = "BMeandat", A = "invspAMeandat", C = "logitCMeandat", D = "logitDMeandat")

  ## 2PL, 30 items. Difficulty fixed on items 1-4, loading on 5-8, both on
  ## 9-10, each at its generating value.
  set.seed(101)
  sim2 <- simIRT(Nsubs = 1000, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1,
                 BSD = 1, logitCMean = -20, logitCSD = 0)
  anchors2 <- local({
    tp <- sim2$itemPars
    x <- data.frame(Item = tp$Item, A = NA_real_, B = NA_real_, C = NA_real_)
    x$B[c(1:4, 9:10)] <- tp$B[c(1:4, 9:10)]
    x$A[5:10] <- tp$A[5:10]
    x[1:10, ]
  })

  test_that("the reference computations reproduce the package when nothing is fixed", {
    fx <- eb_fixture(sim2$dat, pl = 2)
    expect_equal(fx$sdat$NfixedB, 0L)
    expect_equal(fx$sdat$NfixedA, 0L)
    R <- ref_rows(fx$sdat, fx$st, fx$theta)
    expect_equal(R$eta, as.numeric(fx$eff(fx$st)$eta_row), tolerance = 1e-12)
    m <- run_moment(fx)
    ref <- ref_moment(fx$sdat, fx$st, fx$theta, multiplier = 1, min_sd = 0.05)
    for(b in c("B", "A")){
      expect_gt(ref[[b]][["sd"]], 0.05)
      expect_equal(m$sdat[[sd_name[[b]]]], ref[[b]][["sd"]], tolerance = 1e-10)
    }
    pc <- bigIRT:::bigIRT_laplace_hyper_pieces(fx$st, fx$sdat, fx$ctx, fx$eff(fx$st),
                                               fx$rows, fx$post, fx$theta)
    obj <- ref_hyper_objective(fx$sdat, fx$st, fx$theta, fx$post$covariance, pc$active)
    for(lt in list(log(c(1, .3)), log(c(.5, .5)), log(c(2, .1))))
      expect_equal(bigIRT:::bigIRT_laplace_hyper_objective(lt, pc), obj(lt), tolerance = 1e-8)
  })

  fx2 <- eb_fixture(sim2$dat, pl = 2, itemDat = anchors2)

  test_that("moment rule: information is indexed by free parameter when items are fixed", {
    expect_equal(fx2$sdat$NfixedB, 6L)
    expect_equal(fx2$sdat$NfixedA, 6L)
    R <- ref_rows(fx2$sdat, fx2$st, fx2$theta)
    expect_equal(R$eta, as.numeric(fx2$eff(fx2$st)$eta_row), tolerance = 1e-12)
    for(mult in c(1, 2)){
      m <- run_moment(fx2, multiplier = mult)
      ref <- ref_moment(fx2$sdat, fx2$st, fx2$theta, multiplier = mult, min_sd = 0.05)
      for(b in c("B", "A")){
        ## Well clear of the floor, so a floored value cannot pass.
        expect_gt(ref[[b]][["sd"]], 0.3)
        expect_true(sd_name[[b]] %in% m$changed)
        expect_equal(m$sdat[[sd_name[[b]]]], ref[[b]][["sd"]], tolerance = 1e-10)
        expect_equal(m$sdat[[mean_name[[b]]]], ref[[b]][["mean"]], tolerance = 1e-12)
      }
    }
  })

  test_that("Laplace rule: one block per item, over that item's free parameters", {
    expect_no_warning(pc <- bigIRT:::bigIRT_laplace_hyper_pieces(fx2$st, fx2$sdat, fx2$ctx,
      fx2$eff(fx2$st), fx2$rows, fx2$post, fx2$theta))
    expect_equal(pc$active, c("B", "A"))
    obj <- ref_hyper_objective(fx2$sdat, fx2$st, fx2$theta, fx2$post$covariance, pc$active)
    for(lt in list(log(c(1, .3)), log(c(.5, .5)), log(c(2, .1)), log(c(.2, 3))))
      expect_equal(bigIRT:::bigIRT_laplace_hyper_objective(lt, pc), obj(lt), tolerance = 1e-8)
    u <- bigIRT:::bigIRT_laplace_hyper_update(fx2$st, fx2$sdat, fx2$ctx, fx2$eff(fx2$st),
                                              fx2$rows, fx2$post, fx2$theta)
    o <- stats::optim(log(c(1, .3)), obj, method = "BFGS", control = list(reltol = 1e-14))
    o <- stats::optim(o$par, obj, control = list(reltol = 1e-14, maxit = 5000))
    expect_equal(as.numeric(u$tau), exp(o$par), tolerance = 1e-3)
    expect_equal(unname(u$sdat$BSDx), u$tau[["B"]])
    expect_equal(unname(u$sdat$invspASD), u$tau[["A"]])
    expect_equal(u$sdat$BMeandat, mean(fx2$st$Bpars))
  })

  test_that("both rules, 4PL with each item-parameter type fixed on some items", {
    set.seed(202)
    sim <- simIRT(Nsubs = 1500, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1.3,
                  BSD = 1, logitCMean = -1.5, logitCSD = .5, logitDMean = 2, logitDSD = .5)
    tp <- sim$itemPars
    anc <- data.frame(Item = tp$Item, A = NA_real_, B = NA_real_, C = NA_real_, D = NA_real_)
    anc$B[1:3] <- tp$B[1:3]; anc$A[4:6] <- tp$A[4:6]
    anc$C[7:10] <- tp$C[7:10]; anc$D[11:14] <- tp$D[11:14]
    fx <- eb_fixture(sim$dat, pl = 4, itemDat = anc[1:14, ], laplaceRefine = "none")
    expect_equal(c(fx$sdat$NfixedB, fx$sdat$NfixedA, fx$sdat$NfixedC, fx$sdat$NfixedD),
                 c(3L, 3L, 4L, 4L))
    R <- ref_rows(fx$sdat, fx$st, fx$theta)
    e <- fx$eff(fx$st)
    expect_equal(R$eta, as.numeric(e$eta_row), tolerance = 1e-12)
    expect_equal(R$cc, as.numeric(e$c_row), tolerance = 1e-12)
    expect_equal(R$dd, as.numeric(e$d_row), tolerance = 1e-12)

    ## The upper asymptote is too weakly identified at the fit for its
    ## corrected dispersion to clear the floor (its noise term exceeds its
    ## spread), so the moment rule is also checked at a state with the
    ## asymptotes spread out -- the upper one around d = 0.75, where it
    ## carries the most information; spreading it upward only makes it less
    ## identified. The rule is a function of whatever state it is handed.
    st_spread <- fx$st
    set.seed(9)
    st_spread$logitCpars <- fx$st$logitCpars + stats::rnorm(length(fx$st$logitCpars), 0, 1.5)
    st_spread$logitDpars <- stats::rnorm(length(fx$st$logitDpars), 0, 1)
    for(st in list(fx$st, st_spread)){
      m <- run_moment(fx, st = st)
      ref <- ref_moment(fx$sdat, st, fx$theta, multiplier = 1, min_sd = 0.05)
      for(b in c("B", "A", "C", "D"))
        expect_equal(m$sdat[[sd_name[[b]]]], ref[[b]][["sd"]], tolerance = 1e-10)
    }
    ref <- ref_moment(fx$sdat, st_spread, fx$theta, multiplier = 1, min_sd = 0.05)
    ## Every block clear of the 0.05 floor there, so a floored value fails.
    expect_true(all(vapply(ref, function(z) z[["sd"]], numeric(1)) > 0.2))

    expect_no_warning(pc <- bigIRT:::bigIRT_laplace_hyper_pieces(fx$st, fx$sdat, fx$ctx, e,
      fx$rows, fx$post, fx$theta))
    expect_equal(pc$active, c("B", "A", "C", "D"))
    obj <- ref_hyper_objective(fx$sdat, fx$st, fx$theta, fx$post$covariance, pc$active)
    for(lt in list(log(c(1, .3, .5, .5)), log(c(.5, 1, .2, 2)), log(c(2, .1, 1, .1))))
      expect_equal(bigIRT:::bigIRT_laplace_hyper_objective(lt, pc), obj(lt), tolerance = 1e-8)
  })

  test_that("fitIRT empirical Bayes: anchoring items at their generating values keeps the prior", {
    args <- list(dat = sim2$dat, pl = 2, cores = 2, marginalApprox = "laplace", ebayes = TRUE)
    for(method in c("moment", "laplace")){
      free <- suppressWarnings(do.call(fitIRT, c(args, list(ebayesMethod = method))))
      ## The data warn about dropped perfect scores; the Laplace rule used to
      ## warn too, from scattering a rowsum's group 0 into out[0].
      warns <- character(0)
      anc <- withCallingHandlers(
        do.call(fitIRT, c(args, list(ebayesMethod = method, itemDat = anchors2))),
        warning = function(w){
          warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
        })
      expect_false(any(grepl("replacement length", warns)))
      ## Anchoring ten of thirty items at the truth leaves 24 free difficulties
      ## and 24 free loadings drawn from the same population, so the
      ## dispersion estimates should barely move. The moment rule used to
      ## floor both at ebayesMinSD.
      for(nm in c("BSDx", "invspASD")){
        ratio <- anc$prepared[[nm]] / free$prepared[[nm]]
        expect_gt(ratio, 0.75)
        expect_lt(ratio, 1.33)
      }
    }
  })
}
