## Per-item standard errors when a parameter type is free on some items and
## fixed on others (anchor items through itemDat).
##
## Every per-item construction -- the profile and differenced information
## (bigIRT_item_info_blocks, bigIRT_item_hessian_blocks), the exact covariance
## the convergence certificate keeps (bigIRT_certificate_covariance,
## bigIRT_block_covariance, the block-Schur route's blocks), the Newton polish,
## the covariate errors' marginal correction, checkConvergence and the
## effective degrees of freedom -- built a block only for a parameter type free
## on EVERY item, and paired item j with free index j. Anchor one difficulty
## and the B block disappeared from all of them, so every free difficulty lost
## its standard error; anchor difficulties on some items and loadings on others
## and no block survived at all, and itemInformation() stopped.
##
## A fixed (item, parameter) now carries NA, and each item's free sub-block is
## inverted alone. The references find an item's free parameters from sdat's
## fixed flags alone -- free parameters are stored in item order, so item j's
## position is the number of items up to j with that parameter free -- never
## through freeXref or the package's own map, and are checked first against a
## fit with nothing fixed, where every position is the item itself.
if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  quiet <- function(expr) suppressMessages(suppressWarnings(expr))
  prior_sd <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")

  ## Item j's position in each block's free vector, NA where j has it fixed.
  ## A loading is a cell of the item-major (item, scale) grid; an item's
  ## position is that of its one free cell.
  ref_free_pos <- function(sdat){
    ni <- as.integer(sdat$Nitems); K <- as.integer(sdat$Nscales)
    count <- function(free){ p <- cumsum(free); p[!free] <- NA; as.integer(p) }
    cellA <- matrix(count(as.integer(sdat$fixedAlog) == 0L), ni, K, byrow = TRUE)
    A <- apply(cellA, 1, function(z) if(sum(!is.na(z)) == 1L) z[!is.na(z)] else NA_integer_)
    rbind(B = count(as.integer(sdat$fixedB) == 0L), A = as.integer(A),
          C = count(as.integer(sdat$fixedClogit) == 0L),
          D = count(as.integer(sdat$fixedDlogit) == 0L))
  }

  ## Optimiser coordinates of item j's free parameters among `blocks`.
  ref_coords <- function(fp, layout, blocks, j){
    f <- blocks[!is.na(fp[blocks, j])]
    vapply(f, function(b) layout[[b]][fp[b, j]], integer(1))
  }

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
    list(A = A, B = B, C = C, D = D)
  }

  ## Profile information at K = 1, per item over the parameters it has free:
  ## S_j = sum over j's training responses of V V' w, V = dp/d(free raw
  ## parameter), w the profile weight after removing the person. The row
  ## ability carries the person-covariate term.
  ref_profile <- function(sdat, st, theta, Sig, blocks){
    ip <- ref_item_pars(sdat, st)
    fp <- ref_free_pos(sdat)
    keep <- which(as.integer(sdat$trainingLogical) == 1L)
    item <- as.integer(sdat$item)[keep]; id <- as.integer(sdat$id)[keep]
    th <- as.numeric(theta)[id]
    if(isTRUE(sdat$NpersonPreds > 0))
      th <- th + as.numeric(as.matrix(sdat$personPreds)[keep, , drop = FALSE] %*% t(st$Abilitybeta))
    A <- ip$A[item]; cc <- ip$C[item]; dd <- ip$D[item]
    g <- stats::plogis(A * th - ip$B[item])
    p <- cc + (dd - cc) * g
    s <- (dd - cc) * g * (1 - g); r <- p * (1 - p)
    w <- pmax(1 / r - s^2 * A^2 * as.numeric(Sig)[id] / r^2, 0)
    V <- cbind(B = -s, A = s * th * (1 - exp(-A)),
               C = (1 - g) * cc * (1 - 2 * cc), D = g * (2 * dd - 1) * (1 - dd))
    lapply(seq_len(sdat$Nitems), function(j){
      f <- blocks[!is.na(fp[blocks, j])]
      k <- which(item == j)
      crossprod(V[k, f, drop = FALSE], V[k, f, drop = FALSE] * w[k])
    })
  }

  ref_prior <- function(sdat, f) 1 / vapply(f, function(b) as.numeric(sdat[[prior_sd[[b]]]])[1], numeric(1))^2

  fixture <- function(dat, pl, itemDat = NA, ...){
    f <- quiet(fitIRT(dat, pl = pl, cores = 1, marginalApprox = "laplace",
      itemDat = itemDat, ebayes = FALSE, keepInternals = TRUE, ...))
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    list(f = f, d = d, fp = ref_free_pos(d$sdat))
  }

  ## itemInformation(method = "profile") against the per-item reference:
  ## information and covariance on each item's free sub-block, NA on every
  ## (item, parameter) it has fixed.
  check_profile <- function(fx, blocks){
    ii <- itemInformation(fx$f, method = "profile")
    expect_equal(ii$active, blocks)
    post <- fx$d$eval$posterior
    ref <- ref_profile(fx$d$sdat, fx$d$state, post$theta_mode, post$covariance, blocks)
    err_info <- err_cov <- 0; na_ok <- TRUE
    for(j in seq_len(fx$d$sdat$Nitems)){
      f <- !is.na(fx$fp[blocks, j])
      Vj <- solve(ref[[j]] + diag(ref_prior(fx$d$sdat, blocks[f]), sum(f)))
      err_info <- max(err_info, abs(as.numeric(ii$information[f, f, j]) - as.numeric(ref[[j]])) /
                        max(abs(ref[[j]])))
      err_cov <- max(err_cov, abs(as.numeric(ii$covariance[f, f, j]) - as.numeric(Vj)) / max(abs(Vj)),
                     abs(ii$se[j, f] - sqrt(diag(Vj))) / sqrt(diag(Vj)))
      na_ok <- na_ok && all(is.na(ii$se[j, !f])) && all(is.finite(ii$se[j, f])) &&
        all(is.na(ii$information[!f, , j])) && all(is.na(ii$covariance[, !f, j]))
    }
    expect_lt(err_info, 1e-8)
    expect_lt(err_cov, 1e-7)
    expect_true(na_ok)
    ii
  }

  ## Central differences of the analytic gradient at `par`, with every person
  ## mode re-solved, over coordinates `cols`; rows are all coordinates.
  diff_cols <- function(fit, d, cols, tol = 1e-10, max_iter = 500L, eps = 1e-4){
    gf <- function(p) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = d$state,
      sdat = d$sdat, prior_precision = fit$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, keep_covariance = TRUE,
      context = d$context, tol = tol, max_iter = max_iter)$approx_grad
    vapply(cols, function(k){
      up <- d$par; up[k] <- up[k] + eps; dn <- d$par; dn[k] <- dn[k] - eps
      (gf(up) - gf(dn)) / (2 * eps)
    }, numeric(length(d$par)))
  }

  ## The whole differenced Hessian, symmetrised, and its natural-scale
  ## inverse, as the dense certificate route computes it.
  dense_hessian <- function(fx){
    H <- diff_cols(fx$f, fx$d, seq_along(fx$d$par))
    (H + t(H)) / 2
  }
  natural_cov <- function(fx, H){
    ps <- attr(fx$d$layout, "par_scale"); if(length(ps) != nrow(H)) ps <- rep(1, nrow(H))
    solve(-H) * outer(ps, ps)
  }
  ## The block-Schur route keeps each item's own block and its coupling to the
  ## shared coordinates, and drops the coupling between different items: the
  ## same Hessian with those entries zeroed, items found through the reference.
  between_items_dropped <- function(fx, H, blocks){
    owner <- rep(NA_integer_, nrow(H))
    for(j in seq_len(fx$d$sdat$Nitems)) owner[ref_coords(fx$fp, fx$d$layout, blocks, j)] <- j
    same <- outer(owner, owner, "==")
    same[is.na(same)] <- TRUE
    H * same
  }

  ## The exact covariance an itemInformation() result carries, against a
  ## coordinate-level covariance mapped to items through the reference.
  check_exact <- function(ii, Vfull, fx, blocks, tolerance){
    expect_equal(ii$active, blocks)
    err <- 0; na_ok <- TRUE
    for(j in seq_len(fx$d$sdat$Nitems)){
      f <- !is.na(fx$fp[blocks, j])
      cj <- ref_coords(fx$fp, fx$d$layout, blocks, j)
      R <- Vfull[cj, cj, drop = FALSE]
      err <- max(err, abs(as.numeric(ii$covariance[f, f, j]) - as.numeric(R)) / max(abs(R)))
      na_ok <- na_ok && all(is.na(ii$se[j, !f])) && all(is.finite(ii$se[j, f])) &&
        all(is.na(ii$covariance[!f, , j]))
    }
    expect_lt(err, tolerance)
    expect_true(na_ok)
  }

  ## 2PL, 30 items: the reported reproduction anchors the difficulty of items
  ## 1-4 at its generating value; the second anchors difficulty on 1-3 and the
  ## loading on 4-6.
  set.seed(101)
  sim <- simIRT(Nsubs = 1000, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1,
                BSD = 1, logitCMean = -20, logitCSD = 0)
  tp <- sim$itemPars
  ancB <- data.frame(Item = tp$Item[1:4], A = NA_real_, B = tp$B[1:4], C = NA_real_)
  ancBA <- data.frame(Item = tp$Item[1:6], A = NA_real_, B = NA_real_, C = NA_real_)
  ancBA$B[1:3] <- tp$B[1:3]; ancBA$A[4:6] <- tp$A[4:6]

  fx0 <- fixture(sim$dat, pl = 2)
  fxB <- fixture(sim$dat, pl = 2, itemDat = ancB)
  fxBA <- fixture(sim$dat, pl = 2, itemDat = ancBA)

  test_that("the references reproduce the package when nothing is fixed", {
    expect_identical(fx0$fp[c("B", "A"), ], rbind(B = 1:30, A = 1:30))
    expect_true(all(is.na(fx0$fp[c("C", "D"), ])))
    check_profile(fx0, c("B", "A"))
    ## The package's own map is the reference's.
    expect_identical(bigIRT:::bigIRT_item_free_pos(fx0$d$sdat), fx0$fp[c("B", "A"), ])
    expect_identical(bigIRT:::bigIRT_item_free_pos(fxBA$d$sdat), fxBA$fp[c("B", "A"), ])
  })

  test_that("anchored difficulties: every free difficulty keeps its profile error", {
    expect_equal(fxB$d$sdat$NfixedB, 4L)
    expect_true(all(is.na(fxB$fp["B", 1:4])))
    expect_identical(fxB$fp["B", 5:30], 1:26)
    ii <- check_profile(fxB, c("B", "A"))
    expect_true(all(is.na(ii$se[1:4, "B"])))
    expect_true(all(ii$se[5:30, "B"] > 0))
    expect_true(all(ii$se[, "A"] > 0))
    ## The anchored items' loading errors come from a 1 x 1 block: the loading
    ## alone, with nothing to trade off against.
    expect_equal(ii$se[1:4, "A"], sqrt(1 / (ii$information["A", "A", 1:4] +
      ref_prior(fxB$d$sdat, "A"))), tolerance = 1e-12)
    expect_equal(sum(ii$free), 56L)
    expect_output(print(ii), "of 56")
  })

  test_that("difficulties anchored on some items, loadings on others: profile and differenced", {
    expect_no_error(ii <- check_profile(fxBA, c("B", "A")))
    expect_true(all(is.na(ii$se[1:3, "B"])))
    expect_true(all(is.na(ii$se[4:6, "A"])))
    expect_true(all(is.finite(ii$se[-(1:3), "B"])))
    expect_true(all(is.finite(ii$se[-(4:6), "A"])))
    ## The differenced method, against the same differences taken here with
    ## the reference's map: an anchored-difficulty item, an anchored-loading
    ## item, and a free one.
    ih <- itemInformation(fxBA$f, method = "hessian")
    expect_equal(ih$active, c("B", "A"))
    for(j in c(2L, 5L, 10L)){
      cj <- ref_coords(fxBA$fp, fxBA$d$layout, c("B", "A"), j)
      f <- c("B", "A") %in% names(cj)
      cols <- diff_cols(fxBA$f, fxBA$d, cj, tol = 1e-8, max_iter = 200L)
      Hj <- -cols[cj, , drop = FALSE]; Hj <- (Hj + t(Hj)) / 2
      expect_equal(as.numeric(ih$information[f, f, j]), as.numeric(Hj), tolerance = 1e-8)
      expect_equal(as.numeric(ih$covariance[f, f, j]), as.numeric(solve(Hj)), tolerance = 1e-8)
      expect_true(all(is.na(ih$se[j, !f])))
    }
    expect_true(all(is.na(ih$se[1:3, "B"])) && all(is.na(ih$se[4:6, "A"])))
    expect_true(all(is.finite(ih$se[-(1:3), "B"])) && all(is.finite(ih$se[-(4:6), "A"])))
  })

  test_that("tidyIRT reports every free difficulty's error under both methods", {
    for(m in c("profile", "exact")){
      td <- quiet(tidyIRT(fxB$f, conf.int = TRUE, method = m))
      b <- td[td$term == "B", ]
      anchored <- b$item %in% as.character(ancB$Item)
      expect_equal(sum(anchored), 4L)
      expect_true(all(is.na(b$std.error[anchored])))
      expect_true(all(is.finite(b$std.error[!anchored])))
      expect_true(all(is.finite(td$std.error[td$term == "A"])))
      ii <- itemInformation(fxB$f, method = m)
      expect_equal(b$std.error, unname(ii$se[match(b$item, as.character(fxB$f$itemPars$Item)), "B"]))
    }
  })

  test_that("exact covariance: the dense and block-Schur routes map each item to its free coordinates", {
    H <- dense_hessian(fxB)
    fe <- quiet(fitIRT(sim$dat, pl = 2, cores = 1, marginalApprox = "laplace", itemDat = ancB,
      ebayes = FALSE, keepInternals = TRUE, laplaceCertify = "exact"))
    expect_identical(fe$optim$par, fxB$f$optim$par)
    ce <- fe$laplaceStatus$certificate
    expect_equal(ce$route, "dense")
    expect_length(ce$saturated, 0L)
    expect_false(is.null(ce$covariance$items))
    ## Dense: each item's block of the full inverse.
    check_exact(itemInformation(fe, method = "exact"), natural_cov(fxB, H), fxB, c("B", "A"),
                tolerance = 1e-6)
    ## Shared coordinates are what is left once every item's free parameters
    ## are taken out: the free difficulties are no longer among them.
    lay <- fxB$d$layout
    expect_setequal(rownames(ce$covariance$shared),
      bigIRT:::bigIRT_coordinate_names(lay)[c(lay$B_mean, lay$A_mean)])
    fs <- withr::with_options(list(bigIRT.certify_dense_max = 10),
      quiet(fitIRT(sim$dat, pl = 2, cores = 1, marginalApprox = "laplace", itemDat = ancB,
        ebayes = FALSE, keepInternals = TRUE, laplaceCertify = "exact")))
    expect_identical(fs$optim$par, fxB$f$optim$par)
    expect_equal(fs$laplaceStatus$certificate$route, "blockschur")
    ## Block-Schur: the inverse without the coupling between items, which is
    ## what that route computes; grouped by the wrong items it would not be.
    check_exact(itemInformation(fs, method = "exact"),
                natural_cov(fxB, between_items_dropped(fxB, H, c("B", "A"))), fxB, c("B", "A"),
                tolerance = 1e-6)
  })

  test_that("exact covariance: the default fast route maps each item to its free coordinates", {
    ## The fast route's own Hessian, assembled whole and inverted here: its
    ## item blocks from the quadrature pass, its shared columns differenced.
    f <- fxB$f; d <- fxB$d; st <- f$internals$state
    expect_equal(f$laplaceStatus$certificate$route, "fast")
    run <- function(p, ctx) bigIRT:::bigIRT_laplace_direct_objective(par = p, state = st,
      sdat = d$sdat, prior_precision = f$internals$priorPrecision,
      theta_init = d$eval$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L,
      max_iter = 500L, tol = 1e-10, keep_covariance = TRUE, context = ctx)
    n <- length(d$par)
    fh <- bigIRT:::bigIRT_certify_fast_hessian(par = d$par, keep = seq_len(n), context = d$context,
      final = d$eval, run = run, ctx_h = d$context, state = st, sdat = d$sdat, layout = d$layout)
    H <- matrix(0, n, n)
    for(k in seq_along(fh$blocks)){
      ix <- fh$blocks[[k]]
      H[ix, ix] <- (fh$Hblocks[[k]] + t(fh$Hblocks[[k]])) / 2
      H[ix, fh$shared] <- fh$Hcross[[k]]; H[fh$shared, ix] <- t(fh$Hcross[[k]])
    }
    H[fh$shared, fh$shared] <- fh$Hshared
    ps <- attr(d$layout, "par_scale")
    Vfast <- solve(-H) * outer(ps, ps)
    ## Recomputed from the same evaluation, it is that inverse to rounding;
    ## the one the fit stored was taken at the fit's own final evaluation.
    f2 <- f; f2$laplaceStatus$certificate$covariance <- NULL
    check_exact(itemInformation(f2, method = "exact"), Vfast, fxB, c("B", "A"), tolerance = 1e-8)
    check_exact(itemInformation(f, method = "exact"), Vfast, fxB, c("B", "A"), tolerance = 1e-4)
  })

  test_that("the Newton polish steps every free item parameter, by its item's free block", {
    d <- fxB$d; lay <- d$layout
    stp <- bigIRT:::bigIRT_laplace_newton_step(d$eval, d$sdat, d$context, lay)
    expect_false(is.null(stp))
    expect_true(all(stp[c(lay$B, lay$A)] != 0))
    post <- d$eval$posterior
    ref <- ref_profile(d$sdat, d$state, post$theta_mode, post$covariance, c("B", "A"))
    ps <- attr(lay, "par_scale")
    err <- 0
    for(j in seq_len(d$sdat$Nitems)){
      cj <- ref_coords(fxB$fp, lay, c("B", "A"), j)
      Hj <- (ref[[j]] + diag(ref_prior(d$sdat, names(cj)), length(cj))) * outer(ps[cj], ps[cj])
      sj <- solve(Hj, d$eval$approx_grad[cj])
      err <- max(err, abs(stp[cj] - sj) / max(abs(sj)))
    }
    expect_lt(err, 1e-6)
  })

  test_that("covariate errors: the marginal correction covers every free item parameter", {
    ## Uncentred covariates, a 0/1 indicator and one with mean one. A
    ## difficulty's coupling to a coefficient sums w x over its responses,
    ## which is near zero for a centred covariate, so with rnorm() covariates
    ## leaving the difficulties out of the correction moved nothing and this
    ## test passed against the code that did.
    set.seed(101)
    X <- cbind(rbinom(1000, 1, .4), rnorm(1000, 1))
    sx <- simIRT(Nsubs = 1000, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1, BSD = 1,
                 logitCMean = -20, logitCSD = 0, personPreds = X,
                 AbilityPredEffects = matrix(c(.6, -.4), 1))
    anc <- data.frame(Item = sx$itemPars$Item[1:4], A = NA_real_, B = sx$itemPars$B[1:4], C = NA_real_)
    fx <- fixture(sx$dat, pl = 2, itemDat = anc, personPreds = c("V1", "V2"))
    vm <- bigIRT:::bigIRT_covariate_vcov(fx$f, marginal = TRUE)
    ## V_bb = (H_bb - sum_j H_bj V_j H_jb)^-1 over each item's free block, V_j
    ## the profile covariance from the reference.
    d <- fx$d; idx <- d$layout$ability_beta
    cols <- diff_cols(fx$f, d, idx, tol = 1e-8, max_iter = 200L)
    Hbb <- -cols[idx, , drop = FALSE]
    prec <- (Hbb + t(Hbb)) / 2
    post <- d$eval$posterior
    ref <- ref_profile(d$sdat, d$state, post$theta_mode, post$covariance, c("B", "A"))
    for(j in seq_len(d$sdat$Nitems)){
      cj <- ref_coords(fx$fp, d$layout, c("B", "A"), j)
      Vj <- solve(ref[[j]] + diag(ref_prior(d$sdat, names(cj)), length(cj)))
      Hpb <- -cols[cj, , drop = FALSE]
      prec <- prec - t(Hpb) %*% Vj %*% Hpb
    }
    bs <- attr(d$layout, "beta_scale")
    expect_equal(unname(vm), unname(solve(prec) * outer(bs, bs)), tolerance = 1e-6)
  })

  test_that("checkConvergence and the effective degrees of freedom count the free difficulties", {
    f <- fxB$f
    ii <- itemInformation(f)
    expect_equal(ii$method, "exact")
    expect_equal(ii$active, c("B", "A"))
    g <- f$optim$grad; lay <- f$optim$layout
    nd <- 0; wz <- 0; wlab <- NA_character_
    for(j in seq_len(fxB$d$sdat$Nitems)){
      cj <- ref_coords(fxB$fp, lay, c("B", "A"), j)
      gj <- g[cj]
      Vj <- matrix(ii$covariance[names(cj), names(cj), j], length(cj))
      nd <- nd + sum(gj * (Vj %*% gj))
      z <- abs(gj) * sqrt(diag(Vj))
      if(max(z) > wz){ wz <- max(z); wlab <- sprintf("%s[%d]", names(cj)[which.max(z)], j) }
    }
    cc <- checkConvergence(f)
    expect_equal(cc$newton_decrement, nd, tolerance = 1e-10)
    expect_equal(cc$worst_parameter_se_units, wz, tolerance = 1e-10)
    expect_equal(cc$worst_parameter, wlab)
    ## Penalised, so the item blocks count their effective parameters and the
    ## rest at face value; the free item parameters are 26 + 30.
    ll <- logLik(f)
    nfree <- sum(!is.na(fxB$fp[c("B", "A"), ]))
    expect_equal(nfree, 56L)
    expect_true(all(is.finite(ii$edf[5:30, "B"])))
    expect_equal(attr(ll, "df"), sum(ii$edf, na.rm = TRUE) + attr(ll, "nominal_df") - nfree)
  })

  test_that("4PL with each parameter type fixed on some items", {
    set.seed(202)
    s4 <- simIRT(Nsubs = 1500, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1.3,
                 BSD = 1, logitCMean = -1.5, logitCSD = .5, logitDMean = 2, logitDSD = .5)
    t4 <- s4$itemPars
    anc <- data.frame(Item = t4$Item, A = NA_real_, B = NA_real_, C = NA_real_, D = NA_real_)
    anc$B[1:3] <- t4$B[1:3]; anc$A[4:6] <- t4$A[4:6]
    anc$C[7:10] <- t4$C[7:10]; anc$D[11:14] <- t4$D[11:14]
    fx <- fixture(s4$dat, pl = 4, itemDat = anc[1:14, ], laplaceRefine = "none")
    expect_equal(c(fx$d$sdat$NfixedB, fx$d$sdat$NfixedA, fx$d$sdat$NfixedC, fx$d$sdat$NfixedD),
                 c(3L, 3L, 4L, 4L))
    ii <- check_profile(fx, c("B", "A", "C", "D"))
    fixed <- cbind(B = 1:30 %in% 1:3, A = 1:30 %in% 4:6, C = 1:30 %in% 7:10, D = 1:30 %in% 11:14)
    expect_identical(is.na(ii$se), fixed)
  })

  test_that("two dimensions, simple structure: the loading map follows the free cells", {
    set.seed(77)
    sk <- simIRT(Nsubs = 700, Nitems = 8, Nscales = 2, AMean = 1.2, ASD = .3, BMean = 0,
                 BSD = 1, AbilityCorr = matrix(c(1, .5, .5, 1), 2), logitCMean = -20, logitCSD = 0)
    tk <- sk$itemPars
    anc <- data.frame(Item = tk$Item[1:3], A = c(NA, NA, 1), B = c(tk$B[1:2], NA), C = NA_real_)
    fx <- fixture(sk$dat, pl = 2, itemDat = anc)
    ni <- as.integer(fx$d$sdat$Nitems)
    expect_equal(fx$d$sdat$NfixedB, 2L)
    ## One free loading per item, except the item whose loading is anchored.
    expect_true(all(is.na(fx$fp["A", 3])) && all(!is.na(fx$fp["A", -3])))
    ip <- itemInformation(fx$f, method = "profile")
    expect_equal(ip$active, c("B", "A"))
    expect_identical(is.na(ip$se), cbind(B = 1:ni %in% 1:2, A = 1:ni == 3L))
    ih <- itemInformation(fx$f, method = "hessian")
    expect_identical(is.na(ih$se), cbind(B = 1:ni %in% 1:2, A = 1:ni == 3L))
    ## Items on either dimension, one with each kind of anchor.
    for(j in c(1L, 3L, 5L, ni)){
      cj <- ref_coords(fx$fp, fx$d$layout, c("B", "A"), j)
      f <- c("B", "A") %in% names(cj)
      cols <- diff_cols(fx$f, fx$d, cj, tol = 1e-8, max_iter = 200L)
      Hj <- -cols[cj, , drop = FALSE]; Hj <- (Hj + t(Hj)) / 2
      expect_equal(as.numeric(ih$information[f, f, j]), as.numeric(Hj), tolerance = 1e-8)
    }
    expect_identical(bigIRT:::bigIRT_item_free_pos(fx$d$sdat), fx$fp[c("B", "A"), ])
  })
}
