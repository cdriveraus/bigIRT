## Closed-form item information when some item parameters are fixed (anchor
## items through itemDat).
##
## bigIRT_item_info_blocks gathers each response's contribution into its item's
## block, and used to find the item through B_ref -- the free-DIFFICULTY index,
## which is 0 on every row of an item whose difficulty is fixed. A block is
## built only when every item has that parameter free, so anchoring difficulties
## while the loadings stay free left the A block active and grouped by an index
## that hit 0, and the accumulator stopped with "Item index out of range.".
## Nothing showed while every difficulty was free, because B_ref is then the
## item.
##
## Most callers caught the error and carried on without saying so, which is
## the worse half: on an anchored fit the Newton polish took no steps, the
## covariate standard errors silently dropped their marginal correction and
## came back conditional, and summary() and checkConvergence() lost their
## standard errors and Newton decrement.
##
## The reference is built per ITEM from sdat's fixed flags and the free-parameter
## vectors, never through B_ref or A_ref, and is checked first against the
## package on a fit with nothing fixed, where the two indexings coincide.
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
    list(A = A, B = B, C = C, D = D,
         fixed = list(B = sdat$fixedB, A = sdat$fixedAlog,
                      C = sdat$fixedClogit, D = sdat$fixedDlogit))
  }

  ## Per item j, S_j = sum over j's responses of V V' w, with V = dp/d(free raw
  ## parameter) and w the profile weight after removing the person. A block is
  ## present when any item has that parameter free; an item's block covers the
  ## parameters it has free, and is NA where it has one fixed. (Blocks used to
  ## be built only for a parameter free on every item, which dropped every
  ## free difficulty's error once one was anchored; see
  ## test-iteminfo-partial-blocks.R.)
  ref_info_blocks <- function(sdat, st, theta, Sig){
    ip <- ref_item_pars(sdat, st)
    keep <- which(as.integer(sdat$trainingLogical) == 1L)
    item <- as.integer(sdat$item)[keep]; id <- as.integer(sdat$id)[keep]
    th <- as.numeric(theta)[id]
    A <- ip$A[item]; cc <- ip$C[item]; dd <- ip$D[item]
    g <- stats::plogis(A * th - ip$B[item])
    p <- cc + (dd - cc) * g
    s <- (dd - cc) * g * (1 - g); r <- p * (1 - p)
    w <- pmax(1 / r - s^2 * A^2 * as.numeric(Sig)[id] / r^2, 0)
    V <- list(B = -s, A = s * th * (1 - exp(-A)),
              C = (1 - g) * cc * (1 - 2 * cc), D = g * (2 * dd - 1) * (1 - dd))
    active <- names(V)[vapply(names(V), function(b) any(ip$fixed[[b]] == 0L), logical(1))]
    M <- matrix(unlist(V[active]), ncol = length(active), dimnames = list(NULL, active))
    ni <- as.integer(sdat$Nitems)
    S <- array(NA_real_, c(length(active), length(active), ni), dimnames = list(active, active, NULL))
    for(j in seq_len(ni)){
      f <- active[vapply(active, function(b) ip$fixed[[b]][j] == 0L, logical(1))]
      k <- which(item == j)
      S[f, f, j] <- crossprod(M[k, f, drop = FALSE], M[k, f, drop = FALSE] * w[k])
    }
    list(info = S, active = active)
  }

  info_fixture <- function(dat, pl, itemDat = NA, ...){
    f <- suppressWarnings(fitIRT(dat, pl = pl, cores = 1, marginalApprox = "laplace",
      itemDat = itemDat, ebayes = FALSE, keepInternals = TRUE, ...))
    d <- bigIRT:::bigIRT_direct_eval_from_fit(f, keep_covariance = TRUE)
    ref <- ref_info_blocks(d$sdat, d$state, d$eval$posterior$theta_mode,
                           d$eval$posterior$covariance)
    list(f = f, d = d, ref = ref)
  }

  ## 2PL, 30 items, the reported reproduction: difficulty anchored on items 1-4
  ## at its generating value, every loading free.
  set.seed(101)
  sim <- simIRT(Nsubs = 1000, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1,
                BSD = 1, logitCMean = -20, logitCSD = 0)
  anchorsB <- local({
    tp <- sim$itemPars
    data.frame(Item = tp$Item[1:4], A = NA_real_, B = tp$B[1:4], C = NA_real_)
  })

  test_that("the reference reproduces the package when nothing is fixed", {
    fx <- info_fixture(sim$dat, pl = 2)
    expect_equal(fx$d$sdat$NfixedB, 0L)
    ## The two indexings coincide here, which is why the old one passed.
    expect_identical(as.integer(fx$d$context$B_ref), as.integer(fx$d$context$items))
    ii <- itemInformation(fx$f, method = "profile")
    expect_equal(ii$active, c("B", "A"))
    expect_equal(fx$ref$active, ii$active)
    expect_equal(unname(ii$information), unname(fx$ref$info), tolerance = 1e-8)
  })

  fxB <- info_fixture(sim$dat, pl = 2, itemDat = anchorsB)

  test_that("profile information is gathered by item when difficulties are anchored", {
    expect_equal(fxB$d$sdat$NfixedB, 4L)
    expect_equal(fxB$d$sdat$NfixedA, 0L)
    ## The anchored items' rows carry no free-difficulty index.
    expect_true(all(fxB$d$context$B_ref[fxB$d$context$items %in% 1:4] == 0L))
    expect_no_error(ii <- itemInformation(fxB$f, method = "profile"))
    expect_equal(ii$active, c("B", "A"))
    expect_equal(fxB$ref$active, c("B", "A"))
    expect_equal(unname(ii$information), unname(fxB$ref$info), tolerance = 1e-8)
    ## Every item, anchored ones included, has a free loading and so an error;
    ## the anchored difficulties have none.
    expect_true(all(is.finite(ii$se[, "A"])))
    expect_true(all(ii$se[, "A"] > 0))
    expect_true(all(is.na(ii$se[1:4, "B"])))
    expect_true(all(ii$se[5:30, "B"] > 0))
  })

  test_that("tidyIRT reports loading errors on an anchored fit", {
    warns <- character(0)
    td <- withCallingHandlers(
      tidyIRT(fxB$f, conf.int = TRUE, method = "profile"),
      warning = function(w){ warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
    expect_false(any(grepl("could not be computed", warns)))
    a <- td[td$term == "A", ]
    expect_equal(nrow(a), 30L)
    expect_true(all(is.finite(a$std.error)))
  })

  test_that("the Newton polish step exists on an anchored fit and moves every free parameter", {
    ## The free difficulties used to stay put: the B block was built only when
    ## every difficulty was free (see test-iteminfo-partial-blocks.R).
    d <- fxB$d
    expect_no_error(stp <- bigIRT:::bigIRT_laplace_newton_step(d$eval, d$sdat, d$context, d$layout))
    expect_false(is.null(stp))
    expect_true(all(stp[d$layout$A] != 0))
    expect_true(all(stp[d$layout$B] != 0))
  })

  test_that("3PL, difficulties anchored: the A and C blocks together, off-diagonal included", {
    set.seed(303)
    s3 <- simIRT(Nsubs = 1500, Nitems = 20, Nscales = 1, ASD = .3, AMean = 1.3,
                 BSD = 1, logitCMean = -1.5, logitCSD = .5)
    tp <- s3$itemPars
    anc <- data.frame(Item = tp$Item[1:3], A = NA_real_, B = tp$B[1:3], C = NA_real_)
    fx <- info_fixture(s3$dat, pl = 3, itemDat = anc, laplaceRefine = "none")
    expect_equal(fx$d$sdat$NfixedB, 3L)
    ii <- itemInformation(fx$f, method = "profile")
    expect_equal(ii$active, c("B", "A", "C"))
    expect_equal(fx$ref$active, ii$active)
    expect_true(all(abs(fx$ref$info["A", "C", ]) > 0))
    expect_equal(unname(ii$information), unname(fx$ref$info), tolerance = 1e-8)
  })

  test_that("covariate standard errors keep their marginal correction on an anchored fit", {
    ## The marginal variance subtracts a positive semidefinite term from the
    ## covariate precision, so it can only exceed the conditional one. It used
    ## to come back equal to it, because the correction's itemInformation()
    ## call failed inside a try().
    set.seed(101)
    X <- matrix(rnorm(1000 * 2), ncol = 2)
    sx <- simIRT(Nsubs = 1000, Nitems = 30, Nscales = 1, ASD = .3, AMean = 1, BSD = 1,
                 logitCMean = -20, logitCSD = 0, personPreds = X,
                 AbilityPredEffects = matrix(c(.6, -.4), 1))
    tp <- sx$itemPars
    anc <- data.frame(Item = tp$Item[1:4], A = NA_real_, B = tp$B[1:4], C = NA_real_)
    f <- suppressWarnings(fitIRT(sx$dat, pl = 2, cores = 1, marginalApprox = "laplace",
      itemDat = anc, ebayes = FALSE, keepInternals = TRUE, personPreds = c("V1", "V2")))
    vm <- bigIRT:::bigIRT_covariate_vcov(f, marginal = TRUE)
    vc <- bigIRT:::bigIRT_covariate_vcov(f, marginal = FALSE)
    expect_true(all(diag(vm) > diag(vc) * (1 + 1e-4)))
  })
}
