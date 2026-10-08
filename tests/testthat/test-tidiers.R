## Tidiers and the report.
##
## The one thing here that is easy to get quietly wrong is the scale of the
## standard errors. itemInformation() works on the free parameters -- the
## softplus pre-image of discrimination, the logits of the asymptotes -- while
## these functions report the parameters themselves, so the errors have to be
## carried across by the delta method. Reporting them unchanged would put an
## interval on A that is roughly 40 per cent too wide, and nothing about the
## output would look wrong.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  fit_t <- function(pl = 2L, ni = 20L, ns = 900L, seed = 4){
    set.seed(seed)
    s <- simIRT(Nsubs = ns, Nitems = ni, Nscales = 1, NitemsAnswered = min(15L, ni),
      AMean = 1, ASD = .3, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = if(pl >= 3) -1.4 else -20, logitCSD = if(pl >= 3) .2 else 0,
      logitDMean = 20, logitDSD = 0)
    n <- nrow(s$dat)
    set.seed(seed + 1); tr <- sort(sample.int(n, floor(0.85 * n)))
    fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = pl, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      trainingRows = tr, keepInternals = TRUE, laplaceKeepCovariance = TRUE)
  }

  test_that("tidy returns one row per item parameter with usable intervals", {
    f <- fit_t()
    ti <- tidyIRT(f, conf.int = TRUE)
    expect_equal(nrow(ti), as.integer(f$prepared$Nitems) * length(unique(ti$term)))
    expect_true(all(is.finite(ti$estimate)))
    expect_true(all(is.finite(ti$std.error)))
    expect_true(all(ti$conf.low < ti$estimate & ti$estimate < ti$conf.high))
    ## Discrimination is positive and its interval must not cross zero.
    a <- ti[ti$term == "A", ]
    expect_true(all(a$conf.low >= 0))
  })

  test_that("standard errors are carried onto the reported parameter scale", {
    f <- fit_t()
    ## The delta method is under test, and it holds for whichever standard
    ## error it is handed, so both sides name the same method; which one
    ## tidyIRT defaults to is test-certify.R's question.
    ti <- tidyIRT(f, conf.int = TRUE, method = "profile")
    ii <- itemInformation(f, method = "profile")
    a <- ti[order(ti$item), ]
    a <- a[a$term == "A", ]
    a <- a[order(match(a$item, as.character(as.data.frame(f$itemPars)$Item))), ]
    raw <- ii$se[, "A"]
    ## softplus'(z) = 1 - exp(-A), which is strictly below 1, so the reported
    ## error must be strictly smaller than the free-parameter one.
    expect_true(all(a$std.error < raw))
    expect_equal(a$std.error, raw * (1 - exp(-as.numeric(as.data.frame(f$itemPars)$A))),
      tolerance = 1e-8)
  })

  test_that("asymptote errors are transformed and intervals stay in the unit interval", {
    f <- fit_t(pl = 3L)
    ti <- tidyIRT(f, conf.int = TRUE, method = "profile")
    cc <- ti[ti$term == "C", ]
    expect_true(all(cc$conf.low >= 0 & cc$conf.high <= 1))
    ii <- itemInformation(f, method = "profile")
    if("C" %in% ii$active){
      est <- as.numeric(as.data.frame(f$itemPars)$C)
      ## C = logistic(z)/2 (R/fitIRT.R's cfunc), so dC/dz = C(1 - 2C), not the
      ## plain-logistic C(1 - C): verified against finite differences of
      ## cfunc() at bigIRT_dc_dx()'s own definition (R/inference.R). The two
      ## formulas differ by (1-C)/(1-2C), about 12% for the fitted C here.
      expect_equal(sort(cc$std.error), sort(ii$se[, "C"] * bigIRT:::bigIRT_dc_dx(est)),
        tolerance = 1e-8)
    }
  })

  test_that("bigIRT_dc_dx/bigIRT_dd_dx match finite differences of the actual transform", {
    ## The chain-rule factor is a fact about cfunc/dfunc (R/fitIRT.R), not
    ## about any fit, so check it directly against central finite differences
    ## first -- independent of sampling noise, optimiser behaviour or which
    ## parameters happen to be free in a given model.
    cfunc <- bigIRT:::cfunc; dfunc <- bigIRT:::dfunc
    z <- c(-3, -1.4, -0.2, 0.5, 2.1)
    h <- 1e-6
    fd_c <- (cfunc(z + h) - cfunc(z - h)) / (2 * h)
    fd_d <- (dfunc(z + h) - dfunc(z - h)) / (2 * h)
    expect_equal(bigIRT:::bigIRT_dc_dx(cfunc(z)), fd_c, tolerance = 1e-6)
    expect_equal(bigIRT:::bigIRT_dd_dx(dfunc(z)), fd_d, tolerance = 1e-6)
    ## The plain-logistic factors a bug once used are NOT close, so this is
    ## not a tolerance wide enough to pass by accident.
    expect_gt(max(abs(cfunc(z) * (1 - cfunc(z)) - fd_c)), 1e-3)
    expect_gt(max(abs(dfunc(z) * (1 - dfunc(z)) - fd_d)), 1e-3)
  })

  test_that("tidyIRT's C and D errors agree with itemInformation on a 4PL, both free", {
    ## pl = 4 with both asymptotes at non-degenerate values (away from the 0/1
    ## boundary, where either factor would read near zero and the two could
    ## look equal by coincidence rather than by formula).
    set.seed(41)
    s <- simIRT(Nsubs = 1400L, Nitems = 24L, Nscales = 1, NitemsAnswered = 16L,
      AMean = 1.1, ASD = .25, BMean = 0, BSD = 1, AbilityMean = 0, AbilitySD = 1,
      logitCMean = -1.6, logitCSD = .2, logitDMean = 0.6, logitDSD = .15)
    fit4 <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
      pl = 4L, marginalApprox = "laplace", cores = 1L, verbose = 0L, plot = FALSE,
      priors = TRUE, ebayes = FALSE, normalise = FALSE, dropPerfectScores = FALSE,
      keepInternals = TRUE, laplaceKeepCovariance = TRUE)
    ti4 <- tidyIRT(fit4, conf.int = TRUE, method = "profile")
    ii4 <- itemInformation(fit4, method = "profile")
    ip4 <- as.data.frame(fit4$itemPars)
    for(b in intersect(c("C", "D"), ii4$active)){
      est <- as.numeric(ip4[[b]])
      ## Both C and D estimates land away from 0/1 here, so the plain-logistic
      ## and correct factors disagree by a wide margin (checked below), not a
      ## rounding-sized one.
      expect_true(all(est > 0.05 & est < 0.95), label = b)
      tb <- ti4[ti4$term == b, ]
      tb <- tb[order(match(tb$item, ip4$Item)), ]
      factor_fn <- if(b == "C") bigIRT:::bigIRT_dc_dx else bigIRT:::bigIRT_dd_dx
      expect_equal(tb$std.error, ii4$se[, b] * factor_fn(est), tolerance = 1e-8, label = b)
      plain <- ii4$se[, b] * est * (1 - est)
      expect_gt(max(abs(tb$std.error - plain)) / mean(tb$std.error), 0.05, label = b)
    }
  })

  test_that("glance and augment describe the model and the responses", {
    f <- fit_t()
    g <- glanceIRT(f)
    expect_equal(nrow(g), 1L)
    expect_true(is.finite(g$logLik) && is.finite(g$AIC))
    expect_equal(g$nobs_persons, as.integer(f$prepared$Nsubs))
    a <- augmentIRT(f)
    expect_equal(nrow(a), length(f$pars$pcorrect))
    ok <- is.finite(a$.fitted) & is.finite(a$.observed)
    expect_equal(a$.resid[ok], (a$.observed - a$.fitted)[ok], tolerance = 1e-12)
    ## Both training and held-out rows must be present and labelled.
    expect_true(any(a$.training, na.rm = TRUE))
    expect_true(any(!a$.training, na.rm = TRUE))
  })

  test_that("broom's generics reach these methods when broom is installed", {
    skip_if_not_installed("broom")
    f <- fit_t()
    expect_equal(nrow(broom::tidy(f)), nrow(tidyIRT(f)))
    expect_equal(nrow(broom::glance(f)), 1L)
  })

  test_that("the report renders to a self-contained file", {
    skip_if_not_installed("rmarkdown")
    skip_if_not(rmarkdown::pandoc_available() ||
      length(Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/pandoc*")),
      "pandoc not available")
    f <- fit_t(ni = 12L, ns = 500L)
    out <- file.path(tempdir(), "bigirt-test-report.html")
    on.exit(unlink(out), add = TRUE)
    expect_message(reportIRT(f, file = out), "wrote")
    expect_true(file.exists(out))
    expect_gt(file.size(out), 10000)
  })

  ## Every test above fits with keepInternals = TRUE, so none of them saw what a
  ## default fit gets. The certificate keeps the exact covariance on a default
  ## fit, and itemInformation() reads it without the prepared internals; tidyIRT
  ## used to refuse to ask, warn that the fit carried no prepared data, and
  ## return a column of NA standard errors.
  test_that("a default fit (no keepInternals) still gets standard errors", {
    set.seed(4)
    s <- simIRT(Nsubs = 600L, Nitems = 20L, Nscales = 1, NitemsAnswered = 15L,
      AMean = 1, ASD = .3, BMean = 0, BSD = 1,
      logitCMean = -1.4, logitCSD = .2, logitDMean = 20, logitDSD = 0)
    args <- list(s$dat, pl = 3L, marginalApprox = "laplace", cores = 1L, verbose = 0L,
      plot = FALSE, ebayes = FALSE, dropPerfectScores = FALSE)
    f0 <- do.call(fitIRT, args)
    f1 <- do.call(fitIRT, c(args, keepInternals = TRUE))
    expect_null(f0$internals)
    expect_false(is.null(f0$laplaceStatus$certificate$covariance$items))
    t0 <- expect_no_warning(tidyIRT(f0, conf.int = TRUE))
    t1 <- tidyIRT(f1, conf.int = TRUE)
    expect_true(all(is.finite(t0$std.error)))
    expect_equal(t0$std.error, t1$std.error, tolerance = 1e-8)
  })
}
