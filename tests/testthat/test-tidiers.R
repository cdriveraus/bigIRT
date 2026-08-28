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
    expect_equal(nrow(ti), as.integer(f$dat$Nitems) * length(unique(ti$term)))
    expect_true(all(is.finite(ti$estimate)))
    expect_true(all(is.finite(ti$std.error)))
    expect_true(all(ti$conf.low < ti$estimate & ti$estimate < ti$conf.high))
    ## Discrimination is positive and its interval must not cross zero.
    a <- ti[ti$term == "A", ]
    expect_true(all(a$conf.low >= 0))
  })

  test_that("standard errors are carried onto the reported parameter scale", {
    f <- fit_t()
    ti <- tidyIRT(f, conf.int = TRUE)
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
    ti <- tidyIRT(f, conf.int = TRUE)
    cc <- ti[ti$term == "C", ]
    expect_true(all(cc$conf.low >= 0 & cc$conf.high <= 1))
    ii <- itemInformation(f, method = "profile")
    if("C" %in% ii$active){
      est <- as.numeric(as.data.frame(f$itemPars)$C)
      expect_equal(sort(cc$std.error), sort(ii$se[, "C"] * est * (1 - est)),
        tolerance = 1e-8)
    }
  })

  test_that("glance and augment describe the model and the responses", {
    f <- fit_t()
    g <- glanceIRT(f)
    expect_equal(nrow(g), 1L)
    expect_true(is.finite(g$logLik) && is.finite(g$AIC))
    expect_equal(g$nobs_persons, as.integer(f$dat$Nsubs))
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
}
