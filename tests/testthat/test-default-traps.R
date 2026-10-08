## Two defaults that caught a user following the paper's own example.
##
## dropPerfectScores (TRUE by default) decided whom to drop from the means of
## ALL rows, held-out ones included, so a held-out response changed which
## persons and items were fitted. And a Laplace fit with ebayes = FALSE --
## which the paper recommends -- reported no person SDs unless the posterior
## covariance was kept, although every fit keeps the per-person precision.

if(identical(Sys.getenv("NOT_CRAN"), "true")){
  set.seed(5)
  sim <- simIRT(Nsubs = 200, Nitems = 20, Nscales = 1, NitemsAnswered = 10,
    AMean = 1, ASD = .2, BMean = 0, BSD = 1)

  test_that("dropPerfectScores decides from the training rows only", {
    d <- data.table::as.data.table(sim$dat)
    d[, row := .I]
    train <- d[, list(row = row[seq_len(floor(.N / 2))]), by = id]$row
    ## Person 1 correct on every training response, wrong on one held-out one.
    p1 <- d[id == 1, row]; ho1 <- setdiff(p1, train)
    d[p1, score := 1]; d[ho1[1], score := 0]
    fit_ids <- function(dd) {
      f <- suppressWarnings(fitIRT(dd, pl = 2, cores = 1, marginalApprox = "laplace",
        ebayes = FALSE, trainingRows = train, verbose = 0))
      sort(as.character(f$personPars$id))
    }
    ids_a <- fit_ids(d)
    d2 <- data.table::copy(d); d2[ho1, score := 1]
    ids_b <- fit_ids(d2)
    ## Perfect on training, so dropped whatever the held-out rows say; and
    ## changing only held-out responses changes nothing about who is fitted.
    expect_false("1" %in% ids_a)
    expect_identical(ids_a, ids_b)
  })

  test_that("new persons can be scored against a fully fixed bank under the defaults", {
    set.seed(4)
    s <- simIRT(Nsubs = 600, Nitems = 30, Nscales = 1, NitemsAnswered = 15,
      AMean = 1, ASD = .2, BMean = 0, BSD = 1)
    d <- data.table::as.data.table(s$dat)
    bank <- data.table::as.data.table(suppressWarnings(fitIRT(d[id <= 400], pl = 2, cores = 1,
      verbose = 0))$itemPars)
    new <- d[id > 400]
    f <- suppressWarnings(fitIRT(new, pl = 2, cores = 1, verbose = 0, itemDat = bank))
    ## With every item fixed the item-prior centres describe nothing, so the
    ## scores are those of a fit that holds them fixed explicitly.
    f0 <- suppressWarnings(fitIRT(new, pl = 2, cores = 1, verbose = 0, itemDat = bank,
      estMeans = character(0)))
    expect_equal(f$personPars, f0$personPars, tolerance = 1e-8)
    expect_gt(stats::cor(f$personPars[[2]], s$Ability[as.integer(f$personPars[[1]])]), .6)
  })

  test_that("a Laplace fit without empirical Bayes reports person SDs", {
    args <- list(dat = sim$dat, pl = 2, cores = 1, marginalApprox = "laplace", ebayes = FALSE,
      dropPerfectScores = FALSE, verbose = 0)
    f <- suppressWarnings(do.call(fitIRT, args))
    sdcol <- grep("_SD$", names(f$personPars), value = TRUE)
    expect_length(sdcol, 1L)
    expect_true(all(is.finite(f$personPars[[sdcol]]) & f$personPars[[sdcol]] > 0))
    ## The same SDs the full posterior covariance gives.
    fk <- suppressWarnings(do.call(fitIRT, c(args, list(laplaceKeepCovariance = TRUE))))
    expect_equal(f$personPars[[sdcol]], fk$personPars[[grep("_SD$", names(fk$personPars), value = TRUE)]],
      tolerance = 1e-6)
  })
}
