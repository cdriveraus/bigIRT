## simIRT's two generating paths and the conventions they imply.
##
## Both of these failed before September 2026 and neither stopped anything
## from running or looking healthy:
##
##   - legacy mode returned pcorrect as a column of zeros, under a name that
##     reads like a probability. mirt = TRUE always populated it.
##   - legacy mode generates logistic(a (theta - b)) while mirt = TRUE
##     generates logistic(a' theta - b), so B means a difficulty in one and
##     an intercept in the other, and only the second matches what fitIRT
##     reports. extractMIRTpars passed the legacy difficulty through
##     unconverted, so compareIRTmodels compared a difficulty against an
##     intercept and inflated difficulty RMSE by about 2.5x.
##
## The second is the nastier one: the error is (a-1)b, which cancels in mean
## bias, grows with |b| and does not shrink with N. It mimics a calibration
## failure closely enough to be mistaken for one.

test_that("the returned columns reproduce the probabilities that generated the scores", {
  set.seed(10001)
  s <- simIRT(Nsubs = 300, Nitems = 30, Nscales = 1, NitemsAnswered = 15L,
              AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
  d <- as.data.frame(s$dat)
  ## legacy: a (theta - b)
  p_diff <- stats::plogis(d$A * (d$Ability - d$B))
  ## absolute, not expect_equal: with C at plogis(-20) the probabilities
  ## get small enough that a 1e-9 absolute gap is a large relative one.
  expect_lt(max(abs(d$p - p_diff)), 1e-7)
  ## and pcorrect is that probability, not a placeholder
  expect_equal(d$pcorrect, d$p, tolerance = 1e-12)
  expect_gt(stats::sd(d$pcorrect), 0.05)
})

test_that("mirt mode uses the intercept parameterisation and also fills pcorrect", {
  set.seed(31)
  s <- simIRT(Nsubs = 250, Nitems = 24, Nscales = 2, NitemsAnswered = c(8L, 8L),
              mirt = TRUE, AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
  d <- as.data.frame(s$dat)
  A <- as.matrix(s$A); B <- as.vector(s$B); Ab <- as.matrix(s$Ability)
  eta <- rowSums(A[d$Item, , drop = FALSE] * Ab[d$id, , drop = FALSE]) - B[d$Item]
  expect_lt(max(abs(d$p - stats::plogis(eta))), 1e-7)
  expect_equal(d$pcorrect, d$p, tolerance = 1e-12)
})

test_that("extractMIRTpars puts a legacy simulation on the intercept scale", {
  ## This is the conversion compareIRTmodels relies on. Without it the
  ## reference difficulty is the raw legacy B and the comparison is in the
  ## wrong units.
  set.seed(777)
  s <- simIRT(Nsubs = 200, Nitems = 20, Nscales = 1, NitemsAnswered = 12L,
              AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
  ex <- extractMIRTpars(s)
  expect_equal(as.numeric(ex$B), as.vector(s$A) * as.vector(s$B), tolerance = 1e-10)
  ## the discriminations are untouched by the conversion
  expect_equal(as.numeric(ex$A), as.vector(s$A), tolerance = 1e-10)
  ## and the conversion is not a no-op, so this test can fail
  expect_gt(max(abs(as.numeric(ex$B) - as.vector(s$B))), 0.05)
})

test_that("difficulty recovery is on a sane scale once the units agree", {
  skip_on_cran()
  ## With the conversion in place, difficulty RMSE should be the size of a
  ## standard error, not of the item population. Against the unconverted
  ## comparison this came out around 0.31 where the estimator's own error was
  ## about 0.13.
  set.seed(777)
  s <- simIRT(Nsubs = 1500, Nitems = 40, Nscales = 1, NitemsAnswered = 20L,
              AMean = 1, ASD = .3, BMean = 0, BSD = 1,
              logitCMean = -20, logitCSD = 0, logitDMean = 20, logitDSD = 0)
  f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
              pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L,
              plot = FALSE, priors = TRUE, ebayes = FALSE, normalise = FALSE,
              dropPerfectScores = FALSE)
  z <- compareIRTmodels(list(truth = s, estimated = f), score_method = "MAP",
                        curveSampleN = 50L)
  ip <- as.data.frame(z$itempars)
  b <- ip[ip$form == "normalized" & ip$source == "estimated" & ip$parameter == "B", ]
  expect_gt(nrow(b), 0)
  expect_lt(sqrt(mean((b$value - b$referenceVal)^2)), 0.2)
})

test_that("simIRT returns truth tables in fitIRT's shape and convention", {
  ## The point of these is that comparing a simulation against a fit becomes a
  ## join on Item, with no hand conversion of units and no hand ordering.
  ## Both of those steps used to be done at every call site and both failed
  ## silently.
  set.seed(9)
  s <- simIRT(Nsubs = 250, Nitems = 12, Nscales = 1, NitemsAnswered = 8L,
              AMean = 1, ASD = .3, BSD = 1, logitCMean = -20, logitCSD = 0)
  expect_true(all(c("Item", "A", "B", "C", "D") %in% names(s$itemPars)))
  expect_identical(nrow(s$itemPars), 12L)
  ## B is on fitIRT's scale, not the raw legacy difficulty
  expect_equal(s$itemPars$B, as.vector(s$A) * as.vector(s$B), tolerance = 1e-10)
  expect_gt(max(abs(s$itemPars$B - as.vector(s$B))), 0.05)
  ## Item is character and in item order, so a join is safe
  expect_identical(s$itemPars$Item, as.character(1:12))
  expect_identical(nrow(s$personPars), 250L)

  f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
              pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L,
              plot = FALSE, priors = TRUE, ebayes = FALSE, normalise = FALSE,
              dropPerfectScores = FALSE)
  m <- merge(s$itemPars, f$itemPars, by = "Item", suffixes = c(".true", ".fit"))
  expect_identical(nrow(m), 12L)
  expect_gt(stats::cor(m$B.true, m$B.fit), 0.9)
})

test_that("the simIRT self-check passes on every generating configuration", {
  ## It runs on every simulation, so a silent drift between the returned
  ## parameters and the probabilities that made the scores cannot persist.
  expect_silent({ set.seed(9)
    simIRT(Nsubs = 150, Nitems = 12, Nscales = 1, NitemsAnswered = 8L,
           logitCMean = -20, logitCSD = 0) })
  expect_silent({ set.seed(11)
    simIRT(Nsubs = 150, Nitems = 12, Nscales = 1, NitemsAnswered = 8L,
           logitCMean = -1.4, logitCSD = .25, logitDMean = 2.5, logitDSD = .25) })
  expect_silent({ set.seed(12)
    simIRT(Nsubs = 150, Nitems = 12, Nscales = 2, NitemsAnswered = c(6L, 6L),
           logitCMean = -20, logitCSD = 0) })
  expect_silent({ set.seed(31)
    simIRT(Nsubs = 150, Nitems = 18, Nscales = 2, NitemsAnswered = c(6L, 6L),
           mirt = TRUE, logitCMean = -20, logitCSD = 0) })
  expect_silent({ set.seed(32)
    simIRT(Nsubs = 150, Nitems = 24, Nscales = 3, NitemsAnswered = c(5L, 5L, 5L),
           mirt = TRUE, logitCMean = -1.4, logitCSD = .25,
           logitDMean = 2.5, logitDSD = .25) })
})

test_that("the self-check actually fires when the object is inconsistent", {
  ## A check that cannot fail is worse than none. Corrupt a returned parameter
  ## and confirm the same assertion catches it.
  set.seed(9)
  s <- simIRT(Nsubs = 150, Nitems = 12, Nscales = 1, NitemsAnswered = 8L,
              logitCMean = -20, logitCSD = 0)
  s$dat$B <- s$dat$B + 0.5
  expect_warning(bigIRT:::bigIRT_sim_selfcheck(s, intercept = FALSE),
                 "do not reproduce the generating probabilities")
})

test_that("the fit's prepared data is not called dat", {
  ## fit$dat held the 86-element prepared object, not the response data, and
  ## as.data.table(fit$dat) silently returned a recycled frame that looked
  ## like data. Renamed to fit$prepared.
  set.seed(3)
  s <- simIRT(Nsubs = 150, Nitems = 10, Nscales = 1, NitemsAnswered = 6L,
              logitCMean = -20, logitCSD = 0)
  f <- fitIRT(s$dat, score = "score", id = "id", item = "Item", scale = "Scale",
              pl = 2L, marginalApprox = "laplace", cores = 1L, verbose = 0L,
              plot = FALSE, priors = TRUE, ebayes = FALSE, normalise = FALSE,
              dropPerfectScores = FALSE)
  expect_null(f$dat)
  expect_false(is.null(f$prepared))
  expect_true(all(c("Nobs", "Nsubs", "Nitems") %in% names(f$prepared)))
})
