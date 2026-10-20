## A person who answers an item twice contributes two response scores to the
## item's test. Each is exact and their sum is the person's score, but the
## information is a sum over responses and leaves out the covariance between
## them. bigIRT_score_lm_pieces(persons =) adds it, from the projected scores.

test_that("the within-person covariance is added exactly, and only where a person repeats", {
  set.seed(9)
  n <- 60
  S <- matrix(rnorm(n * 3), n, 3)
  G <- matrix(rnorm(n), n, 1)
  persons <- c(1:40, sample(1:40, 20))
  info <- bigIRT:::bigIRT_score_opg(S)
  p0 <- bigIRT:::bigIRT_score_lm_pieces(G, matrix(1, n, 1), S, info, 1L, 2:3)
  p1 <- bigIRT:::bigIRT_score_lm_pieces(G, matrix(1, n, 1), S, info, 1L, 2:3, persons = persons)
  ## Written out: the projected per-response scores and their covariance.
  Sn <- S[, 2:3]; Inn <- crossprod(Sn); Ign <- crossprod(G * S[, 1], Sn)
  r <- G * S[, 1] - Sn %*% solve(Inn, t(Ign))
  rp <- rowsum(r, persons)
  expect_equal(p1$V - p0$V, crossprod(rp) - crossprod(r), tolerance = 1e-10, ignore_attr = TRUE)
  ## With outer-product information the corrected variance is the person-level
  ## outer product of the projected scores.
  expect_equal(as.numeric(p1$V), sum(rp^2), tolerance = 1e-10)
  expect_equal(p1$Ustar, p0$Ustar)
  ## Every person once: nothing added.
  p2 <- bigIRT:::bigIRT_score_lm_pieces(G, matrix(1, n, 1), S, info, 1L, 2:3, persons = seq_len(n))
  expect_identical(p2$V, p0$V)
})

test_that("item screens change only for items someone answered twice", {
  skip_on_cran()
  set.seed(4)
  N <- 800; J <- 10
  th <- rnorm(N); a <- runif(J, .8, 1.6); b <- rnorm(J)
  d <- data.table::CJ(id = 1:N, Item = 1:J); d[, rep1 := 0]
  r2 <- data.table::data.table(id = 1:N, Item = sample(6:10, N, replace = TRUE), rep1 = 1)
  d <- rbind(d, r2); data.table::setorder(d, id, Item, rep1)
  d[, score := rbinom(.N, 1, plogis(a[Item] * th[id] - b[Item]))]
  d[, Scale := 1L]
  f <- fitIRT(d, item = "Item", pl = 2, cores = 1, laplaceRefine = "agq", dropPerfectScores = FALSE, verbose = 0,
    laplaceCertify = FALSE)
  sc <- scoreContributions(f)
  on <- bigIRT:::bigIRT_modify_asymptotes(f, sc)
  old <- options(bigIRT.score_repeat_cov = FALSE)
  off <- bigIRT:::bigIRT_modify_asymptotes(f, sc)
  options(old)
  m <- merge(on, off, by = c("item", "side"), suffixes = c(".on", ".off"))
  expect_equal(m[as.integer(item) <= 5, z.on], m[as.integer(item) <= 5, z.off])
  expect_true(all(m[as.integer(item) > 5, z.on != z.off]))
})
