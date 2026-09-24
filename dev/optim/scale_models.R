# Mindsteps-scale regimes: thousands of items, each person seeing a small,
# ability-dependent fraction of them, a few hundred responses per item.
#
# simIRT builds the full persons x items grid before subsetting, which at
# 1e5 x 1e4 is 1e9 rows, so these are generated directly. Two designs:
#
#   adaptive  items are ordered by difficulty; each person answers n ~ U(20, 40)
#             items drawn around their own ability (targets ~ N(theta, 0.6^2),
#             mapped to the nearest item by difficulty rank), as an adaptive
#             or level-targeted test does. Item-item coupling is then banded
#             in difficulty, and extreme items have few responses.
#   booklet   items in blocks of 20, persons take two consecutive blocks chosen
#             by ability rank -- a vertically linked booklet chain. Each item
#             shares takers with at most 59 others.
#
# Generation is logistic(a (theta - delta)) with a ~ lognormal(log 1.2, 0.25),
# delta ~ N(0, 1.5), theta ~ N(0, 1.3) plus covariate effects when asked.
# Sourced after models.R; adds to bench_regimes and wraps bench_data.

scale_regimes <- list(
  M2  = list(desc = "2PL adaptive  J=2000  N=20000", gen = "adaptive", J = 2000, N = 20000, pl = 2),
  M5  = list(desc = "2PL adaptive  J=5000  N=50000", gen = "adaptive", J = 5000, N = 50000, pl = 2),
  M10 = list(desc = "2PL adaptive  J=10000 N=100000", gen = "adaptive", J = 10000, N = 100000, pl = 2),
  K5  = list(desc = "2PL booklet chain J=5000 N=37500", gen = "booklet", J = 5000, N = 37500, pl = 2),
  P5  = list(desc = "2PL adaptive  J=5000  N=50000, 2 person covariates", gen = "adaptive",
    J = 5000, N = 50000, pl = 2, personPreds = TRUE),
  T2  = list(desc = "3PL adaptive  J=2000  N=20000", gen = "adaptive", J = 2000, N = 20000, pl = 3)
)
bench_regimes <- c(bench_regimes, scale_regimes)

scale_data <- function(R, seed){
  J <- R$J; N <- R$N
  a <- exp(rnorm(J, log(1.2), 0.25))
  delta <- sort(rnorm(J, 0, 1.5))
  cpar <- if(R$pl >= 3) plogis(rnorm(J, -1.5, 0.5)) / 2 else rep(0, J)
  X <- NULL
  theta <- rnorm(N, 0, 1.3)
  if(isTRUE(R$personPreds)){
    X <- matrix(rnorm(N * 2), ncol = 2)
    theta <- theta + X %*% c(0.4, -0.3)
  }
  theta <- as.numeric(theta)
  if(R$gen == "adaptive"){
    n <- sample(20:40, N, replace = TRUE)
    id <- rep.int(seq_len(N), n)
    target <- rnorm(length(id), theta[id], 0.6)
    item <- findInterval(target, delta, all.inside = TRUE)
    item <- pmin(pmax(item + sample(0:1, length(item), TRUE), 1L), J)
    d <- unique(data.table::data.table(id = id, Item = item))
  } else {
    B <- J %/% 20
    k <- pmin(pmax(ceiling(rank(theta + rnorm(N, 0, 0.3)) / N * (B - 1)), 1), B - 1)
    blocks <- cbind(k, k + 1)
    d <- data.table::data.table(id = rep(seq_len(N), each = 40),
      Item = as.integer(t(sapply(seq_len(N), function(i)
        c(((blocks[i, 1] - 1) * 20 + 1):(blocks[i, 1] * 20),
          ((blocks[i, 2] - 1) * 20 + 1):(blocks[i, 2] * 20))))))
  }
  p <- cpar[d$Item] + (1 - cpar[d$Item]) * plogis(a[d$Item] * (theta[d$id] - delta[d$Item]))
  d[, score := rbinom(.N, 1, p)]
  d[, Scale := 1L]
  if(!is.null(X)){ d[, V1 := X[id, 1]]; d[, V2 := X[id, 2]] }
  data.table::setorder(d, id, Item)
  d[]
}

bench_data_small <- bench_data
bench_data <- function(regime, seed){
  R <- bench_regimes[[regime]]
  if(is.null(R$gen)) return(bench_data_small(regime, seed))
  set.seed(1000L * match(regime, names(bench_regimes)) + seed)
  scale_data(R, seed)
}
