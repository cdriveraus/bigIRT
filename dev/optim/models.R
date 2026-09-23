# Regime-balanced model set for the optimiser review.
#
# Each regime is a data generator plus the fitIRT arguments that go with it.
# Data are a pure function of (regime, seed), so every variant of the optimiser
# sees the same responses and starts from the same place: the Laplace path
# always starts from bigIRT_laplace_initial_state(), which is deterministic.
#
# The set spans the axes that change the shape of the problem: persons, items,
# sparsity (responses per person), item model (2PL/3PL/4PL), dimensions with an
# estimated latent correlation, and person covariates (a block whose gradient
# sums over every person). G is the large-N case, where batching could pay.

bench_regimes <- list(
  A = list(desc = "2PL 1D  N=1000  I=30  complete",
    sim = list(Nsubs = 1000, Nitems = 30, NitemsAnswered = 30),
    pl = 2),
  B = list(desc = "2PL 1D  N=5000  I=200 sparse 25/person",
    sim = list(Nsubs = 5000, Nitems = 200, NitemsAnswered = 25),
    pl = 2),
  C = list(desc = "3PL 1D  N=3000  I=40  complete",
    sim = list(Nsubs = 3000, Nitems = 40, NitemsAnswered = 40,
      logitCMean = -1.5, logitCSD = 0.5),
    pl = 3),
  D = list(desc = "2PL 2D  N=2000  I=2x25 complete, corr estimated",
    sim = list(Nsubs = 2000, Nitems = 25, Nscales = 2, NitemsAnswered = 25,
      AbilityCorr = matrix(c(1, .6, .6, 1), 2)),
    pl = 2, fit = list(estimateAbilityCorr = TRUE)),
  E = list(desc = "2PL 1D  N=3000  I=30  complete, 2 person covariates",
    sim = list(Nsubs = 3000, Nitems = 30, NitemsAnswered = 30),
    personPreds = TRUE, pl = 2),
  F = list(desc = "4PL 1D  N=3000  I=40  complete",
    sim = list(Nsubs = 3000, Nitems = 40, NitemsAnswered = 40,
      logitCMean = -2, logitCSD = 0.5, logitDMean = 2.5, logitDSD = 0.5),
    pl = 4),
  G = list(desc = "2PL 1D  N=40000 I=300 sparse 25/person",
    sim = list(Nsubs = 40000, Nitems = 300, NitemsAnswered = 25),
    pl = 2),
  ## Uneven item exposure, as in adaptive or opt-in designs: every person
  ## answers 25 items drawn with probability falling as rank^-1, so item
  ## response counts run from tens to thousands and a single step scale cannot
  ## suit every item. The case a per-item metric exists for.
  H = list(desc = "2PL 1D  N=5000  I=200 uneven exposure 25/person",
    sim = list(Nsubs = 5000, Nitems = 200, NitemsAnswered = 200),
    exposure = list(n = 25, power = 1), pl = 2)
)

bench_data <- function(regime, seed){
  R <- bench_regimes[[regime]]
  set.seed(1000L * match(regime, names(bench_regimes)) + seed)
  args <- modifyList(list(AMean = 1.2, ASD = 0.3, BMean = 0, BSD = 1,
    logitCMean = -20, logitCSD = 0), R$sim)
  if(isTRUE(R$personPreds)){
    X <- matrix(rnorm(args$Nsubs * 2), ncol = 2)
    args$personPreds <- X
    args$AbilityPredEffects <- matrix(c(0.4, -0.3), 1)
  }
  sim <- do.call(bigIRT::simIRT, args)
  dat <- sim$dat
  if(!is.null(R$exposure)){
    ni <- R$sim$Nitems
    w <- seq_len(ni)^(-R$exposure$power)
    keep <- unlist(lapply(split(seq_len(nrow(dat)), dat$id), function(r){
      it <- as.integer(as.character(dat$Item[r]))
      r[it %in% sample.int(ni, R$exposure$n, prob = w)]
    }))
    dat <- dat[sort(keep)]
  }
  dat
}

bench_fit_args <- function(regime){
  R <- bench_regimes[[regime]]
  out <- list(pl = R$pl, marginalApprox = "laplace", ebayes = FALSE, cores = 1,
    dropPerfectScores = TRUE)
  if(isTRUE(R$personPreds)) out$personPreds <- c("V1", "V2")
  if(!is.null(R$fit)) out <- modifyList(out, R$fit)
  out
}
