bigIRT
================
Charles Driver

<!-- badges: start -->
[![R-CMD-check](https://github.com/cdriveraus/bigIRT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cdriveraus/bigIRT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Install

If building on windows, first ensure you have Rtools installed.
<https://cran.r-project.org/bin/windows/Rtools/>

Run the following code to install the package:

``` r
remotes::install_github('cdriveraus/bigIRT', INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
```

## Quick start

``` r
library(bigIRT)
library(data.table)

# Simulate a sparse 2PL-like dataset.
sim <- simIRT(
  Nsubs = 300, Nitems = 60, Nscales = 1,
  AMean = 1, ASD = .15,
  BMean = 0, BSD = .9,
  logitCMean = -10, logitCSD = 0
)

# Fit using joint maximum likelihood / MAP style estimation.
fit <- fitIRT(
  sim$dat,
  pl = 2,
  cores = 1,
  dropPerfectScores = FALSE
)

head(fit$itemPars)
head(fit$personPars)
```

## Laplace backends

`fitIRT()` also supports blockwise and direct Laplace fitting:

``` r
fit_laplace <- fitIRT(
  sim$dat,
  pl = 2,
  marginalApprox = "laplace",
  laplaceDiagnostics = TRUE
)
```

You can inspect diagnostics with:

``` r
plotLaplaceDiagnostics(fit_laplace)
```

There is one Laplace backend, `marginalApprox = "laplace"`. It optimises the
item block directly, re-solving the person modes inside every objective
evaluation and differentiating through them, so the gradient carries the
adjoint term rather than treating the modes as fixed. `"laplace_fast"` and
`"laplace_direct"` are accepted as names for it: those were once two separate
implementations, and the alternating one was withdrawn after it proved slower
for the same answer.

Check `fit_laplace$laplaceStatus` for strict convergence, a stable plateau,
iteration limits, unresolved person modes, and whether covariance matrices were
retained. Full covariances can be large for many people or dimensions.
Predictor columns for `personPreds` and all
`AitemPreds`/`BitemPreds`/`CitemPreds`/`DitemPreds` are response-row aligned
and may vary within a person or item; the likelihood evaluates their exact
row-specific values.

With `ebayes = TRUE` the prior hyperparameters are estimated from the data.
`ebayesMethod = "laplace"` (the default) approximates the integral over the
item parameters and maximises the resulting profile marginal; `"moment"` keeps
an older variance-components rule.

## Multidimensional models

Pass a loading matrix whose fixed zeros define a confirmatory structure and
whose `NA` entries are estimated. The latent correlation can be estimated
alongside it:

``` r
fit <- fitIRT(dat, pl = 2, loadings = loadingMatrix,
              marginalApprox = "laplace",
              estimateAbilityCorr = TRUE)

fit$abilityPrior$corr       # estimated latent correlation
fit$abilityPrior$estimated  # TRUE when estimated rather than held fixed
```

Report `fit$abilityPrior$corr` rather than `cor(fit$pars$Ability)`. The
correlation among fitted ability point estimates is biased: downwards under an
independent prior, because each score is shrunk on its own, and upwards under a
correlated one, because the shrinkage pulls scores in a common direction.

## Row ordering

`fitIRT()` sorts the data by person before fitting. The two per-response
predictions, `fit$pars$p` (probability of the observed response) and
`fit$pars$pcorrect` (probability of a correct response), are mapped back to the
order of the data you passed in, so they can be indexed with the same row
numbers you used for `trainingRows`:

``` r
fit <- fitIRT(dat, pl = 2, trainingRows = train,
              marginalApprox = "laplace")
heldout_logloss <- -mean(log(ifelse(dat$score[-train] == 1,
  fit$pars$pcorrect[-train], 1 - fit$pars$pcorrect[-train])))
```

Every other row-level object (`b_row`, `c_row`, `d_row`, `eta_row`,
`row_loadings`, `row_ability`, and the contents of `fit$dat`) stays in the
internal person-sorted order. Use `fit$pars$originalRow`, the input row index of
each internal row, to move between the two.

## Notes

- Use `normaliseIRT()` when comparing parameter sets estimated on different scales.
- For supported interoperability with `mirt`, use `extractMIRTpars()` and
  `compareIRTmodels()`; install `mirt` separately when needed.
