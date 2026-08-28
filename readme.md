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
iteration limits, a stall short of the gradient tolerance, unresolved person
modes, and whether covariance matrices were retained. `reason` carries the
optimiser's own stop code, so a line search that stops making progress is not
reported as an exhausted iteration budget, and a fit that stops short of the
tolerance warns rather than passing in silence. Full covariances can be large
for many people or dimensions.

After a fit, `itemInformation()` gives per-item standard errors and an
effective parameter count, `checkConvergence()` breaks the gradient down by
block and reports a Newton decrement, `IRTic()` gives AIC and BIC with the
person parameters correctly not counted, `heldoutMetrics()` scores the rows
excluded by `trainingRows`, and `looIRT()` runs PSIS-LOO over draws from the
Laplace person posteriors. `itemInformation()` defaults to a closed-form
weight whose standard errors run about 6 per cent small; `method = "hessian"`
differences the analytic gradient instead and is exact up to a block-diagonal
term worth about 1 per cent, at 2P objective evaluations per item. The default
`method = "auto"` times one evaluation and takes the exact route whenever the
projected cost fits `time_budget`.

For measurement reporting, `testInformation()` gives item and test information
with the conditional standard error, `reliability()` gives empirical and
marginal reliability, `itemFit()` gives infit and outfit in one pass over the
responses, and `empiricalICC()` bins people by ability to compare observed
against model-implied curves. `plot(fit, type = )` draws response curves,
information, a person-item map, or the optimiser trace. All of these are
row-wise or item-wise, so they run on data that never becomes a wide matrix;
statistics that genuinely need one, such as S-X squared, M2 and Yen's Q3, are
better taken from `mirt` through `as.mirt()`, which converts a fit into a
fixed-parameter mirt object and so opens `itemfit()`, `personfit()`, `M2()`,
`residuals(type = "Q3")`, `itemplot()` and `DIF()`. Verify it with
`checkMirtBridge()`, which compares mirt's own trace lines against bigIRT's
response function; it agrees to 1e-9 for every model type. The coercion needs
the wide matrix, so it refuses rather than attempts a fit too large to hold.

Note that `logLik()` returns a marginal log-likelihood, not the optimiser
objective. The objective omits the person prior's normalising constant and, with
priors on, includes the item prior density; both are constant during fitting but
left the reported value 6.5 per cent from the marginal likelihood. Corrected, it
agrees with dense quadrature to 0.04 per cent.

`tidyIRT()`, `glanceIRT()` and `augmentIRT()` return tidy data frames and are
registered against `broom`'s generics when broom is installed; their standard
errors are carried onto the reported parameter scale by the delta method, since
discrimination is estimated as a softplus pre-image and the asymptotes as
logits. `reportIRT()` renders the whole lot to a self-contained HTML report.

The block kernel returns the per-response effective discrimination, difficulty,
asymptotes and linear predictor it derives while evaluating the likelihood, and
the adjoint gradient path uses those rather than rebuilding them in R. Together
with moving the per-person score and slope accumulation to C++, that took a
1.8-million-response covariate fit from 52.3 s to 29.4 s at an unchanged
log-likelihood, and left roughly ninety per cent of an objective evaluation
inside the compiled kernels.

Blocks whose gradient aggregates over many units but which hold few parameters
are rescaled internally before optimisation. That covers the ability covariates,
the mean hyperparameters and the item covariates: with `itemSpecificBetas =
FALSE` there is a single coefficient per predictor whose gradient sums over
every response, and unscaled it held 92 per cent of the gradient norm on a
2,000-item fit. A
covariate coefficient's gradient sums over every person, while an item
parameter's sums only over that item's responses, so the two blocks arrive on
scales differing by roughly the square root of their ratio; a single curvature
estimate then sizes its steps for the item block and stalls on the covariates.
The rescaling is invisible in the returned coefficients. This covers the ability
covariates and the mean hyperparameters alike: a mean is informed by every item,
and at Mindsteps scale `A_mean` alone held 78 per cent of the gradient norm while
the fit reported convergence.

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
