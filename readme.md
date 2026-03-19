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

## Sampled ability updates

`fitIRT()` also supports a sampled-ability alternating routine:

``` r
fit_sampled <- fitIRT(
  sim$dat,
  pl = 2,
  sampledAbilityStep = TRUE,
  sampledAbilityOuterIter = 30,
  sampledAbilityDiagnostics = TRUE
)
```

You can inspect diagnostics with:

``` r
plotSampledAbilityDiagnostics(fit_sampled)
```

For a conceptual overview of the sampled-ability algorithm, see
[`inst/docs/sampledAbility-algorithm.md`](inst/docs/sampledAbility-algorithm.md).

## Notes

- Use `normaliseIRT()` when comparing parameter sets estimated on different scales.
