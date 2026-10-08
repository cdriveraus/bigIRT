# bigIRT 0.1.9

## Changed defaults

* `fitIRT()` now defaults to `marginalApprox = "laplace"` (marginal maximum
  likelihood with the Laplace backend; previously `"none"`, penalised joint
  estimation on the Stan path) and `ebayes = FALSE` (previously `TRUE`). On
  simulated 3PL and 4PL data, empirical-Bayes priors narrowed toward a first
  pass gave item intervals covering .004 to .84 of the time against a nominal
  .95. Pass `marginalApprox = "none", ebayes = TRUE` for the old behaviour.
* `laplaceRefine = "auto"` now finishes a two-parameter fit in two or three
  dimensions on adaptive quadrature (previously second-order Laplace, which
  remains the choice beyond three dimensions). With few responses per person
  and dimension the second-order finish sat off the quadrature optimum in
  every fit (sparse 3D simulations, about eight fitted responses per person:
  primary-loading RMSE .283 against .235), and with 1,350 persons 2 to 4 per
  cent of its fits were certified at a spurious optimum with one item at a
  discrimination of 4 to 10, where quadrature put it at 1.6. Quadrature costs
  four to five times as much there. Pass `laplaceRefine = "laplace2"` for the
  old behaviour. Test: `test-refine-auto.R`, which fails on the old rule.

## Fixes

* The quadrature refinement (`laplaceRefine = "agq"`, and `"auto"` wherever
  it refines by quadrature) could walk an item out along its own ray and
  accept a worse fit. Its rounds on held quadrature nodes treat abilities as
  a discrete distribution, which a step between two nodes fits better the
  steeper it is; each stretch gained on the held nodes and lost on the
  objective. On a three-dimensional 2PL with `priors = FALSE` an item went
  from a discrimination of 58 to 6197, and the refinement ended 46 nats
  below where it started -- where quadrature was 62 nats off the exact
  marginal, whose maximum had that item at 3.3. Each stretch on held nodes is
  now judged with the nodes re-centred and undone if it lost more than
  re-centring alone moves the value (1e-4 nats per person; regularised fits
  moved by 6e-6 at most, the exploited stretches lost 2e-3 and more), the
  fit going on with moving nodes. Items that have run off are pulled back along their
  rays and the refinement restarted from there, keeping whichever end is
  best on its own objective; pulling only where that gains was not enough,
  because a run-off item's neighbours adapt to it. A refinement that ends
  lower on its own objective than it started now reverts to the Laplace
  optimum, as one that moves it implausibly far already did. On that case the
  fit now ends 20 nats higher, with no item run off and quadrature within 0.6
  nats of the exact marginal. Regularised fits barely change: of 18
  simulated 2PL to 4PL fits in one to three dimensions, 14 are bit-identical
  and four end up to 0.005 nats higher, each at one more evaluation. Tests:
  `test-agq-runoff.R`, against the exact marginal on frozen data, which fails
  on the old code.
* `dropPerfectScores` (on by default) decided whom to drop from all rows,
  held-out ones included, so held-out responses changed which persons and
  items were fitted. It now decides from the `trainingRows` only.
* Scoring new persons against a calibrated bank (every item fixed through
  `itemDat`) failed on the Laplace path with "missing value where TRUE/FALSE
  needed": the item-prior centres were still estimated with no free items to
  inform them. A family's centre is now estimated only when it has a free
  item parameter.
* A Laplace fit without empirical Bayes now reports person posterior SDs
  (`personPars`, `_SD` columns), from the per-person posterior precision every
  fit keeps; before, they needed `laplaceKeepCovariance = TRUE`.
* `normaliseIRT()`, `normaliseMIRT()`, `compareIRTmodels()`'s `"normalized"`
  form and `fitIRT(normalise = TRUE)` standardised a solution by the spread of
  its ability point estimates, which are shrunk toward the prior mean, so
  discriminations came out flattened by roughly the square root of the
  reliability. They now use the latent scale: `extractMIRTpars()` returns
  `latent_mean` and `latent_sd` (for a `bigIRT` fit from the posterior, by the
  law of total variance; for `mirt` from its estimated latent distribution),
  and the normalisation functions take `AbilitySD`/`AbilityMean`.
  `normaliseIRT(robust = TRUE)` also set the interquartile range, not a
  standard deviation, to the target scale.
* The convergence certificate's fast route could report negative curvature
  that the exact curvature does not have. Such a rejection is now re-tested by
  the exact route when that is projected to take under
  `getOption("bigIRT.certify_escalate", 60)` seconds, and the exact verdict is
  reported (the fast one is kept as `$fast`).
* `extractMIRTpars()` set every simulated upper asymptote to 1, so 4PL curve
  recovery was scored against the wrong curves.
* `tidyIRT()` returned NA standard errors on a default fit although the
  certificate keeps the covariance.
* Empirical Bayes on the Laplace path misindexed fits with some item
  parameters fixed (anchors through `itemDat`, fixed loadings). With
  `ebayesMethod = "moment"` the prior SD of any parameter type with a fixed
  member floored at `ebayesMinSD` whatever the data said (on a 2PL with six of thirty
  difficulties anchored, 0.05 against a correct 1.03). With the default
  `"laplace"` the hyperparameter objective dropped the responses to items with
  a fixed difficulty, added spurious prior-only terms, and integrated over
  fixed loadings and asymptotes as if they were free (prior SDs about 15 per
  cent high in the same case). Fits with every item parameter free are
  bit-identical to before.
* `itemInformation()` stopped with "Item index out of range." on a Laplace
  fit with some difficulties anchored through `itemDat` and the loadings (or
  asymptotes) all free, whenever it did not read the certificate's exact
  covariance: the closed-form information was gathered by the
  free-difficulty index, which is 0 on an anchored item, rather than by item.
  Most callers caught the error and carried on without saying so. On such a
  fit the Newton polish (`laplaceOptimizer = "mize"`) took no steps, the
  person-covariate standard errors dropped their marginal correction and came
  back conditional, `tidyIRT(method = "profile")` gave NA standard errors,
  and without a kept certificate covariance `summary()` and
  `checkConvergence()` lost their standard errors and Newton decrement. Fits
  with every difficulty free are bit-identical to before.
* Per-item standard errors were built only for a parameter type free on
  every item. Anchoring even one difficulty through `itemDat` removed the
  difficulty block from every method, so all the free difficulties lost their
  standard errors (on a 2PL with four of thirty anchored, none of the 26 free
  ones had one, by `"exact"` or `"profile"`). Anchoring difficulties on some
  items and loadings on others made `itemInformation()` stop with "No per-item
  free parameter blocks were found in this fit." Each item's block now covers
  the parameters it has free, and a fixed one is `NA`. The same pairing of item
  j with free parameter j ran through the certificate's exact covariance, the
  Newton polish, the person-covariate standard errors' marginal correction
  (which left out the free difficulties, about 1 per cent of the variance with
  an uncentred covariate), `checkConvergence()`'s Newton decrement and the
  effective degrees of freedom in `logLik()` and `IRTic()`. All now follow
  each item's free parameters. Fits with every item parameter free are
  bit-identical to before.
* An unregularised fit (`priors = FALSE`) whose item ran off reported its
  loading as `Inf`, an intercept in the thousands and `NaN` in
  `pars$pcorrect`, with `laplaceStatus` saying only that it had not
  converged (16 of 80 three-dimensional 2PL fits with `laplaceRefine =
  "agq"`). The raw discrimination was finite (6197 in one case); R formed
  the loading as `log1p(exp(raw))`, which overflows past 709, while the
  native kernels had always used the stable form. The linear predictor then
  took `Inf * 0` for persons whose ability was exactly the prior mean. The R
  side now uses the native softplus, so loadings and probabilities are finite.
  A loading steeper than `getOption("bigIRT.runaway_slope", 20)` logits per
  ability SD, or an intercept that far beyond every ability within four SDs,
  is now reported. The certificate's verdict is `"diverged"`, and
  `laplaceStatus$diverged` names each such parameter with what pulling its
  item back along its own ray gains. `converged` is `FALSE`, and the table is
  filled even with the certificate off. The case above was the
  approximation's: on the exact marginal that item's maximum-likelihood
  discrimination was 3.3.

## New

* `signedLoadings`: loadings on non-primary dimensions with an unconstrained
  sign (identity link, zero-centred prior).
* `laplaceRefine` documents that the two-parameter Laplace marginals rely on
  the item priors; without them, use `"agq"`.
