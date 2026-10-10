# bigIRT (development version)

## Score-based tests of covariate relations after a fit

* `scoreContributions()` returns, for any fit, each response's contribution
  to the score of the marginal likelihood in its item's parameters (B, every
  free loading, C, D), its observed information, its leave-one-out
  predictive probability, and each person's score in the ability mean. They
  are posterior expectations of the complete-data score (the Fisher
  identity), by adaptive Gauss-Hermite quadrature at each person's Laplace
  mode -- the terms the quadrature kernel already formed for the fit's
  gradient, now written out per row (`row_out` in `src/agq_backend.cpp`).
  Checked person by person against finite differences of each person's
  marginal (agreement 1e-10, where plug-in scores miss by 1e-2), on a 4PL
  with covariates and a 2-D 3PL with some abilities fixed.
* `scoreTest()` turns them into tests of whether an item parameter depends on
  a covariate (DIF, uniform or not, in any of B, A, C, D) or whether ability
  does, without refitting: score (LM) tests with linear, spline or factor
  bases and a one-step estimate of each coefficient; nonparametric
  fluctuation tests (double max, Cramer-von Mises, maxLM) with p-values
  simulated from the process's exact covariance, so that information
  accruing unevenly along the covariate (impact does this) does not
  invalidate them; covariates per person or per response; one test per
  item, per item group, or shared. Each item's other parameters are
  projected out (Neyman's C(alpha)), so the small pull of the item priors is
  handled. `purify = TRUE` anchors on unflagged items -- person posteriors
  and ability coefficients re-estimated from those alone, items held -- and
  retests, against DIF contamination, without a refit.
* The item tests use each response's expected information by default: a
  response's marginal score is exactly the score of the Bernoulli for that
  response given the person's other responses, and that Bernoulli's Fisher
  information is the score's conditional variance, never negative. The
  observed (Louis) information of a single response can be negative, and on
  thin items it made some discrimination tests explode (one statistic of 1257
  on 35 responses). On a simulated thin bank without DIF (50 to 200 responses
  an item) expected information held 3.8 to 5.5 per cent at a nominal 5,
  and the outer product of the scores 7 to 12.
* A covariate related to ability must be in the ability model for its DIF
  test to be valid, but it need not have been in the fit: given the fit,
  `scoreTest(adjust = "auto")` (the default) adds a covariate the ability
  model lacks after the fact -- a centred natural spline, or indicators,
  with coefficients and the ability SD re-estimated and the item parameters
  held -- and tests under that model. A covariate given per response enters
  per response, so ability may also move with it within person. On the
  2025 score-test paper's simulation grid the adjusted tests matched a refit
  with the covariate item by item (median correlation of p values .999
  under impact, the same decision for 99.3 per cent of items) and held 5.0
  to 5.2 per cent under impact (the refit 4.9 to 5.1), where tests that
  ignore it reached 37 to 43; with a covariate varying within person, 5.4
  per cent against 70. The result's `adjustment` records the coefficients,
  the re-estimated ability SD, and the iterations taken, and a warning is
  given if they did not converge. Both the SD's EM and the outer-product
  steps used when predictors vary within person converge linearly, so the
  whole parameter vector is extrapolated jointly (SQUAREM): on the
  Mindsteps mathematics fit 19 and 35 passes where plain iteration took 35
  and 93, to the same point.
* `scoreTest(adjustMethod = "fast")`, the default, holds the fit's own
  ability coefficients and estimates only the added covariate's and the
  residual SD, by a joint Newton step (each person's Louis information for
  the coefficients, a normal posterior's moments for the SD's curvature;
  exact gradients, so the same maximum). Regressing the fit's posterior
  means on the covariate instead recovers only about the reliability's share
  of the impact. On the 2025 paper's grid (14,400 replications) it agreed
  with `"full"` to 5.5e-5 in every item's p value, with the same decisions,
  in a median 4 passes against 8; on the Mindsteps fit 93 s against 232 s,
  the same 34 items flagged. Covariates given per response (no person-level
  Newton step) and multidimensional models use `"full"`.
  `adjust = "none"` tests the fit as it is, and then warns when a
  person-level covariate is related to ability but absent from it. See
  `vignette("score-tests")`. A person-level covariate already in the fit's
  ability model under another name is recognised (it lies in the span of
  the fit's person predictors) and not added again.
* `scoreTest(effects = TRUE)` reports DIF effect sizes. A response's score
  is the score of a Bernoulli given the person's other responses, so the
  derivative of its predicted probability in the item's parameters is
  exact; applied to the one-step DIF estimate it gives the response's
  change in the probability of a correct answer at its own ability.
  `effects` averages it per item and contrast; `dtf` (with `purify`, for a
  person-level covariate) sums it per person over the flagged items, as a
  change in expected number correct and its ability equivalent. Test-level
  effects need anchors: with the covariate's ability effect estimated
  freely, the DIF of all items sums to about zero per group. Simulated
  (50 replications, three DIF items of .8 logits): item effects within
  .006 of the truth, the expected-score change 93 to 95 per cent of it.
* `sandwich::estfun()` on a `bigIRT_scores` object returns the per-person
  (or per-response) score matrix by item and parameter, so
  `strucchange::gefp()` and `sctest()` run on bigIRT fits. With no DIF and
  impact modelled their outer-product decorrelation held 2.8 to 3.9 per cent
  where `scoreTest()` held 4.5 to 5.0.
* `modificationTests()` tests candidate changes to a fitted model at the
  fit: covariates of ability, DIF (purified, with effect sizes), per-item
  lower and upper asymptotes (exact from each response's leave-one-out
  probability, P being linear in c and d), and group dimensions for item
  sets (the variance-component score test, from each person's posterior
  expectation of (sum g)^2 + sum h over the set). Dimensions need no item
  covariate: M = sum E[g g' + diag(h)] is the score for residual covariance
  among items, so its leading eigenvector, found without forming M (M v is
  one quadrature pass), proposes the dimension most worth adding, against a
  simulated null. `improveModel()` refits each kind of change as a batch and
  keeps it only if the held-out persons' marginal log-likelihood rises,
  paired by person (`heldoutLogLik()`, which adds the prior's normalising
  constant the quadrature value leaves out). Every score is checked against
  finite differences of brute-force integration; on a simulation with one of
  each it added the two ability covariates and not the null one, DIF on
  exactly the three DIF items, and the testlet, with or without the topic
  that defines it.

# bigIRT 0.2.0

## Joint MAP in C++, and no more Stan

* The joint posterior-mode estimator is now `marginalApprox = "map"` (`"none"`
  is accepted as its former name) and is written in C++
  (`src/map_backend.cpp`). It replaces a Stan model that optimised every
  person and item coordinate at once by L-BFGS. The persons are now profiled
  out: each evaluation solves every person's mode by Newton and differentiates
  the joint posterior there, which needs no adjoint because the abilities are
  at their maximum. The result runs through the Laplace path's optimiser,
  empirical-Bayes rounds, convergence certificate and output, so the two
  estimators take the same arguments and return the same object. Multiple
  dimensions, confirmatory and cross-loadings, `signedLoadings`, person and
  item covariates (within-person ones included), estimated means, fixed items
  and persons, and whole-person holdouts all work under either.
* On the same data the new and old implementations reach the same optimum. With
  the means fixed (the Stan model put no hyperprior on them), the joint log
  posterior at the two solutions agreed to 1e-4 nats on four small designs.
  On 800,000 responses (20,000 persons, 200 items) the Stan path stopped
  0.1 nats short, where the new one was certified: 6 to 7 s against 12 s
  for a 2PL, and 16 to 25 s against 21 s for a 3PL, which needs more
  iterations. Most of that is now the optimiser's shared metric and fixed R
  overhead rather than the objective: one evaluation, 20,000 Newton solves
  included, takes 0.07 s.
* The gradient agrees with central differences to about 1e-7 in every block
  (2PL to 4PL, one and two dimensions, signed loadings, shared and
  item-specific covariates, fixed items and persons), and the item blocks of
  its Hessian, which the certificate and `itemInformation(method = "exact")`
  use, to about 2e-9 (`test-map.R`). Two deliberately broken kernels were each
  caught by the test aimed at them.
* `estimateAbilityCorr = TRUE` is an error under `"map"`. The joint mode sets
  its latent scale from the priors rather than the data (its loadings come out
  inflated, by a factor of two to four on simulated 2PL data, exactly as the
  Stan implementation's did), and no correlation estimated alongside it held
  up. As a parameter of the joint posterior it runs to the boundary. Estimated
  by EM from the person posteriors it was biased: .39 to .50 for a true .6,
  where the Laplace estimator gave .61.
* `logLik()`, and so `IRTic()`, `AIC()`, `glanceIRT()` and `summary()`, report
  for a `"map"` fit the same model's marginal likelihood at the MAP estimates,
  by the approximation `laplaceRefine` selects. Fits by either estimator are
  then compared on one scale.
* With every ability fixed (`personDat`), `marginalApprox = "laplace"` now
  fits the joint mode, which is the same objective there up to a constant,
  instead of falling back to the Stan path.

## Fixes

* The Laplace objective's gradient was wrong whenever some abilities were
  fixed (`personDat`): off by 1.06 relative against central differences with
  40 of 150 persons fixed, and by 1.15 in two dimensions, so such fits were
  optimised along something that was not the derivative of what they
  evaluated. Two faults. The item kernel took each person's log-determinant
  over every dimension, while the person kernel, whose log-determinant the
  objective's value uses, pins fixed abilities out. And the
  positive-semidefinite guard on the data's curvature ran over every
  dimension, so a fixed ability's curvature could clip the free block and
  drop a log-determinant term that was not flat (7.8e-2 once the first was
  fixed). Both now work on the free abilities only. Second-order Laplace
  inherited both; quadrature and joint MAP were not affected. Fits without
  fixed abilities are unchanged. Test: `test-laplace-direct-gradients.R`,
  which fails on the old code, and on the first fix alone.
* `normalise = TRUE` now moves the whole fit with the abilities. In one
  dimension the person posterior was left on the old metric, so
  `reliability()` changed under what is only a reparameterisation (.859 to
  .868 in one check) and the person SD columns were withheld. In any
  dimension the covariate coefficients were not rescaled, so the reported
  abilities stopped being base plus covariate effect, and the row-level
  loadings and difficulties were not moved, so `looIRT()` gave different
  answers for a normalised multidimensional fit. And `logLik()` read the
  ability prior's normalising constant from the rewritten `abilityPrior`, so
  it moved too; it now uses the prior the fit was estimated with. Test:
  `test-normalise-latent-scale.R`, six of whose fifteen checks fail on the
  old code. `normaliseIRT()` also returns the `center` and `scale` it
  applied.
* The convergence certificate's default route differenced every item-specific
  covariate coefficient (`itemSpecificBetas = TRUE`) as if it were shared
  across items, at two evaluations each. On a 500-item fit with three
  covariates on A and B that is nearly 6,000 evaluations: the certificate
  had run for more than ten minutes, on a fit that took a few seconds, when
  it was stopped. Each coefficient belongs to one item, and both kernels now
  return it in that item's block of the Hessian from the same pass, carried
  row by row so that a covariate varying within an item is exact too. The
  same fit is now certified in 6.3 s in all, 0.65 s of it the certificate.
  The exact route's block form keeps them with their items as well. Tests:
  `test-map.R` and `test-certify.R` check the blocks against central
  differences (agreement 6e-10; built from the item's summed block instead,
  they are off by 0.25 to 1.2) and the certificate's cost on such a fit.
* Fits too large for the dense optimiser metric (over 1,000 free parameters)
  could stop short when the item priors were tight. The block metric dropped
  every coupling between an item's parameters and the shared ones, including
  the prior's tie of each item parameter to its mean hyperparameter. That tie
  has the prior's precision, which empirical Bayes can push to 1e6 (an item
  SD at its 0.001 floor), and without it the metric took moving a mean, with
  the items following it, to be about that many times stiffer than it is.
  L-BFGS then predicted almost no gain and stopped. The item-specific
  empirical-Bayes fit in `test_fixedItemPars.R` ended 0.033 nats short and
  uncertified, and a 300-person, 30-item fit with item SDs of 0.001 ended
  8.9 nats short after two iterations. The metric now keeps the prior's
  border, applied through the shared block's Schur complement: both fits
  are certified, the first in one iteration, as the dense metric did it. On
  tight priors the bordered metric also needs a third to a fifth of the
  iterations where the old one did get there. Fits whose item means are
  fixed have no border and are unchanged. Test: `test-laplace-optimizer.R`,
  two of whose checks fail on the old code.
* Empirical Bayes with a prior centre held fixed (`estMeans` leaving it out)
  measured the items' spread about their own mean, then applied that SD about
  the fixed centre. With the loading centre placed away from the items (raw
  2.5, a = 2.6, against items generated at a = 1) it fitted an SD of about
  .45 and dragged the loadings to a mean of 1.94 to 2.15; it now measures the
  spread about the centre in force, fits 1.86 to 2.03, and the loadings
  average 1.03 to 1.13 (three seeds). With estimated centres, the default,
  nothing changes. Test: `test-ebayes-centre.R`, which fails on the old code.
* A fit whose items run off with item priors in force no longer advises
  `priors = TRUE`; it says the priors in force did not prevent it (joint MAP
  with empirical Bayes can do this, through an estimated centre or a floored
  SD).
* With abilities fixed (`personDat`), the optimiser's metric
  (`src/fisher_blocks.cpp`) formed its Schur term over every person's
  columns, including the all-zero ones of the fixed persons. It now skips
  them: 87.5 s to 7.2 s on a 500-item fit with every ability fixed, with
  identical results.

## Interface

* Removed arguments: `iter`, `tol`, `init` and `carefulfit` (the Stan path's
  optimiser controls; the `laplace*` ones govern both estimators),
  `noptimsteps` (unused since the alternating Laplace backend was withdrawn),
  `ebayesFromFixed` (never ran: it tested `length(personDat) == 1`, which a
  data frame with more than one column never is) and `stochastic`. Old calls
  keep working: `plot` and `noptimsteps`, which never changed a result, are
  accepted in silence, and the others are accepted with a warning that they
  are ignored. `...` takes nothing else. It used to forward anything to the
  Stan optimiser, so a misspelled argument could pass unnoticed; an unknown
  argument is now an error. `verbose` is a named argument.
* `marginalApprox` moved next to `pl` in the argument list, and the arguments
  are grouped by purpose. The `laplace*` optimiser controls keep their names.
* The undocumented `stochastic` optimiser is gone with the Stan path that
  shared it.
* `print()` and `summary()` name the estimator and objective; a MAP fit's
  printed objective is labelled as the joint log posterior.

## Removed

* The Stan model, `rstan`, `rstantools`, `StanHeaders` and `BH` dependencies,
  the rstantools `configure` scripts, `optimIRT()`, the ctsem-derived SGD
  optimiser and the sampled-ability helpers only it used, and the sigma-point
  kernels in `src/person_covariance.cpp`. Installing no longer compiles a Stan
  model, and native routines are registered in `src/init.c`.

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
* An unregularised loading driven onto the flat tail of its softplus (a raw
  value of -124 in one case: slope 1e-54, curvature 1e-108) made the
  optimiser's metric move it by its coupling to every other gradient over
  that curvature, a step of 1e53 that no line search could shorten to sense,
  so the fit took no step at all -- Laplace and the quadrature refinement
  alike -- and stopped wherever it was. Each metric (dense and
  block-diagonal, R and C++) now decouples a coordinate whose curvature is
  below 1e-12 of the largest and gives it an ordinary one, so it stays where
  it is and the certificate judges it. Metrics without such a coordinate,
  which includes every fit with item priors, are exactly as before. Test:
  `test-agq-runoff.R`, which fails on the old code for all four metrics.
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
