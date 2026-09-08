# dev

Exploratory material. Not part of the package: `dev` is in `.Rbuildignore`, so
nothing here is built, installed, or checked, and nothing here is run by the
test suite.

Most of it arrived from `tests/testthat/`, where it had been sitting inside
`if(FALSE)` blocks. That kept it out of the way of the tests but not out of the
way of `R CMD check`, which reads `library()` and `require()` calls whether or
not the branch around them can execute, and reported `TAM`, `future` and
`ggpointdensity` as undeclared dependencies. Declaring three packages the
package does not use, so that a check could be satisfied about code that cannot
run, was the wrong trade; the code moved here instead.

None of these scripts is executed by anything, so none of them is known to
work. Treat them as a record of an approach rather than as working code.

| | |
|---|---|
| `wle-comparison.R` | Weighted-likelihood against MAP ability estimates, and against `TAM` and `mirt`, swept over item counts and known versus estimated items. Was in `test-ability.R`. |
| `scale-corr-tam-comparison.R` | Latent correlation recovery against `TAM`. `TAM` overstates the correlation by less than bigIRT's empirical-Bayes pass understates it. Was in `test-scaleCorr.R`. |
| `categoricalPred-scratch.R` | Categorical person and item covariates against `TAM`. Was `tests/testthat/test-categoricalPred.R`, wrapped in `if(F)` end to end, so it never ran as a test. |
| `laplace-mirt-comparison.Rmd` | A sparse three-factor comparison of bigIRT's methods against `mirt`, through `compareIRTmodels()`. Superseded for estimator comparison by `../../map-vs-laplace/`. |

The rendered `laplace-mirt-comparison.html` is kept beside its source.

Benchmarks live separately, in `tests/noncran/`, which is also build-ignored.
