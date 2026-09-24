# dev/optim: Laplace optimiser and marginal refinement

Studies behind the Laplace-path optimiser and `laplaceRefine`. Measured
2026-09 on the science-cloud boxes (dev1 shared, dev2 for uncontended timing).
The longer narrative, with the original handoff, is
`../../../laplace-optimiser-handoff.md` (in the `bigIRT/` folder, outside the
repo); the full round-1/2 evidence is `optimisation-review-2026-09.md` there.

## What the package does now

- Laplace fit: Fisher-shaped L-BFGS (the metric sets the shape, the secant
  ratio the scale), predicted-gain stop. Metric built in C++
  (`src/fisher_blocks.cpp`, block-diagonal or dense by per-person Schur
  complements; latent-correlation block in closed form).
- Person modes: Newton on the observed curvature, Fisher fallback.
- `laplaceRefine`, finishing on a better marginal from the Laplace optimum:
  - `"agq"`: adaptive Gauss-Hermite at the Laplace mode
    (`src/agq_backend.cpp`), Fisher-identity gradient, exact with the nodes
    held fixed between metric refreshes. Product rule, 9 / 7 / 5 nodes at
    K = 1 / 2 / 3. From K = 4 the anchored-ANOVA expansion
    (`bigIRT_anova_rule` in `R/agq.R`): 7-node rules on every coordinate
    plane and line of the whitened space plus a 3-node three-way correction
    on every cube, combined with signed coefficients; the kernel takes it as
    grouped rules with shared nodes (2,185 a person at K = 9).
  - `"laplace2"`: second-order Laplace, E T4 + E T3^2 / 2
    (`src/laplace2_backend.cpp`), exact reverse-mode gradient with a mode
    adjoint.
  - `"auto"`: no free asymptotes -> none at K = 1, laplace2 at K >= 2;
    free asymptotes -> agq at every K.
  - Guard: a refinement moving the objective by more than max(50, N/2) nats
    reverts to the Laplace optimum.
- One CSR person->rows index (`src/bigirt_obsindex.h`) shared by all kernels.

## Laplace bias, against near-exact optima (SE units of the estimates)

| model | K | loadings, mean | worst | reference |
|---|---|---|---|---|
| 2PL | 1 | 0.03 | -- | GH 21-41 |
| 2PL | 2 / 3 / 5 / 9 | -0.16 / -0.22 / -0.17 / -0.12 | 0.7 | 9^K / QMC 16,384 |
| 3PL | 1 | +0.5 (+4.5%) | 1.5 | GH 21-41 |
| 4PL | 1 | +0.8 to +1.0 (+8-13%) | 2.7 | GH 21-41 |
| 3PL | 2 / 3 | -1.4 / -1.1 (difficulties -2.0 / -1.8) | 4.8 | 9^K |
| 3PL | 9 | -1.05 (difficulties -2.2; loadings -16%) | 6.6 | QMC 16,384 |

Scripts: `laplace_vs_agq.R` (K = 1), `mdim_lowk.R` (K <= 3), `mdim_refine.R`
(K = 9), `mdim_rules.R` (per-person accuracy of candidate rules).
Accuracy of the refinements: GH 3 nodes/dim poor, 5 acceptable (<= 0.4 SE),
7 accurate (<= 0.06); laplace2 accurate for the 2PL at K = 2-9 (<= 0.1 SE).

The T2 "local maximum" was a Laplace artefact: one item held steep with high
guessing plus `C_mean` dragging the rest (`basins.R`, `basin_hybrid.R`,
`item_basin.R`). Refined, both basins reach the same point to 3e-4.

## 3PL/4PL at K >= 4: the anchored-ANOVA rule

The problem was never pointwise accuracy but exploitability: the optimiser
finds where an approximate marginal is wrong. What worked is an expansion
whose every piece is a low-dimensional Gauss-Hermite rule of the kind already
shown not to be exploited at K <= 3:

    log I ~ sum_{k<l} log I_kl - (K - 2) sum_k log I_k + C(K-1, 2) log I_0
            + [three-way correction: order 3 minus order 2, at 3 nodes]

in whitened coordinates (theta = m + U^-1 z, the kernel's own). Full
refinements (`pairs_fit.R`), against 16,384-point QMC optima (which are
themselves within 0.4 SE of a 65,536-point optimum, mean 0.0), N = 2,000,
10 items per dimension, 20% cross loadings; loadings mean / worst, SE units:

| model | K | Laplace | pairs 5 | pairs 7 | pairs 7 + t3 | pairs 7 + t5 | time (Laplace / p7t3 / ref) |
|---|---|---|---|---|---|---|---|
| 3PL | 5 (vs 65,536) | -1.60 / 6.2 | -0.18 / 0.8 | -0.09 / 0.3 | -0.04 / 0.21 | | 3.7 / 14 / 1,651 s |
| 3PL | 9 | -1.05 / 5.7 | -0.11 / 0.9 | -0.09 / 0.7 | -0.02 / 0.5 | | 8 / 84 / 1,027 s |
| 4PL | 5 | -3.77 / 10.1 | -0.87 / 3.4 | -0.56 / 2.6 | -0.39 / 1.95 | -0.24 / 1.34 | 7.8 / 24 / 788 s |

At K = 5 the 16,384-point optimum itself sits -0.01 / 0.21 from the
65,536-point one, so pairs 7 + t3 there is as close as the usual reference.
Difficulties behave the same way (3PL K = 9, pairs 7 + t3: -0.01 / 0.36;
3PL K = 5: -0.09 / 0.31; 4PL K = 5: -0.08 / 1.03). No refinement with this rule ran away or was reverted
(30-60 evaluations each). On the 4PL the pairwise optimum does not improve
past 7 nodes (9 and 11: -0.49, -0.47), so what is left there is truncation;
each added order closes about half of it. Sharing nodes between groups cut
the K = 9 pairs-5 fit from 58.6 to 46.0 s at identical estimates.

Per person (`mdim_pairs.R`, against 65,536 points) the pairwise rule is as
accurate as second-order Laplace at the Laplace optimum and stays accurate at
the points where the others fail (the 256-point QMC runaway, the reference
optimum 7 SE away): K = 5 3PL rmse 0.004-0.007 against laplace2's 0.009-0.013
(max 0.098). QMC with the second-order Taylor polynomial as a control variate
(candidate 2 below) cut the error at the Laplace optimum but not elsewhere,
and was not built.

Still open: the 4PL residual (-0.39 / 1.95 at K = 5 with the default); a
5-node correction halves it at twice the cost. Only one seed per cell.

### What was ruled out on the way

- **QMC on fixed points** (antithetic Halton, Cranley-Patterson shift,
  inflation 1.15): 256 points at K = 9 ran away (693 evaluations); 4,096 left
  a correlation 145 SE off at 4,549 s (Laplace: 32 s). Only 16,384 points was
  trustworthy.
- **laplace2, unfloored**: up to 49 SE off at K = 9; one of two K = 2 fits
  diverged (the guard caught it). E T3^2 grows like a^6 where the 3PL
  curvature is not positive.
- **laplace2 with a curvature floor** w = max(w_obs, kappa w_F)
  (`options(bigIRT.laplace2_floor = kappa)`, off by default, tested): bounded
  and completes, but silently wrong -- 23 SE (K = 2) and 15 SE (K = 3) on
  single items. Pointwise it is accurate (per-person error 0.007 at K = 9).
  Silently wrong is worse than caught, hence off.
- **Newton steps from the Laplace optimum, metric fixed** (`onestep.R`): step
  1 halves the A/B bias but overshoots C by +0.9 SE and loses 11-190 nats
  against a predicted +20 to +38; step 2 diverges. With a line search it is
  the full refinement again; stopping early would be unstated shrinkage.
- **Additive 1-D corrections** (cut-HDMR, `add5_chol` in `mdim_rules.R`):
  bounded, but remove only ~70% of the per-person error at K = 9.
- **Mixture decomposition of the 3PL**: exact for one response, 2^k beyond.

- **QMC with a control variate** (the second-order Taylor polynomial, whose
  Gaussian expectation is the laplace2 correction exactly; `mdim_pairs.R`):
  per-person error at the Laplace optimum 0.0005 against plain QMC's 0.003,
  but no better than plain QMC at the runaway and reference points, which is
  where exploitation happens.

## Performance (dev2 uncontended, `main` before this work -> now)

M5 26 threads 22.9 -> 14.2 s; T2 26 threads 17.7 -> 11.1 s; C and F at 8
threads 4.5 -> 2.9 and 5.5 -> 3.2 s; K = 9 3PL Laplace 35.6 -> 10.6 s (dense
metric in C++). Metric build 0.32 -> 0.135 s at 8 threads (index-only
ownership pass, cached prior curvature). These predate the CSR index; re-time
with `timing_grid.sh`. Remaining serial pieces: ~2.2 s of R data preparation
before the optimiser; see `prof_split.R`, `rprof_fit.R`, `metric_split.R`.

## Workflow gotchas found here

- rstantools' `configure` regenerates `src/RcppExports.cpp` by scanning
  literal `.Call()` names in the R code. `.Call(entry, ...)` through a
  variable registers a symbol called `entry` ("undefined symbol: entry" at
  load). Keep literal `.Call`s, and copy the regenerated file back after a
  build on Linux.
- Building from a Windows checkout on Linux: `configure` has CRLF endings;
  `sed -i 's/\r$//' configure` first.
- The gradient tests must run on 3PL/4PL data, not only the 2PL, where
  observed and Fisher curvature coincide.
