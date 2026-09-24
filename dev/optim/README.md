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
    held fixed between metric refreshes. 9 / 7 / 5 nodes at K = 1 / 2 / 3.
  - `"laplace2"`: second-order Laplace, E T4 + E T3^2 / 2
    (`src/laplace2_backend.cpp`), exact reverse-mode gradient with a mode
    adjoint.
  - `"auto"`: no free asymptotes -> none at K = 1, laplace2 at K >= 2;
    free asymptotes -> agq at K <= 3, none at K >= 4 with a warning.
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

## 3PL/4PL at K >= 4: unsolved, and what has been ruled out

The problem is not pointwise accuracy but exploitability: the optimiser finds
where an approximate marginal is wrong.

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

Untried candidates:
1. Pairwise (many-body) expansion of the per-person log-marginal in 1-D and
   2-D AGQ terms: log I ~ log I_Lap + sum_k d_k + sum_{k<l} d_kl. Each term
   is a low-dimensional AGQ, which is not exploitable at K <= 3; cost at
   K = 9 with 5 nodes ~ 9*5 + 36*25 = 945 row evaluations a person. Unknown
   whether the three-way interactions are small enough in whitened
   coordinates -- check per person with `mdim_rules.R` first.
2. Importance sampling with the second-order correction as a control
   variate, to get 16,384-point accuracy from far fewer points.
3. Accept Laplace plus the warning (current behaviour).

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
