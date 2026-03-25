# Laplace Marginal Likelihood in `fitIRT()`

## Overview

`bigIRT::fitIRT()` now has two uncertainty-aware outer-loop paths:

- `marginalApprox = "laplace_em"`: the primary pure-C++ Laplace path

The new `laplace_em` path no longer calibrates items by averaging over sigma
points. Instead it approximates the person integral directly with a Laplace
approximation and optimizes a frozen-mode Laplace objective between person
refreshes.

## Target Approximation

For item/global parameters `xi` and person latent vectors `theta_j`, the
Laplace objective used by `laplace_em` is

\[
\tilde \ell(\xi)
=
\sum_{j=1}^N
\left[
\log p(y_j,\hat\theta_j \mid \xi)
+ \frac{K}{2}\log(2\pi)
- \frac{1}{2}\log |H_j(\xi)|
\right]
\]

where:

- `hat(theta_j)` is the current person posterior mode
- `H_j` is the person posterior precision at that mode

The current implementation is generalized EM rather than exact joint
optimization:

1. refresh person modes and precisions,
2. freeze those modes for the item update,
3. optimize the approximate Laplace objective in pure C++,
4. refresh again.

This removes the old sigma-point support from the active fitting path.

## Outer Loop

Each `laplace_em` outer iteration does:

1. **Initialization**
   Start from the ordinary Stan/JML fit. This gives a stable parameter state and
   keeps the old path available for comparison.

2. **Person Refresh**
   For fixed item parameters, update person posterior modes with a damped Newton
   step in C++.

3. **Laplace Precision**
   Recompute per-person posterior precision matrices and their Cholesky factors.
   The expected-information form is used so the precision is positive definite
   after small jitter if needed.

4. **Item Update**
   Optimize item-side raw parameters against the Laplace objective with person
   modes frozen. The C++ backend returns:
   - objective value
   - rowwise gradients for effective loadings
   - rowwise gradients for effective `b`, `c`, and `d`
   - log-determinant terms from each person precision

5. **Rescaling Guard**
   In the unmoderated case, the outer loop rescales the latent coordinates back
   toward the initialized JML scale. This prevents the frozen-mode approximation
   from drifting toward tiny discriminations and very large abilities.

6. **Convergence Check**
   The loop tracks:
   - relative objective change
   - item-step RMS movement
   - person-step RMS movement

   The current stopping rule declares convergence after repeated small changes
   across outer iterations.

## Backend Structure

The Laplace path is split into two C++ kernels:

- `bigIRT_laplace_person_step_cpp_impl()`
  - updates person modes
  - returns precision / Cholesky / log-determinant

- `bigIRT_laplace_item_objective_cpp_impl()`
  - evaluates the frozen-mode Laplace objective
  - returns rowwise gradients for effective row parameters

R code is responsible for:

- mapping raw parameter vectors to row-effective `loadings`, `b`, `c`, and `d`
- chaining rowwise gradients back to raw item parameters
- adding priors on raw parameter scales
- assembling final output objects

## Current Scope

`laplace_em` is intended as the future-facing uncertainty-aware backend.

Current practical notes:

- the pure-C++ Laplace path is the only path being extended
- item moderation is supported through row-effective parameter reconstruction
- person-predictor betas are currently kept fixed at their initialization values
  during `laplace_em`

## Output

When `marginalApprox = "laplace_em"`, the fit object stores:

- `fit$personPosterior$mode`
- `fit$personPosterior$precision`
- `fit$personPosterior$precision_chol`
- `fit$personPosterior$logdet_precision`
- `fit$laplaceStatus`
- optionally `fit$laplaceDiagnostics`

This path does not expose sigma-point support objects.
