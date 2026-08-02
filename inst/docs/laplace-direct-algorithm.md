# `bigIRT` Direct Laplace Algorithm

This note describes the current `marginalApprox = "laplace_direct"` path.

## Goal

The direct path optimizes a single Laplace-approximated marginal objective rather
than alternating separate person and item blocks as in `laplace_em`.

For item/global parameters \(\psi\), define the person-specific conditional log
posterior

\[
\ell_i(\theta_i; \psi)
=
\log p(y_i \mid \theta_i, \psi)
 \log p(\theta_i).
\]

Let

\[
\hat\theta_i(\psi) = \arg\max_{\theta_i} \ell_i(\theta_i; \psi)
\]

and let

\[
H_i(\psi)
=
-
\frac{\partial^2 \ell_i(\theta_i; \psi)}
{\partial \theta_i \partial \theta_i^\top}
\Bigg|_{\theta_i = \hat\theta_i(\psi)}
\]

be the person-specific posterior precision.

The direct objective is

\[
\tilde L(\psi)
=
\sum_{i=1}^N
\left[
\ell_i(\hat\theta_i(\psi); \psi)
 \frac{K}{2}\log(2\pi)
 - \frac12 \log |H_i(\psi)|
\right]
 + \log \pi(\psi).
\]

## Current approximation

The current implementation recomputes the objective value above at each optimizer
evaluation, but it does **not** differentiate through the inner person solves.

For item parameters, it now uses:

- the exact direct Laplace objective value
- a mode-adjusted Laplace gradient: the frozen-mode item derivative plus the
  implicit derivative of the log-determinant through the resolved person mode
  map, computed with one adjoint solve per person

Ability-predictor and correlation derivatives still use their frozen-mode
approximations, and the curvature is expected-information rather than the
observed Hessian. Consequently the overall direct optimizer remains
experimental.

So the optimizer sees

- `fn`: the direct Laplace value
- `gr`: a mode-adjusted gradient for item parameters, with remaining
  global-parameter derivatives still approximate

This makes the method a practical single-stage approximation rather than an
exact direct Laplace optimizer.

## Algorithm

For each optimizer evaluation at candidate \(\psi\):

1. Build current row-effective item parameters.
2. Solve each person mode \(\hat\theta_i(\psi)\).
3. Compute each person precision \(H_i(\psi)\) and the Laplace objective value.
4. Build the approximate gradient by treating the resolved person modes as fixed.
5. Return the direct value plus the approximate gradient to the outer optimizer.

The outer optimizer is L-BFGS with warm starts.

## Interpretation

Relative to `laplace_em`:

- `laplace_em` optimizes a blockwise generalized-EM surrogate
- `laplace_direct` optimizes a single direct objective value, but with an
  approximate gradient

So `laplace_direct` is closer to a true one-step marginal optimizer in value,
but less exact in gradient.

## Status

This path is available as an alternative backend for experimentation and
comparison. It is useful when you want:

- a single-stage objective
- no explicit outer person/item alternation
- a direct comparison against `laplace_em`

But it should still be treated as approximate, because the gradient ignores the
dependence of the resolved person modes and Hessians on the item parameters.
