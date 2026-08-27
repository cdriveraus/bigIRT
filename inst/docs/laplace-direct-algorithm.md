# `bigIRT` Direct Laplace Algorithm

This note describes the current `marginalApprox = "laplace_direct"` path.

## Goal

The direct path optimizes a single Laplace-approximated marginal objective rather
than alternating separate person and item blocks as in `laplace_fast`.

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

## What is exact and what is approximate

The objective stated above is the one the implementation now evaluates. That was
not true until recently: the kernel accumulated the data log-likelihood, the
Gaussian constant and the log-determinant, but omitted \(\log p(\hat	heta_i)\)
-- the ability prior evaluated at the mode. Because \(\hat	heta_i\) moves with
\(\psi\), that omission was not a constant offset, and it removed the very term
that makes \(\partial \ell_i/\partial	heta_i\) vanish at the mode. The
reported value was therefore not the Laplace objective, and finite differences of
it disagreed with the analytic gradient by a factor that scaled with
\(d\hat	heta_i/d\psi\). See the `laplace-objective` vignette for the
diagnosis; the short version is that the gradient was right and the value was
incomplete.

For item parameters the gradient is now **exact** for this objective, including
the implicit dependence of the modes on \(\psi\), obtained with one adjoint
solve per person rather than one per item parameter. Verified against central
finite differences with the modes re-solved at every perturbation: correlation
1.000000, maximum absolute difference 8e-08, with each factor of the implicit
term also matching individually at ratio 1.0000.

Two approximations remain, and they are approximations of substance rather than
of bookkeeping:

- The curvature \(H_i\) uses expected (Fisher) information rather than the
  observed Hessian. For the 2PL these coincide exactly, so the distinction bites
  only for the 3PL and 4PL.
- The person modes are solved to a tolerance, not exactly, so the envelope
  argument holds only to that tolerance.

Every parameter block now carries its adjoint. Writing \(s_i\) for the score,
\(H_i\) for the person precision, \(Q\) for the prior precision, \(r_i\) for the
centred mode and \(g_i = \partial \log|H_i| / \partial\theta\), the mode
condition \(s_i - Q r_i = 0\) leaves \(\partial L/\partial r_i\) equal to
\(-g_i/2\) rather than zero, because the log determinant is not part of that
condition. Differentiating the mode condition and cancelling gives

\[
  \frac{\partial L}{\partial \beta} = \sum_i \Big[ s_i - \tfrac{1}{2} Q H_i^{-1} g_i \Big] x_i^{\top},
  \qquad
  \frac{\partial L}{\partial \mu} = \sum_i \Big[ Q r_i - \tfrac{1}{2} Q H_i^{-1} g_i \Big],
\]

the second being the first with the covariate row set to one, since the ability
mean is the intercept the regression coefficients are measured against. Both
agree with central finite differences of the objective, across one and two
scales, one and two predictors, and tight or loose beta priors.

So the optimizer sees a Laplace value and its exact gradient in every block:
item parameters, item and ability regression coefficients, the ability means,
and the latent correlations.

## Algorithm

For each optimizer evaluation at candidate \(\psi\):

1. Build current row-effective item parameters.
2. Solve each person mode \(\hat\theta_i(\psi)\).
3. Compute each person precision \(H_i(\psi)\) and the Laplace objective value.
4. Build the item gradient: the frozen-mode derivative plus the implicit
   correction through the mode map, which together are exact for the objective
   in step 3.
5. Return that value and gradient to the outer optimizer.

The outer optimizer is L-BFGS with warm starts.

## Interpretation

This is the only Laplace backend. It replaced an alternating one, kept for a
while under the name `laplace_fast`, which solved the person modes and then
optimised the item block with those modes held fixed. Head to head on the same
data, converged to the same objective, the alternating scheme needed about 85
item evaluations where this one needed 32: it restarted its inner optimiser at
every outer iteration and lost the accumulated curvature, and it spent the
extra evaluations refining item parameters against a posterior that was about
to move. It also paid a posterior refresh each outer iteration, about 16 per
cent of its runtime, which has no counterpart here.

The premise behind the alternation did not survive measurement either. It
existed to avoid re-solving the person modes, but with warm starts those solves
take about 2.2 Newton iterations and accounted for roughly 1 per cent of its
runtime. It was avoiding a cost that was not there.

The one place the alternating backend looked better was parameter recovery on
weakly identified designs -- 3PL, and sparse response patterns -- where it
returned lower RMSE despite a lower likelihood. That was early stopping rather
than a better estimator: driving it to converge harder moved its likelihood up
to this backend's and its recovery down to match, monotonically and across
seeds. The overfitting it accidentally protected against is real, but it
belongs to the priors, not to where an optimiser happens to stall.

## Status

This is the backend `marginalApprox = "laplace"` selects, and the one the
legacy names `"laplace_fast"` and `"laplace_direct"` now resolve to.

The remaining approximation is the curvature: \(H_i\) uses expected (Fisher)
information rather than the observed Hessian. The two coincide for the 2PL, so
the distinction bites only for the 3PL and 4PL.

Person and item predictors are both supported. Their gradients were checked
against central finite differences of the objective: the item beta slots agree
to about 2e-9, and the ability beta slots to about 1e-8.

## Latent-correlation gradient

The correlation parameters enter through the prior precision \(Q(\rho)\), which
appears in the prior quadratic, in the prior normalising constant, and inside
\(H_i = Q + I_i\); the solved mode moves with them as well. Decomposing the
derivative against finite differences gives four contributions, and the kernel
originally computed the first three:

1. the prior quadratic, \(-\tfrac12\sum_i r_i^\top (dQ) r_i\);
2. the prior normaliser, \(\tfrac{N}{2}\operatorname{tr}(Q^{-1} dQ)\);
3. the log determinants, \(-\tfrac12\sum_i \operatorname{tr}(H_i^{-1} dQ)\);
4. the adjoint, \(\tfrac12\sum_i g_i^\top H_i^{-1} (dQ) r_i\).

The fourth is linear in \(dQ\), since
\(g_i^\top \Sigma_i (dQ) r_i = \operatorname{tr}\!\big(dQ\, r_i g_i^\top \Sigma_i\big)\),
so it is added to the accumulated \(\partial L/\partial Q\) and rides the
existing chain rule rather than needing one of its own.

Two cautions for anyone checking this numerically. At \(\rho = 0\) the prior log
determinant is stationary, so terms 2 and 3 both vanish and a gradient missing
them still looks right; the check has to be run away from zero. And with more
than one correlation parameter the symmetrisation of \(\partial L/\partial Q\)
matters: term 4 is the only asymmetric contribution, and `A = A.transpose()`
aliases in Eigen, so it must go through a temporary.
