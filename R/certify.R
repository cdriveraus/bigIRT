## Certifying a Laplace-path fit on the curvature of the objective it ended on.
##
## What the optimiser's own verdict can say, and what it cannot. The
## Fisher-shaped L-BFGS stops when it predicts less than `laplaceTol` nats still
## to gain, and it makes that prediction with an inverse Hessian seeded by the
## Fisher information of the marginal -- on an objective where F^-1 H has
## eigenvalues up to 2.3. checkConvergence()'s Newton decrement uses the
## block-diagonal item curvature the polish steps in, so it certifies its own
## model. And neither can see a transform that has gone flat: a latent
## correlation at tanh(20) = 1.000 reports a gradient of exactly zero and a
## curvature of exactly zero together, which is how the fit that motivated
## this came to report convergence with its correlation parked at the bound.
##
## What is certified instead is the quantity the optimiser is trying to make
## zero,
##
##     gap = 1/2 g' H^-1 g,
##
## with H the exact Hessian of the objective the fit ended on -- Laplace, or
## the refined marginal when `laplaceRefine` ran. It is in nats, it is
## invariant to any linear reparameterisation, and under the local quadratic
## model it is the objective still available. lambda = sqrt(2 gap) is the
## length of the Newton step in the information metric: the joint displacement
## in standard errors.
##
## The arithmetic below -- the eigen split into trusted and flat directions,
## the probe of the flat ones, the verdicts -- is ctsem's
## (R/ctBackendOptimGap.R: .ctBackendInformationSplit, .ctBackendOptimGap,
## .ctBackendOptimGapProbe, .ctBackendCertify, 2026-09-25), copied rather than
## shared, as are its matrix tests (tests/testthat/test-certify.R). Two copies
## of thirty lines with a test each are cheaper than a dependency between two
## packages on different release cycles. Where the copy departs from ctsem it
## says so.
##
## ## The Hessian
##
## Central differences (step 1e-4) of the analytic gradient in every free
## coordinate, in the optimiser's coordinates. On a quadrature-refined fit the
## nodes are held at the fitted centre, where the Fisher-identity gradient is
## the exact gradient of a smooth function and no person mode is re-solved; on
## a Laplace or second-order Laplace fit every person mode is re-solved to
## 1e-10 at each perturbed point, since otherwise the quotient measures the
## frozen-mode curvature (see itemInformation()). Measured on a 40-item 3PL
## refined by quadrature, the two ways of treating the nodes gave standard
## errors equal to 1e-4, and the asymmetry of the differenced matrix -- its
## numerical error -- was 3e-7 with fixed nodes against 2e-2 with re-solved
## modes, in a matrix of norm 2e4.
##
## Every column is a full gradient, so the coupling between items through
## their shared persons costs nothing to keep, and up to
## `bigIRT.certify_dense_max` free parameters (default 2000) the whole matrix
## is kept and nothing is dropped. Above that only each item's own block, the
## block of the parameters shared across items (mean hyperparameters, latent
## correlations, covariate coefficients, the ability mean) and the coupling
## between those two are kept; items then meet each other only through the
## Schur complement of the shared block. What is dropped is the coupling
## between two items through the persons who answered both. Its effect,
## measured where the full matrix is affordable, is in round 4 of the
## optimisation review.
##
## The cost is two gradient evaluations per free parameter whichever form is
## kept -- dropping the coupling saves memory and a factorisation, not
## evaluations -- so it is gated the way itemInformation(method = "auto") is:
## one evaluation is timed, and the curvature is computed only if the
## projection fits `laplaceCertify` seconds. A skipped certificate says so, and
## `converged` is then the optimiser's verdict.
##
## ## Numerical noise
##
## A differenced Hessian is not exact, and an eigenvalue inside its error
## cannot be classified: a direction of genuine but tiny curvature can come out
## negative and read as a saddle. So the split takes a noise level, the
## Frobenius norm of the antisymmetric part (H - H')/2, which bounds the
## spectral error of the symmetrised matrix to within a small factor. An
## eigenvalue within three noise levels of zero is flat rather than trusted
## or negative, and flat directions are probed rather than believed. ctsem's
## Hessian is exact and its split takes no such argument; with noise = 0 the
## copy here is ctsem's.
##
## ## Saturated transforms
##
## Discrimination is softplus(raw), the asymptotes are logistic(raw)/2 and
## logistic(raw)/2 + 1/2, the latent correlations tanh or a normalised
## Cholesky factor of raw coordinates. Each has a tail where its derivative
## underflows, and there the gradient and the curvature vanish together, so
## neither the gap nor a probe along the gradient can see the coordinate. A
## coordinate is saturated when its transform's derivative, relative to the
## same derivative at zero, is below `bigIRT.saturation_floor` (default 1e-3:
## a correlation beyond 0.9995, a discrimination below 5e-4, a guessing or
## slipping asymptote within 1.25e-4 of its bound). Saturated coordinates are
## left out of the Hessian and pulled back instead -- to 1/2, 1/4, 1/8 and 0 of
## their raw value, one block at a time and each correlation alone -- and the
## objective looked at. If pulling back gains, the optimiser stopped in the
## flat tail (ctsem's `overshot`); if it does not, the coordinate is a boundary
## estimate. The pull-back costs four evaluations a block and runs whatever
## the budget says about the curvature, since it is the one check that catches
## the saturated correlation.
##
## ## Verdicts
##
##   certified      the gap is below tolerance and the flat directions hold
##                  nothing material
##   suboptimal     the gap says the optimum is measurably above the estimate
##   notstationary  stepping along a direction with no curvature gains more
##                  than the tolerance: the gap cannot see it, the probe did
##   saturated      everything else is certified, and a transform sits on its
##                  flat tail where pulling it back gains nothing: a boundary
##                  estimate, whose standard error means nothing
##   notmaximum     negative curvature, or a saturated transform whose
##                  pull-back gains: the optimiser stopped in a flat tail
##   skipped        the curvature was not computed within the budget
##   unknown        the curvature could not be decomposed
##
## `converged` follows: TRUE for certified and saturated, FALSE for suboptimal,
## notstationary and notmaximum, and the optimiser's own verdict for skipped
## and unknown. ctsem reports a saturated coordinate ("unidentified") ahead of
## a suboptimal gap; here the saturated coordinates are outside the gap, so a
## suboptimal remainder is reported as suboptimal.
##
## The tolerance is ten times `laplaceTol`: the optimiser aims one order inside
## the bar, as ctsem's aims two (there the inner rule is a limited-memory
## estimate of the same quantity; here it is too, seeded by Fisher). Round 2
## measured fits stopped at a predicted 1e-4 ending at most 2e-4 short of a
## tight reference.

## --- ctsem's arithmetic -------------------------------------------------------

## The one rule for "this direction has no curvature", ctsem's constant.
bigIRT_flat_rtol <- function() 1e-12

## Eigen-decomposition of the information, with the trusted subspace marked.
## `hessian` is the Hessian of the maximised objective, so -(H + H')/2 is the
## information. `noise` widens the band around zero that is neither trusted nor
## negative (see the header); zero reproduces ctsem.
bigIRT_information_split <- function(hessian, rtol = bigIRT_flat_rtol(),
                                     negative = 1e-8, noise = 0){
  if(is.null(hessian)) return(NULL)
  hessian <- as.matrix(hessian)
  if(nrow(hessian) != ncol(hessian) || !nrow(hessian)) return(NULL)
  if(!all(is.finite(hessian))) return(NULL)
  information <- -(hessian + t(hessian)) / 2
  decomposition <- try(eigen(information, symmetric = TRUE), silent = TRUE)
  if(inherits(decomposition, "try-error")) return(NULL)
  values <- decomposition$values
  band <- 3 * max(as.numeric(noise)[1], 0, na.rm = TRUE)
  scale <- max(values)
  if(!is.finite(scale) || scale <= 0){
    ## Not a maximum in any direction: no trusted subspace at all, and the
    ## caller needs to hear that rather than an empty gap.
    scale <- max(abs(values))
    if(!is.finite(scale) || scale <= 0) return(NULL)
    return(list(values = values, vectors = decomposition$vectors,
      scale = scale, trusted = rep(FALSE, length(values)),
      negative = values < -max(negative * scale, band)))
  }
  list(values = values, vectors = decomposition$vectors, scale = scale,
    trusted = values > max(rtol * scale, band),
    negative = values < -max(negative * scale, band))
}

## The predicted remaining objective, and what is outside it: the gap over the
## trusted subspace, the Newton displacement that would realise it, and the
## gradient component the projection left behind -- as a vector, because the
## caller measures it rather than comparing its norm to anything.
bigIRT_optim_gap <- function(hessian, gradient, rtol = bigIRT_flat_rtol(),
                             negative = 1e-8, noise = 0){
  gradient <- as.numeric(gradient)
  split <- bigIRT_information_split(hessian, rtol = rtol, negative = negative,
    noise = noise)
  empty <- list(gap = NA_real_, lambda = NA_real_, step = NULL,
    residual = NULL, residual_norm = NA_real_, ntrusted = NA_integer_,
    nflat = NA_integer_, nnegative = NA_integer_, ok = FALSE)
  if(is.null(split) || length(gradient) != length(split$values)) return(empty)
  if(!all(is.finite(gradient))) return(empty)
  keep <- split$trusted
  vectors <- split$vectors[, keep, drop = FALSE]
  values <- split$values[keep]
  ## The gap is a sum of squares over curvature, which is where the invariance
  ## comes from: rescale a parameter and the coefficient and the eigenvalue
  ## move to compensate.
  coefficients <- as.numeric(crossprod(vectors, gradient))
  gap <- if(!length(values)) 0 else 0.5 * sum(coefficients^2 / values)
  step <- if(!length(values)) rep(0, length(gradient)) else
    as.numeric(vectors %*% (coefficients / values))
  residual <- gradient - as.numeric(vectors %*% coefficients)
  list(gap = gap, lambda = sqrt(2 * max(gap, 0)), step = step,
    residual = residual, residual_norm = sqrt(sum(residual^2)),
    ntrusted = sum(keep), nflat = sum(!keep & !split$negative),
    nnegative = sum(split$negative),
    negative_vector = if(any(split$negative))
      split$vectors[, which.min(split$values)] else NULL,
    lambda_min = if(length(values)) min(values) else NA_real_, ok = TRUE)
}

## What the excluded directions are actually worth: step along the direction
## and look, since a norm of the leftover gradient has no units that mean
## anything and in a flat direction the quadratic model that would convert it
## is the one that does not hold. Lengths are in the optimiser's coordinates,
## where the item parameters are on their own scale and each shared block is
## rescaled to an item's curvature (bigIRT_laplace_par_scale). The best actual
## improvement is reported; an objective that cannot be evaluated at a trial
## point is a step not taken, not a failed certification.
bigIRT_optim_gap_probe <- function(evaluate, at, direction, value,
                                   lengths = c(0.25, 1, 4)){
  norm <- sqrt(sum(direction^2))
  if(!is.finite(norm) || norm <= 0) return(list(gain = 0, length = 0, ok = TRUE))
  unit <- direction / norm
  best <- 0; bestlength <- 0
  for(len in lengths){
    probe <- try(evaluate(at + len * unit), silent = TRUE)
    if(inherits(probe, "try-error")) next
    probe <- as.numeric(probe)[1L]
    if(!is.finite(probe)) next
    if(probe - value > best){ best <- probe - value; bestlength <- len }
  }
  list(gain = best, length = bestlength, ok = TRUE)
}

## A list of coordinate names for a sentence: the first few, then a count.
bigIRT_name_list <- function(x, show = 4L){
  if(length(x) <= show + 1L) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(show)], collapse = ", "), " and ", length(x) - show, " more")
}

## The verdict. Takes no convergence flag by construction: the question is
## about the curvature, not about how the optimiser felt when it stopped.
## `saturated` names the coordinates left out as saturated, `overshoot` is
## their pull-back probe.
bigIRT_certify_verdict <- function(gap, probe = NULL, tolerance = 1e-3,
                                   saturated = character(0), overshoot = NULL){
  if(is.null(gap) || !isTRUE(gap$ok))
    return(list(status = "unknown", certified = FALSE,
      reason = "the curvature at the estimate could not be decomposed"))
  residual_gain <- if(is.null(probe)) NA_real_ else as.numeric(probe$gain)[1L]
  pulled <- if(is.null(overshoot)) NA_real_ else as.numeric(overshoot$gain)[1L]
  if(is.finite(pulled) && pulled > tolerance)
    return(list(status = "notmaximum", certified = FALSE,
      reason = paste0("pulling ", bigIRT_name_list(overshoot$moved),
        " back from the flat end of ", if(length(overshoot$moved) > 1L) "their transforms"
        else "its transform", " gains ", signif(pulled, 3), " nats: the optimiser ",
        "stopped where the gradient had underflowed")))
  if(isTRUE(gap$nnegative > 0L))
    return(list(status = "notmaximum", certified = FALSE,
      reason = paste0(gap$nnegative, " direction",
        if(gap$nnegative > 1L) "s have" else " has",
        " negative curvature, so this point is not a maximum")))
  if(is.finite(residual_gain) && residual_gain > tolerance)
    return(list(status = "notstationary", certified = FALSE,
      reason = paste0("stepping along the directions with no curvature gains ",
        signif(residual_gain, 3), " nats, so the estimate is not stationary in a ",
        "direction the data do not determine")))
  if(!is.finite(gap$gap))
    return(list(status = "unknown", certified = FALSE,
      reason = "the predicted gap is not a number"))
  if(gap$gap > tolerance)
    return(list(status = "suboptimal", certified = FALSE,
      reason = paste0("the optimum is about ", signif(gap$gap, 3),
        " nats above this estimate")))
  if(length(saturated))
    return(list(status = "saturated", certified = FALSE,
      reason = paste0(bigIRT_name_list(saturated),
        if(length(saturated) > 1L) " sit" else " sits",
        " on the flat end of ", if(length(saturated) > 1L) "their transforms" else
        "its transform", ", and pulling back gains nothing: a boundary estimate, ",
        "whose standard error means nothing. The rest is within ",
        signif(tolerance, 3), " nats of the optimum")))
  list(status = "certified", certified = TRUE,
    reason = paste0("the optimum is within ", signif(tolerance, 3),
      " nats of this estimate"))
}

## What `converged` becomes under each verdict; NA keeps the optimiser's.
bigIRT_certificate_converged <- function(status){
  if(!length(status) || is.na(status[1L])) return(NA)
  switch(status[1L], certified = TRUE, saturated = TRUE, suboptimal = FALSE,
    notstationary = FALSE, notmaximum = FALSE, NA)
}

## One line for print(). The gap goes out through signif(), never through a
## fixed number of decimals: ctsem found a certified gap of 1.9e-04 printed as
## 0 by a summary that rounded to three places, a measurement turned into a
## claim of exactness.
bigIRT_certificate_line <- function(cert){
  status <- cert$status %||% "unknown"
  if(identical(status, "certified"))
    return(sprintf("Certificate: certified -- %s nats of predicted gain left, under %s (%s curvature, %d evaluations)",
      format(signif(cert$gap, 2)), format(signif(cert$tolerance, 2)),
      cert$route %||% "exact", as.integer(cert$evaluations %||% NA_integer_)))
  sprintf("Certificate: %s -- %s", status, cert$reason %||% "")
}

## The gap when the item-item coupling is dropped: items meet only through the
## shared block. `blocks` lists each item's positions (within the coordinates
## the Hessian covers), `shared` the rest. H is never formed: each item block is
## split on its own, the shared block through its Schur complement
## S = C - B' A^+ B, and by Sylvester's law the inertia of the whole is the
## inertia of the pieces. One scale -- the largest eigenvalue anywhere -- and
## one noise band for every piece, so a piece is flat by the same rule the
## dense split applies.
##
## `Hblocks[[j]]` is item j's block of the Hessian (maximised objective),
## `Hcross[[j]]` its rows against the shared coordinates, `Hshared` the shared
## block. The residual is returned as a vector over all covered coordinates:
## an item block's gradient outside its trusted eigenvectors, and for a flat
## direction e of S the direction (-A^+ B e, e) it stands for.
bigIRT_optim_gap_blocks <- function(Hblocks, Hcross, Hshared, blocks, shared,
                                    gradient, rtol = bigIRT_flat_rtol(),
                                    negative = 1e-8, noise = 0){
  gradient <- as.numeric(gradient)
  empty <- list(gap = NA_real_, lambda = NA_real_, residual = NULL,
    residual_norm = NA_real_, ntrusted = NA_integer_, nflat = NA_integer_,
    nnegative = NA_integer_, ok = FALSE)
  m <- length(shared)
  info <- lapply(Hblocks, function(H) -(as.matrix(H) + t(as.matrix(H))) / 2)
  C <- if(m) -(as.matrix(Hshared) + t(as.matrix(Hshared))) / 2 else matrix(0, 0, 0)
  if(!all(is.finite(gradient)) || !all(vapply(info, function(x) all(is.finite(x)), TRUE)) ||
     !all(is.finite(C))) return(empty)
  eig <- lapply(info, function(A) eigen(A, symmetric = TRUE))
  scale <- max(c(unlist(lapply(eig, `[[`, "values")),
    if(m) eigen(C, symmetric = TRUE, only.values = TRUE)$values))
  if(!is.finite(scale) || scale <= 0) return(empty)
  band <- 3 * max(as.numeric(noise)[1], 0, na.rm = TRUE)
  tr_lo <- max(rtol * scale, band); ng_lo <- -max(negative * scale, band)
  residual <- numeric(length(gradient))
  gap <- 0; ntr <- 0L; nng <- 0L; nfl <- 0L; lmin <- Inf
  gS <- if(m) gradient[shared] else numeric(0)
  S <- C
  for(j in seq_along(blocks)){
    ix <- blocks[[j]]; e <- eig[[j]]
    keep <- e$values > tr_lo
    ntr <- ntr + sum(keep); nng <- nng + sum(e$values < ng_lo)
    nfl <- nfl + sum(!keep & !(e$values < ng_lo))
    U <- e$vectors[, keep, drop = FALSE]; v <- e$values[keep]
    if(length(v)) lmin <- min(lmin, v)
    gj <- gradient[ix]
    cj <- as.numeric(crossprod(U, gj))
    gap <- gap + if(length(v)) 0.5 * sum(cj^2 / v) else 0
    residual[ix] <- gj - as.numeric(U %*% cj)
    if(m){
      Bj <- -as.matrix(Hcross[[j]])                 # item rows x shared
      AiB <- U %*% (crossprod(U, Bj) / v)           # A_j^+ B_j
      S <- S - crossprod(Bj, AiB)
      gS <- gS - as.numeric(crossprod(AiB, gj))
    }
  }
  if(m){
    S <- (S + t(S)) / 2
    es <- eigen(S, symmetric = TRUE)
    keep <- es$values > tr_lo
    ntr <- ntr + sum(keep); nng <- nng + sum(es$values < ng_lo)
    nfl <- nfl + sum(!keep & !(es$values < ng_lo))
    U <- es$vectors[, keep, drop = FALSE]; v <- es$values[keep]
    if(length(v)) lmin <- min(lmin, v)
    cs <- as.numeric(crossprod(U, gS))
    gap <- gap + if(length(v)) 0.5 * sum(cs^2 / v) else 0
    flat <- gS - as.numeric(U %*% cs)
    if(any(flat != 0)){
      residual[shared] <- flat
      for(j in seq_along(blocks)){
        ix <- blocks[[j]]; e <- eig[[j]]; k2 <- e$values > tr_lo
        U2 <- e$vectors[, k2, drop = FALSE]
        if(!ncol(U2)) next
        Bj <- -as.matrix(Hcross[[j]])
        residual[ix] <- residual[ix] -
          as.numeric(U2 %*% (crossprod(U2, Bj %*% flat) / e$values[k2]))
      }
    }
  }
  list(gap = gap, lambda = sqrt(2 * max(gap, 0)), residual = residual,
    residual_norm = sqrt(sum(residual^2)), ntrusted = ntr, nflat = nfl,
    nnegative = nng, lambda_min = if(is.finite(lmin)) lmin else NA_real_, ok = TRUE)
}

## --- bigIRT's transforms --------------------------------------------------------

## The largest `x` in each group `g` of 1..n; `empty` where a group has none.
bigIRT_group_max <- function(x, g, n, empty = 0){
  out <- rep(empty, n)
  if(!length(x)) return(out)
  r <- tapply(x, g, max)
  out[as.integer(names(r))] <- as.numeric(r)
  out
}

## Relative slope of each free coordinate's transform at `state`: the
## derivative of what the likelihood sees with respect to the coordinate,
## divided by the same derivative at zero. 1 is a transform in its live range
## (and every identity coordinate); near 0 one whose output no longer moves.
## A coordinate that reaches several rows -- a discrimination answered by many
## persons, with item covariates moving its raw value by row -- takes its
## largest row, as in ctsem: one row still responding means the coordinate
## still does something to the likelihood.
bigIRT_transform_slopes <- function(state, sdat, layout, context,
                                    corr_paramization = "normalized_chol"){
  n <- bigIRT_laplace_layout_size(layout)
  out <- rep(1, n)
  rc <- context$row_context
  if(length(layout$A) || length(layout$C) || length(layout$D)){
    re <- bigIRT_laplace_row_effective(state = state, sdat = sdat,
      rows = context$train_rows, include_raw = TRUE, context = rc)
    if(length(layout$A)){
      best <- numeric(length(layout$A))
      for(k in seq_len(ncol(rc$A_ref))){
        ref <- rc$A_ref[, k]; hit <- ref > 0L & ref <= length(layout$A)
        if(!any(hit)) next
        ## softplus: slope plogis(raw), 1/2 at zero.
        best <- pmax(best, bigIRT_group_max(2 * stats::plogis(re$a_linear[hit, k]),
          ref[hit], length(layout$A)))
      }
      out[layout$A] <- best
    }
    if(length(layout$C)){
      hit <- !rc$C_fixed & rc$C_ref > 0L
      cc <- re$c_row[hit]
      ## c = logistic(x)/2: slope c(1 - 2c), 1/8 at zero.
      out[layout$C] <- bigIRT_group_max(8 * cc * (1 - 2 * cc), rc$C_ref[hit], length(layout$C))
    }
    if(length(layout$D)){
      hit <- !rc$D_fixed & rc$D_ref > 0L
      dd <- re$d_row[hit]
      ## d = logistic(x)/2 + 1/2: slope (2d - 1)(1 - d), 1/8 at zero.
      out[layout$D] <- bigIRT_group_max(8 * (2 * dd - 1) * (1 - dd), rc$D_ref[hit], length(layout$D))
    }
  }
  if(length(layout$corr)){
    K <- as.integer(sdat$Nscales)
    cp <- as.numeric(state$AbilityCorrPars)
    slope <- function(p, j, h = 1e-6){
      up <- p; dn <- p; up[j] <- up[j] + h; dn[j] <- dn[j] - h
      max(abs(bigIRT_laplace_corr_from_packed(up, K, paramization = corr_paramization) -
        bigIRT_laplace_corr_from_packed(dn, K, paramization = corr_paramization))) / (2 * h)
    }
    zero <- numeric(length(cp))
    out[layout$corr] <- vapply(seq_along(cp), function(j)
      slope(cp, j) / max(slope(zero, j), 1e-300), numeric(1))
  }
  out
}

## A readable name for each free coordinate, from the layout: "A[12]", "corr[1]".
bigIRT_coordinate_names <- function(layout){
  n <- bigIRT_laplace_layout_size(layout)
  out <- paste0("par[", seq_len(n), "]")
  for(b in names(layout)){
    ix <- layout[[b]]
    if(length(ix)) out[ix] <- paste0(b, "[", seq_along(ix), "]")
  }
  out
}

## Pull the saturated coordinates back towards the live range and look: each
## block's saturated coordinates together, each latent correlation alone, to
## 1/2, 1/4, 1/8 and 0 of their raw value. The optimiser's coordinates are the
## natural ones divided by a constant, so scaling one scales the other.
bigIRT_certify_pullback <- function(evaluate, par, saturated, layout, value,
                                    fractions = c(1/2, 1/4, 1/8, 0)){
  if(!length(saturated)) return(NULL)
  names_all <- bigIRT_coordinate_names(layout)
  block_of <- rep(NA_character_, length(par))
  for(b in names(layout)) if(length(layout[[b]])) block_of[layout[[b]]] <- b
  grp <- ifelse(block_of[saturated] == "corr", paste0("corr", saturated),
    block_of[saturated])
  best <- 0; moved <- character(0)
  for(gname in unique(grp)){
    ix <- saturated[grp == gname]
    for(f in fractions){
      p <- par; p[ix] <- par[ix] * f
      v <- try(evaluate(p), silent = TRUE)
      if(inherits(v, "try-error")) next
      v <- as.numeric(v)[1L]
      if(is.finite(v) && v - value > best){
        best <- v - value; moved <- names_all[ix]
      }
    }
  }
  list(gain = best, moved = moved, groups = length(unique(grp)))
}

## --- the certificate -------------------------------------------------------------

## The certificate for one fit, at its final point.
##
## `final` is the fit's last evaluation at `par`, with `context` as the fit
## left it (quadrature rule set and its centre cleared when a refinement ran,
## `laplace2` set when second-order Laplace ran). `state`, `sdat`,
## `prior_precision` and `jitter` are what the optimiser evaluated with.
## `budget` is seconds; 0 turns the curvature off (the saturation check still
## runs), Inf insists.
bigIRT_certify <- function(final, par, context, state, sdat, prior_precision,
                           layout, jitter = 1e-6, budget = 60, tolerance = 1e-3,
                           eps = 1e-4, person_tol = 1e-10,
                           floor = getOption("bigIRT.saturation_floor", 1e-3),
                           dense_max = getOption("bigIRT.certify_dense_max", 2000)){
  clock <- function() as.numeric(proc.time()[["elapsed"]])
  t_start <- clock()
  n <- length(par)
  agq <- !is.null(context$agq)
  theta0 <- final$posterior$theta_mode
  ## Held nodes for the curvature, re-centred ones for every value: a trial
  ## point a long way from the fit would otherwise be integrated with nodes
  ## placed for somewhere else.
  ctx_h <- context
  if(agq) ctx_h$agq_centre <- final$posterior
  ctx_v <- context; ctx_v$agq_centre <- NULL
  nevals <- 0L
  run <- function(p, ctx){
    nevals <<- nevals + 1L
    bigIRT_laplace_direct_objective(par = p, state = state, sdat = sdat,
      prior_precision = prior_precision, theta_init = theta0, jitter = jitter,
      max_attempts = 8L, max_iter = 500L, tol = person_tol,
      keep_covariance = TRUE, context = ctx)
  }
  value_at <- function(p) run(p, ctx_v)$value
  names_all <- bigIRT_coordinate_names(layout)
  out <- list(status = "skipped", certified = FALSE, converged = NA,
    reason = NA_character_, tolerance = tolerance, budget = budget,
    marginal = if(agq) "agq" else if(isTRUE(context$laplace2)) "laplace2" else "laplace",
    gap = NA_real_, lambda = NA_real_, ntrusted = NA_integer_, nflat = NA_integer_,
    nnegative = NA_integer_, lambda_min = NA_real_, noise = NA_real_,
    residual_gain = NA_real_, overshoot_gain = NA_real_, saturated = character(0),
    route = NA_character_, evaluations = 0L, seconds = NA_real_,
    projected_seconds = NA_real_)

  t0 <- clock()
  e0 <- run(par, ctx_h)
  t_eval <- max(clock() - t0, 1e-4)
  g <- as.numeric(e0$approx_grad)
  v0 <- if(agq) value_at(par) else e0$value
  slopes <- bigIRT_transform_slopes(e0$state, sdat, layout, context,
    corr_paramization = context$corr_paramization %||% "normalized_chol")
  sat <- which(slopes < floor)
  out$saturated <- names_all[sat]
  overshoot <- NULL
  if(length(sat)){
    overshoot <- bigIRT_certify_pullback(value_at, par, sat, layout, v0)
    out$overshoot_gain <- overshoot$gain
  }
  keep <- setdiff(seq_len(n), sat)
  m <- length(keep)
  out$projected_seconds <- 2 * m * t_eval
  finish <- function(res){
    res$evaluations <- nevals; res$seconds <- clock() - t_start
    res$converged <- bigIRT_certificate_converged(res$status)
    res
  }
  budget <- suppressWarnings(as.numeric(budget)[1L])
  if(is.na(budget) || budget < 0) budget <- 0
  out$budget <- budget
  if(out$projected_seconds > budget){
    if(isTRUE(overshoot$gain > tolerance)){
      v <- bigIRT_certify_verdict(list(ok = TRUE, gap = 0, nnegative = 0L), NULL,
        tolerance = tolerance, saturated = out$saturated, overshoot = overshoot)
      out$status <- v$status; out$certified <- v$certified; out$reason <- v$reason
    } else {
      out$reason <- sprintf(paste0("the exact curvature would need %d evaluations, about ",
        "%.0f s against a budget of %s s (laplaceCertify)"), 2L * m,
        out$projected_seconds, format(budget))
    }
    return(finish(out))
  }

  ## The columns. Dense: the whole matrix over the kept coordinates. Blocks:
  ## each item coordinate keeps its own item's rows and the shared rows, each
  ## shared coordinate its full column.
  dense <- m <= dense_max
  out$route <- if(dense) "dense" else "blockschur"
  col <- function(k){
    up <- par; up[k] <- up[k] + eps; dn <- par; dn[k] <- dn[k] - eps
    (run(up, ctx_h)$approx_grad - run(dn, ctx_h)$approx_grad) / (2 * eps)
  }
  gk <- g[keep]
  if(dense){
    H <- matrix(NA_real_, m, m)
    for(i in seq_len(m)) H[, i] <- col(keep[i])[keep]
    out$noise <- sqrt(sum(((H - t(H)) / 2)^2))
    gap <- bigIRT_optim_gap(H, gk, noise = out$noise)
  } else {
    item_blocks <- intersect(c("B", "A", "C", "D"), names(layout))
    item_blocks <- item_blocks[vapply(item_blocks, function(b)
      length(layout[[b]]) == as.integer(sdat$Nitems), logical(1))]
    ## Positions within `keep`, per item.
    pos <- match(seq_len(n), keep)
    blocks <- lapply(seq_len(as.integer(sdat$Nitems)), function(j){
      ix <- pos[vapply(item_blocks, function(b) layout[[b]][j], integer(1))]
      ix[!is.na(ix)]
    })
    blocks <- blocks[lengths(blocks) > 0L]
    shared <- setdiff(seq_len(m), unlist(blocks))
    Hblocks <- vector("list", length(blocks)); Hcross <- Hblocks
    Hshared <- matrix(NA_real_, length(shared), length(shared))
    asym <- 0
    for(j in seq_along(blocks)){
      ix <- blocks[[j]]; B <- matrix(NA_real_, length(ix), length(ix))
      X <- matrix(NA_real_, length(shared), length(ix))
      for(a in seq_along(ix)){
        cc <- col(keep[ix[a]])[keep]
        B[, a] <- cc[ix]; X[, a] <- cc[shared]
      }
      asym <- asym + sum(((B - t(B)) / 2)^2)
      Hblocks[[j]] <- B; Hcross[[j]] <- t(X)
    }
    Xs <- matrix(NA_real_, m, length(shared))
    for(a in seq_along(shared)) Xs[, a] <- col(keep[shared[a]])[keep]
    Hshared <- Xs[shared, , drop = FALSE]
    asym <- asym + sum(((Hshared - t(Hshared)) / 2)^2)
    ## The shared columns hold the coupling twice, once from each side.
    for(j in seq_along(blocks)){
      d2 <- Hcross[[j]] - Xs[blocks[[j]], , drop = FALSE]
      asym <- asym + 2 * sum((d2 / 2)^2)
      Hcross[[j]] <- (Hcross[[j]] + Xs[blocks[[j]], , drop = FALSE]) / 2
    }
    out$noise <- sqrt(asym)
    gap <- bigIRT_optim_gap_blocks(Hblocks, Hcross, Hshared, blocks, shared,
      gk, noise = out$noise)
  }
  out$gap <- gap$gap; out$lambda <- gap$lambda; out$ntrusted <- gap$ntrusted
  out$nflat <- gap$nflat; out$nnegative <- gap$nnegative; out$lambda_min <- gap$lambda_min
  probe <- NULL
  ## Only when a direction was excluded: with every direction trusted the
  ## residual is rounding, and ctsem's `residual_norm > 0` alone spent three
  ## evaluations probing it.
  if(isTRUE(gap$ok) && isTRUE(gap$nflat > 0L) && isTRUE(gap$residual_norm > 0)){
    direction <- numeric(n); direction[keep] <- gap$residual
    probe <- bigIRT_optim_gap_probe(value_at, par, direction, v0)
    out$residual_gain <- probe$gain
  }
  v <- bigIRT_certify_verdict(gap, probe, tolerance = tolerance,
    saturated = out$saturated, overshoot = overshoot)
  out$status <- v$status; out$certified <- v$certified; out$reason <- v$reason
  ## The covariance of the item blocks and of the shared block, from the full
  ## inverse, for the standard errors (itemInformation(method = "exact")):
  ## kept only when every direction was trusted, since an inverse over the
  ## trusted subspace would hand a flat direction a finite standard error.
  if(dense && isTRUE(gap$ok) && isTRUE(gap$ntrusted == m)){
    V <- tryCatch(solve(-(H + t(H)) / 2), error = function(e) NULL)
    if(!is.null(V)) out$covariance <- bigIRT_certificate_covariance(V, keep,
      layout, sdat, names_all)
  }
  finish(out)
}

## itemInformation(method = "exact"): each item block of the full inverse of the
## exact Hessian of the fitted objective, so every item's standard error allows
## for the other items and the shared parameters being estimated too -- the
## marginal variance, where "hessian" inverts the item's own block alone and
## gives the conditional one. Read from the certificate when it kept the
## covariance, which needs neither internals nor any evaluation; computed
## afresh otherwise, at the cost of the certificate's curvature.
bigIRT_iteminfo_exact <- function(fit, cores = 1L){
  cov <- fit$laplaceStatus$certificate$covariance
  if(is.null(cov$items)){
    if(is.null(fit$internals))
      stop("This fit kept no exact covariance (its certificate was skipped or off) and ",
        "carries no prepared data to compute one. Refit with `keepInternals = TRUE`.")
    d <- bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE, cores = cores)
    ce <- bigIRT_certify(final = d$eval, par = d$par, context = d$context,
      state = fit$internals$state, sdat = d$sdat,
      prior_precision = fit$internals$priorPrecision, layout = d$layout,
      budget = Inf, tolerance = fit$laplaceStatus$certificate$tolerance %||% 1e-3)
    cov <- ce$covariance
    if(is.null(cov$items))
      stop("The exact curvature of this fit cannot be inverted for standard errors (",
        ce$status, ": ", ce$reason, "). A direction with no curvature would get a ",
        "finite standard error from an inverse over the rest; see ",
        "itemInformation(method = \"hessian\") for the item blocks alone.")
  }
  V <- cov$items
  active <- dimnames(V)[[1]]; P <- length(active); ni <- dim(V)[3]
  sdat <- fit$prepared
  prior_sd <- c(B = "BSDx", A = "invspASD", C = "logitCSD", D = "logitDSD")
  prec <- matrix(0, nrow = P, ncol = ni, dimnames = list(active, NULL))
  if(isTRUE(as.logical(sdat$dopriors))) for(b in active){
    sd_b <- as.numeric(sdat[[prior_sd[[b]]]])
    if(length(sd_b)) prec[b, ] <- 1 / rep_len(pmax(sd_b, 1e-8), ni)^2
  }
  se <- matrix(NA_real_, ni, P, dimnames = list(NULL, active))
  edf <- se
  info <- array(NA_real_, c(P, P, ni), dimnames = list(active, active, NULL))
  for(j in seq_len(ni)){
    Vj <- matrix(V[, , j], P, P)
    if(anyNA(Vj)) next
    se[j, ] <- sqrt(pmax(diag(Vj), 0))
    ## Effective parameters as the other methods define them, diag(S V) with S
    ## the likelihood part: diag(I - prior V), since S = H - prior.
    edf[j, ] <- pmax(pmin(1 - prec[, j] * diag(Vj), 1), 0)
    Ij <- try(solve(Vj), silent = TRUE)
    if(!inherits(Ij, "try-error")) info[, , j] <- Ij
  }
  structure(list(
    ## The inverse of each marginal block: the information an item's own
    ## parameters carry once everything else is allowed to move.
    information = info, prior_precision = prec * 0,
    covariance = V, se = se, edf = edf, active = active, method = "exact",
    penalised = isTRUE(as.logical(sdat$dopriors)),
    n_items = ni, edf_total = sum(edf, na.rm = TRUE)
  ), class = "bigIRT_iteminfo")
}

## Item-block and shared-block covariances in natural coordinates, from the
## inverse information over the kept coordinates (optimiser coordinates).
bigIRT_certificate_covariance <- function(V, keep, layout, sdat, names_all){
  n <- length(names_all)
  ps <- attr(layout, "par_scale"); if(length(ps) != n) ps <- rep(1, n)
  Vn <- V * outer(ps[keep], ps[keep])
  pos <- match(seq_len(n), keep)
  ni <- as.integer(sdat$Nitems)
  active <- intersect(c("B", "A", "C", "D"), names(layout))
  active <- active[vapply(active, function(b) length(layout[[b]]) == ni, logical(1))]
  items <- NULL
  if(length(active)){
    P <- length(active)
    items <- array(NA_real_, c(P, P, ni), dimnames = list(active, active, NULL))
    for(j in seq_len(ni)){
      ix <- pos[vapply(active, function(b) layout[[b]][j], integer(1))]
      ok <- !is.na(ix)
      if(any(ok)) items[ok, ok, j] <- Vn[ix[ok], ix[ok]]
    }
  }
  item_par <- unlist(layout[active])
  sh <- setdiff(keep, item_par)
  shared <- if(length(sh)){
    S <- Vn[pos[sh], pos[sh], drop = FALSE]
    dimnames(S) <- list(names_all[sh], names_all[sh])
    S
  } else NULL
  list(items = items, shared = shared)
}
