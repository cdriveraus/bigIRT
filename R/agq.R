## Adaptive Gauss-Hermite quadrature for the person integrals; see the note at
## the top of src/agq_backend.cpp for why it exists and what it computes.

## Gauss-Hermite rule for a standard normal weight, by Golub-Welsch: the nodes
## are the eigenvalues of the Jacobi matrix of the probabilists' Hermite
## polynomials, the weights the squared first components of its eigenvectors.
## Exact for polynomials of degree 2Q - 1 against N(0, 1).
bigIRT_gh_rule <- function(Q){
  Q <- as.integer(Q)
  if(Q <= 1L) return(list(nodes = 0, log_weights = 0))
  J <- matrix(0, Q, Q)
  off <- sqrt(seq_len(Q - 1L))
  J[cbind(seq_len(Q - 1L), 2:Q)] <- off
  J[cbind(2:Q, seq_len(Q - 1L))] <- off
  x <- sort(eigen(J, symmetric = TRUE, only.values = TRUE)$values)
  ## The eigenvector weights lose about seven digits in the high moments, so
  ## polish the nodes by Newton on He_Q and take the weights from the closed
  ## form w_i proportional to 1 / He_{Q-1}(x_i)^2, both by the recurrence
  ## He_{n+1} = x He_n - n He_{n-1}, He_n' = n He_{n-1}.
  he <- function(x){
    h0 <- rep(1, length(x)); h1 <- x
    for(n in seq_len(Q - 1L)){ h2 <- x * h1 - n * h0; h0 <- h1; h1 <- h2 }
    list(Q = h1, Qm1 = h0)
  }
  for(it in 1:3){ h <- he(x); x <- x - h$Q / (Q * h$Qm1) }
  lw <- -2 * log(abs(he(x)$Qm1))
  lw <- lw - max(lw)
  list(nodes = x, log_weights = lw - log(sum(exp(lw))))
}

## Nodes per dimension for a fit with K free dimensions. The product rule
## costs Q^K likelihood passes per person, so fewer per dimension as K grows.
## Measured against 9^K optima on 2PL and 3PL data with cross loadings
## (dev/optim/mdim_lowk.R): 3 nodes a dimension left loadings and difficulties
## up to 1.7 SE off at K = 2 and 3; 5 up to 0.4; 7 within 0.06.
bigIRT_agq_default_nodes <- function(K) c(9L, 7L, 5L)[min(max(as.integer(K), 1L), 3L)]

## The rules the quadrature kernel uses, one for each number d = 1..K of free
## dimensions a person can have. Each is a node matrix Z (M x d) for a
## standard normal weight and its log weights.
##
## Up to three dimensions: the product Gauss-Hermite rule, `nodes` per
## dimension (default 9, 7, 5). Beyond that the product rule is out of reach
## (3^9 = 19,683 nodes per person at nine dimensions), and the fully symmetric
## and sparse-grid rules that stay polynomial in d have negative weights from
## about four dimensions on, which an exponentiated integrand does not
## forgive.
##
## So from four dimensions, by default (`pairs`), the anchored-ANOVA
## expansion of the log integral (bigIRT_anova_rule): Gauss-Hermite rules
## with `nodes` per dimension (default 7) on every coordinate plane and line
## of the whitened space, plus a coarse three-way correction on every cube
## (`nodes3`, default 3), combined with signed coefficients (see
## agq_backend.cpp). Each piece is a low-dimensional rule of the kind that is
## not exploited at K <= 3, and the pairwise part is already exact for the
## fourth-order term of the integrand's departure from the Laplace Gaussian.
## Cost grows as d^2 Q^2 / 2 + d^3 (Q3 - 1)^3 / 6 distinct nodes a person
## (2,185 at d = 9). Measured against 16,384-point QMC optima
## (dev/optim/pairs_fit.R; that reference is itself within 0.4 SE of a
## 65,536-point one), on data with cross loadings where Laplace was up to 7
## (3PL) and 13 (4PL) SE off, in loadings, mean / worst:
##   3PL K = 9: pairs 5 -0.11 / 0.87; pairs 7 + t3 -0.02 / 0.50 (84 s,
##              Laplace 8 s, reference 1,027 s)
##   4PL K = 5: pairs 7 -0.56 / 2.6, and no better at 9 or 11 nodes (the
##              truncation, not the quadrature); pairs 7 + t3 -0.39 / 1.95;
##              pairs 7 + t5 -0.24 / 1.34 (47 s, reference 788 s).
##   4PL K = 9: Laplace -1.73 / 7.0 (asymptotes up to 9.5); pairs 7 + t3
##              -0.19 / 1.02 (87 s, Laplace 18 s, reference 992 s).
## No refinement with this rule ran away or was reverted by the guard.
##
## With `points`: that many quasi-Monte Carlo points with equal weights --
## importance sampling through the Laplace Gaussian -- taken in antithetic
## pairs z, -z, which integrates every odd-order term of the integrand's
## departure from that Gaussian exactly. The points are a Halton sequence
## with a fixed Cranley-Patterson shift, so the rule, and the objective, are
## the same at every evaluation. Held fixed, they are exploitable at any count
## an optimiser can afford: 256 points at nine dimensions ran away, and 4,096
## left a correlation 145 SE off.
##
## `inflate` widens the quasi-Monte Carlo proposal to s times the Laplace
## scale. The kernel's formula stays exact: with Z = s z the weights become
## -log M + d log s - (s^2 - 1)|z|^2 / 2.
bigIRT_agq_rules <- function(K, nodes = NULL, points = NULL, inflate = NULL, pairs = is.null(points),
  nodes3 = bigIRT_agq_default_nodes3()){
  K <- as.integer(K)
  Z <- vector("list", K); W <- vector("list", K); desc <- character(K)
  grp <- vector("list", K); coef <- vector("list", K)
  for(d in seq_len(K)){
    Q <- if(!is.null(nodes)) as.integer(nodes) else bigIRT_agq_default_nodes(d)
    if(isTRUE(pairs) && d >= 4L){
      Q <- if(!is.null(nodes)) as.integer(nodes) else 7L
      ex <- bigIRT_anova_rule(d, Q, as.integer(nodes3 %||% 0L))
      Z[[d]] <- ex$Z; W[[d]] <- ex$log_weights; grp[[d]] <- ex$members; coef[[d]] <- ex$coefs
      desc[d] <- sprintf("pairs%d%s^%d", Q, if(isTRUE(nodes3 > 0)) sprintf("+t%d", nodes3) else "", d)
      next
    }
    if(d <= 3L && (is.null(points) || Q^d <= 2L * as.integer(points))){
      r <- bigIRT_gh_rule(Q)
      g <- as.matrix(expand.grid(rep(list(seq_len(Q)), d)))
      Z[[d]] <- matrix(r$nodes[g], ncol = d)
      W[[d]] <- rowSums(matrix(r$log_weights[g], ncol = d))
      desc[d] <- sprintf("gh%d^%d", Q, d)
    } else {
      M <- if(is.null(points)) bigIRT_agq_default_points(d) else as.integer(points)
      h <- bigIRT_halton(ceiling(M / 2), d)
      z <- stats::qnorm(h)
      z <- rbind(z, -z)
      s <- if(is.null(inflate)) bigIRT_agq_default_inflate(d) else as.numeric(inflate)
      Z[[d]] <- s * z
      W[[d]] <- -log(nrow(z)) + d * log(s) - 0.5 * (s^2 - 1) * rowSums(z^2)
      desc[d] <- sprintf("qmc%d%s", nrow(z), if(s != 1) sprintf("x%.2g", s) else "")
    }
  }
  structure(list(Z = Z, log_weights = W, groups = grp, coefs = coef, desc = desc,
    nodes = nodes, points = points, inflate = inflate, pairs = isTRUE(pairs),
    nodes3 = if(isTRUE(pairs)) as.integer(nodes3 %||% 0L) else NULL),
    class = "bigIRT_agq_rules")
}

## The anchored-ANOVA (cut-HDMR) expansion of a d-dimensional log integral,
## as groups for the quadrature kernel. Truncated at order s, with every
## subset u of the coordinates integrated by a Q^|u| product rule on the
## coordinates in u (the others at the anchor, zero):
##   log I ~ sum_{|u| <= s} (-1)^(s - |u|) C(d - |u| - 1, s - |u|) log I_u.
## The rule here is the order-2 expansion at Q nodes plus, if Q3 > 0, the
## order-3 correction (order 3 minus order 2) at Q3 nodes: pairs resolved
## finely, and the three-way interactions a pairwise expansion misses taken
## coarsely. The correction's coefficients sum to zero, the whole rule's to
## one, and identical groups (same subset, same rule) are merged by adding
## their coefficients. Nodes shared between groups are stored once.
##
## Why the correction: on 4PL data at five dimensions the pairwise optimum
## stopped improving past 7 nodes (loadings still 0.47 SE low on average
## against a 16,384-point reference), the signature of truncation rather
## than quadrature error; the 3-node three-way correction halved the
## per-person error there (dev/optim/mdim_pairs.R).
bigIRT_anova_rule <- function(d, Q, Q3 = 0L){
  terms <- list()   # key -> list(u, Q, coef)
  add_order <- function(s, q, sign){
    for(t in 0:min(s, d)){
      cf <- sign * (-1)^(s - t) * choose(d - t - 1, s - t)
      if(cf == 0) next
      us <- if(t == 0L) list(integer(0)) else lapply(seq_len(choose(d, t)), function(j) utils::combn(d, t)[, j])
      for(u in us){
        key <- if(t == 0L) "anchor" else paste(q, paste(u, collapse = "."))
        if(is.null(terms[[key]])) terms[[key]] <<- list(u = u, Q = q, coef = 0)
        terms[[key]]$coef <<- terms[[key]]$coef + cf
      }
    }
  }
  add_order(2L, Q, 1)
  if(Q3 > 0L){ add_order(3L, Q3, 1); add_order(2L, Q3, -1) }
  terms <- Filter(function(x) abs(x$coef) > 1e-12, terms)
  zs <- vector("list", length(terms)); ws <- zs; gs <- zs
  for(g in seq_along(terms)){
    u <- terms[[g]]$u; t <- length(u)
    if(t == 0L){ zs[[g]] <- matrix(0, 1L, d); ws[[g]] <- 0; gs[[g]] <- g; next }
    r <- bigIRT_gh_rule(terms[[g]]$Q)
    ix <- as.matrix(expand.grid(rep(list(seq_along(r$nodes)), t)))
    z <- matrix(0, nrow(ix), d); z[, u] <- r$nodes[ix]
    zs[[g]] <- z; ws[[g]] <- rowSums(matrix(r$log_weights[ix], ncol = t)); gs[[g]] <- rep(g, nrow(ix))
  }
  Zf <- do.call(rbind, zs)
  key <- apply(round(Zf, 12), 1, paste, collapse = ",")
  list(Z = Zf[!duplicated(key), , drop = FALSE],
    members = cbind(match(key, key[!duplicated(key)]), unlist(gs)),
    log_weights = unlist(ws), coefs = vapply(terms, function(x) x$coef, 1))
}

## Nodes for the three-way correction of the pairwise expansion (0: none).
bigIRT_agq_default_nodes3 <- function() as.integer(getOption("bigIRT.agq_nodes3", 3L))

## Measured at nine dimensions (dev/optim/mdim_study.R): 1.15 gave the
## smallest error at 256, 1024 and 4096 points; 1.5 was worse than none.
bigIRT_agq_default_inflate <- function(d) getOption("bigIRT.agq_default_inflate", 1.15)

bigIRT_agq_default_points <- function(d) 256L

## n points of the d-dimensional Halton sequence (skipping the origin), with
## a fixed Cranley-Patterson shift so no point sits on the unit cube's edge.
bigIRT_halton <- function(n, d){
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113)
  if(d > length(primes)) stop("bigIRT_halton: at most 30 dimensions.")
  shift <- (sqrt(primes[seq_len(d)]) * 7.3) %% 1
  vapply(seq_len(d), function(k){
    b <- primes[k]; i <- seq_len(n); x <- numeric(n); f <- 1 / b
    while(any(i > 0)){ x <- x + f * (i %% b); i <- i %/% b; f <- f / b }
    (x + shift[k]) %% 1
  }, numeric(n))
}

bigIRT_agq_fg_cpp_impl <- function(id, score, theta_mode, precision,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_beta_row, A_pred, A_signed,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask, rule, want_person = FALSE, grain_size = 64L,
  block_items = integer(0), row_out = 0L, slot_out = integer(0), row_hold = integer(0),
  vc_group = integer(0), vc_n = 0L, mv_v = matrix(0, 0, 0), shape_out = 0L){
  nr <- length(id); K <- ncol(theta_mode)
  .Call(`_bigIRT_agq_fg_cpp_impl`,
    as.integer(id), as.integer(score), as.matrix(theta_mode), as.numeric(precision),
    as.matrix(person_pred),
    bigirt_int_matrix(fixed_ability, nr, K), as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nr, K), as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nr, K), as.matrix(A_pred),
    ## One per free loading, 1 for the identity link (signedLoadings).
    as.integer(A_signed),
    as.integer(B_ref), as.numeric(B_fixed_value), as.integer(B_beta_row), as.matrix(B_pred),
    as.integer(C_ref), as.numeric(C_fixed_value), as.integer(C_beta_row), as.matrix(C_pred),
    as.integer(D_ref), as.numeric(D_fixed_value), as.integer(D_beta_row), as.matrix(D_pred),
    as.matrix(Abilitybeta), as.numeric(invspApars), as.matrix(invspAbeta),
    as.numeric(Bpars), as.matrix(Bbeta), as.numeric(logitCpars), as.matrix(logitCbeta),
    as.numeric(logitDpars), as.matrix(logitDbeta),
    as.matrix(prior_mean), as.numeric(prior_precision),
    bigirt_int_matrix(free_mask, nrow(theta_mode), K),
    lapply(rule$Z, function(z) matrix(as.numeric(z), nrow = nrow(z))),
    lapply(rule$log_weights, as.numeric),
    if(any(!vapply(rule$groups, is.null, TRUE))) lapply(rule$groups, function(g)
      if(is.null(g)) NULL else matrix(as.integer(g), ncol = 2L)) else NULL,
    if(any(!vapply(rule$groups, is.null, TRUE))) lapply(rule$coefs, function(g) if(is.null(g)) NULL else as.numeric(g)) else NULL,
    as.logical(want_person)[1], as.integer(grain_size),
    ## The item of each row, to have the item blocks of the Hessian back
    ## (R/certify.R); empty for none.
    as.integer(block_items),
    ## Per-response scores (1) and their information too (2), in the item
    ## slots listed, 0-based (R/scores.R).
    as.integer(row_out)[1], as.integer(slot_out),
    ## Rows held out of the posterior but scored (purification); empty for none.
    as.integer(row_hold),
    ## Group-variance scores: a candidate set label per row (0 none), and the
    ## number of sets (R/modify.R); 0 for none.
    as.integer(vc_group), as.integer(vc_n)[1],
    ## Residual-covariance products: each row's weight per direction (rows x
    ## m), for the matrix-free M v of R/modify.R; 0 x 0 for none.
    as.matrix(mv_v),
    ## Item-shape scores (R/modify.R): 1 asks, per row, for E[g t^p | y],
    ## p = 1, 2, 3, t the row's ability along its item's loadings.
    as.integer(shape_out)[1])
}

## Second-order Laplace (src/laplace2_backend.cpp): the leading correction to
## each person's Laplace value and its exact gradient. Arguments as for the
## quadrature kernel, without the rule and the posterior precision (the
## kernel rebuilds the observed Hessian at the mode itself).
bigIRT_laplace2_fg_cpp_impl <- function(id, score, theta_mode,
  person_pred, fixed_ability, fixed_ability_value,
  A_ref, A_fixed_value, A_pred, A_signed, B_ref, B_fixed_value, B_pred,
  C_ref, C_fixed_value, C_pred, D_ref, D_fixed_value, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask, grain_size = 64L){
  nr <- length(id); K <- ncol(theta_mode)
  .Call(`_bigIRT_laplace2_fg_cpp_impl`,
    as.integer(id), as.integer(score), as.matrix(theta_mode), as.matrix(person_pred),
    bigirt_int_matrix(fixed_ability, nr, K), as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nr, K), as.matrix(A_fixed_value), as.matrix(A_pred),
    as.integer(A_signed),
    as.integer(B_ref), as.numeric(B_fixed_value), as.matrix(B_pred),
    as.integer(C_ref), as.numeric(C_fixed_value), as.matrix(C_pred),
    as.integer(D_ref), as.numeric(D_fixed_value), as.matrix(D_pred),
    as.matrix(Abilitybeta), as.numeric(invspApars), as.matrix(invspAbeta),
    as.numeric(Bpars), as.matrix(Bbeta), as.numeric(logitCpars), as.matrix(logitCbeta),
    as.numeric(logitDpars), as.matrix(logitDbeta),
    as.matrix(prior_mean), as.numeric(prior_precision),
    bigirt_int_matrix(free_mask, nrow(theta_mode), K),
    as.numeric(getOption("bigIRT.laplace2_floor", 0)), as.integer(grain_size))
}

## The correction's gradient in the packed correlation parameters, from its
## gradient in the prior precision entries, G: sum_kl G_kl dP0_kl / drho_j,
## with dP0 differenced through the constructor the objective uses.
bigIRT_laplace2_corr_grad <- function(corr_par, G, sdat, paramization, jitter, h = 1e-6){
  K <- sdat$Nscales
  P0 <- function(p) bigIRT_laplace_prior_mats(sdat,
    AbilityCorr = bigIRT_laplace_corr_from_packed(p, K, paramization = paramization),
    jitter = jitter)$precision
  vapply(seq_along(corr_par), function(j){
    pp <- corr_par; pm <- corr_par; pp[j] <- pp[j] + h; pm[j] <- pm[j] - h
    sum(G * (P0(pp) - P0(pm))) / (2 * h)
  }, numeric(1))
}

## Gradient of the person-prior terms in the latent correlation parameters,
## given the summed posterior second moments S = sum_i E[(theta-mu)(theta-mu)'].
## Those terms are N/2 log|P0| - tr(P0 S)/2, with P0 the prior precision; by the
## Fisher identity the posterior moments are held fixed, so this is a closed
## form in K x K matrices. It is differenced in the packed parameters through
## the same constructor the objective uses, so it cannot drift from it.
bigIRT_agq_corr_grad <- function(corr_par, S, sdat, paramization, jitter, h = 1e-6){
  N <- sdat$Nsubs; K <- sdat$Nscales
  f <- function(p){
    P0 <- bigIRT_laplace_prior_mats(sdat,
      AbilityCorr = bigIRT_laplace_corr_from_packed(p, K, paramization = paramization),
      jitter = jitter)$precision
    0.5 * N * as.numeric(determinant(P0, logarithm = TRUE)$modulus) - 0.5 * sum(P0 * S)
  }
  vapply(seq_along(corr_par), function(j){
    pp <- corr_par; pm <- corr_par; pp[j] <- pp[j] + h; pm[j] <- pm[j] - h
    (f(pp) - f(pm)) / (2 * h)
  }, numeric(1))
}
