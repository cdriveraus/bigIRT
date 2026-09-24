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
bigIRT_agq_default_nodes <- function(K) c(9L, 5L, 3L)[min(max(as.integer(K), 1L), 3L)]

## The rules the quadrature kernel uses, one for each number d = 1..K of free
## dimensions a person can have. Each is a node matrix Z (M x d) for a
## standard normal weight and its log weights.
##
## Up to three dimensions: the product Gauss-Hermite rule, `nodes` per
## dimension (default 9, 5, 3). Beyond that the product rule is out of reach
## (3^9 = 19,683 nodes per person at nine dimensions), and the fully symmetric
## and sparse-grid rules that stay polynomial in d have negative weights from
## about four dimensions on, which an exponentiated integrand does not
## forgive. So from four dimensions: `points` quasi-Monte Carlo points with
## equal weights -- importance sampling through the Laplace Gaussian --
## taken in antithetic pairs z, -z, which integrates every odd-order term of
## the integrand's departure from that Gaussian exactly. The points are a
## Halton sequence with a fixed Cranley-Patterson shift, so the rule, and the
## objective, are the same at every evaluation.
##
## `inflate` widens the quasi-Monte Carlo proposal to s times the Laplace
## scale. The kernel's formula stays exact: with Z = s z the weights become
## -log M + d log s - (s^2 - 1)|z|^2 / 2.
bigIRT_agq_rules <- function(K, nodes = NULL, points = NULL, inflate = NULL){
  K <- as.integer(K)
  Z <- vector("list", K); W <- vector("list", K); desc <- character(K)
  for(d in seq_len(K)){
    Q <- if(!is.null(nodes)) as.integer(nodes) else bigIRT_agq_default_nodes(d)
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
  structure(list(Z = Z, log_weights = W, desc = desc, nodes = nodes, points = points,
    inflate = inflate), class = "bigIRT_agq_rules")
}

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
  A_ref, A_fixed_value, A_beta_row, A_pred,
  B_ref, B_fixed_value, B_beta_row, B_pred,
  C_ref, C_fixed_value, C_beta_row, C_pred,
  D_ref, D_fixed_value, D_beta_row, D_pred,
  Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
  prior_mean, prior_precision, free_mask, rule, want_person = FALSE, grain_size = 64L){
  nr <- length(id); K <- ncol(theta_mode)
  .Call(`_bigIRT_agq_fg_cpp_impl`,
    as.integer(id), as.integer(score), as.matrix(theta_mode), as.numeric(precision),
    as.matrix(person_pred),
    bigirt_int_matrix(fixed_ability, nr, K), as.matrix(fixed_ability_value),
    bigirt_int_matrix(A_ref, nr, K), as.matrix(A_fixed_value),
    bigirt_int_matrix(A_beta_row, nr, K), as.matrix(A_pred),
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
    as.logical(want_person)[1], as.integer(grain_size))
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
