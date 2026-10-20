## Model modification from score tests.
##
## Candidate changes to a fitted model are tested at the fit, without fitting
## the alternative: DIF on item intercepts or slopes along person or response
## covariates (scoreTest), covariates of ability (scoreTest, "Ability"),
## per-item lower and upper asymptotes, and group dimensions shared by sets of
## items. improveModel() refits the promising ones and keeps a change only if
## the held-out persons' marginal likelihood improves.

## Each person's score in a shared group-effect variance tau^2 at zero, per
## candidate item set: 1/2 E[(sum g)^2 + sum h] over the person's posterior,
## g and h the set's rows' first and second derivatives in their linear
## predictors (src/agq_backend.cpp). `set_of_item` gives each item index its
## set (1..G, 0 for none). Returns an N x G matrix.
bigIRT_modify_vc_scores <- function(fit, set_of_item, cores = 1L, nodes = NULL, st = NULL, rule = NULL){
  if(is.null(st)) st <- bigIRT_score_setup(fit, cores = cores)
  if(is.null(rule)) rule <- bigIRT_score_rule(fit, st$K, nodes)
  G <- max(0L, as.integer(set_of_item))
  if(G < 1L) return(matrix(0, st$N, 0))
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(
    theta_mode = st$posterior$theta_mode, precision = st$posterior$precision, rule = rule,
    vc_group = as.integer(set_of_item)[st$rc$items], vc_n = G)))
  aq$vc_scores
}

#' Held-out marginal log-likelihood
#'
#' The marginal log-likelihood of the persons a fit held out entirely (none
#' of their responses in `trainingRows`), each over all of their responses,
#' at the fitted parameters: the criterion [improveModel()] uses to compare
#' models. It integrates each person's ability by the fit's quadrature rule
#' around their posterior mode, and includes the ability prior's normalising
#' constant, so models with different numbers of dimensions or ability
#' variances compare correctly.
#'
#' @param fit A fit from [fitIRT()] with whole persons held out of
#'   `trainingRows`.
#' @param cores,nodes As for [scoreContributions()].
#' @return A list: `loglik`, `persons`, `responses`; `NULL` when no person
#'   was held out entirely.
#' @export
heldoutLogLik <- function(fit, cores = 1L, nodes = NULL){
  sdat <- fit$prepared
  trn <- as.integer(sdat$trainingLogical) == 1L
  id <- as.integer(sdat$id)
  ho_persons <- setdiff(unique(id[!trn]), unique(id[trn]))
  if(!length(ho_persons)) return(NULL)
  bigIRT_modify_marginal(fit, which(id %in% ho_persons), cores = cores, nodes = nodes)
}

## The training persons' marginal log-likelihood -- the fit's own logLik(),
## which over the training persons is the quadrature marginal heldoutLogLik()
## computes for the held-out ones (test-modify-trainloglik.R) -- and the fit's
## number of free parameters: what an in-sample criterion (likelihood ratio,
## AIC, BIC) would judge a batch by. improveModel records both for every
## batch, so that the held-out gate can be set against those criteria on the
## same batches.
bigIRT_modify_train_loglik <- function(fit){
  sdat <- fit$prepared
  trn <- which(as.integer(sdat$trainingLogical) == 1L)
  ll <- logLik(fit, effective = FALSE)
  ## The nominal count includes item-specific coefficients whose covariate is
  ## zero on every one of the item's training responses: they have no data
  ## and stay at their prior's centre. A DIF batch's column is zero for every
  ## item outside the batch, so two items with DIF counted as a coefficient
  ## for every item in the bank. A criterion should not count those.
  nominal <- attr(ll, "nominal_df")
  inert <- 0
  if(isTRUE(as.integer(sdat$itemSpecificBetas) == 1L) && isTRUE(as.integer(sdat$NitemPreds) > 0L)){
    it <- as.integer(sdat$item)[trn]
    X <- as.matrix(sdat$itemPreds)[trn, , drop = FALSE]
    nz <- matrix(FALSE, as.integer(sdat$Nitems), ncol(X))
    s <- rowsum((X != 0) * 1, it, reorder = TRUE)
    nz[as.integer(rownames(s)), ] <- s > 0
    for(cols in list(sdat$AitemPreds, sdat$BitemPreds, sdat$CitemPreds, sdat$DitemPreds))
      if(length(cols)) inert <- inert + sum(!nz[, as.integer(cols), drop = FALSE])
  }
  list(loglik = as.numeric(ll), persons = length(unique(as.integer(sdat$id)[trn])), df = nominal - inert,
    nominal_df = nominal)
}

## A fit's marginal log-likelihood per person over the given rows of its
## prepared data (whole persons), integrating each person's ability by the
## fit's quadrature rule around their posterior mode from those rows.
bigIRT_modify_marginal <- function(fit, rows, cores = 1L, nodes = NULL){
  ho <- rows
  st <- bigIRT_score_setup(fit, cores = cores, rows = ho)
  rule <- bigIRT_score_rule(fit, st$K, nodes)
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(
    theta_mode = st$posterior$theta_mode, precision = st$posterior$precision, rule = rule)))
  ids <- sort(unique(st$rc$ids))
  K <- st$K; P0a <- st$prior_prec
  P0_at <- function(i) if(length(dim(P0a)) == 3L) matrix(P0a[, , i], K, K) else matrix(P0a, K, K)
  fm <- matrix(st$args$free_mask, ncol = K)
  ## The kernel's value omits the prior's normalising constant (constant
  ## within a model, not between models): 1/2 log|P0| - d/2 log(2 pi) over
  ## the person's free dimensions (the conditional precision of those, for a
  ## person with some abilities fixed).
  const <- vapply(ids, function(i){
    f <- which(fm[i, ] == 1L)
    if(!length(f)) return(0)
    Pf <- P0_at(i)[f, f, drop = FALSE]
    0.5 * as.numeric(determinant(Pf, logarithm = TRUE)$modulus) - 0.5 * length(f) * log(2 * pi)
  }, 0)
  bp <- aq$value_by_subj[ids] + const
  names(bp) <- (rownames(fit$pars$Ability) %||% as.character(seq_len(st$N)))[ids]
  list(loglik = sum(bp), persons = length(ids), responses = length(ho), by_person = bp)
}

## Held-out gain of model b over model a, paired by person: total gain and
## its z (mean gain over its standard error across persons).
## Between two alternatives that both passed the gate (each a list with the
## batch's `tr` -- its held-out evaluation `h` and `gain` -- and its number
## of `terms`): the one with fewer terms, unless the larger beats it on the
## held-out persons by the gate itself (gain >= minGain, paired z >= minZ).
## With equal terms, the larger gain. Choosing by the larger gain alone left
## near ties to noise: with a gap specific to one subdomain at 20,000
## simulated persons, DIF on all 20 of its items beat the 2-term effect by
## dimension in 9 of 50 runs, by 0.02 to 2.7 nats out of about 100. Returns
## whether `cand` replaces `best`, and the larger's gain and z over the
## smaller.
bigIRT_modify_prefer <- function(best, cand, minGain, minZ, clusters = NULL){
  if(isTRUE(all.equal(cand$terms, best$terms)))
    return(list(cand = isTRUE(cand$tr$gain > best$tr$gain), gain = NA_real_, z = NA_real_))
  cand_small <- cand$terms < best$terms
  small <- if(cand_small) cand else best
  large <- if(cand_small) best else cand
  gm <- bigIRT_modify_gain(small$tr$h, large$tr$h, clusters = clusters)
  large_wins <- isTRUE(gm$gain >= minGain && gm$z >= minZ)
  list(cand = if(cand_small) !large_wins else large_wins, gain = gm$gain, z = gm$z)
}

## With `clusters` (cluster labels named by person), the standard error is
## cluster-robust (CR1): the summed gain's variance from the cluster totals of
## the centred per-person gains, times G/(G - 1). With every person in a
## cluster of their own it is exactly the default.
bigIRT_modify_gain <- function(a, b, clusters = NULL){
  p <- intersect(names(a$by_person), names(b$by_person))
  dlt <- b$by_person[p] - a$by_person[p]
  se <- if(is.null(clusters)) stats::sd(dlt) * sqrt(length(dlt)) else {
    cl <- clusters[p]
    if(anyNA(cl)) stop("gateCluster has no cluster for some held-out persons.")
    u <- rowsum(dlt - mean(dlt), cl)[, 1]; G <- length(u)
    if(G > 1L) sqrt(G / (G - 1) * sum(u^2)) else NA_real_
  }
  list(gain = sum(dlt), z = if(is.finite(se) && se > 0) sum(dlt) / se else NA_real_)
}

## Per-item tests of a fixed asymptote, against freeing it. At a fixed lower
## asymptote c (upper d), P = c + (d - c) logistic(eta) is linear in c (d),
## so the derivative of a response's leave-one-out probability pi is exact
## from pi itself: (d - pi)/(d - c) in c, (pi - c)/(d - c) in d. The
## response's score is that of the Bernoulli given the person's other
## responses, and its expected information follows (R/scores.R); the item's
## own free parameters are projected out. One-sided: an asymptote can only
## move inward, c up and d down. Returns item, side, n, z, p, one-step
## estimate (on the probability scale), and `identified`: the share of the
## asymptote's information left once the item's own parameters are projected
## out. Where an item is only ever answered near its difficulty (adaptive
## designs do this) the asymptote is nearly collinear with the slope and
## intercept, the share is small, and both the test and the one-step
## estimate rest on what little is left.
bigIRT_modify_asymptotes <- function(fit, sc, sides = c("lower", "upper"), minN = 50L){
  sdat <- fit$prepared
  R <- sc$rows; slots <- names(sc$slots)
  ## A fit with every item fixed has no item slots; as.matrix() of an empty
  ## data.table would drop the rows too.
  S <- if(length(slots)) as.matrix(R[, slots, with = FALSE]) else matrix(0, nrow(R), 0L)
  cfix <- as.integer(sdat$fixedClogit) == 1L; dfix <- as.integer(sdat$fixedDlogit) == 1L
  cval <- as.numeric(sdat$Cdata); dval <- as.numeric(sdat$Ddata)
  ## The fit's asymptotes, for an item whose other asymptote is free.
  ip <- data.table::as.data.table(fit$itemPars)
  m <- match(sc$item_labels, as.character(ip[[1L]]))
  cfit <- if("C" %in% names(ip)) ip$C[m] else rep(0, length(m))
  dfit <- if("D" %in% names(ip)) ip$D[m] else rep(1, length(m))
  by_item <- split(seq_len(nrow(R)), R$item)
  out <- list()
  for(side in sides){
    for(js in names(by_item)){
      j <- as.integer(js); u <- by_item[[js]]
      if(length(u) < minN) next
      if(side == "lower" && !cfix[j]) next
      if(side == "upper" && !dfix[j]) next
      c0 <- if(cfix[j]) cval[j] else cfit[j]
      d0 <- if(dfix[j]) dval[j] else dfit[j]
      if(!is.finite(c0)) c0 <- 0
      if(!is.finite(d0)) d0 <- 1
      pp <- R$ppred[u]; y <- R$y[u]
      dpi <- if(side == "lower") (d0 - pp) / (d0 - c0) else -(pp - c0) / (d0 - c0)
      s_new <- (y - pp) / (pp * (1 - pp)) * dpi
      nuis <- if(ncol(S)) which(sc$item_free[j, ]) else integer(0)
      Sa <- cbind(S[u, nuis, drop = FALSE], s_new)
      info <- bigIRT_score_expected_info(Sa, y, pp)
      tst <- ncol(Sa)
      pc <- bigIRT_score_lm_pieces(matrix(1, length(u), 1), matrix(1, length(u), 1), Sa, info,
        tst, seq_len(tst - 1L), persons = R$person[u])
      V <- as.numeric(pc$V)
      ## `info` is per response; the asymptote's own information is its sum.
      Itt <- as.numeric(bigIRT_score_kron(matrix(1, length(u), 1), matrix(1, length(u), 1), info, tst, tst))
      z <- if(V > 0) pc$Ustar / sqrt(V) else NA_real_
      out[[length(out) + 1L]] <- data.table::data.table(item = sc$item_labels[j], side = side,
        n = length(u), z = z, p = stats::pnorm(z, lower.tail = FALSE),
        estimate = if(V > 0) pc$Ustar / V else NA_real_, identified = V / Itt)
    }
  }
  data.table::rbindlist(out)
}

## Per-item tests of the response function's SHAPE: curvature in ability that
## the item's own parameters (intercept, slope, asymptotes) cannot express.
## Two terms are added to the item's linear predictor,
## gamma2 He2(t) + gamma3 He3(t), with t the person's ability along the
## item's loadings and He2 = t^2 - 1, He3 = t^3 - 3t (Hermite polynomials,
## orthogonal to 1 and t under a standard normal ability). Their scores are
## exact posterior expectations from one quadrature pass (the kernel's
## `shape_out`); the item's free parameters are projected out (C(alpha)) and
## the expected information is that of the leave-one-out Bernoulli, as for
## every item test here. Two-sided, 2 df. A significant item is one whose
## curve bends where the model's cannot: a plateau, a dip, a curve steeper on
## one side than the other. Returns item, n, LM, df, p, the one-step gammas,
## and dP: the largest change they make in the probability of a correct
## response over t in [-2.5, 2.5] along the item's fitted curve.
bigIRT_modify_shape <- function(fit, cores = 1L, nodes = NULL, minN = 50L){
  st <- bigIRT_score_setup(fit, cores = cores)
  sdat <- st$sdat; K <- st$K
  slots <- bigIRT_score_free_slots(sdat)
  rule <- bigIRT_score_rule(fit, K, nodes)
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(theta_mode = st$posterior$theta_mode,
    precision = st$posterior$precision, rule = rule, row_out = 1L, slot_out = as.integer(slots),
    row_hold = if(any(st$hold == 1L)) st$hold else integer(0), shape_out = 1L)))
  rc <- st$rc
  ns <- length(slots)
  sm <- aq$row_scores; sh <- aq$shape
  g0 <- sm[, ns + 1L]; pp <- sm[, ns + 2L]
  y <- as.integer(sdat$score[rc$rows])
  he2 <- sh[, 2] - g0
  he3 <- sh[, 3] - 3 * sh[, 1]
  S <- sm[, seq_len(ns), drop = FALSE]
  free <- bigIRT_score_item_free(sdat, slots)
  item_labels <- rownames(fit$pars$B) %||% as.character(seq_len(sdat$Nitems))
  ## The item's fitted curve along t, for the effect size.
  re <- bigIRT_laplace_row_effective(st$state, sdat, thetaBase = matrix(0, st$N, K), rows = rc$rows[!duplicated(rc$items)])
  first <- rc$items[!duplicated(rc$items)]
  anorm <- sqrt(rowSums(re$loadings^2))
  tg <- seq(-2.5, 2.5, by = .05)
  keep_row <- if(any(st$hold == 1L)) st$hold == 0L else rep(TRUE, length(y))
  by_item <- split(which(keep_row), rc$items[keep_row])
  out <- vector("list", length(by_item))
  for(ix in seq_along(by_item)){
    j <- as.integer(names(by_item)[ix]); u <- by_item[[ix]]
    if(length(u) < minN) next
    nuis <- which(free[j, ])
    Sa <- cbind(S[u, nuis, drop = FALSE], he2[u], he3[u])
    info <- bigIRT_score_expected_info(Sa, y[u], pp[u])
    m <- ncol(Sa)
    pc <- tryCatch(bigIRT_score_lm_pieces(matrix(1, length(u), 1), matrix(1, length(u), 1), Sa, info,
      c(m - 1L, m), seq_len(m - 2L), persons = rc$ids[u]), error = function(e) NULL)
    if(is.null(pc)) next
    V <- as.matrix(pc$V); U <- pc$Ustar
    pv <- bigIRT_score_pinv(V)
    LM <- as.numeric(crossprod(U, pv$inv %*% U))
    gam <- as.numeric(pv$inv %*% U)
    f <- match(j, first)
    eta0 <- anorm[f] * tg - re$b_row[f]
    cc <- re$c_row[f]; dd <- re$d_row[f]
    P0 <- cc + (dd - cc) * stats::plogis(eta0)
    P1 <- cc + (dd - cc) * stats::plogis(eta0 + gam[1] * (tg^2 - 1) + gam[2] * (tg^3 - 3 * tg))
    out[[ix]] <- data.table::data.table(item = item_labels[j], n = length(u), LM = LM, df = pv$rank,
      p = stats::pchisq(LM, pv$rank, lower.tail = FALSE), gamma2 = gam[1], gamma3 = gam[2],
      dP = max(abs(P1 - P0)))
  }
  data.table::rbindlist(out)
}

## The data and fitIRT() arguments for a model specification: the base
## arguments plus the terms added so far. Ability terms are indicators (a
## factor, or few values) or a centred 3-df natural spline of a person
## covariate. DIF terms are the covariate's basis times an indicator of the
## selected items, with item-specific coefficients, so only those items get
## one (any other item's column is zero and its coefficient stays at zero).
## Freed asymptotes go through itemDat with pl raised to 3 or 4; group
## dimensions through `loadings`, each set's items loading on their own
## scale and on the set's dimension, uncorrelated with the others.
##
## Also: `spec$impact`, covariates whose effect on ability is added (the
## response-level basis, only the columns the ability predictors do not
## already span: a DIF covariate's impact must be in the model before its DIF
## is judged); `spec$pooled`, the sides ("lower", "upper") whose asymptote is
## freed on every item, pooled by the asymptote priors; `spec$partition`, an
## item-set column that covers every item, whose levels become correlated
## ability dimensions.
bigIRT_modify_build <- function(dat, spec, base){
  ## A new table on the same column vectors, not a copy of them: the build
  ## only adds whole columns (set() with a full-length value replaces a
  ## column, it never writes into one), and a full copy for every batch and
  ## screen held a second copy of the data beside each fit (on Mindsteps
  ## about 6 GB).
  d <- data.table::setDT(as.list(dat))
  it <- as.character(d[[base$item]])
  pp <- spec$personPreds
  for(a in spec$abil){
    B <- bigIRT_modify_basis(d[[a$covariate]], a$kind, d[[base$id]])
    nm <- paste0(".abil_", make.names(a$covariate), "_", seq_len(ncol(B)))
    for(k in seq_len(ncol(B))) data.table::set(d, j = nm[k], value = B[, k])
    pp <- c(pp, nm)
  }
  for(v in spec$impact){
    z <- d[[v]]
    B <- bigIRT_modify_basis(z, if(is.character(z) || is.factor(z) || is.logical(z) ||
      length(unique(stats::na.omit(z))) <= 4L) "factor" else "linear", NULL)
    keep <- bigIRT_modify_unspanned(d, pp, B)
    nm <- paste0(".imp_", make.names(v), "_", seq_len(ncol(B)))
    for(k in keep) data.table::set(d, j = nm[k], value = B[, k])
    pp <- c(pp, nm[keep])
  }
  Bp <- character(); Ap <- character()
  for(t in bigIRT_modify_merge_dif(spec$dif)){
    B <- bigIRT_modify_basis(d[[t$covariate]], t$kind, NULL)
    mask <- it %in% t$items
    nm <- paste0(".dif_", t$parameter, "_", make.names(t$covariate), "_", seq_len(ncol(B)))
    for(k in seq_len(ncol(B))) data.table::set(d, j = nm[k], value = B[, k] * mask)
    if(t$parameter == "B") Bp <- c(Bp, nm) else Ap <- c(Ap, nm)
  }
  stopifnot(!anyDuplicated(Bp), !anyDuplicated(Ap))
  pl <- base$pl
  free_c <- "lower" %in% spec$pooled; free_d <- "upper" %in% spec$pooled
  if(length(spec$upper) || free_d) pl <- 4L else if(length(spec$lower) || free_c) pl <- max(pl, 3L)
  items <- unique(it)
  itemDat <- NA
  if(pl > base$pl){
    itemDat <- data.frame(items, B = NA_real_,
      C = if(base$pl >= 3L || free_c) NA_real_ else ifelse(items %in% spec$lower, NA_real_, 0),
      D = if(base$pl >= 4L || free_d) NA_real_ else ifelse(items %in% spec$upper, NA_real_, 1))
    names(itemDat)[1] <- base$item
    if(pl < 4L) itemDat$D <- NULL
    ## Every asymptote free on both sides: nothing left to fix.
    if(all(is.na(itemDat$C)) && (is.null(itemDat$D) || all(is.na(itemDat$D)))) itemDat <- NA
  }
  scale <- if(length(spec$partition)) spec$partition else base$scale
  loadings <- NA
  fa <- base$fitArgs
  if(length(spec$partition)) fa$estimateAbilityCorr <- TRUE
  if(length(spec$groups)){
    sc0 <- as.character(d[[base$scale]])
    own <- tapply(sc0, it, function(x) x[1])
    sc1 <- sc0
    for(k in seq_along(spec$groups)) sc1[it %in% spec$groups[[k]]$items] <- paste0(".g", k)
    data.table::set(d, j = ".bigIRT_scale", value = sc1)
    scale <- ".bigIRT_scale"
    cols <- c(sort(unique(sc0)), paste0(".g", seq_along(spec$groups)))
    L <- matrix(0, length(items), length(cols), dimnames = list(items, cols))
    L[cbind(items, own[items])] <- NA
    for(k in seq_along(spec$groups)) L[items %in% spec$groups[[k]]$items, paste0(".g", k)] <- NA
    loadings <- L
  }
  ## The ability predictors' effects across dimensions (base$commonEffects):
  ## one effect common to a partition's dimensions, none on a group
  ## dimension, so that a dimension is judged on the dependence it explains
  ## and not on group differences. The covariates in spec$bydim have their
  ## own effect on each dimension.
  map <- NULL
  if(isTRUE(base$commonEffects) && length(pp) && (length(spec$partition) || length(spec$groups))){
    own <- bigIRT_modify_bydim_terms(spec, pp, base)
    if(length(spec$partition)){
      rows <- unique(as.character(d[[spec$partition]])); gen <- rep(FALSE, length(rows))
    } else {
      rows <- colnames(loadings); gen <- rows %in% as.character(unique(d[[base$scale]]))
    }
    map <- matrix(0L, length(rows), length(pp), dimnames = list(rows, pp)); n <- 0L
    for(p in pp){
      if(length(spec$partition) && !p %in% own){ n <- n + 1L; map[, p] <- n; next }
      ## Each general dimension's own, as without groups; a group dimension's
      ## only for the covariates freed by dimension.
      fr <- gen | p %in% own
      map[fr, p] <- n + seq_len(sum(fr)); n <- n + sum(fr)
    }
  }
  args <- c(list(dat = d, score = base$score, id = base$id, item = base$item, scale = scale, pl = pl,
    personPreds = pp, BitemPreds = Bp, AitemPreds = Ap, itemSpecificBetas = length(c(Bp, Ap)) > 0,
    itemDat = itemDat, loadings = loadings, normalise = FALSE, abilityBetaMap = map), fa)
  args
}

## Tests of each covariate's effect on ability differing by dimension, at a
## fit whose ability regression shares it across dimensions (abilityBetaMap).
## `terms` names, for each covariate, the ability predictors that carry its
## effect; their person means are tested on every dimension, against the
## nuisance the fit has (an intercept per dimension, and every predictor's
## coefficients as shared or zeroed), so what is tested is what freeing
## them by dimension would add: K - 1 directions per term on a partition,
## one per group dimension. With many responses a person, a gap specific to
## one dimension is taken up by that dimension's abilities and barely shows
## item by item as DIF; it shows here, as a misfit of the ability
## regression. (Offered only after DIF flags, the alternative was never
## reached for a subdomain-specific gap on simulated data.) A row-varying
## predictor enters by its person mean, an approximation for a screen.
bigIRT_modify_bydim_tests <- function(sc, terms){
  K <- sc$K; P <- sc$persons
  if(K < 2L || !length(terms) || is.null(sc$person_pred_means) || is.null(sc$ability_beta_map)) return(NULL)
  dn <- as.character(seq_len(K))
  S <- as.matrix(P[, paste0("mu", dn), with = FALSE])
  ic <- character()
  for(l in seq_len(K)) for(k in seq_len(l)) ic <- c(ic, paste0("Imu.", k, ".", l))
  info <- as.matrix(P[, ic, with = FALSE])
  X <- as.matrix(sc$person_pred_means)
  Nb <- cbind(1, X)
  ntrans <- bigIRT_score_nuis_map(sc$ability_beta_map, K, ncol(X))
  if(is.null(ntrans)) return(NULL)
  data.table::rbindlist(lapply(names(terms), function(v){
    j <- match(terms[[v]], sc$person_preds)
    if(!length(j) || anyNA(j)) return(NULL)
    pc <- bigIRT_score_lm_pieces(X[, j, drop = FALSE], Nb, S, info, seq_len(K), seq_len(K), ntrans = ntrans)
    pv <- bigIRT_score_pinv(pc$V)
    if(pv$rank == 0L) return(NULL)
    lm <- as.numeric(crossprod(pc$Ustar, pv$inv %*% pc$Ustar))
    data.table::data.table(covariate = v, terms = length(j), LM = lm, df = pv$rank,
      p = stats::pchisq(lm, pv$rank, lower.tail = FALSE))
  }))
}

## The ability predictors that carry a covariate's effect, among `pp`: the
## predictor of that name, those `abilityTerms` names for it, and the terms
## improveModel() added for it (its ability terms and its impact).
bigIRT_modify_cov_terms <- function(v, pp, abilityTerms = list()){
  mn <- gsub(".", "\\.", make.names(v), fixed = TRUE)
  pp[pp %in% c(v, abilityTerms[[v]]) | grepl(paste0("^\\.(abil|imp)_", mn, "_[0-9]+$"), pp)]
}

bigIRT_modify_bydim_terms <- function(spec, pp, base)
  unique(unlist(lapply(spec$bydim, bigIRT_modify_cov_terms, pp = pp, abilityTerms = base$abilityTerms)))

## DIF along one covariate and parameter is one term -- one set of columns,
## with a coefficient per item -- so a later batch along a covariate already
## in the model adds its items to that term. Kept as separate entries they
## shared a column name, and the later overwrote the earlier's column with its
## own items' mask: on Mindsteps a round-2 batch of 2 age items removed the
## age DIF of the 3,883 items round 1 had added, and listed the column twice.
bigIRT_modify_merge_dif <- function(dif){
  if(length(dif) < 2L) return(dif)
  key <- vapply(dif, function(t) paste(t$covariate, t$parameter, sep = "\r"), "")
  unname(lapply(split(seq_along(dif), factor(key, levels = unique(key))), function(ix){
    t <- dif[[ix[1L]]]
    t$items <- unique(unlist(lapply(dif[ix], `[[`, "items")))
    t
  }))
}

## Which columns of B add to the span of the ability predictors `pp` (with an
## intercept), judged on up to 200,000 evenly spaced rows. A covariate whose
## effect is already in the model in another form (context indicators split
## into a person mean and a deviation, say) adds nothing; its other levels do.
bigIRT_modify_unspanned <- function(d, pp, B, max_rows = 2e5L, tol = 1e-7){
  n <- nrow(d)
  idx <- if(n > max_rows) unique(round(seq(1, n, length.out = max_rows))) else seq_len(n)
  X <- matrix(1, length(idx), 1L)
  if(length(pp)) X <- cbind(X, as.matrix(d[idx, pp, with = FALSE]))
  r0 <- qr(X, tol = tol)$rank
  keep <- integer(0)
  for(k in seq_len(ncol(B))){
    X1 <- cbind(X, B[idx, k])
    r1 <- qr(X1, tol = tol)$rank
    if(r1 > r0){ X <- X1; r0 <- r1; keep <- c(keep, k) }
  }
  keep
}

## A covariate's basis: indicators for a factor or few values, else a centred
## natural spline (3 df) for ability terms or the centred value for DIF terms
## (kind "linear"). With `id`, person-level: the basis of each person's value.
bigIRT_modify_basis <- function(z, kind, id = NULL){
  if(kind == "factor"){
    f <- factor(z)
    B <- vapply(levels(f)[-1L], function(l) as.numeric(f == l), numeric(length(f)))
    return(matrix(B, nrow = length(f)))
  }
  z <- as.numeric(z)
  if(kind == "linear") return(matrix(z - mean(z, na.rm = TRUE), ncol = 1L))
  B <- splines::ns(z, df = 3)
  B <- matrix(as.numeric(B), nrow = length(z))
  sweep(B, 2, colMeans(B))
}

## Tests of a group dimension per candidate item set (shared person effect
## u ~ N(0, tau^2) on the set's linear predictors), against tau^2 = 0. The
## per-person scores are independent across persons, so their sum is tested
## against their outer product, with the set's own item parameters projected
## out (by their per-person scores, sandwich::estfun()) when that is
## affordable, and conservatively not otherwise. One-sided: a variance is not
## negative. Returns set, items, persons, z, p, one-step tau^2.
##
## The one-step tau^2 divides the score by its NULL information, the summed
## squares of the persons' scores in data simulated from the fitted model
## (`nsimNull` data sets), not by the observed outer product. Under the
## alternative every person's score has a mean of order k^2 tau^2 (k their
## responses in the set), and the observed outer product carries those
## squared means: with true SD 0.5 the observed-outer-product estimate fell
## from 0.52 to 0.09 as k went from 5 to 100 items per person, while the null
## information held it steady (diag_set_onestep*.R in the model-building
## study). On Mindsteps it had put subdomains at SD 0.06 that a correlated
## three-subdomain model shows to be distinct. The test itself (z) uses the
## observed outer product, which is right under the null.
bigIRT_modify_groups <- function(fit, sc, sets, cores = 1L, nodes = NULL, max_proj = 200L,
  nsimNull = 2L, seed = 1L, persons = NULL){
  ## `persons` (logical, one per person): sum over these persons only -- the
  ## half not used to choose a set from the scores (modificationTests()).
  if(!length(sets)) return(NULL)
  ## The kernel takes one set per response, so overlapping sets (a subdomain
  ## and a competence area within it) cannot share a pass: labelling them in
  ## one pass let the last set take every shared item, and on Mindsteps left
  ## the three subdomains empty. Sets are dealt into layers of disjoint sets,
  ## one pass per layer.
  ni <- length(sc$item_labels)
  layer_of <- integer(length(sets)); used <- list()
  for(k in seq_along(sets)){
    idx <- match(sets[[k]], sc$item_labels)
    L <- 1L
    while(L <= length(used) && any(used[[L]][idx])) L <- L + 1L
    if(L > length(used)) used[[L]] <- logical(ni)
    used[[L]][idx] <- TRUE; layer_of[k] <- L
  }
  st <- bigIRT_score_setup(fit, cores = cores)
  rule <- bigIRT_score_rule(fit, st$K, nodes)
  soi_of <- lapply(seq_along(used), function(L){
    ks <- which(layer_of == L); soi <- integer(ni)
    for(j in seq_along(ks)) soi[match(sets[[ks[j]]], sc$item_labels)] <- j
    list(ks = ks, soi = soi)
  })
  scores <- function(s){
    Tm <- matrix(0, st$N, length(sets))
    for(l in soi_of) Tm[, l$ks] <- bigIRT_modify_vc_scores(fit, l$soi, st = s, rule = rule)
    Tm
  }
  pk <- if(is.null(persons)) rep(TRUE, st$N) else as.logical(persons)
  Tm <- scores(st)[pk, , drop = FALSE]
  V0 <- rep(NA_real_, length(sets))
  if(nsimNull > 0L){
    old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv()))
    set.seed(seed)
    ## (A matrix even for one set, which vapply would return as a vector.)
    V0 <- rowMeans(matrix(vapply(seq_len(nsimNull), function(b)
      colSums(scores(bigIRT_modify_null_state(st))[pk, , drop = FALSE]^2),
      numeric(length(sets))), nrow = length(sets)))
  }
  out <- list()
  for(k in seq_along(sets)){
    t <- Tm[, k]
    its <- sets[[k]]
    persons <- sum(t != 0)
    if(persons < 10L || length(its) < 2L) next
    U <- sum(t); V <- V_raw <- sum(t^2)
    Psi <- tryCatch(if(sum(sc$item_free[match(its, sc$item_labels), ]) <= max_proj)
      estfun.bigIRT_scores(sc, items = its)[pk, , drop = FALSE] else NULL, error = function(e) NULL)
    if(!is.null(Psi) && ncol(Psi)){
      tp <- crossprod(Psi, t); pinv <- bigIRT_score_pinv(crossprod(Psi))$inv
      U <- U - as.numeric(crossprod(tp, pinv %*% colSums(Psi)))
      V <- V - as.numeric(crossprod(tp, pinv %*% tp))
    }
    z <- if(V > 0) U / sqrt(V) else NA_real_
    ## The null information, reduced by the projection in the same proportion
    ## as the observed outer product.
    I0 <- if(is.finite(V0[k]) && V0[k] > 0 && V_raw > 0) V0[k] * V / V_raw else V
    out[[length(out) + 1L]] <- data.table::data.table(set = names(sets)[k], items = length(its),
      persons = persons, z = z, p = stats::pnorm(z, lower.tail = FALSE),
      tau2 = if(I0 > 0) max(U / I0, 0) else NA_real_)
  }
  data.table::rbindlist(out)
}

## Exploratory dimensions from the scores alone. M = sum over persons of
## E[g g' + diag(h)], items by items, is the score for adding residual
## covariance among items; a new dimension with loadings v has score v'Mv/2,
## so the direction most worth adding is M's leading eigenvector, and no
## item covariate is needed to find it. M is never formed: M v is one
## quadrature pass (src/agq_backend.cpp), so a few dozen passes give the
## leading eigenpairs even for thousands of items. Entries are scaled by the
## items' response counts, D^-1/2 M D^-1/2, so that heavily answered items do
## not dominate the direction.
bigIRT_modify_mv <- function(st, rule, V, scl, rowmask = NULL){
  Vr <- (V * scl)[st$rc$items, , drop = FALSE]
  ## A row with weight zero contributes nothing, and neither does any row of
  ## a person whose rows all have weight zero: M from those persons only.
  if(!is.null(rowmask)) Vr <- Vr * rowmask
  aq <- do.call(bigIRT_agq_fg_cpp_impl, c(st$args, list(theta_mode = st$posterior$theta_mode,
    precision = st$posterior$precision, rule = rule, mv_v = Vr)))
  out <- matrix(0, nrow(V), ncol(V))
  sm <- rowsum(aq$mv, st$rc$items, reorder = TRUE)
  out[as.integer(rownames(sm)), ] <- sm
  out * scl
}

## Leading (largest algebraic) eigenpairs of the scaled M by subspace
## iteration with Rayleigh-Ritz, shifted when the largest-magnitude
## eigenvalue is negative.
bigIRT_modify_eigen <- function(st, rule, m = 2L, iters = 60L, tol = 1e-6, seed = 1L, rowmask = NULL){
  J <- max(st$rc$items)
  scl <- 1 / sqrt(pmax(tabulate(st$rc$items[if(is.null(rowmask)) TRUE else rowmask > 0], J), 1))
  old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv()))
  set.seed(seed)
  V <- qr.Q(qr(matrix(stats::rnorm(J * m), J, m)))
  run <- function(shift){
    vals <- rep(NA_real_, m)
    for(it in seq_len(iters)){
      W <- bigIRT_modify_mv(st, rule, V, scl, rowmask) + shift * V
      H <- crossprod(V, W); e <- eigen((H + t(H)) / 2, symmetric = TRUE)
      Vn <- qr.Q(qr(W %*% e$vectors))
      new <- e$values - shift
      if(all(is.finite(vals)) && max(abs(new - vals)) < tol * max(1, abs(new))) { V <- V %*% e$vectors; vals <- new; break }
      vals <- new; V <- Vn
    }
    list(values = vals, vectors = V)
  }
  r <- run(0)
  if(r$values[1] < 0 || any(r$values < 0)){
    s <- 1.5 * max(abs(r$values))
    r <- run(s)
  }
  o <- order(r$values, decreasing = TRUE)
  list(values = r$values[o], vectors = r$vectors[, o, drop = FALSE])
}

## Responses simulated from the fitted model for the same persons and items:
## base abilities from each person's prior (ability regression and SD), each
## response from its item curve. For the reference distribution of the
## leading eigenvalue.
bigIRT_modify_simulate <- function(st){
  K <- st$K; N <- st$N
  P0a <- st$prior_prec
  P0 <- if(length(dim(P0a)) == 3L) matrix(P0a[, , 1], K, K) else matrix(P0a, K, K)
  Lc <- chol(solve(P0))
  th <- st$args$prior_mean + matrix(stats::rnorm(N * K), N, K) %*% Lc
  re <- bigIRT_laplace_row_effective(st$state, st$sdat, thetaBase = th, rows = st$rc$rows, context = st$rc)
  p <- re$c_row + (re$d_row - re$c_row) * stats::plogis(re$eta_row)
  as.integer(stats::runif(length(p)) < p)
}

## The scoring state for responses simulated from the fitted model: same
## persons, items and rule, new responses, each person's posterior re-solved.
bigIRT_modify_null_state <- function(st){
  s2 <- st
  s2$args$score <- bigIRT_modify_simulate(st)
  s2$args_post <- if(any(st$hold == 1L)) bigIRT_score_args_rows(s2$args, st$hold == 0L) else s2$args
  s2$posterior <- do.call(bigIRT_laplace_person_step_block_cpp_impl, c(s2$args_post, list(
    theta_init = st$posterior$theta_mode, jitter = 1e-6, max_attempts = 8L, max_iter = 100L,
    tol = 1e-10, keep_covariance = FALSE)))
  s2
}

bigIRT_modify_dimensions <- function(fit, m = 2L, nsim = 19L, iters = 60L, cores = 1L, nodes = NULL, seed = 1L,
  persons = NULL){
  ## `persons` (logical, one per person): find the directions from these
  ## persons only, so that sets chosen from them can be tested on the others.
  st <- bigIRT_score_setup(fit, cores = cores)
  rule <- bigIRT_score_rule(fit, st$K, nodes)
  rowmask <- if(is.null(persons)) NULL else as.numeric(as.logical(persons)[st$rc$ids])
  obs <- bigIRT_modify_eigen(st, rule, m = m, iters = iters, seed = seed, rowmask = rowmask)
  null <- numeric(0)
  if(nsim > 0L){
    old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv()))
    set.seed(seed + 1L)
    null <- vapply(seq_len(nsim), function(b)
      bigIRT_modify_eigen(bigIRT_modify_null_state(st), rule, m = 1L, iters = iters, seed = seed + b,
        rowmask = rowmask)$values[1], 0)
  }
  item_labels <- rownames(fit$pars$B) %||% as.character(seq_len(fit$prepared$Nitems))
  list(values = obs$values, vectors = obs$vectors, null = null,
    p = vapply(obs$values, bigIRT_modify_null_p, 0, null = null), item_labels = item_labels)
}

## The p-value of an eigenvalue against the simulated null of the leading
## one: empirical, or from a normal fitted to the null draws when the value
## lies beyond all of them (where the empirical p is only a bound, 1/(B+1)).
bigIRT_modify_null_p <- function(x, null){
  if(!length(null)) return(NA_real_)
  if(x <= max(null) || length(null) < 5L) return((1 + sum(null >= x)) / (1 + length(null)))
  stats::pnorm((x - mean(null)) / stats::sd(null), lower.tail = FALSE)
}

#' Score tests for modifying a fitted model
#'
#' Tests candidate changes to a fit at the fit itself, without fitting any
#' alternative: covariates of ability, DIF on item intercepts or slopes along
#' person or response covariates, per-item lower and upper asymptotes, and
#' group dimensions shared by sets of items. Each comes with a one-step
#' estimate of the change.
#'
#' Ability covariates and DIF use [scoreTest()] (DIF purified, so it is
#' measured against anchors, with effect sizes). An asymptote fixed at c (or
#' d) is tested against freeing it: P is linear in c, so a response's score is
#' exact from its leave-one-out probability. A set of items is tested for a
#' shared person effect u ~ N(0, tau^2) on their linear predictors, the
#' variance-component score test (Lin, 1997) from each person's posterior
#' expectation of (sum g)^2 + sum h over the set; a significant one is local
#' dependence among the set's items, which a group dimension models.
#' Asymptote and group tests are one-sided.
#'
#' @param fit A fit from [fitIRT()].
#' @param dat The data the fit was made from (for covariates and item sets).
#' @param personCovariates Columns of `dat` constant within person: tested as
#'   covariates of ability, and for DIF.
#' @param responseCovariates Columns of `dat` varying by response: tested for
#'   DIF.
#' @param itemSets Columns of `dat` constant within item; each level with at
#'   least `minSetItems` items is a candidate group dimension.
#' @param dif Item parameters tested for DIF: `"B"` (intercept), `"A"`
#'   (slope), or none.
#' @param difCovariates The covariates tested for DIF, among
#'   `personCovariates` and `responseCovariates`; `NULL` (default) tests all
#'   of them. Each is a purified screen over every item, the costly part on
#'   large data.
#' @param asymptotes `"lower"`, `"upper"`, both or none.
#' @param purify Purify the DIF tests (recommended: it sets the anchors).
#' @param exploratory Look for group dimensions in the scores alone (the
#'   leading directions of residual covariance), as well as in `itemSets`.
#' @param nsimDim Data sets simulated from the fit for the null distribution
#'   of the leading eigenvalue.
#' @param minSetItems Smallest item set tested for a group dimension.
#' @param shape Also test each item's response function for curvature its
#'   own parameters cannot express (`shape`, below). Diagnostic only:
#'   [improveModel()] does not add such terms.
#' @param cores Threads.
#' @param byDimension A named list: for each covariate, the ability
#'   predictors that carry its effect, tested for an effect that differs by
#'   dimension. Only meaningful at a fit that shares those effects across its
#'   dimensions ([fitIRT()]'s `abilityBetaMap`); [improveModel()] supplies it.
#' @return A list of data.tables: `ability`, `dif` (with effect sizes),
#'   `asymptotes`, `groups`, `bydim` (effects by dimension); each with
#'   p-values adjusted by Benjamini-Hochberg (`p_fdr`) within its kind; and
#'   `dimensions`, the exploratory eigenpairs with their null draws.
#' @seealso [improveModel()], [scoreTest()]
#' @export
modificationTests <- function(fit, dat, personCovariates = character(), responseCovariates = character(),
  itemSets = character(), dif = c("B", "A"), difCovariates = NULL, asymptotes = c("lower", "upper"), purify = TRUE,
  exploratory = TRUE, nsimDim = 19L, minSetItems = 3L, shape = FALSE, cores = 1L, byDimension = list()){
  if(is.null(difCovariates)) difCovariates <- c(personCovariates, responseCovariates)
  if(length(bad <- setdiff(difCovariates, c(personCovariates, responseCovariates))))
    stop("difCovariates not among personCovariates or responseCovariates: ", paste(bad, collapse = ", "))
  dat <- data.table::as.data.table(dat)
  idc <- fit$call_args$id %||% "id"; itc <- fit$call_args$item %||% "Item"
  sc <- scoreContributions(fit, cores = cores)
  idn <- sc$id_name; itn <- sc$item_name
  if(!idn %in% names(dat)) idn <- idc
  if(!itn %in% names(dat)) itn <- itc
  pv <- function(v){
    x <- dat[, list(z = get(v)[1]), by = c(idn)]
    out <- data.frame(id = as.character(x[[idn]])); names(out) <- sc$id_name
    out[[v]] <- if(is.character(x$z) || is.factor(x$z) || is.logical(x$z) ||
      length(unique(stats::na.omit(x$z))) <= 4L) factor(x$z) else x$z
    out
  }
  fdr <- function(x, col = "p") if(!is.null(x) && nrow(x)) x[, p_fdr := stats::p.adjust(get(col), "BH")] else x
  ## A screen that fails is left out, but said so: silently dropped, a failed
  ## DIF screen looked exactly like a covariate with no DIF candidates (on a
  ## simulated bank, a third of the runs of one design). The screens' own
  ## warnings are muffled, except that purification fell back on its anchor
  ## floor, which is passed on.
  failed <- character(); floored <- character()
  screen <- function(expr, what) withCallingHandlers(
    tryCatch(expr, error = function(e){ failed <<- c(failed, sprintf("%s: %s", what, conditionMessage(e))); NULL }),
    warning = function(w){
      if(inherits(w, "bigIRT_anchor_floor")) floored <<- c(floored, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  ## Covariates of ability not already in the model.
  ab <- data.table::rbindlist(lapply(personCovariates, function(v){
    cv <- pv(v); isf <- is.factor(cv[[v]])
    r <- screen(scoreTest(sc, cv, parameters = "Ability", form = if(isf) "factor" else "spline"),
      sprintf("ability, %s", v))
    if(is.null(r) || !nrow(r$tests)) return(NULL)
    data.table::data.table(covariate = v, kind = if(isf) "factor" else "spline", LM = r$tests$LM[1],
      df = r$tests$df[1], p = r$tests$p[1])
  }), fill = TRUE)
  ab <- fdr(ab)
  ## DIF, purified, with effect sizes.
  dl <- list()
  for(v in difCovariates) for(par in dif){
    ## A response covariate with few values is a factor, as a person
    ## covariate is (pv): read as continuous, a 0/1 indicator that is rarely
    ## 1 got an effect contrast of zero (on Mindsteps, repeated attempts: 434
    ## items significant, none passing the effect floor).
    cv <- if(v %in% personCovariates) pv(v) else {
      z <- dat[[v]]
      o <- data.frame(z = if(is.character(z) || is.factor(z) || is.logical(z) ||
        length(unique(stats::na.omit(z))) <= 4L) factor(z) else z)
      names(o) <- v; o
    }
    r <- screen(scoreTest(fit, cv, parameters = par, purify = purify, effects = TRUE, cores = cores,
      contributions = sc), sprintf("DIF in %s along %s", par, v))
    if(is.null(r) || !nrow(r$tests)) next
    e <- r$effects
    eff <- if(is.null(e)) NULL else e[, list(dP = dP[which.max(abs(dP))], dP_abs = max(dP_abs)), by = item]
    t <- r$tests[, list(item, covariate = v, parameter = par, form, n, LM, p, p_fdr)]
    if(!is.null(eff)) t <- merge(t, eff, by = "item", all.x = TRUE)
    t[, kind := if(is.factor(cv[[v]])) "factor" else "linear"]
    dl[[length(dl) + 1L]] <- t
  }
  di <- data.table::rbindlist(dl, fill = TRUE)
  ## Effects by dimension, for covariates whose effect the fit shares across
  ## dimensions.
  bd <- if(length(byDimension)) fdr(screen(bigIRT_modify_bydim_tests(sc, byDimension), "effects by dimension")) else NULL
  for(m in floored) warning(warningCondition(m, class = "bigIRT_anchor_floor"))
  if(length(failed)) warning("These screens failed and are left out: ", paste(failed, collapse = "; "), call. = FALSE)
  ## Asymptotes and group dimensions.
  as <- if(length(asymptotes)) fdr(bigIRT_modify_asymptotes(fit, sc, sides = asymptotes)) else NULL
  gr <- NULL; dims <- NULL
  sets <- list()
  for(s in itemSets){
    x <- unique(dat[, c(itn, s), with = FALSE])
    for(l in sort(unique(as.character(x[[s]])))){
      its <- intersect(as.character(x[[itn]][as.character(x[[s]]) == l]), sc$item_labels)
      if(length(its) >= minSetItems && length(its) < length(sc$item_labels)) sets[[paste0(s, "=", l)]] <- its
    }
  }
  ## From the scores alone: the leading directions of residual covariance,
  ## against their simulated null, found from a random half of the persons;
  ## a direction's heavily loading items, split by sign, become candidate
  ## sets, tested and sized on the OTHER half. A set chosen by maximising over
  ## the same persons it is then tested on looks larger than it is: on
  ## Mindsteps such sets read SD 2.3 to 3.8 where the named sets read 0.35 to
  ## 0.60. On the
  ## other half the choice is fixed in advance, so the set's own test is valid
  ## and its size unbiased.
  ## The set a batch would add takes its members from the same direction
  ## found on all persons (matched to the half's, sign aligned): half the
  ## persons give a noisier direction, which on a simulated testlet dropped
  ## one of its five items. The test and size stay those of the half-chosen
  ## set on the other half; the held-out gate judges the batch itself.
  eig_sets <- list(); eig_members <- list()
  if(isTRUE(exploratory) && length(sc$item_labels) >= 2L * minSetItems){
    half <- local({ old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
      on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv())); set.seed(1L)
      stats::runif(length(sc$person_labels)) < .5 })
    dims <- tryCatch(bigIRT_modify_dimensions(fit, m = 2L, nsim = nsimDim, cores = cores, persons = half),
      error = function(e) NULL)
    dims_all <- if(is.null(dims)) NULL else tryCatch(bigIRT_modify_dimensions(fit, m = 2L, nsim = 0L, cores = cores),
      error = function(e) NULL)
    if(!is.null(dims)){
      J <- length(sc$item_labels)
      pick <- function(vs) { thr <- max(max(vs) / 2, 1 / sqrt(J)); sc$item_labels[vs >= thr] }
      for(k in seq_along(dims$values)){
        v <- dims$vectors[, k]
        vf <- v
        if(!is.null(dims_all)){
          cc <- as.numeric(stats::cor(v, dims_all$vectors))
          kk <- which.max(abs(cc)); vf <- sign(cc[kk]) * dims_all$vectors[, kk]
        }
        ## A set: the items loading at least half the direction's largest
        ## loading of that sign, and above the level of an even spread.
        for(sg in c(1, -1)){
          its <- pick(sg * v)
          if(length(its) >= minSetItems){
            nm <- paste0("eigen", k, if(sg > 0) "+" else "-")
            eig_sets[[nm]] <- its
            mem <- pick(sg * vf)
            eig_members[[nm]] <- if(length(mem) >= minSetItems) mem else its
          }
        }
      }
      dims$values_all <- if(is.null(dims_all)) NULL else dims_all$values
    }
  }
  gr <- NULL
  if(length(sets)){
    gr <- bigIRT_modify_groups(fit, sc, sets, cores = cores)
    if(!is.null(gr) && nrow(gr)){
      gr[, `:=`(source = sub("=.*$", "", set), level = sub("^[^=]*=", "", set))]
      gr[, p_fdr := stats::p.adjust(p, "BH")]
    }
  }
  if(length(eig_sets)){
    ge <- bigIRT_modify_groups(fit, sc, eig_sets, cores = cores, persons = !half)
    if(!is.null(ge) && nrow(ge)){
      ge[, `:=`(source = "eigen", level = set, p_fdr = stats::p.adjust(p, "BH"))]
      gr <- data.table::rbindlist(list(gr, ge), fill = TRUE)
    }
  }
  if(!is.null(gr)){
    attr(gr, "sets") <- c(sets, eig_members)
    attr(gr, "sets_tested") <- c(sets, eig_sets)
  }
  sh <- if(isTRUE(shape)) fdr(bigIRT_modify_shape(fit, cores = cores)) else NULL
  list(ability = ab, dif = di, asymptotes = as, groups = gr, dimensions = dims, shape = sh, bydim = bd)
}

#' Improve a model by score tests and held-out likelihood
#'
#' Starting from a base model, repeatedly tests candidate changes at the
#' current fit ([modificationTests()]), refits the promising ones, and keeps
#' a change only if it improves the marginal likelihood of persons held out of
#' the fit entirely ([heldoutLogLik()]). Changes come in four kinds, each
#' refitted as a batch: covariates of ability (indicators, or a 3-df natural
#' spline), DIF on item intercepts or slopes for the flagged items along
#' person or response covariates, freed lower or upper asymptotes for the
#' flagged items, and group dimensions for item sets with local dependence
#' (each set's items load on a dimension of its own, uncorrelated with the
#' rest). A candidate must be significant after Benjamini-Hochberg within its
#' kind and large enough to matter (`minEffect` change in the probability of
#' a correct answer for DIF, `minAsymptote` for an asymptote, `minGroupSD` for
#' a group dimension's SD on the logit scale); a batch must raise the held-out
#' log-likelihood by at least `minGain`.
#'
#' Within a round the kinds are taken in turn (`order`), the more general
#' first: ability, then dimensions, then asymptotes, then DIF (with the
#' impact of DIF covariates just before it). Each kind is screened and judged
#' at the model as it stands after the batches kept before it. So DIF is
#' judged with the dimensions in, where a group difference specific to one
#' subdomain can be the covariate's effect on that dimension rather than DIF
#' on its items (in one dimension it looks exactly like DIF, and was kept as
#' such in 90 to 100 per cent of simulated runs with none). An unmodelled
#' asymptote likewise looks like DIF between groups of different ability. A
#' covariate's effect on ability is in the model before DIF along it is
#' judged (DIF absorbs impact otherwise). Each kind is screened when its turn
#' comes, for itself alone.
#'
#' * DIF along each covariate (and parameter) is a batch of its own, the
#'   strongest first, so each covariate's DIF has to earn its place.
#' * With `difImpact`, a DIF covariate whose effect on ability the model lacks
#'   in any form (judged by the span of the ability predictors) first gets it,
#'   as a batch of its own; a response covariate's within-person effect
#'   included.
#' * With `asymptotePooled`, the screened asymptotes have an alternative: the
#'   asymptote free on every item, pooled by its prior.
#' * Group dimensions go in one per round, the strongest first. With
#'   `partitions`, an item-set column that covers every item (a partition,
#'   such as subdomains) is offered as an alternative: its levels as
#'   correlated ability dimensions.
#' * With `commonEffects`, a dimension is judged on the dependence it
#'   explains, not on group differences: the ability predictors keep one
#'   effect common to a partition's dimensions and have none on a group
#'   dimension. Freed by dimension, a covariate's effects would let a
#'   dimension take in DIF on a few of its items. In the DIF stage each DIF
#'   covariate's effect on ability is then also screened for differing by
#'   dimension (DIF on all of a dimension's items at once), by a score test
#'   of its own: a dimension's abilities take up most of such a gap, so the
#'   item DIF screens barely see it. Where a covariate has both, the two are
#'   alternatives; the next round screens both again. A covariate's effect is
#'   carried by the predictor of that name, the terms added for it here, and
#'   any `abilityTerms` names for it.
#'
#' Of alternatives that pass the gate, the one with fewer terms is kept,
#' unless a larger one beats it on the held-out persons by the gate itself
#' (`minGain`, `minZ`, the larger against the smaller). Chosen by the larger
#' gain alone, near ties went to noise: DIF on all of a subdomain's items
#' beat the subdomain's own effect, 20 terms against 2, by under 3 nats out
#' of about 100. The comparison is in `path` (`vs_smaller_gain`,
#' `vs_smaller_z`).
#'
#' The next round screens the result, for the kinds whose verdict could have
#' changed: a kind is tried again only if it was kept, or a kind after it
#' was, in the round before (a kind rejected in a round was already judged
#' against every batch kept before it). The final model is refitted on all
#' persons.
#'
#' Selection uses the data, so the final model's estimates and standard
#' errors are conditional on it: the held-out gain is the honest measure of
#' what the changes are worth, not the in-sample fit.
#'
#' On large data a run takes hours, so with `checkpoint` a directory keeps
#' each completed step (the base fit, each round's screens, each batch's
#' result and the fits kept) and a rerun resumes from them. A step is reused
#' only when the data size, holdout, settings and model specification it was
#' made under are identical; anything else is recomputed. Only completed steps
#' are written. Fits are kept uncompressed and can be gigabytes.
#'
#' @param dat Long data, one row per response.
#' @param score,id,item,scale,pl As for [fitIRT()]; the base model.
#' @param personPreds Person predictors in the base model's ability
#'   regression.
#' @param personCovariates,responseCovariates,itemSets,dif,difCovariates,asymptotes
#'   What to test; see [modificationTests()].
#' @param holdout Share of persons held out to judge changes.
#' @param alpha FDR level for candidates.
#' @param minEffect,minAsymptote,minGroupSD Smallest changes worth adding.
#' @param minZ,minGain A batch is kept when the held-out persons' log-
#'   likelihood rises, compared person by person: by at least `minGain` nats
#'   in total and by at least `minZ` standard errors of the per-person
#'   differences (default the one-sided 5 per cent point). In simulation (50
#'   replications per truth) this found 80 per cent of DIF items and the
#'   testlet in 88 to 94 per cent of runs, where z = 2 found 70 and 72 to 84,
#'   and with no true change it added nothing at either.
#' @param nsimDim Simulations for the null of the exploratory dimensions (0
#'   turns them off); see [modificationTests()].
#' @param maxRounds,maxGroups Limits on rounds and on group dimensions. Group
#'   dimensions are added one per round, the strongest set first, so two
#'   dimensions need two rounds: until a real dimension is modelled, other sets
#'   look dependent too.
#' @param seed For the holdout split.
#' @param cores Threads.
#' @param verbose Print progress.
#' @param fitArgs Further [fitIRT()] arguments (default `laplaceRefine = "agq"`).
#'   Leave `dropPerfectScores` at its default (FALSE): dropping all-correct
#'   persons biases the upper-asymptote screen (null size 15 per cent on a
#'   simulated adaptive bank, against 5 with them kept).
#' @param checkpoint A directory to keep completed steps in, so that an
#'   interrupted run resumes; `NULL` (default) keeps nothing.
#' @param difImpact,asymptotePooled,partitions Offer the impact of DIF
#'   covariates, pooled asymptotes and partitions into correlated dimensions
#'   (see Details).
#' @param order The kinds in the order they are screened and judged within a
#'   round: each of "ability", "dimensions", "asymptotes" and "dif" once
#'   (default in that order, the more general first; see Details).
#' @param commonEffects Judge dimensions with the ability predictors'
#'   effects common to them, and offer each DIF covariate's effect by
#'   dimension as an alternative to its item DIF (see Details). `FALSE`
#'   gives every dimension its own effects as soon as it is added.
#' @param abilityTerms A named list: for a covariate, the `personPreds` that
#'   carry its effect on ability under other names (an age covariate entering
#'   as a person mean and a deviation, say), freed with it by dimension.
#' @param combineBydim Where a covariate's effect by dimension and its item
#'   DIF both pass their screens, offer a third alternative besides the two:
#'   the effect by dimension together with item DIF screened again at the
#'   model that has the effect. Item DIF flagged against a common effect
#'   covers a dimension's items wholesale where the gap differs by
#'   dimension, so without this a gap specific to one dimension with true
#'   DIF inside it can be described only as one or the other: on simulated
#'   data the item DIF won, took in the gap on many of the dimension's items,
#'   and the next round could not undo it.
#' @param gateCluster Optional: a column of `dat`, constant within person,
#'   naming the cluster each person belongs to (a school, say). The gate's z
#'   (and the comparison between alternatives) then uses a cluster-robust
#'   (CR1) standard error of the summed per-person gain, for held-out persons
#'   who are not independent. With every person their own cluster it equals
#'   the default.
#' @return A `bigIRT_improved` object: `fit` (final model, all persons),
#'   `spec` (the terms added), `path` (each batch tried: round, kind, terms,
#'   held-out log-likelihood before and after, gain and its z, accepted,
#'   seconds to fit and evaluate, for a larger alternative its gain and z
#'   over the smaller, the batch fit's warnings, such as a fit that stopped
#'   on unresolved person modes, and the training log-likelihood and free
#'   parameters before and after, `train_loglik_before`,
#'   `train_loglik_after`, `df_before`, `df_after`, `train_persons`: what a
#'   likelihood ratio, AIC or BIC would judge the batch by; the free
#'   parameters leave out item-specific coefficients whose covariate is zero
#'   on all of the item's training responses), `screens` (each kind's tests, from the
#'   screen it was last judged on), `screens_all` (every screen made, named by
#'   step: a round's first screen `round1_screens`, then
#'   `round1_screens-<kind>`), `base_fit` (the base model on the training persons),
#'   `holdout_persons`, `timing` (seconds: base fit, each screen, final fit),
#'   `base` (the settings a specification is built under: to refit `spec`
#'   elsewhere, pass this rather than rebuilding it), `fit_warnings` (the
#'   base and final fits' warnings).
#' @seealso [modificationTests()], [heldoutLogLik()]
#' @export
improveModel <- function(dat, score = "score", id = "id", item = "Item", scale = "Scale", pl = 2L,
  personPreds = character(), personCovariates = character(), responseCovariates = character(),
  itemSets = character(), dif = c("B", "A"), difCovariates = NULL, asymptotes = c("lower", "upper"),
  holdout = 0.2, alpha = 0.05, minEffect = 0.03, minAsymptote = 0.05, minGroupSD = 0.3,
  minZ = stats::qnorm(.95), minGain = 0, nsimDim = 19L, maxRounds = 3L, maxGroups = 2L, seed = 1L, cores = 1L,
  verbose = TRUE, fitArgs = list(laplaceRefine = "agq"), checkpoint = NULL,
  difImpact = TRUE, asymptotePooled = TRUE, partitions = TRUE,
  order = c("ability", "dimensions", "asymptotes", "dif"), commonEffects = TRUE, abilityTerms = list(),
  combineBydim = TRUE, gateCluster = NULL){
  kinds_all <- c("ability", "dimensions", "asymptotes", "dif")
  order <- match.arg(order, kinds_all, several.ok = TRUE)
  if(length(order) != length(kinds_all) || anyDuplicated(order))
    stop("order must name each of 'ability', 'dimensions', 'asymptotes' and 'dif' once.", call. = FALSE)
  if(length(abilityTerms) && (!is.list(abilityTerms) || is.null(names(abilityTerms)) || any(names(abilityTerms) == "")))
    stop("abilityTerms must be a named list: covariate = the ability predictors that carry its effect.", call. = FALSE)
  if(length(bad <- setdiff(unlist(abilityTerms), personPreds)))
    stop("abilityTerms names predictors not in personPreds: ", paste(bad, collapse = ", "), call. = FALSE)
  dat <- data.table::as.data.table(dat)
  if(!scale %in% names(dat)) { dat <- data.table::copy(dat); data.table::set(dat, j = scale, value = 1L) }
  persons <- unique(dat[[id]])
  ## The gate's clusters, by person (gateCluster), if any.
  clus <- NULL
  if(!is.null(gateCluster)){
    if(!gateCluster %in% names(dat)) stop("gateCluster names no column of dat.", call. = FALSE)
    cl <- dat[, list(n = data.table::uniqueN(get(gateCluster)), c = get(gateCluster)[1]), by = c(id)]
    if(any(cl$n != 1L) || anyNA(cl$c)) stop("gateCluster must be one non-missing value per person.", call. = FALSE)
    clus <- stats::setNames(as.character(cl$c), as.character(cl[[id]]))
  }
  ho <- local({ if(!is.null(seed)) { old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv())); set.seed(seed) }
    sample(persons, round(holdout * length(persons))) })
  train <- which(!dat[[id]] %in% ho)
  fa <- utils::modifyList(list(cores = cores, verbose = 0, laplaceCertify = FALSE), fitArgs)
  base <- list(score = score, id = id, item = item, scale = scale, pl = as.integer(pl), fitArgs = fa,
    commonEffects = isTRUE(commonEffects), abilityTerms = abilityTerms)
  ## A batch is the current model with terms added, so it starts from the
  ## current model's estimates (`start`): it begins at the model it extends
  ## and can only climb, in far fewer evaluations. (On Mindsteps one batch, a
  ## 3-D 4PL with four DIF terms added, came back 16,551 held-out nats worse
  ## than the model it extended, in a run at its memory limit; refitted alone
  ## from the priors it was 2.7 nats worse. The cause was not established; a
  ## nested batch that starts where the current model ends cannot do that.)
  fit_spec <- function(spec, rows, start = NULL){
    a <- bigIRT_modify_build(dat, spec, base)
    if(!is.null(rows)) a$trainingRows <- rows
    if(!is.null(start)) a$startFrom <- start
    ## A fit's warnings are kept (attribute fit_warnings; the batch's row of
    ## `path`), not dropped. Suppressed, a fit that stopped short -- on
    ## unresolved person modes, say, which on simulated data left fits up to
    ## 33,000 nats below a normal one -- looked exactly like a batch that did
    ## not help.
    w <- character()
    f <- withCallingHandlers(do.call(fitIRT, a), warning = function(x){
      w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning")
    })
    attr(f, "fit_warnings") <- unique(w)
    f
  }
  fit_warns <- function(f) if(is.null(f)) character() else attr(f, "fit_warnings") %||% character()
  say <- function(...) if(isTRUE(verbose)) message(sprintf(...))
  ## What a cached step must match to be reused. Threads and printing do not
  ## change results, so they are left out.
  fp <- list(n = nrow(dat), cols = names(dat), holdout = sort(as.character(ho)),
    base = base[c("score", "id", "item", "scale", "pl", "commonEffects", "abilityTerms")],
    fitArgs = base$fitArgs[setdiff(names(base$fitArgs), c("cores", "verbose"))],
    tests = list(personCovariates, responseCovariates, itemSets, dif, difCovariates, asymptotes, alpha, minEffect,
      minAsymptote, minGroupSD, minZ, minGain, nsimDim, maxGroups, difImpact, asymptotePooled, partitions,
      order))
  ## (Only when set: a checkpoint made without them stays valid.)
  if(isTRUE(combineBydim)) fp$combineBydim <- TRUE
  if(!is.null(gateCluster)) fp$gateCluster <- gateCluster
  ## Each fit's training log-likelihood and parameter count (from the fit).
  train_ll <- function(f) if(!is.null(f)) tryCatch(bigIRT_modify_train_loglik(f), error = function(e) NULL) else NULL
  step <- bigIRT_modify_checkpoint(checkpoint, fp)
  secs <- function(t0) proc.time()[[3]] - t0
  timing <- list()
  spec <- list(personPreds = personPreds, abil = list(), impact = character(), dif = list(), lower = character(),
    upper = character(), pooled = character(), groups = list(), partition = character(), bydim = character())
  base_spec <- spec
  b <- step("base", spec, function(){
    t0 <- proc.time()[[3]]; f <- fit_spec(spec, train)
    list(fit = f, h = heldoutLogLik(f, cores = cores), tl = train_ll(f), seconds = secs(t0), warnings = fit_warns(f))
  })
  cur <- b$fit; cur_ho <- b$h; cur_tl <- b$tl; timing$base <- b$seconds; b_warnings <- b$warnings %||% character()
  ## With a checkpoint the base fit is read back at the end rather than held
  ## through the rounds: on large data each fit is many GB, and a refit holds
  ## the current fit and the new one besides.
  base_fit <- if(is.null(checkpoint)) cur else NULL
  rm(b)
  say("base model: held-out log-likelihood %.1f", cur_ho$loglik)
  path <- list()
  ## The kinds, in the order they are screened and judged within a round
  ## (`order`; "dif" brings its impact stage with it). A kind rejected in a
  ## round already met every batch kept before it, so it is tried again only
  ## if it was kept or a kind after it was (otherwise nothing that could
  ## change its verdict changed, and a retry would only refit the same
  ## candidates).
  kinds <- unname(c(ability = "ability", dimensions = "groups", asymptotes = "asymptotes", dif = "dif")[order])
  live <- kinds
  ## The accepted step whose stored fit is the current model's (none while
  ## the base is current: the base fit is returned and kept).
  holder <- NULL
  ## The current model's fit when it is needed (a screen, a batch's start,
  ## the final refit). A resumed run whose choice between alternatives now
  ## falls the other way has no fit for the chosen one in the checkpoint;
  ## it is refitted here, once, and stored under its step.
  ensure_cur <- function(){
    if(is.null(cur)){
      say("refitting the current model: its fit is not in the checkpoint")
      cur <<- fit_spec(spec, train)
      if(!is.null(checkpoint) && !is.null(holder)){
        v <- holder$tr; v$fit <- cur; bigIRT_modify_store(checkpoint, holder$name, fp, holder$s, v)
      }
    }
    cur
  }
  ## The screen each kind was last judged on (returned as `screens`) and every
  ## screen made (`screens_all`).
  judged_on <- list(); screens_all <- list()
  ## Whether the model has dimensions whose covariate effects are shared
  ## (commonEffects), so that an effect by dimension can be screened.
  multi_dims <- function(s) isTRUE(base$commonEffects) && (length(s$partition) > 0L || length(s$groups) > 0L)
  ## The screen of one kind, at the model as it stands when its turn comes.
  ## A screen made before a batch was kept no longer describes the model, so
  ## each kind is screened at the model it will be judged against. (Screened
  ## once per round, at the round's start, DIF was judged against a
  ## one-dimensional model whose subdomain dimensions came later in the round.
  ## A group gap specific to one subdomain then looks like DIF on that
  ## subdomain's items: on a simulated bank with no DIF it was kept in 90 to
  ## 100 per cent of runs, and on Mindsteps it moved the gender and
  ## first-language gaps by up to 0.14 years of growth.) Each kind is screened
  ## alone: screened ahead, while nothing was yet kept, a later kind's tests
  ## were wasted whenever a batch before it was kept, and a DIF screen takes
  ## hours on Mindsteps (it also warned about anchors for a screen that was
  ## never used).
  screen_at <- function(left, name){
    more_groups <- "groups" %in% left && !length(spec$partition) && length(spec$groups) < maxGroups
    sr <- step(name, spec, function(){
      t0 <- proc.time()[[3]]
      a_cur <- bigIRT_modify_build(dat, spec, base)
      ## With dimensions whose covariate effects are shared (commonEffects),
      ## the DIF screen also tests each DIF covariate's effect by dimension.
      bdt <- list()
      if("dif" %in% left && multi_dims(spec)){
        dcovs <- setdiff(if(is.null(difCovariates)) c(personCovariates, responseCovariates) else difCovariates, spec$bydim)
        bdt <- Filter(length, stats::setNames(lapply(dcovs, bigIRT_modify_cov_terms, pp = a_cur$personPreds,
          abilityTerms = base$abilityTerms), dcovs))
      }
      s <- modificationTests(ensure_cur(), a_cur$dat, personCovariates = personCovariates,
        responseCovariates = responseCovariates, itemSets = if(more_groups) itemSets else character(),
        dif = if("dif" %in% left) dif else character(), difCovariates = difCovariates,
        asymptotes = if("asymptotes" %in% left) asymptotes else character(),
        exploratory = more_groups && nsimDim > 0L, nsimDim = nsimDim,
        cores = cores, byDimension = bdt)
      list(screens = s, seconds = secs(t0))
    })
    timing[[name]] <<- sr$seconds
    s <- sr$screens; attr(s, "more_groups") <- more_groups
    screens_all[[name]] <<- s
    s
  }
  ## The batches one kind offers, from its screen: one stage per batch, or a
  ## choice between alternative batches of the kind.
  stages_for <- function(kind, screens){
    stages <- list()
    stage <- function(kind, ...) list(kind = kind, alts = list(...))
    alt <- function(label, apply, terms) list(label = label, apply = apply, terms = terms)
    if(kind == "ability"){
      ab <- screens$ability
      covs_in <- vapply(spec$abil, function(a) a$covariate, "")
      pc_left <- setdiff(personCovariates, c(covs_in, personPreds))
      if(!is.null(ab) && nrow(ab)){
        ab <- ab[covariate %in% pc_left & p_fdr < alpha]
        if(nrow(ab)) local({
          ## (local: each batch's function must keep its own terms, not a loop
          ## variable that later candidates overwrite.)
          xa <- lapply(seq_len(nrow(ab)), function(k) list(covariate = ab$covariate[k], kind = ab$kind[k]))
          stages[[length(stages) + 1L]] <<- stage("ability", alt("ability", function(s){ s$abil <- c(s$abil, xa); s }, length(xa)))
        })
      }
    }
    if(kind == "dif"){
      di <- screens$dif
      dif_groups <- list()
      if(!is.null(di) && nrow(di)){
        di <- di[!is.na(p_fdr) & p_fdr < alpha & !is.na(dP_abs) & dP_abs >= minEffect]
        done <- data.table::rbindlist(lapply(spec$dif, function(t) data.table::data.table(covariate = t$covariate,
          parameter = t$parameter, item = t$items)))
        if(nrow(done)) di <- di[!done, on = c("covariate", "parameter", "item")]
        ## DIF along each covariate (and parameter) is its own batch, the
        ## strongest first, so each covariate's DIF has to earn its place.
        if(nrow(di)) dif_groups <- lapply(split(di, by = c("covariate", "parameter"), keep.by = TRUE), function(x)
          list(covariate = x$covariate[1], parameter = x$parameter[1], kind = x$kind[1], items = x$item,
            strength = sum(x$LM, na.rm = TRUE)))
        dif_groups <- dif_groups[order(-vapply(dif_groups, `[[`, 0, "strength"))]
      }
      ## A DIF covariate whose effect on ability the model lacks (in any form)
      ## gets it first, as a batch of its own: DIF along it would otherwise
      ## absorb impact, and judged together the two could not be told apart.
      ## What is missing is judged against the model as it stands when the
      ## stage is reached: a batch before it may have added the covariate
      ## already.
      if(isTRUE(difImpact) && length(dif_groups)){
        dif_covs <- unique(vapply(dif_groups, `[[`, "", "covariate"))
        need_for <- function(s){
          cand <- setdiff(dif_covs, s$impact)
          if(!length(cand)) return(character())
          a <- bigIRT_modify_build(dat, s, base)
          Filter(function(v){
            z <- a$dat[[v]]
            B <- bigIRT_modify_basis(z, if(is.character(z) || is.factor(z) || is.logical(z) ||
              length(unique(stats::na.omit(z))) <= 4L) "factor" else "linear", NULL)
            length(bigIRT_modify_unspanned(a$dat, a$personPreds, B)) > 0L
          }, cand)
        }
        if(length(need_for(spec))) stages[[length(stages) + 1L]] <- stage("impact",
          alt("impact", function(s){ s$impact <- union(s$impact, need_for(s)); s },
            function(s) length(need_for(s))))
      }
      ## With dimensions in the model and the covariates' effects common to
      ## them (commonEffects), a covariate's effect on ability may differ by
      ## dimension: DIF in a broad sense, on all of a dimension's items at
      ## once. It has a screen of its own (bigIRT_modify_bydim_tests), since
      ## a dimension's abilities take up most of such a gap and item DIF
      ## screens barely see it. Where the covariate also has item DIF, the
      ## two are alternatives, judged against each other, and the better that
      ## passes the gate is kept; the next round screens both again at the
      ## new model. (Judged after dimensions with free effects instead, a
      ## dimension took in DIF on a few of its items: recall of true DIF fell
      ## from 66 to 34 per cent on a simulated bank.) Effects by dimension
      ## without item DIF are offered first, as the more general change.
      bd <- screens$bydim
      bd_ok <- if(multi_dims(spec) && !is.null(bd) && nrow(bd)) bd[!is.na(p_fdr) & p_fdr < alpha & !covariate %in% spec$bydim] else NULL
      bydim_alt <- function(v){
        nd <- if(length(spec$partition)) data.table::uniqueN(dat[[spec$partition]]) - 1L else length(spec$groups)
        alt(paste("bydim", make.names(v), sep = "-"), function(s){ s$bydim <- union(s$bydim, v); s },
          nd * bd_ok$terms[match(v, bd_ok$covariate)])
      }
      dif_covs <- unique(vapply(dif_groups, `[[`, "", "covariate"))
      if(!is.null(bd_ok)) for(v in bd_ok[order(-LM)][!covariate %in% dif_covs, covariate]) local({ vv <- v
        stages[[length(stages) + 1L]] <<- stage("dif", bydim_alt(vv))
      })
      offered <- character()
      for(x in dif_groups) local({ xx <- x[c("covariate", "parameter", "kind", "items")]
        alts <- list(alt(paste("dif", make.names(xx$covariate), xx$parameter, sep = "-"),
          function(s){ s$dif <- bigIRT_modify_merge_dif(c(s$dif, list(xx))); s }, length(xx$items)))
        if(!is.null(bd_ok) && xx$covariate %in% bd_ok$covariate && !xx$covariate %in% offered){
          b_alt <- bydim_alt(xx$covariate); offered <<- c(offered, xx$covariate)
          if(isTRUE(combineBydim)){
            ## A third alternative: the effect by dimension AND item DIF,
            ## screened again at the model that has the effect (the DIF the
            ## current screen flags is measured against a common effect, so
            ## where the gap differs by dimension it covers the dimension's
            ## items wholesale). Without it, a gap specific to one subdomain
            ## with true DIF inside it had to be either one or the other: on
            ## simulated data the item DIF won, took in the gap on half the
            ## subdomain's items, and could not be undone by the next round
            ## (false DIF on 9 to 11 items a run). The effect by dimension is
            ## tried first and keeps its fit, which the combination is
            ## screened at and starts from; then the combination; then item
            ## DIF alone.
            b_alt$keep_fit <- TRUE
            combo <- list(label = paste0(b_alt$label, "+dif"), terms = NA_integer_,
              combo = list(after = b_alt$label, base_terms = b_alt$terms, covariate = xx$covariate,
                parameter = xx$parameter, kind = xx$kind))
            alts <- c(list(b_alt, combo), alts)
          } else alts <- c(alts, list(b_alt))
        }
        stages[[length(stages) + 1L]] <<- do.call(stage, c(list("dif"), alts))
      })
    }
    if(kind == "asymptotes"){
      as <- screens$asymptotes
      as_alts <- list()
      if(!is.null(as) && nrow(as) && length(setdiff(asymptotes, spec$pooled))){
        as <- as[p_fdr < alpha & estimate >= minAsymptote]
        if(nrow(as)){
          lo <- as[side == "lower", item]; up <- as[side == "upper", item]
          as_alts[[1]] <- alt("asymptotes", function(s){ s$lower <- union(s$lower, lo); s$upper <- union(s$upper, up); s },
            length(lo) + length(up))
        }
        ## The alternative: the asymptote free on EVERY item, pooled by the
        ## asymptote priors. Where slips or guesses are many, small and each
        ## barely identified, screening finds only the large ones (on Mindsteps
        ## a 4PL everywhere beat the screened selection by 431 held-out nats).
        sa <- screens$asymptotes
        sides <- setdiff(intersect(asymptotes, unique(sa[!is.na(p_fdr) & p_fdr < alpha, side])), spec$pooled)
        if(isTRUE(asymptotePooled) && length(sides))
          as_alts[[length(as_alts) + 1L]] <- alt(paste0("asymptotes-pooled-", paste(sides, collapse = "-")),
            function(s){ s$pooled <- union(s$pooled, sides); s }, length(unique(screens$asymptotes$item)) * length(sides))
      }
      if(length(as_alts)) stages[[length(stages) + 1L]] <- do.call(stage, c(list("asymptotes"), as_alts))
    }
    if(kind == "groups"){
      gr <- screens$groups
      gr_alts <- list()
      if(isTRUE(attr(screens, "more_groups")) && !is.null(gr) && nrow(gr)){
        ## One group dimension per round, the strongest first. Until a real
        ## dimension is modelled the general ability leans towards its items,
        ## and the other items share the leftover covariance: tried together
        ## with a real testlet, a 15-item set with no structure of its own rode
        ## in with it in 10 per cent of simulated runs. The next round re-screens
        ## with the first dimension in the model.
        keep <- bigIRT_modify_pick_groups(gr, attr(gr, "sets"), unlist(lapply(spec$groups, `[[`, "items")),
          min(1L, maxGroups - length(spec$groups)), alpha, minGroupSD)
        if(length(keep)) gr_alts[[1]] <- alt("groups", function(s){ s$groups <- c(s$groups, keep); s }, length(keep))
        ## The alternative when an item-set column covers every item (a
        ## partition, such as subdomains): its levels as correlated dimensions,
        ## which is what a partition calls for; group factors on a general
        ## ability can add at most maxGroups of its levels, orthogonally.
        if(isTRUE(partitions) && !length(spec$groups) && !length(spec$partition)){
          for(v in itemSets){
            lv <- unique(dat[, c(item, v), with = FALSE])
            if(anyNA(lv[[v]]) || data.table::uniqueN(lv[[v]]) < 2L || anyDuplicated(lv[[item]])) next
            gv <- gr[source == v]
            if(!nrow(gv) || !any(gv$p_fdr < alpha & sqrt(gv$tau2) >= minGroupSD, na.rm = TRUE)) next
            local({ vv <- v; nl <- data.table::uniqueN(lv[[v]])
              gr_alts[[length(gr_alts) + 1L]] <<- alt(paste0("partition-", make.names(vv)),
                function(s){ s$partition <- vv; s }, nl) })
          }
        }
      }
      if(length(gr_alts)) stages[[length(stages) + 1L]] <- do.call(stage, c(list("groups"), gr_alts))
    }
    stages
  }
  ## Whether a kind has anything to test against the model as it stands (a
  ## screen costs a pass over the data even when it has nothing to do).
  has_tests <- function(kind) switch(kind,
    ability = length(setdiff(personCovariates, c(vapply(spec$abil, function(a) a$covariate, ""), personPreds))) > 0L,
    groups = !length(spec$partition) && length(spec$groups) < maxGroups && (length(itemSets) > 0L || nsimDim > 0L),
    asymptotes = length(setdiff(asymptotes, spec$pooled)) > 0L,
    dif = length(dif) > 0L && length(if(is.null(difCovariates)) c(personCovariates, responseCovariates) else difCovariates) > 0L)
  for(round in seq_len(maxRounds)){
    ho_start <- cur_ho$loglik; kept <- character(); passed <- list(); tried <- FALSE
    first_screen <- TRUE
    for(ki in seq_along(kinds)){
      kind <- kinds[ki]
      if(!kind %in% live || !has_tests(kind)) next
      scr <- screen_at(kind, if(first_screen) sprintf("round%d_screens", round) else sprintf("round%d_screens-%s", round, kind))
      first_screen <- FALSE
      judged_on[[kind]] <- scr
      for(sg in stages_for(kind, scr)){
        tried <- TRUE
        ## Each alternative judged against the current model. Of those that
        ## pass the gate, the one with fewer terms is kept unless a larger one
        ## beats it by the gate as well (bigIRT_modify_prefer).
        best <- NULL
        ## Fits an alternative keeps for a later one of the same stage (the
        ## effect by dimension, for its combination with item DIF).
        stage_fits <- list()
        for(a in sg$alts){
          if(!is.null(a$combo)){
            ## Item DIF screened again at the model with the covariate's effect
            ## by dimension, and that model plus the items flagged there.
            pr <- stage_fits[[a$combo$after]]
            if(is.null(pr)){ say("round %d, %s: the effect by dimension has no fit to screen at", round, a$label); next }
            sname <- sprintf("round%d_%s-screen", round, a$label)
            rs <- step(sname, pr$s, function(){
              t0 <- proc.time()[[3]]
              a_b <- bigIRT_modify_build(dat, pr$s, base)
              sc <- tryCatch(suppressWarnings(modificationTests(pr$fit, a_b$dat, personCovariates = personCovariates,
                responseCovariates = responseCovariates, itemSets = character(), dif = a$combo$parameter,
                difCovariates = a$combo$covariate, asymptotes = character(), exploratory = FALSE, cores = cores)),
                error = function(e) NULL)
              d <- if(is.null(sc)) NULL else sc$dif
              items <- if(!is.null(d) && nrow(d)) as.character(d[!is.na(p_fdr) & p_fdr < alpha & !is.na(dP_abs) &
                dP_abs >= minEffect, item]) else character()
              list(items = items, seconds = secs(t0))
            })
            timing[[sname]] <- rs$seconds
            if(!length(rs$items)){ say("round %d, %s: screened at the effect by dimension, no item has DIF", round, a$label); next }
            local({ xx <- list(covariate = a$combo$covariate, parameter = a$combo$parameter, kind = a$combo$kind,
              items = rs$items); s_b <- pr$s
              a$apply <<- function(s0){ s <- s_b; s$dif <- bigIRT_modify_merge_dif(c(s$dif, list(xx))); s } })
            a$terms <- a$combo$base_terms + length(rs$items)
            a$start <- pr$fit
          }
          if(is.function(a$terms)) a$terms <- a$terms(spec)
          s <- a$apply(spec)
          ## Nothing left to add once the batches before it are in: no refit.
          if(identical(s, spec)){ say("round %d, %s: nothing left to add", round, a$label); next }
          tr <- step(sprintf("round%d_%s", round, a$label), s, function(){
            t0 <- proc.time()[[3]]
            ## A batch whose fit or held-out evaluation fails is rejected, and
            ## the reason kept: a failure is not evidence against the change.
            err <- NULL
            f <- tryCatch(fit_spec(s, train, start = if(!is.null(a$start)) a$start else ensure_cur()),
              error = function(e){ err <<- conditionMessage(e); NULL })
            h <- if(is.null(f)) NULL else tryCatch(heldoutLogLik(f, cores = cores),
              error = function(e){ err <<- paste("heldoutLogLik:", conditionMessage(e)); NULL })
            gm <- if(is.null(h)) list(gain = NA_real_, z = NA_real_) else bigIRT_modify_gain(cur_ho, h, clusters = clus)
            ok <- isTRUE(gm$gain >= minGain && gm$z >= minZ)
            ## Only a fit that passes is needed again, or one a later
            ## alternative starts from.
            list(fit = if(ok || isTRUE(a$keep_fit)) f else NULL, h = h, tl = train_ll(f), gain = gm$gain, z = gm$z, ok = ok,
              seconds = secs(t0), error = err, warnings = fit_warns(f))
          })
          if(isTRUE(a$keep_fit) && !is.null(tr$fit))
            stage_fits[[a$label]] <- list(fit = tr$fit, s = s, name = sprintf("round%d_%s", round, a$label), tr = tr)
          path[[length(path) + 1L]] <- data.table::data.table(round = round, kind = sg$kind, batch = a$label,
            terms = a$terms, heldout_before = cur_ho$loglik, heldout_after = if(is.null(tr$h)) NA_real_ else tr$h$loglik,
            gain = tr$gain, z = tr$z, accepted = FALSE, seconds = tr$seconds,
            error = if(is.null(tr$error)) NA_character_ else tr$error,
            vs_smaller_gain = NA_real_, vs_smaller_z = NA_real_,
            fit_warnings = if(length(tr$warnings)) paste(tr$warnings, collapse = " | ") else NA_character_)
          path[[length(path)]][, `:=`(
            train_loglik_before = cur_tl$loglik %||% NA_real_, train_loglik_after = tr$tl$loglik %||% NA_real_,
            df_before = cur_tl$df %||% NA_real_, df_after = tr$tl$df %||% NA_real_,
            train_persons = cur_tl$persons %||% NA_real_)]
          if(!is.null(tr$error)) say("round %d, %s: %d terms, failed (%s)", round, a$label, a$terms, tr$error)
          else say("round %d, %s: %d terms, held-out gain %.1f (z %.1f)%s", round, a$label, a$terms,
            tr$gain, tr$z, if(tr$ok) " (passes)" else "")
          if(length(tr$warnings)) say("  (the fit warned: %s)", paste(tr$warnings, collapse = " | "))
          if(tr$ok){
            ## Only the stage's best fit is held in memory; the others keep
            ## their verdicts.
            cand <- list(tr = tr, s = s, at = length(path), terms = a$terms)
            is_best <- is.null(best) || {
              pr <- bigIRT_modify_prefer(best, cand, minGain, minZ, clusters = clus)
              ## The comparison, recorded on the larger alternative's row.
              li <- if(isTRUE(all.equal(cand$terms, best$terms))) NA_integer_ else
                if(cand$terms > best$terms) cand$at else best$at
              if(!is.na(li)){ path[[li]]$vs_smaller_gain <- pr$gain; path[[li]]$vs_smaller_z <- pr$z }
              pr$cand
            }
            slim_tr <- tr; slim_tr$fit <- NULL
            passed[[length(passed) + 1L]] <- list(name = sprintf("round%d_%s", round, a$label), s = s,
              tr = if(is_best) tr else slim_tr)
            if(is_best){
              if(!is.null(best)) passed[[best$slot]]$tr$fit <- NULL
              best <- c(cand, list(slot = length(passed)))
            }
            cand <- NULL
          }
          tr <- NULL; gc(verbose = FALSE)
        }
        if(!is.null(best)){
          ## (On a resumed run the chosen alternative's fit may not be in the
          ## checkpoint, if it lost under the rule the run was made with:
          ## ensure_cur refits it when it is next needed.)
          spec <- best$s; cur <- best$tr$fit; cur_ho <- best$tr$h; cur_tl <- best$tr$tl; kept <- c(kept, sg$kind)
          path[[best$at]]$accepted <- TRUE
          ## Only the latest accepted fit is needed again (a resumed run takes
          ## its model from it), so the checkpoint keeps every other step's
          ## verdict but not its fit: the alternatives that passed and lost,
          ## and the fit this one supersedes. On Mindsteps each fit is 13 GB,
          ## and a round can accept a batch per DIF covariate.
          if(!is.null(checkpoint)){
            slim <- c(passed[-best$slot], if(!is.null(holder)) list(holder))
            for(x in slim){ v <- x$tr; v$fit <- NULL; bigIRT_modify_store(checkpoint, x$name, fp, x$s, v) }
          }
          holder <- passed[[best$slot]]
        }
        ## A fit kept for the combination and not chosen stays in the
        ## checkpoint only as a verdict, like any other step.
        if(!is.null(checkpoint) && length(stage_fits)){
          bn <- if(!is.null(best)) passed[[best$slot]]$name else ""
          for(x in stage_fits) if(!identical(x$name, bn)){ v <- x$tr; v$fit <- NULL; bigIRT_modify_store(checkpoint, x$name, fp, x$s, v) }
        }
        stage_fits <- NULL
        passed <- list()
      }
    }
    if(!tried){ say("round %d: nothing to add", round); break }
    if(!length(kept)) break
    say("round %d: held-out log-likelihood %.1f -> %.1f", round, ho_start, cur_ho$loglik)
    ## (An impact batch belongs to the DIF kind.)
    kk <- ifelse(kept == "impact", "dif", kept)
    last_kept <- max(match(kk, kinds))
    live <- kinds[kinds %in% kk | seq_along(kinds) < last_kept]
  }
  fin <- step("final", spec, function(){
    t0 <- proc.time()[[3]]; f <- fit_spec(spec, NULL, start = ensure_cur())
    list(fit = f, seconds = secs(t0), warnings = fit_warns(f))
  })
  timing$final <- fin$seconds
  rm(cur)
  if(is.null(base_fit)) base_fit <- step("base", base_spec, function() stop("the checkpoint lost the base step"))$fit
  ## Each kind's tests as it was last judged on them.
  screens <- list(ability = judged_on$ability$ability, dif = judged_on$dif$dif,
    asymptotes = judged_on$asymptotes$asymptotes, groups = judged_on$groups$groups,
    dimensions = judged_on$groups$dimensions, bydim = judged_on$dif$bydim)
  structure(list(fit = fin$fit, spec = spec, path = data.table::rbindlist(path, fill = TRUE), screens = screens,
    screens_all = screens_all, base_fit = base_fit, holdout_persons = ho, timing = unlist(timing), base = base,
    fit_warnings = list(base = b_warnings, final = fin$warnings %||% character())),
    class = "bigIRT_improved")
}

## Group dimensions to try: significant sets above the size floor that share
## no item with a group already in the model or with one picked before them,
## at most `slots` of them. Named sets (from item labels) come first, by z,
## then sets found from the scores alone: a labelled set was specified in
## advance and says what the dimension is, and the exploratory route is for
## structure the labels miss.
bigIRT_modify_pick_groups <- function(gr, sets, used, slots, alpha, minGroupSD){
  gr <- gr[p_fdr < alpha & sqrt(tau2) >= minGroupSD]
  gr <- gr[order(source == "eigen", -z)]
  keep <- list()
  for(k in seq_len(nrow(gr))){
    if(length(keep) >= slots) break
    its <- sets[[gr$set[k]]]
    if(any(its %in% c(used, unlist(lapply(keep, `[[`, "items"))))) next
    keep[[length(keep) + 1L]] <- list(name = gr$set[k], items = its)
  }
  keep
}

## A step runner for improveModel(): with no directory, runs the step; with
## one, returns the stored result when it was made under the same fingerprint
## and specification, else runs the step and stores the result. Only a step
## that completed without error is written, and it is written to a temporary
## name first so that an interrupted write cannot leave a truncated file
## behind.
bigIRT_modify_checkpoint <- function(dir, fp){
  if(is.null(dir)) return(function(name, spec, run) run())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  function(name, spec, run){
    f <- file.path(dir, paste0(name, ".rds"))
    if(file.exists(f)){
      x <- tryCatch(readRDS(f), error = function(e) NULL)
      if(!is.null(x) && identical(x$fp, fp) && identical(x$spec, spec)) return(x$value)
    }
    v <- run()
    ## A step that failed (it carries an error) is not stored, so a rerun
    ## tries it again rather than reusing the failure.
    if(!is.null(v$error)) return(v)
    bigIRT_modify_store(dir, name, fp, spec, v)
    v
  }
}

## Writes a checkpoint step, to a temporary name first so that an interrupted
## write cannot leave a truncated file behind.
bigIRT_modify_store <- function(dir, name, fp, spec, value){
  f <- file.path(dir, paste0(name, ".rds"))
  tmp <- paste0(f, ".part")
  ## Uncompressed by default, for speed; option bigIRT.checkpoint_compress =
  ## TRUE writes gzip (a large fit shrinks about sevenfold, at a few minutes a
  ## write), for a disk shared with other work.
  saveRDS(list(fp = fp, spec = spec, value = value), tmp,
    compress = isTRUE(getOption("bigIRT.checkpoint_compress", FALSE)))
  if(file.exists(f)) unlink(f)
  file.rename(tmp, f)
  invisible(f)
}

#' @export
print.bigIRT_improved <- function(x, ...){
  s <- x$spec
  cat("Model improved by score tests and held-out likelihood\n")
  cat(sprintf("  ability terms: %s\n", if(length(s$abil)) paste(vapply(s$abil, function(a) a$covariate, ""), collapse = ", ") else "none"))
  if(length(s$impact)) cat(sprintf("  impact of DIF covariates: %s\n", paste(s$impact, collapse = ", ")))
  cat(sprintf("  DIF terms: %d items (%s)\n", sum(lengths(lapply(s$dif, `[[`, "items"))),
    if(length(s$dif)) paste(unique(vapply(s$dif, function(t) paste0(t$covariate, ":", t$parameter), "")), collapse = ", ") else "none"))
  cat(sprintf("  lower asymptotes freed: %s; upper: %s\n",
    if("lower" %in% s$pooled) "all (pooled)" else length(s$lower), if("upper" %in% s$pooled) "all (pooled)" else length(s$upper)))
  if(length(s$partition)) cat(sprintf("  correlated dimensions: the levels of %s\n", s$partition))
  if(length(s$bydim)) cat(sprintf("  covariates with an effect of their own on each dimension: %s\n", paste(s$bydim, collapse = ", ")))
  cat(sprintf("  group dimensions: %s\n", if(length(s$groups)) paste(vapply(s$groups, `[[`, "", "name"), collapse = ", ") else "none"))
  if(nrow(x$path)) { cat("Batches tried:\n"); print(x$path) }
  invisible(x)
}
