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
  ho <- which(id %in% ho_persons)
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
bigIRT_modify_gain <- function(a, b){
  p <- intersect(names(a$by_person), names(b$by_person))
  dlt <- b$by_person[p] - a$by_person[p]
  se <- stats::sd(dlt) * sqrt(length(dlt))
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
        tst, seq_len(tst - 1L))
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
      c(m - 1L, m), seq_len(m - 2L)), error = function(e) NULL)
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
  d <- data.table::copy(data.table::as.data.table(dat))
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
  for(t in spec$dif){
    B <- bigIRT_modify_basis(d[[t$covariate]], t$kind, NULL)
    mask <- it %in% t$items
    nm <- paste0(".dif_", t$parameter, "_", make.names(t$covariate), "_", seq_len(ncol(B)))
    for(k in seq_len(ncol(B))) data.table::set(d, j = nm[k], value = B[, k] * mask)
    if(t$parameter == "B") Bp <- c(Bp, nm) else Ap <- c(Ap, nm)
  }
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
  args <- c(list(dat = d, score = base$score, id = base$id, item = base$item, scale = scale, pl = pl,
    personPreds = pp, BitemPreds = Bp, AitemPreds = Ap, itemSpecificBetas = length(c(Bp, Ap)) > 0,
    itemDat = itemDat, loadings = loadings, normalise = FALSE), fa)
  args
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
#' @return A list of data.tables: `ability`, `dif` (with effect sizes),
#'   `asymptotes`, `groups`; each with p-values adjusted by Benjamini-Hochberg
#'   (`p_fdr`) within its kind; and `dimensions`, the exploratory eigenpairs
#'   with their null draws.
#' @seealso [improveModel()], [scoreTest()]
#' @export
modificationTests <- function(fit, dat, personCovariates = character(), responseCovariates = character(),
  itemSets = character(), dif = c("B", "A"), difCovariates = NULL, asymptotes = c("lower", "upper"), purify = TRUE,
  exploratory = TRUE, nsimDim = 19L, minSetItems = 3L, shape = FALSE, cores = 1L){
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
  ## Covariates of ability not already in the model.
  ab <- data.table::rbindlist(lapply(personCovariates, function(v){
    cv <- pv(v); isf <- is.factor(cv[[v]])
    r <- tryCatch(suppressWarnings(scoreTest(sc, cv, parameters = "Ability", form = if(isf) "factor" else "spline")),
      error = function(e) NULL)
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
    r <- tryCatch(suppressWarnings(scoreTest(fit, cv, parameters = par, purify = purify, effects = TRUE, cores = cores,
      contributions = sc)),
      error = function(e) NULL)
    if(is.null(r) || !nrow(r$tests)) next
    e <- r$effects
    eff <- if(is.null(e)) NULL else e[, list(dP = dP[which.max(abs(dP))], dP_abs = max(dP_abs)), by = item]
    t <- r$tests[, list(item, covariate = v, parameter = par, form, n, LM, p, p_fdr)]
    if(!is.null(eff)) t <- merge(t, eff, by = "item", all.x = TRUE)
    t[, kind := if(is.factor(cv[[v]])) "factor" else "linear"]
    dl[[length(dl) + 1L]] <- t
  }
  di <- data.table::rbindlist(dl, fill = TRUE)
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
  list(ability = ab, dif = di, asymptotes = as, groups = gr, dimensions = dims, shape = sh)
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
#' Within a round the kinds are tried in turn -- ability, then the impact of
#' DIF covariates, then DIF, then asymptotes, then dimensions -- each against
#' the model as it stands after the batches kept before it, so a covariate's
#' effect on ability is in the model before DIF along it is judged (DIF
#' absorbs impact otherwise).
#'
#' * DIF along each covariate (and parameter) is a batch of its own, the
#'   strongest first, so each covariate's DIF has to earn its place.
#' * With `difImpact`, a DIF covariate whose effect on ability the model lacks
#'   in any form (judged by the span of the ability predictors) first gets it,
#'   as a batch of its own; a response covariate's within-person effect
#'   included.
#' * With `asymptotePooled`, the screened asymptotes have an alternative: the
#'   asymptote free on every item, pooled by its prior. The better of the two
#'   that passes the gate is kept.
#' * Group dimensions go in one per round, the strongest first. With
#'   `partitions`, an item-set column that covers every item (a partition,
#'   such as subdomains) is offered as an alternative: its levels as
#'   correlated ability dimensions.
#'
#' The next round screens the result, for the kinds whose verdict could have
#' changed: a kind is tried again only if it was kept, or a kind after it
#' was, in the round before (a kind rejected in a round was already judged
#' against every batch kept before it). Screens are made at each round's
#' starting model. The final model is refitted on all persons.
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
#' @return A `bigIRT_improved` object: `fit` (final model, all persons),
#'   `spec` (the terms added), `path` (each batch tried: round, kind, terms,
#'   held-out log-likelihood before and after, gain and its z, accepted,
#'   seconds to fit and evaluate), `screens` (the last round's tests),
#'   `base_fit` (the base model on the training persons), `holdout_persons`,
#'   `timing` (seconds: base fit, each round's screens, final fit).
#' @seealso [modificationTests()], [heldoutLogLik()]
#' @export
improveModel <- function(dat, score = "score", id = "id", item = "Item", scale = "Scale", pl = 2L,
  personPreds = character(), personCovariates = character(), responseCovariates = character(),
  itemSets = character(), dif = c("B", "A"), difCovariates = NULL, asymptotes = c("lower", "upper"),
  holdout = 0.2, alpha = 0.05, minEffect = 0.03, minAsymptote = 0.05, minGroupSD = 0.3,
  minZ = stats::qnorm(.95), minGain = 0, nsimDim = 19L, maxRounds = 3L, maxGroups = 2L, seed = 1L, cores = 1L,
  verbose = TRUE, fitArgs = list(laplaceRefine = "agq"), checkpoint = NULL,
  difImpact = TRUE, asymptotePooled = TRUE, partitions = TRUE){
  dat <- data.table::as.data.table(dat)
  if(!scale %in% names(dat)) { dat <- data.table::copy(dat); data.table::set(dat, j = scale, value = 1L) }
  persons <- unique(dat[[id]])
  ho <- local({ if(!is.null(seed)) { old <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if(!is.null(old)) assign(".Random.seed", old, globalenv())); set.seed(seed) }
    sample(persons, round(holdout * length(persons))) })
  train <- which(!dat[[id]] %in% ho)
  fa <- utils::modifyList(list(cores = cores, verbose = 0, laplaceCertify = FALSE), fitArgs)
  base <- list(score = score, id = id, item = item, scale = scale, pl = as.integer(pl), fitArgs = fa)
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
    suppressWarnings(do.call(fitIRT, a))
  }
  say <- function(...) if(isTRUE(verbose)) message(sprintf(...))
  ## What a cached step must match to be reused. Threads and printing do not
  ## change results, so they are left out.
  fp <- list(n = nrow(dat), cols = names(dat), holdout = sort(as.character(ho)),
    base = base[c("score", "id", "item", "scale", "pl")],
    fitArgs = base$fitArgs[setdiff(names(base$fitArgs), c("cores", "verbose"))],
    tests = list(personCovariates, responseCovariates, itemSets, dif, difCovariates, asymptotes, alpha, minEffect,
      minAsymptote, minGroupSD, minZ, minGain, nsimDim, maxGroups, difImpact, asymptotePooled, partitions))
  step <- bigIRT_modify_checkpoint(checkpoint, fp)
  secs <- function(t0) proc.time()[[3]] - t0
  timing <- list()
  spec <- list(personPreds = personPreds, abil = list(), impact = character(), dif = list(), lower = character(),
    upper = character(), pooled = character(), groups = list(), partition = character())
  base_spec <- spec
  b <- step("base", spec, function(){
    t0 <- proc.time()[[3]]; f <- fit_spec(spec, train)
    list(fit = f, h = heldoutLogLik(f, cores = cores), seconds = secs(t0))
  })
  cur <- b$fit; cur_ho <- b$h; timing$base <- b$seconds
  ## With a checkpoint the base fit is read back at the end rather than held
  ## through the rounds: on large data each fit is many GB, and a refit holds
  ## the current fit and the new one besides.
  base_fit <- if(is.null(checkpoint)) cur else NULL
  rm(b)
  say("base model: held-out log-likelihood %.1f", cur_ho$loglik)
  path <- list(); screens <- NULL
  ## The kinds open to change. A kind rejected in a round already met every
  ## batch kept before it, so it is tried again only if it was kept or a kind
  ## after it was (otherwise nothing that could change its verdict changed,
  ## and a retry would only refit the same candidates).
  kinds <- c("ability", "impact", "dif", "asymptotes", "groups")
  live <- kinds
  ## The accepted step whose stored fit is the current model's (none while
  ## the base is current: the base fit is returned and kept).
  holder <- NULL
  for(round in seq_len(maxRounds)){
    covs_in <- vapply(spec$abil, function(a) a$covariate, "")
    pc_left <- setdiff(personCovariates, c(covs_in, personPreds))
    more_groups <- !length(spec$partition) && length(spec$groups) < maxGroups && "groups" %in% live
    sr <- step(sprintf("round%d_screens", round), spec, function(){
      t0 <- proc.time()[[3]]
      dat_cur <- bigIRT_modify_build(dat, spec, base)$dat
      s <- modificationTests(cur, dat_cur, personCovariates = personCovariates,
        responseCovariates = responseCovariates, itemSets = if(more_groups) itemSets else character(),
        dif = if("dif" %in% live) dif else character(), difCovariates = difCovariates,
        asymptotes = if("asymptotes" %in% live) asymptotes else character(),
        exploratory = more_groups && nsimDim > 0L, nsimDim = nsimDim,
        cores = cores)
      list(screens = s, seconds = secs(t0))
    })
    screens <- sr$screens; timing[[sprintf("round%d_screens", round)]] <- sr$seconds
    ## Candidates, kind by kind. Each stage below is one batch, or a choice
    ## between alternative batches of one kind, judged in turn against the
    ## model as it stands after the batches kept before it.
    stages <- list()
    stage <- function(kind, ...) list(kind = kind, alts = list(...))
    alt <- function(label, apply, terms) list(label = label, apply = apply, terms = terms)
    ab <- screens$ability
    if("ability" %in% live && !is.null(ab) && nrow(ab)) {
      ab <- ab[covariate %in% pc_left & p_fdr < alpha]
      if(nrow(ab)) local({
        ## (local: each batch's function must keep its own terms, not a loop
        ## variable that later candidates overwrite.)
        xa <- lapply(seq_len(nrow(ab)), function(k) list(covariate = ab$covariate[k], kind = ab$kind[k]))
        stages[[length(stages) + 1L]] <<- stage("ability", alt("ability", function(s){ s$abil <- c(s$abil, xa); s }, length(xa)))
      })
    }
    di <- screens$dif
    dif_groups <- list()
    if("dif" %in% live && !is.null(di) && nrow(di)){
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
    ## stage is reached: the ability batch before it may have added the
    ## covariate already (judged at the round's start, the impact batch then
    ## refitted the same model).
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
    for(x in dif_groups) local({ xx <- x[c("covariate", "parameter", "kind", "items")]
      stages[[length(stages) + 1L]] <<- stage("dif", alt(paste("dif", make.names(xx$covariate), xx$parameter, sep = "-"),
        function(s){ s$dif <- c(s$dif, list(xx)); s }, length(xx$items)))
    })
    as <- screens$asymptotes
    as_alts <- list()
    if("asymptotes" %in% live && !is.null(as) && nrow(as) && length(setdiff(asymptotes, spec$pooled))){
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
    gr <- screens$groups
    gr_alts <- list()
    if(more_groups && !is.null(gr) && nrow(gr)){
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
    if(!length(stages)){ say("round %d: nothing to add", round); break }
    ho_start <- cur_ho$loglik; kept <- character(); passed <- list()
    for(sg in stages){
      ## Each alternative judged against the current model; the best that
      ## passes the gate is kept.
      best <- NULL
      for(a in sg$alts){
        if(is.function(a$terms)) a$terms <- a$terms(spec)
        s <- a$apply(spec)
        ## Nothing left to add once the batches before it are in: no refit.
        if(identical(s, spec)){ say("round %d, %s: nothing left to add", round, a$label); next }
        tr <- step(sprintf("round%d_%s", round, a$label), s, function(){
          t0 <- proc.time()[[3]]
          ## A batch whose fit or held-out evaluation fails is rejected, and
          ## the reason kept: a failure is not evidence against the change.
          err <- NULL
          f <- tryCatch(fit_spec(s, train, start = cur), error = function(e){ err <<- conditionMessage(e); NULL })
          h <- if(is.null(f)) NULL else tryCatch(heldoutLogLik(f, cores = cores),
            error = function(e){ err <<- paste("heldoutLogLik:", conditionMessage(e)); NULL })
          gm <- if(is.null(h)) list(gain = NA_real_, z = NA_real_) else bigIRT_modify_gain(cur_ho, h)
          ok <- isTRUE(gm$gain >= minGain && gm$z >= minZ)
          ## Only a fit that passes is needed again.
          list(fit = if(ok) f else NULL, h = h, gain = gm$gain, z = gm$z, ok = ok, seconds = secs(t0), error = err)
        })
        path[[length(path) + 1L]] <- data.table::data.table(round = round, kind = sg$kind, batch = a$label,
          terms = a$terms, heldout_before = cur_ho$loglik, heldout_after = if(is.null(tr$h)) NA_real_ else tr$h$loglik,
          gain = tr$gain, z = tr$z, accepted = FALSE, seconds = tr$seconds,
          error = if(is.null(tr$error)) NA_character_ else tr$error)
        if(!is.null(tr$error)) say("round %d, %s: %d terms, failed (%s)", round, a$label, a$terms, tr$error)
        else say("round %d, %s: %d terms, held-out gain %.1f (z %.1f)%s", round, a$label, a$terms,
          tr$gain, tr$z, if(tr$ok) " (passes)" else "")
        if(tr$ok){
          ## Only the stage's best fit is held in memory; the others keep
          ## their verdicts.
          is_best <- is.null(best) || tr$gain > best$tr$gain
          slim_tr <- tr; slim_tr$fit <- NULL
          passed[[length(passed) + 1L]] <- list(name = sprintf("round%d_%s", round, a$label), s = s,
            tr = if(is_best) tr else slim_tr)
          if(is_best){
            if(!is.null(best)) passed[[best$slot]]$tr$fit <- NULL
            best <- list(tr = tr, s = s, at = length(path), slot = length(passed))
          }
        }
        tr <- NULL; gc(verbose = FALSE)
      }
      if(!is.null(best)){
        spec <- best$s; cur <- best$tr$fit; cur_ho <- best$tr$h; kept <- c(kept, sg$kind)
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
      passed <- list()
    }
    if(!length(kept)) break
    say("round %d: held-out log-likelihood %.1f -> %.1f", round, ho_start, cur_ho$loglik)
    last_kept <- max(match(kept, kinds))
    live <- kinds[kinds %in% kept | seq_along(kinds) < last_kept]
  }
  fin <- step("final", spec, function(){
    t0 <- proc.time()[[3]]; list(fit = fit_spec(spec, NULL, start = cur), seconds = secs(t0))
  })
  timing$final <- fin$seconds
  rm(cur)
  if(is.null(base_fit)) base_fit <- step("base", base_spec, function() stop("the checkpoint lost the base step"))$fit
  structure(list(fit = fin$fit, spec = spec, path = data.table::rbindlist(path, fill = TRUE), screens = screens,
    base_fit = base_fit, holdout_persons = ho, timing = unlist(timing)), class = "bigIRT_improved")
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
  saveRDS(list(fp = fp, spec = spec, value = value), tmp, compress = FALSE)
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
  cat(sprintf("  group dimensions: %s\n", if(length(s$groups)) paste(vapply(s$groups, `[[`, "", "name"), collapse = ", ") else "none"))
  if(nrow(x$path)) { cat("Batches tried:\n"); print(x$path) }
  invisible(x)
}
