## Coercion to a mirt object.
##
## One conversion buys the whole classical diagnostic suite -- itemfit() with
## S-X squared and G squared, personfit(), M2(), residuals() for Yen's Q3 and
## local dependence, itemplot(), empirical_plot(), DIF() -- none of which is
## worth reimplementing here, because all of them need the wide person-by-item
## matrix and that, not the arithmetic, is the binding constraint.
##
## Which is also this bridge's limit. bigIRT exists to fit data that never
## becomes a wide matrix; anything that does not fit in memory as one cannot
## cross, and `max_cells` says so rather than letting R try.
##
## The parameterisations differ and the translation is the part that can be
## silently wrong:
##   bigIRT   p = C + (D - C) logistic(A theta - B)
##   mirt     p = g + (u - g) logistic(a1 theta + d)
## so a1 = A, d = -B, g = C, u = D. That is asserted against mirt's own trace
## lines in `checkMirtBridge()` rather than trusted, because a coercion that is
## quietly wrong produces diagnostics that look entirely reasonable.

## Wide response matrix from a fit, persons by items, NA where unanswered.
bigIRT_wide_responses <- function(fit, max_cells = 5e7){
  sdat <- fit$dat
  ns <- as.integer(sdat$Nsubs); ni <- as.integer(sdat$Nitems)
  if(as.numeric(ns) * as.numeric(ni) > max_cells)
    stop(sprintf(paste0("A wide matrix for this fit would be %d x %d = %.1f billion cells. ",
      "That is what bigIRT exists to avoid; use the native diagnostics, or raise ",
      "`max_cells` if you really have the memory."), ns, ni, ns * as.numeric(ni) / 1e9))
  W <- matrix(NA_real_, nrow = ns, ncol = ni)
  W[cbind(as.integer(sdat$id), as.integer(sdat$item))] <- as.numeric(sdat$score)
  ip <- as.data.frame(fit$itemPars)
  colnames(W) <- if("Item" %in% names(ip)) as.character(ip$Item) else paste0("item", seq_len(ni))
  W
}

## Which mirt item type covers the parameters this fit actually estimated.
bigIRT_mirt_itemtype <- function(ip){
  cc <- if("C" %in% names(ip)) as.numeric(ip$C) else rep(0, nrow(ip))
  dd <- if("D" %in% names(ip)) as.numeric(ip$D) else rep(1, nrow(ip))
  cc[!is.finite(cc)] <- 0; dd[!is.finite(dd)] <- 1
  has_c <- any(abs(cc) > 1e-8)
  has_d <- any(abs(dd - 1) > 1e-8)
  if(has_c && has_d) "4PL" else if(has_c) "3PL" else "2PL"
}

## Confirmatory model string from the fitted loading pattern.
bigIRT_mirt_model <- function(A, item_names){
  K <- ncol(A)
  if(K == 1L) return(1L)
  lines <- character(0)
  for(k in seq_len(K)){
    idx <- which(abs(A[, k]) > 1e-10)
    if(!length(idx)) next
    lines <- c(lines, sprintf("F%d = %s", k, paste(idx, collapse = ",")))
  }
  mirt::mirt.model(paste(lines, collapse = "\n"))
}

## Build a mirt object holding this fit's parameters, without estimating.
##
## `TOL = NaN` is mirt's own way of saying evaluate here and stop; every item
## parameter is marked fixed as well, so nothing can drift.
#' Coerce a fit to a fixed-parameter mirt object
#'
#' Opens mirt's diagnostic suite -- `itemfit()`, `personfit()`, `M2()`,
#' `residuals(type = "Q3")`, `itemplot()`, `DIF()` -- none of which is worth
#' reimplementing, because all of them need the wide person-by-item matrix and
#' that, not the arithmetic, is the binding constraint. Data too large to hold
#' as one cannot cross, and is refused rather than attempted.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param max_cells Numeric. Refuse a wide matrix larger than this.
#' @param verbose Logical. Passed to `mirt::mirt()`.
#'
#' @return A `SingleGroupClass` object with every parameter fixed at this
#'   fit's values.
#'
#' @seealso [checkMirtBridge()], which verifies the translation.
#' @export
as.mirt <- function(fit, max_cells = 5e7, verbose = FALSE){
  if(!requireNamespace("mirt", quietly = TRUE))
    stop("as.mirt needs the 'mirt' package. install.packages('mirt')")
  ip <- as.data.frame(fit$itemPars)
  if(!nrow(ip)) stop("This fit carries no item parameters.")
  W <- bigIRT_wide_responses(fit, max_cells = max_cells)
  A <- as.matrix(fit$pars$A)
  if(nrow(A) != ncol(W)) stop("Item parameter and response matrix dimensions disagree.")
  K <- ncol(A)
  itemtype <- bigIRT_mirt_itemtype(ip)
  model <- bigIRT_mirt_model(A, colnames(W))

  b <- as.numeric(ip$B)
  cc <- if("C" %in% names(ip)) as.numeric(ip$C) else rep(0, nrow(ip))
  dd <- if("D" %in% names(ip)) as.numeric(ip$D) else rep(1, nrow(ip))
  cc[!is.finite(cc)] <- 0; dd[!is.finite(dd)] <- 1

  pars <- mirt::mirt(W, model, itemtype = itemtype, pars = "values")
  items <- colnames(W)
  for(j in seq_along(items)){
    rows <- pars$item == items[j]
    for(k in seq_len(K)){
      nm <- paste0("a", k)
      sel <- rows & pars$name == nm
      if(any(sel)) pars$value[sel] <- A[j, k]
    }
    sel <- rows & pars$name == "d";  if(any(sel)) pars$value[sel] <- -b[j]
    sel <- rows & pars$name == "g";  if(any(sel)) pars$value[sel] <- cc[j]
    sel <- rows & pars$name == "u";  if(any(sel)) pars$value[sel] <- dd[j]
  }
  ## Guessing and upper asymptote are bounded in (0,1) and mirt's template
  ## carries rows for them even under 2PL and 3PL, where bigIRT leaves them at
  ## their limits. Nudge off the exact boundary so any internal logit stays
  ## finite, but by far less than the tolerance the coercion is checked at --
  ## an earlier 1e-4 nudge moved every 2PL and 3PL trace line by exactly that
  ## much and looked like a parameterisation error.
  gu <- pars$name %in% c("g", "u")
  pars$value[gu] <- pmin(pmax(pars$value[gu], 1e-9), 1 - 1e-9)
  pars$est <- FALSE

  m <- mirt::mirt(W, model, itemtype = itemtype, pars = pars, TOL = NaN,
    verbose = isTRUE(verbose))
  attr(m, "bigIRT") <- list(itemtype = itemtype, dimensions = K,
    persons = nrow(W), items = ncol(W))
  m
}

## Verify the coercion reproduces this fit's response curves.
##
## Compares mirt's own trace lines against bigIRT's response function on a grid
## of abilities. This tests the parameter translation directly and does not
## involve marginalisation, so it separates a coercion error from a difference
## between Laplace and quadrature.
#' Verify the mirt coercion reproduces this fit's curves
#'
#' Compares mirt's own trace lines against bigIRT's response function on a grid
#' of abilities. This tests the parameter translation directly, without
#' marginalisation, so a coercion error is separated from the difference
#' between Laplace and quadrature. A coercion that is quietly wrong produces
#' diagnostics that look entirely reasonable, which is why this exists.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param mirt_fit Optional object from [as.mirt()]; built if absent.
#' @param theta Numeric vector of ability points.
#' @param tolerance Numeric. Largest acceptable probability difference.
#' @param max_items Integer. Items to check.
#'
#' @return An object of class `bigIRT_mirtcheck`.
#'
#' @export
checkMirtBridge <- function(fit, mirt_fit = NULL, theta = seq(-3, 3, 0.5),
                            tolerance = 1e-6, max_items = 25L){
  if(!requireNamespace("mirt", quietly = TRUE))
    stop("checkMirtBridge needs the 'mirt' package.")
  if(is.null(mirt_fit)) mirt_fit <- as.mirt(fit)
  ip <- as.data.frame(fit$itemPars)
  A <- as.matrix(fit$pars$A)
  K <- ncol(A)
  b <- as.numeric(ip$B)
  cc <- if("C" %in% names(ip)) as.numeric(ip$C) else rep(0, nrow(ip))
  dd <- if("D" %in% names(ip)) as.numeric(ip$D) else rep(1, nrow(ip))
  cc[!is.finite(cc)] <- 0; dd[!is.finite(dd)] <- 1
  js <- seq_len(min(as.integer(max_items), nrow(A)))
  worst <- 0; worst_item <- NA_integer_
  for(j in js){
    ## Vary the dimension this item actually loads on; hold the others at zero.
    k <- which.max(abs(A[j, ]))
    Th <- matrix(0, nrow = length(theta), ncol = K)
    Th[, k] <- theta
    pm <- mirt::probtrace(mirt::extract.item(mirt_fit, j), Th)[, 2]
    eta <- as.numeric(Th %*% A[j, ]) - b[j]
    pb <- cc[j] + (dd[j] - cc[j]) * inv_logit(eta)
    dmax <- max(abs(pm - pb))
    if(dmax > worst){ worst <- dmax; worst_item <- j }
  }
  ok <- worst <= tolerance
  out <- list(agrees = ok, max_abs_difference = worst, worst_item = worst_item,
    items_checked = length(js), tolerance = tolerance,
    bigIRT_logLik = tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_),
    mirt_logLik = tryCatch(as.numeric(mirt::extract.mirt(mirt_fit, "logLik")),
      error = function(e) NA_real_))
  class(out) <- "bigIRT_mirtcheck"
  out
}

#' @export
print.bigIRT_mirtcheck <- function(x, ...){
  cat(sprintf("bigIRT to mirt coercion: %s\n",
    if(isTRUE(x$agrees)) "response curves agree" else "MISMATCH"))
  cat(sprintf("Largest probability difference %.2e over %d items (tolerance %.0e)\n",
    x$max_abs_difference, x$items_checked, x$tolerance))
  if(!isTRUE(x$agrees))
    cat(sprintf("Worst item: %d. Do not trust diagnostics taken through this object.\n",
      x$worst_item))
  if(is.finite(x$mirt_logLik) && is.finite(x$bigIRT_logLik)){
    cat(sprintf("Marginal log-likelihood: bigIRT %.2f (Laplace), mirt %.2f (quadrature)\n",
      x$bigIRT_logLik, x$mirt_logLik))
    ## The two integrate the person parameters differently, so they are not
    ## expected to match exactly; a large gap is worth knowing about.
    cat(sprintf("Difference %.2f (%.3f%% of the bigIRT value)\n",
      x$mirt_logLik - x$bigIRT_logLik,
      100 * abs(x$mirt_logLik - x$bigIRT_logLik) / max(abs(x$bigIRT_logLik), 1)))
  }
  invisible(x)
}
