## Measurement reporting: information curves, reliability, item fit, empirical
## response curves, and a plot method.
##
## Everything here is computed row-wise or item-wise, in one pass over the
## responses, so it inherits the package's scaling and works on data that never
## has to become a wide person-by-item matrix. That is the dividing line: the
## classical statistics that genuinely need the wide matrix -- S-X squared, M2,
## Yen's Q3 -- are better taken from mirt on data small enough to hold one, and
## are deliberately not reimplemented here.

## Item and test information across the ability range.
##
## For the four-parameter model the response probability is
##   p = c + (d - c) g,  g = logistic(a theta - b),
## so dp/dtheta = (d - c) a g (1 - g) and the information in one response is
##   I(theta) = (dp/dtheta)^2 / (p (1 - p)).
## That reduces to a^2 g (1 - g) in the two-parameter case, as it should.
##
## The conditional standard error of measurement is 1/sqrt(test information),
## which is what a reader wants when asking where on the scale a test actually
## measures anything.
#' Item and test information across the ability range
#'
#' Fisher information in each response, summed over items, with the conditional
#' standard error of measurement that follows from it.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param theta Numeric vector of ability points.
#' @param scale Optional scale name to restrict to.
#' @param items Optional item index to restrict to.
#'
#' @return An object of class `bigIRT_information`, carrying `theta`,
#'   `item_information`, `test_information` and `sem`.
#'
#' @export
testInformation <- function(fit, theta = seq(-4, 4, 0.05), scale = NULL, items = NULL){
  ip <- as.data.frame(fit$itemPars)
  if(!nrow(ip)) stop("This fit carries no item parameters.")
  if(!is.null(scale) && "Scale" %in% names(ip)) ip <- ip[ip$Scale %in% scale, , drop = FALSE]
  if(!is.null(items)) ip <- ip[items, , drop = FALSE]
  a <- if("A" %in% names(ip)) as.numeric(ip$A) else rep(1, nrow(ip))
  b <- if("B" %in% names(ip)) as.numeric(ip$B) else rep(0, nrow(ip))
  cc <- if("C" %in% names(ip)) as.numeric(ip$C) else rep(0, nrow(ip))
  dd <- if("D" %in% names(ip)) as.numeric(ip$D) else rep(1, nrow(ip))
  cc[!is.finite(cc)] <- 0; dd[!is.finite(dd)] <- 1
  theta <- as.numeric(theta)
  ## rows are ability points, columns are items
  eta <- outer(theta, a) - matrix(b, nrow = length(theta), ncol = length(b), byrow = TRUE)
  g <- inv_logit(eta)
  u <- matrix(dd - cc, nrow = length(theta), ncol = length(b), byrow = TRUE)
  cmat <- matrix(cc, nrow = length(theta), ncol = length(b), byrow = TRUE)
  amat <- matrix(a, nrow = length(theta), ncol = length(a), byrow = TRUE)
  p <- pmin(pmax(cmat + u * g, 1e-10), 1 - 1e-10)
  dp <- u * amat * g * (1 - g)
  info <- dp^2 / (p * (1 - p))
  tif <- rowSums(info)
  out <- list(
    theta = theta,
    item_information = info,
    test_information = tif,
    sem = 1 / sqrt(pmax(tif, .Machine$double.eps)),
    item_ids = if("Item" %in% names(ip)) ip$Item else seq_len(nrow(ip)),
    n_items = nrow(ip))
  class(out) <- "bigIRT_information"
  out
}

#' @export
print.bigIRT_information <- function(x, ...){
  pk <- x$theta[which.max(x$test_information)]
  cat(sprintf("bigIRT test information over %d items\n", x$n_items))
  cat(sprintf("Peak information %.2f at theta %.2f (conditional SEM %.3f)\n",
    max(x$test_information), pk, min(x$sem)))
  ## Where the test measures to a given precision is the practical question.
  for(s in c(0.3, 0.5)){
    ok <- x$sem <= s
    if(any(ok)) cat(sprintf("SEM <= %.1f between theta %.2f and %.2f\n",
      s, min(x$theta[ok]), max(x$theta[ok])))
    else cat(sprintf("SEM never reaches %.1f\n", s))
  }
  invisible(x)
}

## Reliability of the fitted abilities.
##
## Empirical reliability compares the spread of the estimates against that
## spread plus the average error variance. Marginal reliability compares the
## average error variance against the prior variance instead. They answer
## slightly different questions and disagree when the estimates are shrunk, so
## both are reported.
#' Reliability of the fitted abilities
#'
#' Empirical reliability compares the spread of the estimates against that
#' spread plus the average error variance; marginal reliability compares the
#' average error variance against the prior variance. They disagree when the
#' estimates are shrunk, so both are reported.
#'
#' @param fit A fitted object from [fitIRT()], fitted with
#'   `laplaceKeepCovariance = TRUE`.
#'
#' @return A data frame with one row per latent dimension.
#'
#' @export
reliability <- function(fit){
  Sig <- fit$personPosterior$covariance
  th <- as.matrix(fit$pars$Ability)
  if(is.null(Sig))
    stop("Reliability needs the person posterior covariances. Refit with `laplaceKeepCovariance = TRUE`.")
  K <- dim(Sig)[1]
  prior_sd <- as.numeric(fit$abilityPrior$sd)
  out <- data.frame(scale = seq_len(K), mean_error_variance = NA_real_,
    observed_variance = NA_real_, empirical = NA_real_, marginal = NA_real_,
    mean_sem = NA_real_)
  for(k in seq_len(K)){
    ev <- mean(Sig[k, k, ], na.rm = TRUE)
    ov <- stats::var(th[, k], na.rm = TRUE)
    pv <- if(length(prior_sd) >= k) prior_sd[k]^2 else ov + ev
    out$mean_error_variance[k] <- ev
    out$observed_variance[k] <- ov
    out$empirical[k] <- ov / (ov + ev)
    out$marginal[k] <- 1 - ev / pv
    out$mean_sem[k] <- sqrt(ev)
  }
  if(!is.null(colnames(th))) out$scale <- colnames(th)
  out
}

## Item fit by standardised residuals.
##
## Outfit is the plain mean of squared standardised residuals and so is pulled
## about by unexpected responses far from an item's difficulty. Infit weights
## by information and is therefore the more stable of the two. Both are one
## pass over the responses.
##
## These are Rasch-family statistics: their reference distribution is only
## well behaved for one- and two-parameter models, and they should not be read
## as calibrated tests under a three- or four-parameter fit.
#' Item fit by standardised residuals
#'
#' Outfit is the mean squared standardised residual, and so is pulled about by
#' unexpected responses far from an item's difficulty; infit weights by
#' information and is the more stable of the two. Both are one pass over the
#' responses.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param training_only Logical. Score training rows only.
#'
#' @return A data frame of item, response count, infit, outfit and a
#'   standardised outfit.
#'
#' @section Note:
#' These are Rasch-family statistics. Their reference distributions are only
#' well behaved for one- and two-parameter models, and should not be read as
#' calibrated tests under a three- or four-parameter fit.
#'
#' @seealso [empiricalICC()]
#' @export
itemFit <- function(fit, training_only = TRUE){
  sdat <- fit$dat
  ord <- fit$pars$originalRow
  p_in <- fit$pars$pcorrect
  if(is.null(p_in) || is.null(ord)) stop("This fit carries no per-response predictions.")
  rows <- if(isTRUE(training_only)) which(as.integer(sdat$trainingLogical) == 1L)
          else seq_along(sdat$score)
  p <- p_in[ord[rows]]
  y <- as.numeric(sdat$score)[rows]
  item <- as.integer(sdat$item)[rows]
  ok <- is.finite(p) & is.finite(y) & is.finite(item)
  p <- pmin(pmax(p[ok], 1e-10), 1 - 1e-10); y <- y[ok]; item <- item[ok]
  w <- p * (1 - p)                      # information in a Bernoulli response
  resid2 <- (y - p)^2
  agg <- function(x){
    s <- rowsum(x, item)
    out <- rep(NA_real_, as.integer(sdat$Nitems))
    out[as.integer(rownames(s))] <- as.numeric(s)
    out
  }
  n <- agg(rep(1, length(y)))
  sum_z2 <- agg(resid2 / w)
  sum_r2 <- agg(resid2)
  sum_w <- agg(w)
  outfit <- sum_z2 / n
  infit <- sum_r2 / sum_w
  ## A standardised form so the two are comparable across items with different
  ## response counts; the usual Wilson-Hilferty cube-root transform.
  t_out <- (outfit^(1/3) - 1) * (3 / sqrt(2 / n)) + (sqrt(2 / n) / 3)
  ids <- if("Item" %in% names(as.data.frame(fit$itemPars)))
    as.data.frame(fit$itemPars)$Item else seq_len(as.integer(sdat$Nitems))
  data.frame(item = ids, n = n, infit = infit, outfit = outfit, outfit_t = t_out,
    stringsAsFactors = FALSE)
}

## Observed against model-implied response curves.
##
## People are binned by fitted ability and the observed proportion correct in
## each bin is compared with the model's prediction for that bin. This says how
## an item misfits, not merely that it does, which is why it is worth more than
## any single fit statistic.
##
## One caution, and it is a property of the method rather than of this
## implementation. The bins are formed on *estimated* ability, which is both
## shrunk toward the prior and measured with error, so people are sorted partly
## by noise. That flattens the observed curve relative to the model: extreme
## bins hold people whose true abilities are less extreme than their estimates.
## Measured on correctly specified simulated data, the model falls inside the
## nominal 95 per cent interval for about 80 per cent of bins rather than 95.
## Read a mild, symmetric shortfall as this artefact; read a one-sided or
## sharply localised departure as real misfit.
#' Observed against model-implied response curves
#'
#' People are binned by fitted ability and the observed proportion correct in
#' each bin is compared with the model's prediction, which says how an item
#' misfits rather than only that it does.
#'
#' @param fit A fitted object from [fitIRT()].
#' @param items Optional item index to restrict to.
#' @param bins Integer. Number of ability bins.
#' @param scale_index Integer. Latent dimension to bin on.
#' @param training_only Logical. Use training rows only.
#'
#' @return A data frame of item, bin, count, mean ability, observed and
#'   expected proportion, and a Wilson interval.
#'
#' @section Note:
#' Bins are formed on estimated ability, which is shrunk and measured with
#' error, so people sort partly by noise and the observed curve flattens
#' against the model. On correctly specified data the model falls inside the
#' nominal 95 per cent interval for about 80 per cent of bins. Read a mild
#' symmetric shortfall as that artefact, and a one-sided or localised departure
#' as misfit.
#'
#' @export
empiricalICC <- function(fit, items = NULL, bins = 10L, scale_index = 1L,
                         training_only = TRUE){
  sdat <- fit$dat
  ord <- fit$pars$originalRow
  p_in <- fit$pars$pcorrect
  if(is.null(p_in) || is.null(ord)) stop("This fit carries no per-response predictions.")
  rows <- if(isTRUE(training_only)) which(as.integer(sdat$trainingLogical) == 1L)
          else seq_along(sdat$score)
  th <- as.matrix(fit$pars$Ability)[, scale_index]
  ids <- as.integer(sdat$id)[rows]
  p <- p_in[ord[rows]]
  y <- as.numeric(sdat$score)[rows]
  item <- as.integer(sdat$item)[rows]
  keep <- is.finite(p) & is.finite(y) & is.finite(item) & is.finite(th[ids])
  p <- p[keep]; y <- y[keep]; item <- item[keep]; tt <- th[ids[keep]]
  if(!is.null(items)) {
    sel <- item %in% items
    p <- p[sel]; y <- y[sel]; item <- item[sel]; tt <- tt[sel]
  }
  if(!length(y)) return(NULL)
  br <- stats::quantile(tt, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE)
  br[1] <- br[1] - 1e-8; br[length(br)] <- br[length(br)] + 1e-8
  bin <- cut(tt, breaks = unique(br), labels = FALSE, include.lowest = TRUE)
  key <- paste(item, bin, sep = "|")
  agg <- function(x) tapply(x, key, mean)
  cnt <- tapply(rep(1, length(y)), key, sum)
  obs <- agg(y); exp_ <- agg(p); mid <- agg(tt)
  parts <- do.call(rbind, strsplit(names(obs), "|", fixed = TRUE))
  n <- as.numeric(cnt)
  ph <- as.numeric(obs)
  out <- data.frame(
    item = as.integer(parts[, 1]), bin = as.integer(parts[, 2]),
    n = n, theta = as.numeric(mid), observed = ph, expected = as.numeric(exp_),
    stringsAsFactors = FALSE)
  ## Wilson interval: behaves at proportions near zero and one, where the
  ## normal approximation puts the bound outside the unit interval.
  z <- 1.96
  den <- 1 + z^2 / n
  centre <- (ph + z^2 / (2 * n)) / den
  half <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
  out$lower <- pmax(centre - half, 0)
  out$upper <- pmin(centre + half, 1)
  out[order(out$item, out$bin), ]
}

## Plot a fitted model.
##
## `type` picks the view: response curves against the observed proportions,
## test information with the conditional standard error, a person-item map, or
## the optimiser trace.
#' @export
plot.bigIRT_fit <- function(x, type = c("icc", "information", "wright", "convergence"),
                            items = NULL, theta = seq(-4, 4, 0.05), bins = 10L,
                            scale_index = 1L, ...){
  type <- match.arg(type)
  if(identical(type, "convergence")) return(plotLaplaceDiagnostics(x))

  if(identical(type, "information")){
    ti <- testInformation(x, theta = theta)
    op <- graphics::par(mar = c(4.2, 4.2, 2, 4.2)); on.exit(graphics::par(op))
    plot(ti$theta, ti$test_information, type = "l", lwd = 2,
      xlab = expression(theta), ylab = "Test information",
      main = "Test information and conditional SEM", ...)
    graphics::par(new = TRUE)
    plot(ti$theta, ti$sem, type = "l", lty = 2, col = "grey40",
      axes = FALSE, xlab = "", ylab = "")
    graphics::axis(4); graphics::mtext("SEM", side = 4, line = 2.6)
    graphics::legend("topright", c("Information", "SEM"), lty = c(1, 2),
      lwd = c(2, 1), col = c("black", "grey40"), bty = "n")
    return(invisible(ti))
  }

  if(identical(type, "wright")){
    ip <- as.data.frame(x$itemPars)
    th <- as.matrix(x$pars$Ability)[, scale_index]
    b <- as.numeric(ip$B)
    op <- graphics::par(mfrow = c(2, 1), mar = c(2, 4.2, 2, 1)); on.exit(graphics::par(op))
    rg <- range(c(th, b), na.rm = TRUE)
    graphics::hist(th, breaks = 30, xlim = rg, col = "grey80", border = "white",
      main = "Persons", xlab = "")
    graphics::par(mar = c(4.2, 4.2, 1, 1))
    graphics::hist(b, breaks = 30, xlim = rg, col = "grey50", border = "white",
      main = "Items", xlab = expression(theta))
    return(invisible(list(ability = th, difficulty = b)))
  }

  ## icc: observed proportions against the fitted curve
  e <- empiricalICC(x, items = items, bins = bins, scale_index = scale_index)
  if(is.null(e)) stop("No responses available for an empirical curve.")
  ip <- as.data.frame(x$itemPars)
  sel <- if(is.null(items)) unique(e$item)[seq_len(min(6L, length(unique(e$item))))] else items
  nr <- ceiling(sqrt(length(sel))); nc <- ceiling(length(sel) / nr)
  op <- graphics::par(mfrow = c(nr, nc), mar = c(4, 4, 2, 1)); on.exit(graphics::par(op))
  for(j in sel){
    ej <- e[e$item == j, ]
    a <- if("A" %in% names(ip)) ip$A[j] else 1
    b <- if("B" %in% names(ip)) ip$B[j] else 0
    cc <- if("C" %in% names(ip)) ip$C[j] else 0
    dd <- if("D" %in% names(ip)) ip$D[j] else 1
    if(!is.finite(cc)) cc <- 0; if(!is.finite(dd)) dd <- 1
    curve_p <- cc + (dd - cc) * inv_logit(a * theta - b)
    plot(theta, curve_p, type = "l", lwd = 2, ylim = c(0, 1),
      xlab = expression(theta), ylab = "P(correct)",
      main = sprintf("Item %s", if("Item" %in% names(ip)) ip$Item[j] else j))
    graphics::arrows(ej$theta, ej$lower, ej$theta, ej$upper, length = 0.02,
      angle = 90, code = 3, col = "grey50")
    graphics::points(ej$theta, ej$observed, pch = 19, cex = 0.8)
  }
  invisible(e)
}
