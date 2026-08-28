## broom tidiers and a self-contained fit report.
##
## The tidiers are plain functions here and are registered against broom's
## generics on load if broom is installed, so broom stays a suggestion rather
## than a dependency and the functions still work without it.

## One row per item parameter, with the standard error and a Wald interval.
##
## `conf.int` costs a call to itemInformation(); with `method = "profile"` that
## is free, and with "hessian" it is two objective evaluations per parameter.
#' Tidy the item parameters
#'
#' One row per item parameter, with a standard error and Wald interval when
#' asked. Standard errors are carried onto the reported parameter scale by the
#' delta method, since discrimination is estimated as a softplus pre-image and
#' the asymptotes as logits. Registered against `broom::tidy()` when broom is
#' installed.
#'
#' @param x A fitted object from [fitIRT()].
#' @param conf.int Logical. Compute standard errors and intervals.
#' @param conf.level Numeric. Interval coverage.
#' @param method Character. Passed to [itemInformation()].
#' @param cores Integer. Cores for the information calculation.
#' @param ... Unused.
#'
#' @return A data frame of item, term and estimate and, when requested,
#'   standard error and interval.
#'
#' @export
tidyIRT <- function(x, conf.int = FALSE, conf.level = 0.95, method = "profile",
                    cores = 1L, ...){
  ip <- as.data.frame(x$itemPars)
  if(!nrow(ip)) stop("This fit carries no item parameters.")
  ## A two-parameter fit still carries C and D columns, pinned at 0 and 1.
  ## Emitting a row per item for a parameter that was never estimated is noise,
  ## and it arrives with no standard error, so drop those blocks.
  blocks <- intersect(c("A", "B", "C", "D"), names(ip))
  estimated <- function(b){
    v <- as.numeric(ip[[b]]); v <- v[is.finite(v)]
    if(!length(v)) return(FALSE)
    switch(b, C = any(abs(v) > 1e-8), D = any(abs(v - 1) > 1e-8), TRUE)
  }
  blocks <- blocks[vapply(blocks, estimated, logical(1))]
  if(!length(blocks)) stop("This fit has no estimated item parameters to report.")
  item <- if("Item" %in% names(ip)) as.character(ip$Item) else as.character(seq_len(nrow(ip)))
  out <- do.call(rbind, lapply(blocks, function(b) data.frame(
    item = item, term = b, estimate = as.numeric(ip[[b]]),
    stringsAsFactors = FALSE)))
  out$std.error <- NA_real_
  info <- if(isTRUE(conf.int) && !is.null(x$internals))
    tryCatch(itemInformation(x, cores = cores, method = method),
      error = function(e) NULL) else NULL
  if(!is.null(info)){
    ## itemInformation works on the free parameters, which are not the
    ## parameters reported here: discrimination is estimated as the softplus
    ## pre-image of A, and the asymptotes as logits of C and D. Putting those
    ## standard errors beside A, C or D unchanged would be a units error, and a
    ## quiet one, so carry them across by the delta method:
    ##   A = softplus(z),   dA/dz = logistic(z)      = A' where A' is sigmoid
    ##   C = logistic(z),   dC/dz = C (1 - C)
    ## Difficulty is estimated directly and needs no transform.
    for(b in intersect(blocks, info$active)){
      sel <- out$term == b
      se_b <- info$se[, b]
      est_b <- as.numeric(ip[[b]])
      jac <- switch(b,
        B = rep(1, length(est_b)),
        ## softplus'(z) = logistic(z) = 1 - exp(-A)
        A = 1 - exp(-pmax(est_b, 0)),
        C = , D = est_b * (1 - est_b),
        rep(1, length(est_b)))
      out$std.error[sel] <- se_b * abs(jac)
    }
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    out$conf.low <- out$estimate - z * out$std.error
    out$conf.high <- out$estimate + z * out$std.error
    ## A and the asymptotes are bounded, so a symmetric interval can escape;
    ## clip rather than print an impossible bound.
    bnd <- out$term %in% c("A")
    out$conf.low[bnd] <- pmax(out$conf.low[bnd], 0)
    bnd <- out$term %in% c("C", "D")
    out$conf.low[bnd] <- pmax(out$conf.low[bnd], 0)
    out$conf.high[bnd] <- pmin(out$conf.high[bnd], 1)
    attr(out, "se_scale") <- "natural scale, delta method from the free parameters"
  }
  out[order(match(out$term, blocks), out$item), ]
}

## One row of model-level summary.
#' One-row model summary
#'
#' Registered against `broom::glance()` when broom is installed.
#'
#' @param x A fitted object from [fitIRT()].
#' @param ... Unused.
#'
#' @return A one-row data frame of backend, log-likelihood, degrees of freedom,
#'   AIC, BIC, sample sizes, convergence, reliability and held-out log loss.
#'
#' @export
glanceIRT <- function(x, ...){
  st <- x$laplaceStatus
  ll <- tryCatch(logLik(x), error = function(e) NULL)
  rel <- tryCatch(reliability(x), error = function(e) NULL)
  ho <- tryCatch(heldoutMetrics(x), error = function(e) NULL)
  data.frame(
    backend = x$backend %||% NA_character_,
    logLik = if(!is.null(ll)) as.numeric(ll) else NA_real_,
    df = if(!is.null(ll)) as.numeric(attr(ll, "df")) else NA_real_,
    AIC = if(!is.null(ll)) tryCatch(AIC(x), error = function(e) NA_real_) else NA_real_,
    BIC = if(!is.null(ll)) tryCatch(BIC(x), error = function(e) NA_real_) else NA_real_,
    nobs_persons = as.integer(x$dat$Nsubs),
    nobs_responses = as.integer(x$dat$Nobs),
    nitems = as.integer(x$dat$Nitems),
    ndim = as.integer(x$dat$Nscales),
    converged = isTRUE(st$converged),
    termination = st$reason %||% NA_character_,
    reliability = if(!is.null(rel)) rel$empirical[1] else NA_real_,
    heldout_log_loss = if(!is.null(ho)) ho$log_loss else NA_real_,
    stringsAsFactors = FALSE)
}

## One row per response, with the fitted probability and residuals.
##
## Returned in the order the data were supplied, which is what a caller who
## wants to bind these back onto their own data frame needs.
#' Per-response fitted values and residuals
#'
#' Returned in the order the data were supplied, so the result can be bound
#' back onto the caller's own data frame. Registered against `broom::augment()`
#' when broom is installed.
#'
#' @param x A fitted object from [fitIRT()].
#' @param ... Unused.
#'
#' @return A data frame of row index, observed response, fitted probability,
#'   training flag, residual and standardised residual.
#'
#' @export
augmentIRT <- function(x, ...){
  sdat <- x$dat
  ord <- x$pars$originalRow
  if(is.null(ord)) stop("This fit carries no row mapping.")
  n <- length(x$pars$pcorrect)
  y <- rep(NA_real_, n); y[ord] <- as.numeric(sdat$score)
  train <- rep(NA, n); train[ord] <- as.integer(sdat$trainingLogical) == 1L
  p <- as.numeric(x$pars$pcorrect)
  out <- data.frame(.row = seq_len(n), .observed = y, .fitted = p,
    .training = train, stringsAsFactors = FALSE)
  out$.resid <- out$.observed - out$.fitted
  ## Standardised (Pearson) residual for a Bernoulli response.
  out$.std.resid <- out$.resid / sqrt(pmax(p * (1 - p), 1e-12))
  out
}

.onLoad <- function(libname, pkgname){
  ## Attach to broom's generics only if broom is there, so it stays optional.
  if(requireNamespace("broom", quietly = TRUE)){
    registerS3method("tidy", "bigIRT_fit", tidyIRT, envir = asNamespace("broom"))
    registerS3method("glance", "bigIRT_fit", glanceIRT, envir = asNamespace("broom"))
    registerS3method("augment", "bigIRT_fit", augmentIRT, envir = asNamespace("broom"))
  }
  invisible()
}

## Render a self-contained HTML report for a fit.
##
## Everything in it comes from the functions above, so the report is a
## convenience rather than a separate implementation; if a number looks wrong
## here it is wrong in the function that produced it.
#' Render a model report
#'
#' A self-contained HTML report: convergence, the item table, information and
#' reliability, person and item coverage, item fit, the worst-fitting response
#' curves, and held-out performance. Everything in it comes from the exported
#' functions, so it is a convenience rather than a second implementation.
#'
#' @param fit A fitted object from [fitIRT()], fitted with
#'   `keepInternals = TRUE`.
#' @param file Character. Output path.
#' @param title Character. Report title.
#' @param method Character. Passed to [itemInformation()].
#' @param max_icc Integer. Worst-fitting items to draw curves for.
#' @param cores Integer. Cores for the information calculation.
#' @param quiet Logical. Passed to `rmarkdown::render()`.
#'
#' @return The output path, invisibly.
#'
#' @export
reportIRT <- function(fit, file = "bigIRT-report.html", title = "bigIRT model report",
                      method = "profile", max_icc = 9L, cores = 1L, quiet = TRUE){
  if(!requireNamespace("rmarkdown", quietly = TRUE))
    stop("reportIRT needs the 'rmarkdown' package. install.packages('rmarkdown')")
  ## pandoc is on the path inside RStudio but usually not for Rscript, and its
  ## absence is the most likely reason this fails. Look where it normally lives
  ## before giving up.
  if(!rmarkdown::pandoc_available()){
    cand <- Sys.glob(c(
      file.path(Sys.getenv("RSTUDIO_PANDOC"), "pandoc*"),
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/pandoc*",
      "C:/Program Files/RStudio/bin/pandoc/pandoc*",
      "/usr/lib/rstudio/bin/quarto/bin/tools/pandoc",
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/pandoc"))
    cand <- cand[file.exists(cand)]
    if(length(cand)) Sys.setenv(RSTUDIO_PANDOC = dirname(cand[[1]]))
  }
  if(!rmarkdown::pandoc_available())
    stop("reportIRT needs pandoc, which was not found. Install pandoc, or set ",
      "RSTUDIO_PANDOC to the directory holding it.")
  tpl <- system.file("rmd", "report.Rmd", package = "bigIRT")
  if(!nzchar(tpl)) stop("The report template is missing from the installed package.")
  file <- normalizePath(file, mustWork = FALSE)
  rmarkdown::render(tpl, output_file = basename(file), output_dir = dirname(file),
    params = list(fit = fit, title = title, method = method,
                  max_icc = max_icc, cores = cores),
    envir = new.env(parent = globalenv()), quiet = isTRUE(quiet))
  message("wrote ", file)
  invisible(file)
}
