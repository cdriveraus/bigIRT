\name{IRTcurve}
\alias{IRTcurve}
\title{Evaluate IRT Item Response Curves}
\usage{
IRTcurve(
  A,
  B,
  C = 0,
  D = 1,
  theta = seq(-3, 3, 0.01),
  plot = TRUE,
  rescale = FALSE,
  add = FALSE,
  item = NULL,
  ...
)
}
\arguments{
  \item{A}{Numeric item-by-factor loading matrix, or a numeric vector for a single-factor model.}
  \item{B}{Numeric vector of item intercept/difficulty parameters.}
  \item{C}{Numeric vector of lower asymptotes.}
  \item{D}{Numeric vector of upper asymptotes.}
  \item{theta}{Numeric matrix of latent trait values (rows are evaluation points, columns are factors), or a numeric vector for a single-factor model.}
  \item{plot}{Logical; if \code{TRUE}, produce a 1D plot.}
  \item{rescale}{Logical; if \code{TRUE}, standardize \code{theta} before evaluation.}
  \item{add}{Logical; when plotting 1D curves, add to an existing plot.}
  \item{item}{Optional item index or indices to evaluate.}
  \item{...}{Additional graphical arguments passed to plotting functions.}
}
\value{
A numeric vector for a single item or a numeric matrix with rows corresponding to \code{theta} points and columns to items.
}
\description{
Evaluates one-dimensional or multidimensional 4PL response curves under the \code{bigIRT} parameterization \eqn{\eta = \theta A^\top - B}.
}
