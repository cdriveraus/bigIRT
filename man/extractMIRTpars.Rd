\name{extractMIRTpars}
\alias{extractMIRTpars}
\title{Extract Comparable MIRT Parameters}
\usage{
extractMIRTpars(object, score_method = "EAP")
}
\arguments{
  \item{object}{A fitted \code{bigIRT} object or a \code{mirt} \code{SingleGroupClass} object.}
  \item{score_method}{Character string passed to \code{mirt::fscores()} when extracting abilities from a \code{mirt} fit.}
}
\value{
A list with components \code{source}, \code{A}, \code{B}, \code{C}, \code{D}, \code{Ability}, and \code{corr}.
}
\description{
Extracts a common set of multidimensional IRT parameters from either a \code{bigIRT} fit or a \code{mirt} fit.
}
