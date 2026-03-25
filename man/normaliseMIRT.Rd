\name{normaliseMIRT}
\alias{normaliseMIRT}
\title{Normalize Multidimensional IRT Parameters}
\usage{
normaliseMIRT(
  B,
  Ability,
  A,
  AbilityCorr = NULL,
  normaliseScale = 1,
  normaliseMean = 0,
  jitter = 1e-08,
  referenceA = NULL,
  align = c("none", "orthogonal")
)
}
\arguments{
  \item{B}{Numeric vector of item intercept/difficulty parameters.}
  \item{Ability}{Numeric matrix of person abilities with rows corresponding to persons and columns to latent factors.}
  \item{A}{Numeric item-by-factor loading matrix.}
  \item{AbilityCorr}{Optional latent correlation matrix. If omitted, the correlation is estimated from \code{Ability}.}
  \item{normaliseScale}{Numeric scalar giving the target latent SD after whitening.}
  \item{normaliseMean}{Numeric scalar or vector giving the target latent mean after normalization.}
  \item{jitter}{Small positive constant added for numerical stability.}
  \item{referenceA}{Optional reference loading matrix used for orthogonal Procrustes alignment.}
  \item{align}{Either \code{"none"} or \code{"orthogonal"}.}
}
\value{
A list containing normalized \code{A}, \code{B}, \code{Ability}, and affine transform pieces including \code{center}, \code{chol_cov}, \code{inv_chol}, \code{AbilityCorr}, \code{sign}, and \code{rotation}.
}
\description{
Applies a curve-preserving affine transform to a multidimensional IRT solution so that the latent ability distribution is centered and whitened, with optional orthogonal alignment to a reference loading matrix.
}
