#' Atkinson–Riani–Welsh (ARW) Adjusted Cutoff for Robust Mahalanobis Distances
#'
#' Implements the ARW procedure to compute an adjusted cutoff for squared Mahalanobis distances,
#' then re-estimates location and scatter excluding points beyond the cutoff.
#'
#' @param x A numeric matrix or data frame of observations (rows) by variables (columns), with at least 2 columns.
#' @param m0 A numeric vector of initial location estimates (length equal to number of columns in \code{x}).
#' @param c0 A numeric covariance matrix corresponding to \code{m0}.
#' @param alpha Numeric; significance level for the chi-square threshold. Defaults to 0.05 if not provided.
#' @param pcrit Numeric; minimal proportion for the adjusted cutoff. If not provided, it is computed as:
#'   \eqn{(0.24 - 0.003p)/\sqrt{n}} if \eqn{p \leq 10}, or \eqn{(0.252 - 0.0018p)/\sqrt{n}} if \eqn{p > 10}.
#'
#' @return A list with the following components:
#' \code{m}, the updated location vector after excluding outliers;
#' \code{c}, the updated covariance matrix;
#' \code{cn}, the adjusted cutoff on Mahalanobis distances;
#' \code{w}, a logical vector indicating which observations have distance less than or equal to \code{cn}.
#'
#' @importFrom stats mahalanobis qchisq pchisq
#' @export
arw_adjustment <- function (x, m0, c0, alpha, pcrit)
{
  n <- nrow(x)
  p <- ncol(x)
  if (missing(pcrit)) {
    if (p <= 10)
      pcrit <- (0.24 - 0.003 * p)/sqrt(n)
    if (p > 10)
      pcrit <- (0.252 - 0.0018 * p)/sqrt(n)
  }
  if (missing(alpha))
    delta <- qchisq(0.975, p)
  else delta <- qchisq(1 - alpha, p)
  d2 <- mahalanobis(x, m0, c0)
  d2ord <- sort(d2)
  dif <- pchisq(d2ord, p) - (0.5:n)/n
  i <- (d2ord >= delta) & (dif > 0)
  if (sum(i) == 0)
    alfan <- 0
  else alfan <- max(dif[i])
  if (alfan < pcrit)
    alfan <- 0
  if (alfan > 0)
    cn <- max(d2ord[n - ceiling(n * alfan)], delta)
  else cn <- Inf
  w <- d2 < cn
  if (sum(w) == 0) {
    m <- m0
    c <- c0
  }
  else {
    m <- apply(x[w, ], 2, mean)
    c1 <- as.matrix(x - rep(1, n) %*% t(m))
    c <- (t(c1 * w) %*% c1)/sum(w)
  }
  list(m = m, c = c, cn = cn, w = w)
}
