#' Deprecated Function for Henze-Zirklers's Multivariate Normality Test
#'
#' Please use 'mvn' function instead
#'
#'@param data a numeric matrix or data frame
#'@param cov If \code{TRUE} covariance matrix is normalized by \code{n}, if \code{FALSE} it is normalized by \code{n-1}
#'@param qqplot \code{TRUE} it creates chi-square Q-Q plot
#'
#'@export
hzTest <- function (data, cov = TRUE, qqplot = FALSE)
{

  .Deprecated("mvn")

}
