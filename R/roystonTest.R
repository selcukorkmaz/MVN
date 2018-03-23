#' Deprecated Function for Royston's Multivariate Normality Test
#'
#' Please use 'mvn' function instead
#'
#'@param data a numeric matrix or data frame
#'@param qqplot \code{TRUE} it creates chi-square Q-Q plot
#'
#'@export
roystonTest <-function (data, qqplot = FALSE)
{
  .Deprecated("mvn")
}
