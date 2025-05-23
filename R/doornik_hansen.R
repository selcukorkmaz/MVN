#' Doornik-Hansen Test for Multivariate Normality
#'
#' Performs the Doornik–Hansen omnibus test by transforming the data to approximate normality
#' and combining skewness and kurtosis measures to test for multivariate normality.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#'
#' @return A data frame with one row containing the following columns:
#' \code{Test}, the name of the test ("Doornik-Hansen");
#' \code{Statistic}, the value of the test statistic;
#' \code{df}, the degrees of freedom;
#' and \code{p.value}, the p-value from a chi-square approximation.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' dh_result <- doornik_hansen(data)
#' dh_result
#' }
#'
#' @importFrom stats complete.cases cov pchisq sd
#' @importFrom moments skewness kurtosis
#' @export
doornik_hansen <- function(data) {
  # Convert and filter numeric columns
  df <- as.data.frame(data)
  num_cols <- vapply(df, is.numeric, logical(1))
  if (!all(num_cols)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!num_cols], collapse=", "))
    df <- df[, num_cols, drop=FALSE]
  }
  if (ncol(df) < 2) stop("Need at least two numeric variables for Doornik-Hansen test.")
  
  # Handle missing values
  keep <- stats::complete.cases(df)
  dropped <- sum(!keep)
  if (dropped > 0) warning(sprintf("%d rows with missing values removed.", dropped))
  df <- df[keep, , drop = FALSE]
  
  # Dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  # Compute covariance matrix
  S <- stats::cov(x)
  
  # Decorrelation and standardization
  sdv <- sqrt(diag(S))
  V   <- diag(1 / sdv)
  C   <- V %*% S %*% V
  eig <- eigen(C)
  H   <- eig$vectors
  L   <- diag(eig$values^(-0.5))
  Xc  <- t(df - sapply(df, mean))
  yi  <- H %*% L %*% t(H) %*% V %*% Xc
  
  # Univariate skewness and kurtosis for transformed data
  B1 <- apply(yi, 1, moments::skewness)
  B2 <- apply(yi, 1, moments::kurtosis)
  
  # Parameters for skewness component
  del1 <- (n - 3) * (n + 1) * (n^2 + 15 * n - 4)
  a1   <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + 27 * n - 70)) / (6 * del1)
  c1   <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + 2 * n - 5))   / (6 * del1)
  k1   <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + 11 * n - 313)) / (12 * del1)
  alpha <- a1 + c1 * B1^2
  chi   <- 2 * k1 * (B2 - 1 - B1^2)
  Z2    <- sqrt(9 * alpha) * (((chi / (2 * alpha))^(1/3) - 1 + 1/(9 * alpha)))
  
  # Parameters for skewness component Z1
  beta <- (3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3)) / ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  w2   <- -1 + sqrt(2 * (beta - 1))
  y    <- B1 * sqrt(((w2 - 1)/2) * ((n + 1)*(n + 3)/(6*(n - 2))))
  del2 <- 1 / sqrt(log(sqrt(w2)))
  Z1   <- del2 * log(y + sqrt(y^2 + 1))
  
  # Combined statistic
  E     <- sum(Z1^2) + sum(Z2^2)
  df_E  <- 2 * p
  p_val <- stats::pchisq(E, df = df_E, lower.tail = FALSE)

  # Return
  result <- data.frame(
    Test    = "Doornik-Hansen",
    Statistic  = E,
    df      = df_E,
    p.value = p_val,
    stringsAsFactors = FALSE
  )
  return(result)
}