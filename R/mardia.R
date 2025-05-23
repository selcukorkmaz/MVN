#' Mardia's Test for Multivariate Normality
#'
#' Performs Mardia’s skewness and kurtosis tests to assess multivariate normality
#' in a multivariate dataset.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param use_population Logical; if \code{TRUE}, uses the population covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise uses the sample covariance. Default is \code{TRUE}.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when inverting the covariance matrix.
#' Default is \code{1e-25}.
#'
#' @return A data frame with two rows, one for Mardia's skewness test and one for the kurtosis test.
#' Each row contains the name of the test (\code{Test}), the test statistic (\code{Statistic}),
#' and the associated p-value (\code{p.value}).
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' mardia_result <- mardia(data)
#' mardia_result
#' }
#'
#' @importFrom stats cov complete.cases pchisq pnorm
#' @export
mardia <- function(data, use_population = TRUE, tol = 1e-25) {
  # Convert to data frame
  df <- as.data.frame(data)
  
  # Drop non-numeric columns
  numeric_cols <- sapply(df, is.numeric)
  if (!all(numeric_cols)) {
    dropped <- names(df)[!numeric_cols]
    warning("Dropping non-numeric columns: ", paste(dropped, collapse = ", "))
    df <- df[, numeric_cols, drop = FALSE]
  }
  if (ncol(df) < 2) {
    stop("Need at least two numeric variables for Mardia's test.")
  }
  
  # Handle missing values
  complete <- stats::complete.cases(df)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", n_dropped))
  }
  df <- df[complete, , drop = FALSE]
  
  # Convert to matrix and set dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  # Center the data
  x_centered <- scale(x, center = TRUE, scale = FALSE)
  
  # Compute covariance matrix
  if (use_population) {
    S <- ((n - 1) / n) * stats::cov(x_centered)
  } else {
    S <- stats::cov(x_centered)
  }
  
  # Invert covariance matrix with tolerance
  invS <- tryCatch(
    solve(S, tol = tol),
    error = function(e) stop("Covariance matrix is singular or near-singular: ", e$message)
  )
  
  # Compute matrix of Mahalanobis distances
  D <- x_centered %*% invS %*% t(x_centered)
  
  # Mardia's measures
  g1p <- sum(D^3) / n^2
  g2p <- sum((diag(D))^2) / n
  
  # Skewness test
  df_skew   <- p * (p + 1) * (p + 2) / 6
  k         <- ((p + 1) * (n + 1) * (n + 3)) / (n * ((n + 1) * (p + 1) - 6))
  skew_stat <- if (n < 20) {
    n * k * g1p / 6
  } else {
    n * g1p / 6
  }
  p_skew <- stats::pchisq(skew_stat, df_skew, lower.tail = FALSE)
  
  # Kurtosis test
  kurt_stat <- (g2p - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
  p_kurt    <- 2 * stats::pnorm(abs(kurt_stat), lower.tail = FALSE)
  
  
  # Assemble results
  result <- data.frame(
    Test      = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(skew_stat, kurt_stat),
    p.value   = c(p_skew, p_kurt),
    stringsAsFactors = FALSE
  )
  
  return(result)
}
