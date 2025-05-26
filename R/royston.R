#' Royston's Multivariate Normality Test
#'
#' Performs Royston’s test for multivariate normality by combining univariate W-statistics
#' (Shapiro–Wilk or Shapiro–Francia) across variables and adjusting for the correlation structure.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when inverting the covariance matrix. Default is \code{1e-25}.
#'
#' @return A data frame with one row containing the test name (\code{Test}), the Royston test statistic (\code{Statistic}),
#' and the associated p-value (\code{p.value}) from a chi-square approximation.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' royston_result <- royston(data)
#' royston_result
#' }
#'
#' @importFrom stats complete.cases cov cor pchisq pnorm qnorm shapiro.test
#' @importFrom moments kurtosis
#' @importFrom nortest sf.test
#' @export
royston <- function(data, tol = 1e-25) {
  # Convert and filter numeric columns
  df <- as.data.frame(data)
  num_cols <- vapply(df, is.numeric, logical(1))
  if (!all(num_cols)) {
    warning("Dropping non-numeric columns: ",
            paste(names(df)[!num_cols], collapse = ", "))
    df <- df[, num_cols, drop = FALSE]
  }
  if (ncol(df) < 2) {
    stop("Need at least two numeric variables for Royston's test.")
  }
  
  # Handle missing data
  keep <- stats::complete.cases(df)
  dropped <- sum(!keep)
  if (dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", dropped))
  }
  df <- df[keep, , drop = FALSE]
  
  # Dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  if (n <= 3) stop("Sample size must be greater than 3.")
  if (n > 2000) stop("Sample size must be less than or equal to 2000.")
  
  # Compute univariate W-statistics and z-transform
  z_vals <- numeric(p)
  if (n >= 4 && n <= 11) {
    g <- -2.273 + 0.459 * n
    m <- 0.544 - 0.39978 * n + 0.025054 * n^2 - 0.0006714 * n^3
    s <- exp(1.3822 - 0.77857 * n + 0.062767 * n^2 - 0.0020322 * n^3)
    for (i in seq_len(p)) {
      vec <- x[, i]
      w   <- if (moments::kurtosis(vec) > 3) nortest::sf.test(vec)$statistic
      else stats::shapiro.test(vec)$statistic
      z_vals[i] <- (-log(g - log(1 - w)) - m) / s
    }
  } else if (n >= 12 && n <= 2000) {
    lx <- log(n)
    m  <- -1.5861 - 0.31082 * lx - 0.083751 * lx^2 + 0.0038915 * lx^3
    s  <- exp(-0.4803 - 0.082676 * lx + 0.0030302 * lx^2)

    for (i in seq_len(p)) {
      vec <- x[, i]
      w   <- if (moments::kurtosis(vec) > 3) nortest::sf.test(vec)$statistic
      else stats::shapiro.test(vec)$statistic
      z_vals[i] <- (log(1 - w) - m) / s
    }
  }
  
  # Adjust for correlation among variables
  u <- 0.715
  v <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
  l <- 5
  C  <- stats::cor(x)
  NC <- (C^l) * (1 - (u * (1 - C)^u) / v)
  T  <- sum(NC) - p
  mC <- T / (p^2 - p)
  edf <- p / (1 + (p - 1) * mC)
  
  # Compute Royston H statistic
  H_stat <- (edf * sum((stats::qnorm(stats::pnorm(-z_vals) / 2))^2)) / p
  p_val   <- stats::pchisq(H_stat, df = edf, lower.tail = FALSE)

  # Return result
  result <- data.frame(
    Test    = "Royston",
    Statistic = H_stat,
    p.value = p_val,
    stringsAsFactors = FALSE
  )
  return(result)
}
