#' Descriptive Statistics for Numeric Data
#'
#' Computes key descriptive statistics for each numeric variable in a vector, matrix, or data frame.
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#'
#' @return A data frame where each row corresponds to a variable and each column represents a summary statistic:
#' number of non-missing observations (\code{n}), arithmetic mean (\code{Mean}),
#' standard deviation (\code{Std.Dev}), median (\code{Median}), minimum (\code{Min}),
#' maximum (\code{Max}), first quartile (\code{25th}), third quartile (\code{75th}),
#' sample skewness (\code{Skew}, from \code{moments::skewness}), and sample kurtosis
#' (\code{Kurtosis}, from \code{moments::kurtosis}).
#'
#' @examples
#' \dontrun{
#' data <- iris[1:4]
#' descriptives(data)
#' }
#'
#' @importFrom stats sd median quantile setNames
#' @importFrom moments skewness kurtosis
#' @export
descriptives <- function(data) {
  # Coerce to data frame
  df <- if (is.vector(data)) {
    stats::setNames(data.frame(data), deparse(substitute(data)))
  } else if (is.matrix(data) || is.data.frame(data)) {
    as.data.frame(data)
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  
  # Identify numeric columns
  is_num <- vapply(df, is.numeric, logical(1))
  if (!any(is_num)) stop("No numeric columns to summarize.")
  if (!all(is_num)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!is_num], collapse = ", "))
    df <- df[ , is_num, drop = FALSE]
  }
  
  # Calculate per-column sample sizes
  n    <- colSums(!is.na(df))
  # Descriptive statistics
  mean_    <- colMeans(df, na.rm = TRUE)
  sd_      <- vapply(df, stats::sd, numeric(1), na.rm = TRUE)
  median_  <- vapply(df, stats::median, numeric(1), na.rm = TRUE)
  min_     <- vapply(df, min, numeric(1), na.rm = TRUE)
  max_     <- vapply(df, max, numeric(1), na.rm = TRUE)
  quals    <- vapply(df, stats::quantile, numeric(2), probs = c(0.25, 0.75), na.rm = TRUE)
  q1_      <- quals[1, ]
  q3_      <- quals[2, ]
  skew_    <- vapply(df, moments::skewness, numeric(1), na.rm = TRUE)
  kurt_    <- vapply(df, moments::kurtosis, numeric(1), na.rm = TRUE)
  
  # Assemble output
  out <- data.frame(
    n      = n,
    Mean   = mean_,
    Std.Dev= sd_,
    Median = median_,
    Min    = min_,
    Max    = max_,
    `25th` = q1_,
    `75th` = q3_,
    Skew   = skew_,
    Kurtosis = kurt_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  return(out)
}
