#' Apply Power Transformation to Numeric Data
#'
#' Applies a power transformation to numeric input data using the \code{car::powerTransform} function. Supported transformation families include Box-Cox (\code{"bcPower"}), Box-Cox with negative values (\code{"bcnPower"}), and Yeo-Johnson (\code{"yjPower"}). The function estimates either optimal or rounded lambda values for each numeric variable and transforms the data accordingly.
#'
#' @param data A numeric vector, matrix, or data frame. Only numeric columns will be transformed. Non-numeric columns are dropped with a warning.
#' @param family A character string specifying the transformation family. Must be one of \code{"bcPower"}, \code{"bcnPower"}, or \code{"yjPower"}.
#' @param type A character string specifying whether to use the estimated optimal lambda values (\code{"optimal"}) or the rounded values (\code{"rounded"}).
#'
#' @return A list containing two elements. The first is a data frame of transformed numeric columns. The second is a named numeric vector of the lambda values used for the transformation.
#'
#' @details Rows with missing values are removed prior to estimating lambda parameters. A warning is issued if any non-numeric columns are dropped or if any rows are excluded due to missingness. The same estimated lambda values are then applied to the original data (excluding dropped rows or columns).
#'
#' @examples
#' if (requireNamespace("car", quietly = TRUE)) {
#'   x <- rnorm(100, mean = 10, sd = 2)
#'   y <- rexp(100, rate = 0.2)
#'   df <- data.frame(x = x, y = y)
#'   result <- power_transform(df, family = "bcPower", type = "optimal")
#'   head(result$data)
#'   result$lambda
#' }
#'
#' @importFrom car powerTransform
#' @importFrom stats complete.cases
#' @export

power_transform <- function(data, family = c("bcPower", "bcnPower", "yjPower"), type = c("optimal", "rounded")) {
    
  family <- match.arg(family)
  type <- match.arg(type)
  
  # Coerce input to data frame or vector
  if (is.vector(data)) {
    df <- data.frame(.var = data)
    names(df) <- deparse(substitute(data))
  } else if (is.matrix(data) || is.data.frame(data)) {
    df <- as.data.frame(data)
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  
  # Identify numeric columns
  nums <- vapply(df, is.numeric, logical(1))
  if (!any(nums)) stop("No numeric variables to transform.")
  if (!all(nums)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!nums], collapse=", "))
    df <- df[, nums, drop = FALSE]
  }
  
  # Remove rows with any missing values
  complete_rows <- stats::complete.cases(df)
  if (sum(!complete_rows) > 0) {
    warning(sprintf("Removed %d rows with missing values before estimating lambda.", sum(!complete_rows)))
    df_complete <- df[complete_rows, , drop = FALSE]
  } else {
    df_complete <- df
  }
  
  # Estimate power transformation
  pt      <- car::powerTransform(df_complete, family = family)
  pt_sum  <- summary(pt)$result
  lambda_est  <- pt_sum[, "Est Power"]
  lambda_rounded <- pt_sum[, "Rounded Pwr"]
  lambda <- switch(type,
                   optimal = lambda_est,
                   rounded = lambda_rounded)
  names(lambda) <- names(df)
  
  # Apply transformation to each column
  transformed <- as.data.frame(
    mapply(
      function(x, lam) {
        if (lam == 0) {
          log(x)
        } else {
          x^lam
        }
      },
      df, lambda,
      SIMPLIFY = FALSE
    ),
    stringsAsFactors = FALSE
  )
  names(transformed) <- names(df)
  
  return(list(data = transformed, lambda = lambda))
}
