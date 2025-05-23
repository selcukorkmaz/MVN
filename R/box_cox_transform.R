#' Box-Cox Transformation for Numeric Data
#'
#' Applies a Box-Cox power transformation to each numeric variable in the input,
#' estimating or rounding the transformation parameter (lambda) using \code{car::powerTransform}.
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' @param type Character string; either \code{"optimal"} (use the estimated lambda) or \code{"rounded"} (use the rounded lambda).
#' Default is \code{"optimal"}.
#'
#' @return A list with two components:
#' \code{data}, a data frame (or vector) of transformed variables;
#' and \code{lambda}, a numeric vector of the lambda values applied to each variable.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:4]
#' # Apply Box-Cox to the first 4 numeric columns
#' res <- box_cox_transform(data, type = "rounded")
#' head(res$data)
#' res$lambda
#' }
#'
#' @importFrom car powerTransform
#' @importFrom stats complete.cases
#' @export
box_cox_transform <- function(data, type = c("optimal", "rounded")) {
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
  pt      <- car::powerTransform(df_complete)
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
