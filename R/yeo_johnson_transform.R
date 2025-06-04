#' Yeo-Johnson Transformation for Numeric Data
#'
#' Applies a Yeo-Johnson power transformation to each numeric variable in the input,
#' estimating or rounding the transformation parameter (lambda) using `car::powerTransform`.
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' @param type Character string; either `"optimal"` (use the estimated lambda) or `"rounded"` (use the rounded lambda).
#'   Default is `"optimal"`.
#'
#' @return A list with two components:
#'   `data`, a data frame (or vector) of transformed variables;
#'   and `lambda`, a numeric vector of the lambda values applied to each variable.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:4]
#' # Apply Yeo-Johnson to the first 4 numeric columns
#' res <- yeo_johnson_transform(data, type = "rounded")
#' head(res$data)
#' res$lambda
#' }
#'
#' @importFrom car powerTransform yjPower
#' @importFrom stats complete.cases
#' @export
yeo_johnson_transform <- function(data, type = c("optimal", "rounded")) {
  type <- match.arg(type)

  if (is.vector(data)) {
    df <- data.frame(.var = data)
    names(df) <- deparse(substitute(data))
  } else if (is.matrix(data) || is.data.frame(data)) {
    df <- as.data.frame(data)
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }

  nums <- vapply(df, is.numeric, logical(1))
  if (!any(nums)) stop("No numeric variables to transform.")
  if (!all(nums)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!nums], collapse = ", "))
    df <- df[, nums, drop = FALSE]
  }

  complete_rows <- stats::complete.cases(df)
  if (sum(!complete_rows) > 0) {
    warning(sprintf("Removed %d rows with missing values before estimating lambda.", sum(!complete_rows)))
    df_complete <- df[complete_rows, , drop = FALSE]
  } else {
    df_complete <- df
  }

  pt      <- car::powerTransform(df_complete, family = "yjPower")
  pt_sum  <- summary(pt)$result
  lambda_est  <- pt_sum[, "Est Power"]
  lambda_rounded <- pt_sum[, "Rounded Pwr"]
  lambda <- switch(type,
                   optimal = lambda_est,
                   rounded = lambda_rounded)
  names(lambda) <- names(df)

  transformed <- as.data.frame(
    mapply(car::yjPower, df, lambda, SIMPLIFY = FALSE),
    stringsAsFactors = FALSE
  )
  names(transformed) <- names(df)

  list(data = transformed, lambda = lambda)
}
