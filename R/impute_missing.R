#' Impute Missing Values
#'
#' Replace \code{NA}s in numeric variables using simple methods or \code{mice}-based imputation.
#'
#' @param data A numeric matrix or data frame.
#' @param method Character; one of \code{"mean"}, \code{"median"}, or \code{"mice"}. Default: \code{"mean"}.
#' @param m Integer; number of multiple imputations when \code{method = "mice"}. Default: 5.
#' @param seed Integer; random seed for \code{mice} imputation. Default: 123.
#' @param ... Additional arguments passed to \code{mice::mice} when \code{method = "mice"}.
#'
#' @importFrom mice mice complete
#'
#' @return A data frame with missing values imputed.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
#' impute_missing(df, method = "mice")
#' }
#' @export
impute_missing <- function(data, method = c("mean", "median", "mice"), m = 5, seed = 123, ...) {
  method <- match.arg(method)
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data frame or matrix")
  }
  df <- as.data.frame(data)
  
  if (method == "mean") {
    for (nm in names(df)) {
      if (is.numeric(df[[nm]])) {
        df[[nm]][is.na(df[[nm]])] <- mean(df[[nm]], na.rm = TRUE)
      }
    }
  } else if (method == "median") {
    for (nm in names(df)) {
      if (is.numeric(df[[nm]])) {
        df[[nm]][is.na(df[[nm]])] <- median(df[[nm]], na.rm = TRUE)
      }
    }
  } else if (method == "mice") {
    if (!requireNamespace("mice", quietly = TRUE)) {
      stop("Package 'mice' is required for method = 'mice'")
    }
    set.seed(seed)
    imp <- mice::mice(df, m = m, printFlag = FALSE, ...)
    df <- mice::complete(imp)
  }
  
  df
}
