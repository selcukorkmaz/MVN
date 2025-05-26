#' Summarize Multivariate Normality Analysis Results
#'
#' Provides a structured summary of the results from an object of class \code{mvn},
#' including multivariate and univariate normality tests, descriptive statistics,
#' and multivariate outlier detection (if applicable).

#' @param object An object of class \code{mvn}, as returned by the \code{\link{mvn}} function.
#' @param select A character vector specifying which components to display. 
#'   Must be one or more of \code{"mvn"}, \code{"univariate"}, \code{"descriptives"}, \code{"outliers"}, or \code{"new_data"}.
#'   Defaults to showing all available sections.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:4]
#' result <- mvn(data)
#'
#' summary(result)  # Show all sections
#' summary(result, select = c("mvn", "outliers"))  # Show selected sections only
#' }
#'
#' @importFrom cli cli_h1 cli_alert_info
#' @importFrom utils head
#' @method summary mvn
#' @export
summary.mvn  <- function(object, select = c("mvn", "univariate", "descriptives", "outliers", "new_data"), ...) {
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' is required for pretty printing. Please install it.")
  }
  
  select <- match.arg(select, choices = c("mvn", "univariate", "descriptives", "outliers", "new_data"), several.ok = TRUE)
  
  if ("mvn" %in% select && !is.null(object$multivariate_normality)) {
    cli::cli_h1("Multivariate Normality Test Results")
    print(object$multivariate_normality)
  }
  
  if ("univariate" %in% select && !is.null(object$univariate_normality)) {
    cli::cli_h1("Univariate Normality Test Results")
    print(object$univariate_normality)
  }
  
  if ("descriptives" %in% select && !is.null(object$descriptives)) {
    cli::cli_h1("Descriptive Statistics")
    print(object$descriptives)
  }
  
  if ("outliers" %in% select && !is.null(object$multivariate_outliers)) {
    cli::cli_h1("Multivariate Outliers")
    print(object$multivariate_outliers)
  }
  
  if ("new_data" %in% select && !is.null(object$new_data)) {
    cli::cli_h1("New Data (After Removing Outliers)")
    print(object$new_data)
  }
  
  invisible(object)
}
