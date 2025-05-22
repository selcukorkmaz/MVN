#' Diagnostic Plots for Univariate and Multivariate Data
#'
#' Generates QQ plots, histograms with density overlays, boxplots, or scatterplot matrices
#' for numeric data (vector, matrix, or data frame).
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' @param type Character; type of plot. One of:
#'   \describe{
#'     \item{"qqplot"}{Normal Q-Q plot}
#'     \item{"histogram"}{Histogram with normal density overlay}
#'     \item{"boxplot"}{Boxplots of (scaled) variables}
#'     \item{"scatter"}{Scatterplot matrix (for p ≥ 2)}
#'   }. Default selects the first.
#' @param mfrow Numeric vector of length 2 to set the plotting layout via \code{par(mfrow=mfrow)}. If \code{NULL}, layout is chosen automatically for multiple variables.
#' @param ... Additional graphical parameters passed to the plotting functions.
#' @examples
#' \dontrun{
#' data = iris[1:4]
#' uni_plot(data, type = "histogram")
#' uni_plot(rnorm(100), type = "qqplot")
#' }
#' @importFrom graphics hist curve boxplot pairs par
#' @importFrom stats complete.cases dnorm sd qqnorm qqline
#' @export
uni_plot <- function(data,
                     type  = c("qqplot", "histogram", "boxplot", "scatter"),
                     mfrow = NULL,
                     ...) {
  type <- match.arg(type)
  
  # Coerce input to data frame
  if (is.vector(data)) {
    df <- data.frame(value = data)
    var_names <- deparse(substitute(data))
    names(df) <- var_names
  } else if (is.matrix(data) || is.data.frame(data)) {
    df <- as.data.frame(data)
    var_names <- if (!is.null(colnames(df))) colnames(df) else paste0("V", seq_len(ncol(df)))
    names(df) <- var_names
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  
  # Drop non-numeric columns
  is_num <- vapply(df, is.numeric, logical(1))
  if (!all(is_num)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!is_num], collapse=", "))
    df <- df[, is_num, drop = FALSE]
    var_names <- names(df)
  }
  p <- ncol(df)
  if (p < 1) stop("No numeric variables to plot.")
  
  # Remove rows with any NA
  complete_rows <- complete.cases(df)
  if (sum(!complete_rows) > 0) {
    warning(sprintf("Removed %d rows with missing values.", sum(!complete_rows)))
    df <- df[complete_rows, , drop = FALSE]
  }
  
  # Determine layout for multiple plots
  if (is.null(mfrow) && p > 1 && type %in% c("qqplot", "histogram", "boxplot")) {
    ncol <- ceiling(sqrt(p))
    nrow <- ceiling(p / ncol)
    par(mfrow = c(nrow, ncol))
  } else if (!is.null(mfrow)) {
    par(mfrow = mfrow)
  }
  
  # Plot according to type
  if (type == "histogram") {
    for (i in seq_len(p)) {
      x <- df[[i]]
      graphics::hist(x, freq = FALSE, main = "", xlab = var_names[i], ...)
      dens_x <- seq(min(x), max(x), length.out = 100)
      graphics::curve(stats::dnorm(x, mean = mean(x), sd = sd(x)),
                      from = min(x), to = max(x), add = TRUE, col = "red", ...)
    }
  } else if (type == "qqplot") {
    for (i in seq_len(p)) {
      x <- df[[i]]
      stats::qqnorm(x, main = paste("Q-Q Plot:", var_names[i]), ...)
      stats::qqline(x, ...)
    }
  } else if (type == "boxplot") {
    warning("Boxplots are drawn on standardized data (centered and scaled).")
    graphics::boxplot(scale(df), names = var_names, main = "", ...)
  } else if (type == "scatter") {
    if (p < 2) stop("Scatterplot matrix requires at least two variables.")
    graphics::pairs(df, labels = var_names, ...)
  }
  
  invisible(NULL)
}
