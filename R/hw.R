#' Henze-Wagner High-Dimensional Test for Multivariate Normality
#'
#' Performs the high-dimensional version of the BHEP test for
#' multivariate normality as proposed by Henze and Wagner (1997).
#' When the covariance matrix is singular (e.g., when p > n) a
#' Moore-Penrose pseudoinverse is used.
#'
#' @param data A numeric matrix or data frame with observations in rows
#'   and variables in columns.
#' @param use_population Logical; if \code{TRUE}, uses the population
#'   covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise
#'   uses the sample covariance. Default is \code{TRUE}.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when
#'   inverting the covariance matrix. Default is \code{1e-25}.
#'
#' @return A data frame with one row containing the following columns:
#'   \code{Test} ("Henze-Wagner"), \code{Statistic} and \code{p.value}.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' hw_result <- hw(data)
#' hw_result
#' }
#'
#' @importFrom stats cov complete.cases plnorm
#' @importFrom MASS ginv
#' @export
hw <- function(data, use_population = TRUE, tol = 1e-25) {
  df <- as.data.frame(data)
  numeric_cols <- sapply(df, is.numeric)
  if (!all(numeric_cols)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!numeric_cols], collapse = ", "))
    df <- df[, numeric_cols, drop = FALSE]
  }
  if (ncol(df) < 2) stop("Need at least two numeric variables for Henze-Wagner test.")
  
  keep <- stats::complete.cases(df)
  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", n_dropped))
  }
  df <- df[keep, , drop = FALSE]
  
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  x_centered <- scale(x, center = TRUE, scale = FALSE)
  
  if (use_population) {
    S <- ((n - 1) / n) * stats::cov(x_centered)
  } else {
    S <- stats::cov(x_centered)
  }
  
  invS <- tryCatch(
    solve(S, tol = tol),
    error = function(e) MASS::ginv(S)
  )
  
  D <- x_centered %*% invS %*% t(x_centered)
  Dj <- diag(D)
  Djk <- outer(Dj, Dj, "+") - 2 * D
  
  b <- 1
  
  part1 <- sum(exp(-(b^2) / 2 * Djk)) / (n^2)
  part2 <- 2 * (1 + b^2)^(-p/2) * sum(exp(-(b^2)/(2 * (1 + b^2)) * Dj)) / n
  hz_stat <- n * (part1 - part2 + (1 + 2 * b^2)^(-p/2))
  
  a   <- 1 + 2 * b^2
  wb  <- (1 + b^2) * (1 + 3 * b^2)
  mu  <- 1 - a^(-p/2) * (1 + (p * b^2) / a + (p * (p + 2) * b^4) / (2 * a^2))
  si2 <-  2 * (1 + 4 * b^2)^(-p/2) +
    2 * a^(-p) * (1 + (2 * p * b^4) / a^2 + (3 * p * (p + 2) * b^8) / (4 * a^4)) -
    4 * wb^(-p/2) * (1 + (3 * p * b^4) / (2 * wb) + (p * (p + 2) * b^8) / (2 * wb^2))
  pmu <- log(sqrt(mu^4 / (si2 + mu^2)))
  psi <- sqrt(log((si2 + mu^2) / mu^2))
  
  p_value <- stats::plnorm(hz_stat, pmu, psi, lower.tail = FALSE)
  
  data.frame(
    Test = "Henze-Wagner",
    Statistic = hz_stat,
    p.value = p_value,
    stringsAsFactors = FALSE
  )
}