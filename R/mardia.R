#' Mardia's Test for Multivariate Normality
#'
#' Performs Mardia’s skewness and kurtosis tests to assess multivariate normality
#' in a multivariate dataset.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param use_population Logical; if \code{TRUE}, uses the population covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise uses the sample covariance. Default is \code{TRUE}.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when inverting the covariance matrix.
#' Default is \code{1e-25}.
#' @param bootstrap Logical; if \code{TRUE}, compute p-values via a bootstrap
#'   distribution of the test statistics. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Only used when
#'   \code{bootstrap = TRUE}. Default is \code{1000}.
#' @param cores Integer; number of cores to use when \code{bootstrap = TRUE}.
#'   Parallelisation is done via \code{parallel::mclapply}. Default is 1.
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
mardia <- function(data, use_population = TRUE, tol = 1e-25,
                   bootstrap = FALSE, B = 1000, cores = 1) {
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
  
  if (bootstrap && B > 0) {
    boot_fun <- function(i) {
      idx <- sample.int(n, n, replace = TRUE)
      xb <- x[idx, , drop = FALSE]
      xb_c <- scale(xb, center = TRUE, scale = FALSE)
      Sb <- if (use_population) {
        ((n - 1) / n) * stats::cov(xb_c)
      } else {
        stats::cov(xb_c)
      }
      invSb <- tryCatch(solve(Sb, tol = tol),
                        error = function(e) return(matrix(NA, ncol = ncol(Sb), nrow = nrow(Sb))))
      if (anyNA(invSb)) return(c(NA_real_, NA_real_))
      Db <- xb_c %*% invSb %*% t(xb_c)
      g1b <- sum(Db^3) / n^2
      g2b <- sum((diag(Db))^2) / n
      skew_b <- if (n < 20) {
        n * k * g1b / 6
      } else {
        n * g1b / 6
      }
      kurt_b <- (g2b - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
      c(skew_b, kurt_b)
    }
    
    if (cores > 1) {
      boot_stats <- parallel::mclapply(seq_len(B), boot_fun, mc.cores = cores)
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }
    boot_mat <- do.call(rbind, boot_stats)
    boot_mat <- boot_mat[complete.cases(boot_mat), , drop = FALSE]
    if (nrow(boot_mat) > 0) {
      p_skew <- mean(boot_mat[, 1] >= skew_stat)
      p_kurt <- mean(abs(boot_mat[, 2]) >= abs(kurt_stat))
    } else {
      p_skew <- NA_real_
      p_kurt <- NA_real_
    }
  }
  
  # Assemble results
  result <- data.frame(
    Test      = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(skew_stat, kurt_stat),
    p.value   = c(p_skew, p_kurt),
    stringsAsFactors = FALSE
  )
  
  return(result)
}
