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
  
  # Mardia's measures (observed)
  g1p_obs <- sum(D^3) / n^2
  g2p_obs <- sum((diag(D))^2) / n
  
  # Skewness test (observed)
  df_skew   <- p * (p + 1) * (p + 2) / 6
  k_const   <- ((p + 1) * (n + 1) * (n + 3)) / (n * ((n + 1) * (p + 1) - 6))
  skew_stat <- if (n < 20) {
    n * k_const * g1p_obs / 6
  } else {
    n * g1p_obs / 6
  }
  p_skew    <- stats::pchisq(skew_stat, df_skew, lower.tail = FALSE)
  
  # Kurtosis test (observed)
  kurt_stat <- (g2p_obs - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
  p_kurt    <- 2 * stats::pnorm(abs(kurt_stat), lower.tail = FALSE)
  
  # By default, record the “Method” and “N.Boot” fields for output
  method_skew <- "asymptotic"
  method_kurt <- "asymptotic"
  n_boot_used <- NA_integer_
  
  if (bootstrap && B > 0) {
    # Estimate parametric-null parameters from the observed data
    mu_hat    <- colMeans(x)  # sample mean of original x
    if (use_population) {
      Sigma_hat <- ((n - 1) / n) * stats::cov(x_centered)
    } else {
      Sigma_hat <- stats::cov(x_centered)
    }
    
    # Precompute constants for each replicate
    df_skew   <- p * (p + 1) * (p + 2) / 6
    k_const   <- ((p + 1) * (n + 1) * (n + 3)) / (n * ((n + 1) * (p + 1) - 6))
    sq_factor <- sqrt(n / (8 * p * (p + 2)))
    bb_const  <- (n^(1/(p + 4))) * (((2 * p + 1) / 4)^(1/(p + 4))) / sqrt(2)
    
    boot_fun <- function(i) {
      # 1) Draw a parametric‐bootstrap sample from N(mu_hat, Sigma_hat)
      xb <- MASS::mvrnorm(n = n, mu = mu_hat, Sigma = Sigma_hat)
      
      # 2) Center that bootstrap sample
      xb_c <- scale(xb, center = TRUE, scale = FALSE)
      
      # 3) Recompute covariance for the bootstrap draw
      Sb <- if (use_population) {
        ((n - 1) / n) * stats::cov(xb_c)
      } else {
        stats::cov(xb_c)
      }
      
      # 4) Invert Sb (if singular, drop this replicate)
      invSb <- tryCatch(
        solve(Sb, tol = tol),
        error = function(e) return(matrix(NA, ncol = ncol(Sb), nrow = nrow(Sb)))
      )
      if (anyNA(invSb)) return(c(NA_real_, NA_real_))
      
      # 5) Compute Mahalanobis distance matrix for the bootstrap sample
      Db   <- xb_c %*% invSb %*% t(xb_c)
      Djb  <- diag(Db)
      Djkb <- outer(Djb, Djb, "+") - 2 * Db
      
      # 6) Mardia’s skewness on bootstrap replicate
      g1p_b   <- sum(Db^3) / n^2
      skew_b  <- if (n < 20) {
        n * k_const * g1p_b / 6
      } else {
        n * g1p_b / 6
      }
      
      # 7) Mardia’s kurtosis on bootstrap replicate
      g2p_b   <- sum(Djb^2) / n
      kurt_b  <- (g2p_b - p * (p + 2)) * sq_factor
      
      return(c(skew_b, kurt_b))
    }
    
    # Run B replicates (possibly in parallel)
    if (cores > 1) {
      boot_stats <- parallel::mclapply(seq_len(B), boot_fun, mc.cores = cores)
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }
    boot_mat <- do.call(rbind, boot_stats)   # B × 2 matrix: [skew_rep, kurt_rep]
    # Drop any rows with NA (failed replicates)
    boot_mat <- boot_mat[complete.cases(boot_mat), , drop = FALSE]
    
    if (nrow(boot_mat) > 0) {
      boot_skew <- boot_mat[, 1]
      boot_kurt <- boot_mat[, 2]
      n_boot_used <- nrow(boot_mat)
      
      # Parametric‐bootstrap p‐values:
      p_skew <- mean(boot_skew >= skew_stat)
      p_kurt <- mean(abs(boot_kurt) >= abs(kurt_stat))
      
      method_skew <- "bootstrap"
      method_kurt <- "bootstrap"
    } else {
      p_skew <- NA_real_
      p_kurt <- NA_real_
    }
  }
  
  # Assemble results
  if (bootstrap) {
    result <- data.frame(
      Test      = c("Mardia Skewness", "Mardia Kurtosis"),
      Statistic = c(skew_stat, kurt_stat),
      p.value   = c(p_skew, p_kurt),
      Method    = c(method_skew, method_kurt),
      N.Boot    = rep(n_boot_used, 2),
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      Test      = c("Mardia Skewness", "Mardia Kurtosis"),
      Statistic = c(skew_stat, kurt_stat),
      p.value   = c(p_skew, p_kurt),
      Method    = c(method_skew, method_kurt),
      stringsAsFactors = FALSE
    )
  }
  
  
  return(result)
}

