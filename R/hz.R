#' Henze-Zirkler Test for Multivariate Normality
#'
#' Performs Henze and Zirkler's test to assess multivariate normality based on a log-normal approximation of the test statistic.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param use_population Logical; if \code{TRUE}, uses the population covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise uses the sample covariance. Default is \code{TRUE}.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when inverting the covariance matrix. Default is \code{1e-25}.
#' @param bootstrap Logical; if \code{TRUE}, compute p-value via bootstrap
#'   resampling. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates used when
#'   \code{bootstrap = TRUE}. Default is \code{1000}.
#' @param cores Integer; number of cores for parallel computation when
#'   \code{bootstrap = TRUE}. Default is 1.
#'
#' @return A data frame with one row, containing the following columns:
#' \code{Test}, the name of the test ("Henze-Zirkler"); 
#' \code{HZ}, the test statistic (numeric); 
#' and \code{p.value}, the p-value computed from a log-normal approximation.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' hz_result <- hz(data)
#' hz_result
#' }
#'
#' @importFrom stats cov complete.cases plnorm
#' @export
hz <- function(data, use_population = TRUE, tol = 1e-25,
               bootstrap = FALSE, B = 1000, cores = 1) {
  # Convert to data frame and drop non-numeric columns
  df <- as.data.frame(data)
  numeric_cols <- sapply(df, is.numeric)
  if (!all(numeric_cols)) {
    dropped <- names(df)[!numeric_cols]
    warning("Dropping non-numeric columns: ", paste(dropped, collapse = ", "))
    df <- df[, numeric_cols, drop = FALSE]
  }
  if (ncol(df) < 2) stop("Need at least two numeric variables for Henze-Zirkler test.")
  
  # Handle missing values
  complete_rows <- stats::complete.cases(df)
  n_dropped <- sum(!complete_rows)
  if (n_dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", n_dropped))
  }
  df <- df[complete_rows, , drop = FALSE]
  
  # Convert to matrix and dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  # Center data
  x_centered <- scale(x, center = TRUE, scale = FALSE)
  
  # Covariance matrix
  if (use_population) {
    S <- ((n - 1) / n) * stats::cov(x_centered)
  } else {
    S <- stats::cov(x_centered)
  }
  
  # Invert covariance
  invS <- tryCatch(
    solve(S, tol = tol),
    error = function(e) stop("Covariance matrix is singular or near-singular: ", e$message)
  )
  
  # Squared Mahalanobis distances
  D <- x_centered %*% invS %*% t(x_centered)
  Dj <- diag(D)
  
  # Pairwise squared differences
  # Djk_{ij} = D_{ii} + D_{jj} - 2*D_{ij}
  Djk <- outer(Dj, Dj, "+") - 2 * D
  
  # Smoothing parameter b
  b <- (n^(1/(p + 4))) * (((2 * p + 1) / 4)^(1/(p + 4))) / sqrt(2)
  
  # Compute HZ statistic
  part1 <- sum(exp(- (b^2)/2 * Djk)) / (n^2)
  part2 <- 2 * (1 + b^2)^(-p/2) * sum(exp(- (b^2)/(2 * (1 + b^2)) * Dj)) / n
  hz_stat <- n * (part1 - part2 + (1 + 2 * b^2)^(-p/2))
  
  # Log-normal approximation parameters
  a   <- 1 + 2 * b^2
  wb  <- (1 + b^2) * (1 + 3 * b^2)
  mu  <- 1 - a^(-p/2) * (1 + (p * b^2) / a + (p * (p + 2) * b^4) / (2 * a^2))
  si2 <-  2 * (1 + 4 * b^2)^(-p/2) +
    2 * a^(-p) * (1 + (2 * p * b^4) / a^2 + (3 * p * (p + 2) * b^8) / (4 * a^4)) -
    4 * wb^(-p/2) * (1 + (3 * p * b^4) / (2 * wb) + (p * (p + 2) * b^8) / (2 * wb^2))
  pmu <- log(sqrt(mu^4 / (si2 + mu^2)))
  psi <- sqrt(log((si2 + mu^2) / mu^2))
  
  # P-value
  p_value <- stats::plnorm(hz_stat, pmu, psi, lower.tail = FALSE)
  
  if (bootstrap && B > 0) {
    # Estimate parametric-null parameters from the observed data:
    mu_hat    <- colMeans(x)                  # sample mean of original x
    # We want Σ̂ for drawing N(mu_hat, Σ̂). Use the same "population vs. sample" rule:
    if (use_population) {
      Sigma_hat <- ((n - 1) / n) * stats::cov(x_centered)
    } else {
      Sigma_hat <- stats::cov(x_centered)
    }

    boot_fun <- function(i) {
      # 1) Draw a parametric‐bootstrap sample of size n from N(mu_hat, Sigma_hat)
      xb <- MASS::mvrnorm(n = n, mu = mu_hat, Sigma = Sigma_hat)

      # 2) Center that bootstrap sample
      xb_c <- scale(xb, center = TRUE, scale = FALSE)

      # 3) Recompute covariance for the bootstrap draw (same rule as before)
      Sb <- if (use_population) {
        ((n - 1) / n) * stats::cov(xb_c)
      } else {
        stats::cov(xb_c)
      }

      # 4) Invert Sb (if singular, return NA to drop this replicate)
      invSb <- tryCatch(
        solve(Sb, tol = tol),
        error = function(e) return(matrix(NA, ncol = ncol(Sb), nrow = nrow(Sb)))
      )
      if (anyNA(invSb)) return(NA_real_)

      # 5) Compute pairwise Mahalanobis distances for the bootstrap sample
      Db   <- xb_c %*% invSb %*% t(xb_c)
      Djb  <- diag(Db)
      Djkb <- outer(Djb, Djb, "+") - 2 * Db

      # 6) Use the same smoothing parameter formula (n, p are unchanged)
      bb <- (n^(1/(p + 4))) * (((2 * p + 1) / 4)^(1/(p + 4))) / sqrt(2)

      # 7) Compute HZ on the bootstrap sample
      part1b <- sum(exp(-(bb^2)/2 * Djkb)) / (n^2)
      part2b <- 2 * (1 + bb^2)^(-p/2) * sum(exp(-(bb^2)/(2 * (1 + bb^2)) * Djb)) / n
      hz_b   <- n * (part1b - part2b + (1 + 2 * bb^2)^(-p/2))
      return(hz_b)
    }

    # Run B replicates (in parallel if cores > 1)
    if (cores > 1) {
      boot_stats <- parallel::mclapply(seq_len(B), boot_fun, mc.cores = cores)
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }

    # Collect and discard any NA (singular‐covariance draws)
    boot_vec <- unlist(boot_stats)
    boot_vec <- boot_vec[!is.na(boot_vec)]

    if (length(boot_vec) > 0) {
      # 8) Now compute the parametric‐bootstrap p-value:
      p_value <- mean(boot_vec >= hz_stat)
    } else {
      p_value <- NA_real_
    }
  }
  

  if (bootstrap) {
    result <- data.frame(
      Test         = "Henze-Zirkler",
      Statistic    = hz_stat,
      p.value  = p_value,
      Method       = "bootstrap",
      N.Boot       = length(boot_vec),
      stringsAsFactors = FALSE
    )
  } else{
    # Return result
    result <- data.frame(
      Test    = "Henze-Zirkler",
      Statistic = hz_stat,
      p.value = p_value,
      Method       = "asymptotic",
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}
