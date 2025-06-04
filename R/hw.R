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
#' @param bootstrap Logical; if \code{TRUE}, compute p-value via bootstrap
#'   resampling. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates used when
#'   \code{bootstrap = TRUE}. Default is \code{1000}.
#' @param cores Integer; number of cores for parallel computation when
#'   \code{bootstrap = TRUE}. Default is 1.
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
hw <- function(data, use_population = TRUE, tol = 1e-25,
               bootstrap = FALSE, B = 1000, cores = 1) {
  # Convert to data frame and drop non-numeric columns
  df <- as.data.frame(data)
  numeric_cols <- sapply(df, is.numeric)
  if (!all(numeric_cols)) {
    dropped <- names(df)[!numeric_cols]
    warning(
      "Dropping non-numeric columns: ",
      paste(dropped, collapse = ", ")
    )
    df <- df[, numeric_cols, drop = FALSE]
  }
  if (ncol(df) < 2) {
    stop("Need at least two numeric variables for Henze-Wagner test.")
  }
  
  # Handle missing values
  keep <- stats::complete.cases(df)
  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", n_dropped))
  }
  df <- df[keep, , drop = FALSE]
  
  # Convert to matrix and dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  # Center data
  x_centered <- scale(x, center = TRUE, scale = FALSE)
  
  # Compute covariance
  if (use_population) {
    S <- ((n - 1) / n) * stats::cov(x_centered)
  } else {
    S <- stats::cov(x_centered)
  }
  
  # Invert covariance (use Moore-Penrose if singular)
  invS <- tryCatch(
    solve(S, tol = tol),
    error = function(e) MASS::ginv(S)
  )
  
  # Compute distance matrices
  D <- x_centered %*% invS %*% t(x_centered)
  Dj <- diag(D)
  Djk <- outer(Dj, Dj, "+") - 2 * D
  
  # Smoothing parameter b fixed to 1 for HW
  b <- 1
  
  # Compute Henze-Wagner statistic (observed)
  part1 <- sum(exp(-(b^2) / 2 * Djk)) / (n^2)
  part2 <- 2 * (1 + b^2)^(-p / 2) * sum(exp(
    -(b^2) / (2 * (1 + b^2)) * Dj
  )) / n
  hw_stat <- n * (part1 - part2 + (1 + 2 * b^2)^(-p / 2))
  
  # Log-normal approximation parameters for p-value
  a   <- 1 + 2 * b^2
  wb  <- (1 + b^2) * (1 + 3 * b^2)
  mu  <- 1 - a^(-p / 2) * (
    1 +
      (p * b^2) / a +
      (p * (p + 2) * b^4) / (2 * a^2)
  )
  si2 <- 2 * (1 + 4 * b^2)^(-p / 2) +
    2 * a^(-p) * (
      1 +
        (2 * p * b^4) / a^2 +
        (3 * p * (p + 2) * b^8) / (4 * a^4)
    ) -
    4 * wb^(-p / 2) * (
      1 +
        (3 * p * b^4) / (2 * wb) +
        (p * (p + 2) * b^8) / (2 * wb^2)
    )
  pmu <- log(sqrt(mu^4 / (si2 + mu^2)))
  psi <- sqrt(log((si2 + mu^2) / mu^2))
  
  # Asymptotic p-value (log-normal)
  p_value <- stats::plnorm(hw_stat, pmu, psi, lower.tail = FALSE)
  
  # Default method label and N.Boot counter
  method <- "approximation"
  n_boot_used <- NA_integer_
  
  if (bootstrap && B > 0) {
    # Estimate parametric-null parameters: mean and covariance of x
    mu_hat    <- colMeans(x)
    Sigma_hat <- stats::cov(x)
    
    # Define bootstrap function (parametric MVN)
    boot_fun <- function(i) {
      # 1) Draw n observations from MVN(mu_hat, Sigma_hat)
      xb <- MASS::mvrnorm(n = n, mu = mu_hat, Sigma = Sigma_hat)
      
      # 2) Center bootstrap sample
      xb_c <- scale(xb, center = TRUE, scale = FALSE)
      
      # 3) Compute covariance of xb_c
      Sb <- if (use_population) {
        ((n - 1) / n) * stats::cov(xb_c)
      } else {
        stats::cov(xb_c)
      }
      
      # 4) Invert Sb (use ginv if singular)
      invSb <- tryCatch(
        solve(Sb, tol = tol),
        error = function(e) MASS::ginv(Sb)
      )
      
      # 5) Compute distances for bootstrap
      Db  <- xb_c %*% invSb %*% t(xb_c)
      Djb <- diag(Db)
      Djkb <- outer(Djb, Djb, "+") - 2 * Db
      
      # 6) Compute HW statistic for bootstrap replicate
      part1b <- sum(exp(-(b^2) / 2 * Djkb)) / (n^2)
      part2b <- 2 * (1 + b^2)^(-p / 2) * sum(exp(
        -(b^2) / (2 * (1 + b^2)) * Djb
      )) / n
      hw_b <- n * (part1b - part2b + (1 + 2 * b^2)^(-p / 2))
      return(hw_b)
    }
    
    # Run B replicates (parallel if cores > 1)
    if (cores > 1) {
      boot_stats <- parallel::mclapply(
        seq_len(B),
        boot_fun,
        mc.cores = cores
      )
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }
    
    # Collect and discard any NA replicates
    boot_vec <- unlist(boot_stats)
    boot_vec <- boot_vec[!is.na(boot_vec)]
    n_boot_used <- length(boot_vec)
    
    if (n_boot_used > 0) {
      # Parametric-bootstrap p-value
      p_value <- mean(boot_vec >= hw_stat)
      method <- "bootstrap"
    } else {
      p_value <- NA_real_
    }
  }
  
  if (bootstrap) {
    result <- data.frame(
      Test         = "Henze-Wagner",
      Statistic    = hw_stat,
      p.value  = p_value,
      Method       = "bootstrap",
      N.Boot       = n_boot_used,
      stringsAsFactors = FALSE
    )
  } else{
    # Return result
    result <- data.frame(
      Test    = "Henze-Wagner",
      Statistic = hw_stat,
      p.value = p_value,
      Method       = "asymptotic",
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}
