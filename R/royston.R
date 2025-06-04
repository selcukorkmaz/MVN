#' Royston's Multivariate Normality Test
#'
#' Performs Royston’s test for multivariate normality by combining univariate W-statistics
#' (Shapiro–Wilk or Shapiro–Francia) across variables and adjusting for the correlation structure.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param tol Numeric tolerance passed to \code{\link[base]{solve}} when inverting the covariance matrix. Default is \code{1e-25}.
#' @param bootstrap Logical; if \code{TRUE}, compute p-value via bootstrap
#'   resampling. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates used when
#'   \code{bootstrap = TRUE}. Default is \code{1000}.
#' @param cores Integer; number of cores for parallel computation when
#'   \code{bootstrap = TRUE}. Default is 1.
#'
#' @return A data frame with one row containing the test name (\code{Test}), the Royston test statistic (\code{Statistic}),
#' and the associated p-value (\code{p.value}) from a chi-square approximation.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:4]
#' royston_result <- royston(data)
#' royston_result
#' }
#'
#' @importFrom stats complete.cases cov cor pchisq pnorm qnorm shapiro.test
#' @importFrom moments kurtosis
#' @importFrom nortest sf.test
#' @export
royston <- function(data, tol = 1e-25, bootstrap = FALSE, B = 1000, cores = 1) {
  # Convert and filter numeric columns
  df <- as.data.frame(data)
  num_cols <- vapply(df, is.numeric, logical(1))
  if (!all(num_cols)) {
    warning(
      "Dropping non-numeric columns: ",
      paste(names(df)[!num_cols], collapse = ", ")
    )
    df <- df[, num_cols, drop = FALSE]
  }
  if (ncol(df) < 2) {
    stop("Need at least two numeric variables for Royston's test.")
  }
  
  # Handle missing data
  keep <- stats::complete.cases(df)
  dropped <- sum(!keep)
  if (dropped > 0) {
    warning(sprintf("%d rows with missing values were removed.", dropped))
  }
  df <- df[keep, , drop = FALSE]
  
  # Dimensions and checks
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  if (n <= 3) stop("Sample size must be greater than 3.")
  if (n > 2000) stop("Sample size must be less than or equal to 2000.")
  
  # Center the data for covariance estimation
  x_centered <- scale(x, center = TRUE, scale = FALSE)
  
  # Preallocate vector of z-values (one per variable)
  z_vals <- numeric(p)
  
  # Compute univariate W-statistics and z-transform
  if (n >= 4 && n <= 11) {
    # Small sample adjustment
    g <- -2.273 + 0.459 * n
    m <- 0.544 -
      0.39978 * n +
      0.025054 * n^2 -
      0.0006714 * n^3
    s <- exp(
      1.3822 -
        0.77857 * n +
        0.062767 * n^2 -
        0.0020322 * n^3
    )
    
    for (i in seq_len(p)) {
      vec <- x[, i]
      w <- if (moments::kurtosis(vec) > 3) {
        nortest::sf.test(vec)$statistic
      } else {
        stats::shapiro.test(vec)$statistic
      }
      z_vals[i] <- (-log(g - log(1 - w)) - m) / s
    }
  } else if (n >= 12 && n <= 2000) {
    # Large sample adjustment
    lx <- log(n)
    m  <- -1.5861 -
      0.31082 * lx -
      0.083751 * lx^2 +
      0.0038915 * lx^3
    s  <- exp(
      -0.4803 -
        0.082676 * lx +
        0.0030302 * lx^2
    )
    
    for (i in seq_len(p)) {
      vec <- x[, i]
      w <- if (moments::kurtosis(vec) > 3) {
        nortest::sf.test(vec)$statistic
      } else {
        stats::shapiro.test(vec)$statistic
      }
      z_vals[i] <- (log(1 - w) - m) / s
    }
  }
  
  # Adjust for correlation among variables
  u  <- 0.715
  v  <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
  l  <- 5
  C  <- stats::cor(x)
  NC <- (C^l) * (1 - (u * (1 - C)^u) / v)
  T  <- sum(NC) - p
  mC <- T / (p^2 - p)
  edf <- p / (1 + (p - 1) * mC)
  
  # Compute observed Royston H statistic
  H_stat <- (edf * sum((stats::qnorm(stats::pnorm(-z_vals) / 2))^2)) / p
  
  # Asymptotic p-value
  p_val <- stats::pchisq(H_stat, df = edf, lower.tail = FALSE)
  
  # Prepare default method labels
  method <- "asymptotic"
  n_boot_used <- NA_integer_
  
  if (bootstrap && B > 0) {
    # Estimate parametric-null parameters (mean and covariance)
    mu_hat    <- colMeans(x)
    Sigma_hat <- stats::cov(x_centered)
    
    # Precompute constants
    if (n >= 4 && n <= 11) {
      g_pb <- -2.273 + 0.459 * n
      m_pb <- 0.544 -
        0.39978 * n +
        0.025054 * n^2 -
        0.0006714 * n^3
      s_pb <- exp(
        1.3822 -
          0.77857 * n +
          0.062767 * n^2 -
          0.0020322 * n^3
      )
      small_sample <- TRUE
    } else {
      lx <- log(n)
      m_pb <- -1.5861 -
        0.31082 * lx -
        0.083751 * lx^2 +
        0.0038915 * lx^3
      s_pb <- exp(
        -0.4803 -
          0.082676 * lx +
          0.0030302 * lx^2
      )
      small_sample <- FALSE
    }
    u_pb <- 0.715
    v_pb <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
    l_pb <- 5
    
    # Define a function to compute H for one parametric bootstrap replicate
    boot_fun <- function(i) {
      # 1) Simulate n observations from N(mu_hat, Sigma_hat)
      xb <- MASS::mvrnorm(n = n, mu = mu_hat, Sigma = Sigma_hat)
      
      # 2) Center that bootstrap sample
      xb_c <- scale(xb, center = TRUE, scale = FALSE)
      
      # 3) Recompute correlation matrix for xb
      Cb <- stats::cor(xb)
      
      # 4) Compute z-values for each variable in xb
      z_b <- numeric(p)
      if (small_sample) {
        for (j in seq_len(p)) {
          vec_b <- xb[, j]
          w_b <- if (moments::kurtosis(vec_b) > 3) {
            nortest::sf.test(vec_b)$statistic
          } else {
            stats::shapiro.test(vec_b)$statistic
          }
          z_b[j] <- (-log(g_pb - log(1 - w_b)) - m_pb) / s_pb
        }
      } else {
        for (j in seq_len(p)) {
          vec_b <- xb[, j]
          w_b <- if (moments::kurtosis(vec_b) > 3) {
            nortest::sf.test(vec_b)$statistic
          } else {
            stats::shapiro.test(vec_b)$statistic
          }
          z_b[j] <- (log(1 - w_b) - m_pb) / s_pb
        }
      }
      
      # 5) Compute the NC matrix adjustment for correlation
      NCb <- (Cb^l_pb) * (1 - (u_pb * (1 - Cb)^u_pb) / v_pb)
      Tb  <- sum(NCb) - p
      mCb <- Tb / (p^2 - p)
      edfb <- p / (1 + (p - 1) * mCb)
      
      # 6) Compute H for this replicate
      H_b <- (edfb * sum((stats::qnorm(stats::pnorm(-z_b) / 2))^2)) / p
      return(H_b)
    }
    
    # Run bootstrap replicates (parallel if requested)
    if (cores > 1) {
      boot_stats <- parallel::mclapply(
        seq_len(B),
        boot_fun,
        mc.cores = cores
      )
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }
    
    # Collect and remove any NA replicates
    boot_vec <- unlist(boot_stats)
    boot_vec <- boot_vec[!is.na(boot_vec)]
    n_boot_used <- length(boot_vec)
    
    if (n_boot_used > 0) {
      # Bootstrap p-value:
      p_val <- mean(boot_vec >= H_stat)
      method <- "parametric bootstrap"
    } else {
      p_val <- NA_real_
    }
  }
  
  if (bootstrap) {
    result <- data.frame(
      Test         = "Royston",
      Statistic    = H_stat,
      p.value  = p_val,
      Method       = "bootstrap",
      N.Boot       = n_boot_used,
      stringsAsFactors = FALSE
    )
  } else{
    result <- data.frame(
      Test    = "Royston",
      Statistic = H_stat,
      p.value = p_val,
      Method       = "asymptotic",
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
  return(result)
}

