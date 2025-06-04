#' Doornik-Hansen Test for Multivariate Normality
#'
#' Performs the Doornik–Hansen omnibus test by transforming the data to approximate normality
#' and combining skewness and kurtosis measures to test for multivariate normality.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns.
#' @param bootstrap Logical; if \code{TRUE}, compute p-value via bootstrap
#'   resampling. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates used when
#'   \code{bootstrap = TRUE}. Default is \code{1000}.
#' @param cores Integer; number of cores for parallel computation when
#'   \code{bootstrap = TRUE}. Default is 1.
#'
#' @return A data frame with one row containing the following columns:
#' \code{Test}, the name of the test ("Doornik-Hansen");
#' \code{Statistic}, the value of the test statistic;
#' \code{df}, the degrees of freedom;
#' and \code{p.value}, the p-value from a chi-square approximation.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:2]
#' dh_result <- doornik_hansen(data)
#' dh_result
#' }
#'
#' @importFrom stats complete.cases cov pchisq sd
#' @importFrom moments skewness kurtosis
#' @export
doornik_hansen <- function(data, bootstrap = FALSE, B = 1000, cores = 1) {
  # Convert to data frame and drop non-numeric columns
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
    stop("Need at least two numeric variables for Doornik-Hansen test.")
  }
  
  # Handle missing values
  keep <- stats::complete.cases(df)
  dropped <- sum(!keep)
  if (dropped > 0) {
    warning(sprintf("%d rows with missing values removed.", dropped))
  }
  df <- df[keep, , drop = FALSE]
  
  # Dimensions
  x <- as.matrix(df)
  n <- nrow(x)
  p <- ncol(x)
  
  # Compute sample mean and covariance once
  mu_hat    <- colMeans(x)
  Sigma_hat <- stats::cov(x)
  
  # ---- Original asymptotic Doornik-Hansen statistic ----
  # 1) Compute covariance matrix S from original data
  S <- Sigma_hat
  
  # 2) Decorrelation and standardization
  sdv <- sqrt(diag(S))
  V   <- diag(1 / sdv)
  C   <- V %*% S %*% V
  eig <- eigen(C)
  H   <- eig$vectors
  L   <- diag(eig$values^(-0.5))
  
  # 3) Center original data as a data.frame
  Xc <- t(df - sapply(df, mean))
  
  # 4) Transform: yi = H L t(H) V Xc
  yi <- H %*% L %*% t(H) %*% V %*% Xc
  
  # 5) Univariate skewness and kurtosis for transformed data
  B1 <- apply(yi, 1, moments::skewness)
  B2 <- apply(yi, 1, moments::kurtosis)
  
  # 6) Parameters for skewness component Z2
  del1  <- (n - 3) * (n + 1) * (n^2 + 15 * n - 4)
  a1    <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + 27 * n - 70)) / (6 * del1)
  c1    <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + 2 * n - 5))   / (6 * del1)
  k1    <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + 11 * n - 313)) / (12 * del1)
  alpha <- a1 + c1 * B1^2
  chi   <- 2 * k1 * (B2 - 1 - B1^2)
  Z2    <- sqrt(9 * alpha) * (((chi / (2 * alpha))^(1/3) - 1 + 1 / (9 * alpha)))
  
  # 7) Parameters for skewness component Z1
  beta <- (3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3)) /
    ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  w2   <- -1 + sqrt(2 * (beta - 1))
  y    <- B1 * sqrt(((w2 - 1) / 2) * ((n + 1) * (n + 3) / (6 * (n - 2))))
  del2 <- 1 / sqrt(log(sqrt(w2)))
  Z1   <- del2 * log(y + sqrt(y^2 + 1))
  
  # 8) Combined Doornik-Hansen statistic (observed)
  E_obs <- sum(Z1^2) + sum(Z2^2)
  df_E  <- 2 * p
  p_asym <- stats::pchisq(E_obs, df = df_E, lower.tail = FALSE)
  
  # Prepare default output values
  method <- "asymptotic"
  n_boot_used <- NA_integer_
  p_value <- p_asym
  
  # ---- Parametric Bootstrap (if requested) ----
  if (bootstrap && B > 0) {
    # We'll reuse these asymptotic constants inside each replicate
    del1_pb <- del1
    a1_pb   <- a1
    c1_pb   <- c1
    k1_pb   <- k1
    beta_pb <- beta
    w2_pb   <- w2
    del2_pb <- del2
    
    boot_fun <- function(i) {
      # 1) Simulate n observations from N(mu_hat, Sigma_hat)
      xb <- MASS::mvrnorm(n = n, mu = mu_hat, Sigma = Sigma_hat)
      dfb <- as.data.frame(xb)
      
      # 2) Compute covariance matrix Sb for the replicate
      Sb <- stats::cov(xb)
      
      # 3) Decorrelation and standardization for replicate
      sdv_b <- sqrt(diag(Sb))
      Vb    <- diag(1 / sdv_b)
      Cb    <- Vb %*% Sb %*% Vb
      eig_b <- eigen(Cb)
      Hb    <- eig_b$vectors
      Lb    <- diag(eig_b$values^(-0.5))
      
      # 4) Center replicate data as data.frame
      Xc_b <- t(dfb - sapply(dfb, mean))
      
      # 5) Transform: yi_b = Hb Lb t(Hb) Vb Xc_b
      yi_b <- Hb %*% Lb %*% t(Hb) %*% Vb %*% Xc_b
      
      # 6) Univariate skewness and kurtosis for yi_b
      B1_b <- apply(yi_b, 1, moments::skewness)
      B2_b <- apply(yi_b, 1, moments::kurtosis)
      
      # 7) Compute Z2_b
      alpha_b <- a1_pb + c1_pb * B1_b^2
      chi_b   <- 2 * k1_pb * (B2_b - 1 - B1_b^2)
      Z2_b    <- sqrt(9 * alpha_b) * (((chi_b / (2 * alpha_b))^(1/3)
                                       - 1 + 1 / (9 * alpha_b)))
      
      # 8) Compute Z1_b
      y_b <- B1_b * sqrt(((w2_pb - 1) / 2) * ((n + 1) * (n + 3) / (6 * (n - 2))))
      Z1_b <- del2_pb * log(y_b + sqrt(y_b^2 + 1))
      
      # 9) Combined statistic for replicate
      E_b <- sum(Z1_b^2) + sum(Z2_b^2)
      return(E_b)
    }
    
    # Run B replicates (parallel if cores > 1)
    if (cores > 1) {
      boot_stats <- parallel::mclapply(seq_len(B), boot_fun, mc.cores = cores)
    } else {
      boot_stats <- lapply(seq_len(B), boot_fun)
    }
    
    # Collect and discard any NA replicates
    boot_vec <- unlist(boot_stats)
    boot_vec <- boot_vec[!is.na(boot_vec)]
    n_boot_used <- length(boot_vec)
    
    if (n_boot_used > 0) {
      # Parametric-bootstrap p-value
      p_value <- mean(boot_vec >= E_obs)
      method  <- "parametric bootstrap"
    } else {
      p_value <- NA_real_
    }
  }
  
  # Assemble final result
  if (bootstrap) {
    result <- data.frame(
      Test         = "Doornik-Hansen",
      Statistic = E_obs,
      df        = df_E,
      p.value  = p_value,
      Method       = "bootstrap",
      N.Boot       = n_boot_used,
      stringsAsFactors = FALSE
    )
  } else{
    # Return result
    result <- data.frame(
      Test    = "Doornik-Hansen",
      Statistic = E_obs,
      df        = df_E,
      p.value = p_value,
      Method       = "asymptotic",
      stringsAsFactors = FALSE
    )
  }
  return(result)
}
