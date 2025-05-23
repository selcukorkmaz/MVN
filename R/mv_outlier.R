#' Identify Multivariate Outliers via Robust Mahalanobis Distances
#'
#' Computes robust Mahalanobis distances for multivariate data using Minimum Covariance Determinant (MCD),
#' flags outliers by either a chi-square quantile or an adjusted cutoff, and optionally displays a QQ-plot.
#'
#' @param data A numeric matrix or data frame with observations in rows and variables in columns (p ≥ 2).
#' @param qqplot Logical; if TRUE, produces a Chi-Square QQ-plot of robust distances. Default is TRUE.
#' @param alpha Numeric; significance level for the adjusted cutoff method. Ignored if method="quan". Default is 0.05.
#' @param method Character; outlier detection method, either "quan" (chi-square 97.5% cutoff) or "adj" (adjusted cutoff via arw_adjustment). Default is "quan".
#' @param label Logical; if TRUE and qqplot=TRUE, labels identified outliers on the QQ-plot. Default is TRUE.
#' @param title Character; title title for QQ-plot. Default is NULL.
#' @return A list with two elements:
#'   - outlier: A data frame of all observations sorted by descending MD, with a logical Outlier flag.
#'   - newData: A subset of the original data with non-outliers only.
#' @importFrom stats complete.cases qchisq mahalanobis
#' @importFrom graphics plot abline legend text
#' @importFrom MASS cov.mcd
#' @importFrom dplyr arrange
#' @importFrom ggplot2 ggplot aes geom_point geom_text scale_color_manual
#'   scale_shape_manual geom_vline labs annotate theme_minimal theme element_text
#'   element_rect element_blank
#' @export
mv_outlier <- function(data,
                       outlier = TRUE,
                       qqplot = TRUE,
                       alpha = 0.05,
                       method = c("quan", "adj"),
                       label = TRUE,
                       title = "Chi-Square Q-Q Plot") {
  
  if (!is.data.frame(data) && !is.matrix(data))
    stop("Input must be one of classes \"data frame\" or \"matrix\"")
  if (dim(data)[2] < 2 || is.null(dim(data))) {
    stop("Number of variables must be equal or greater than 2")
  }
  
  data <- data[complete.cases(data), ]
  data <- as.data.frame(data)
  method <- match.arg(method)
  n <- nrow(data)
  p <- ncol(data)
  
  set.seed(123)
  covr <- cov.mcd(data, method = "mcd")
  mah <- mahalanobis(data, center = covr$center, cov = covr$cov)
  dname <- deparse(substitute(data))
  
  sortMah <- data.frame(Mahalanobis = mah)
  row.names(sortMah) <- row.names(data)
  sortMah <- sortMah[order(-sortMah$Mahalanobis), , drop = FALSE]
  
  out <- data.frame(
    Observation = rownames(sortMah),
    Mahalanobis.Distance = round(sortMah$Mahalanobis, 3),
    Outlier = NA
  )
  
  qq_outlier_plot <- NULL
  newData <- NULL
  
  if (method == "adj") {
    crt <- arw_adjustment(x = data, m0 = covr$center, c0 = covr$cov, alpha = 0.025)$cn
    out$Outlier <- ifelse(out$Mahalanobis.Distance > crt, "TRUE", "FALSE")
    
    if (qqplot) {
      df <- data.frame(
        distance = mah,
        chi2q = qchisq((rank(mah) - 0.5) / n, df = p),
        is_outlier = factor(mah > crt, levels = c(FALSE, TRUE)),
        label = rownames(data)
      )
      
      vline_layer <- if (any(df$is_outlier == "TRUE")) {
        geom_vline(xintercept = crt, linetype = "dashed", linewidth = 1, color = "#003049")
      } else NULL
      
      text_layer <- if (label && any(df$is_outlier == "TRUE")) {
        geom_text(
          data = subset(df, is_outlier == "TRUE"),
          aes(label = label),
          nudge_x = 0.05 * max(df$distance),
          size = 3,
          color = "#D62828"
        )
      } else NULL
      
      annotate_layer <- if (max(df$distance) >= crt) {
        annotate(
          "text",
          x = crt,
          y = max(df$chi2q) * 0.05,
          label = paste0("Cut-off: ", round(crt, 3)),
          angle = 90, vjust = -0.5, hjust = 0,
          color = "#003049", size = 3
        )
      } else NULL
      
      qq_outlier_plot <- ggplot(df, aes(x = distance, y = chi2q, color = is_outlier, shape = is_outlier)) +
        geom_point(size = 3, alpha = 0.7) +
        vline_layer +
        text_layer +
        annotate_layer +
        scale_color_manual(values = c("FALSE" = "#4C4C4C", "TRUE" = "#D62828"),
                           labels = c("Non-outlier", "Outlier")) +
        scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                           labels = c("Non-outlier", "Outlier")) +
        labs(
          title = title,
          x = "Robust Squared Mahalanobis Distance",
          y = "Chi-Square Quantile",
          color = NULL, shape = NULL
        ) +
        theme_minimal(base_family = "sans") +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 11),
          legend.position = c(0.85, 0.15),
          legend.background = element_rect(fill = "transparent"),
          panel.grid.minor = element_blank()
        )
      
    }
    
    non_outlier_obs <- out$Observation[out$Outlier == "FALSE"]
    newData <- data[rownames(data) %in% non_outlier_obs, , drop = FALSE]
    newData <- newData[order(match(rownames(newData), rownames(data))), , drop = FALSE]
  }
  
  if (method == "quan") {
    chiSq <- qchisq(0.975, p)
    out$Outlier <- ifelse(out$Mahalanobis.Distance > chiSq, "TRUE", "FALSE")
    
    if (qqplot) {
      df2 <- data.frame(
        distance = mah,
        chi2q = qchisq((rank(mah) - 0.5) / n, df = p),
        is_outlier = factor(mah > chiSq, levels = c(FALSE, TRUE)),
        label = rownames(data)
      )
      
      text_layer <- if (label && any(df2$is_outlier == "TRUE")) {
        geom_text(
          data = subset(df2, is_outlier == "TRUE"),
          aes(label = label),
          nudge_x = 0.05 * max(df2$distance),
          size = 3,
          color = "#D62828"
        )
      } else NULL
      
      annotate_layer <- if (max(df2$distance) >= chiSq) {
        annotate(
          "text",
          x = chiSq,
          y = max(df2$chi2q) * 0.05,
          label = paste0("Cut-off: ", round(chiSq, 3)),
          angle = 90, vjust = -0.5, hjust = 0,
          color = "#D62828", size = 3
        )
      } else NULL
      
      qq_outlier_plot <- ggplot(df2, aes(x = distance, y = chi2q, color = is_outlier, shape = is_outlier)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_vline(xintercept = chiSq, linetype = "dashed", size = 1, color = "#D62828") +
        text_layer +
        annotate_layer +
        scale_color_manual(values = c("FALSE" = "#4C4C4C", "TRUE" = "#D62828"),
                           labels = c("Non-outlier", "Outlier")) +
        scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                           labels = c("Non-outlier", "Outlier")) +
        labs(
          title = title,
          x = "Robust Squared Mahalanobis Distance",
          y = "Chi-Square Quantile",
          color = NULL, shape = NULL
        ) +
        theme_minimal(base_family = "sans") +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 11),
          legend.position = c(0.85, 0.15),
          legend.background = element_rect(fill = "transparent"),
          panel.grid.minor = element_blank()
        )
    }
    
    non_outlier_obs <- out$Observation[out$Outlier == "FALSE"]
    newData <- data[rownames(data) %in% non_outlier_obs, , drop = FALSE]
    newData <- newData[order(match(rownames(newData), rownames(data))), , drop = FALSE]
  }
  
  result <- list()
  if (outlier) result$outlier <- out
  if (qqplot) result$qq_outlier_plot <- qq_outlier_plot
  if (!is.null(newData)) result$newData <- newData
  
  return(result)
}
