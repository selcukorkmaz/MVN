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
#' @param position Integer (1–4); text position for labels on QQ-plot. Default is 3.
#' @param offset Numeric; text offset for labels on QQ-plot. Default is 0.5.
#' @param main Character; main title for QQ-plot. Default is NULL.
#' @return A list with two elements:
#'   \describe{
#'     \item{outlier}{A data frame of all observations sorted by descending MD, with a logical Outlier flag.}
#'     \item{newData}{A subset of the original data with non-outliers only.}
#'   }
#' @importFrom stats complete.cases qchisq mahalanobis
#' @importFrom graphics plot abline legend text
#' @importFrom MASS cov.mcd
#' @export
mv_outlier <- function (data,
                       qqplot = TRUE,
                       alpha = 0.05,
                       method = c("quan", "adj"),
                       label = TRUE,
                       position = NULL,
                       offset = 0.5,
                       main)
{
  if (!is.data.frame(data) && !is.matrix(data))
    stop("Input must be one of classes \"data frame\" or \"matrix\"")
  if (dim(data)[2] < 2 || is.null(dim(data))) {
    stop("number of variables must be equal or greater than 2")
  }
  data = data[complete.cases(data), ]
  dataframe = as.data.frame(data)
  dname <- deparse(substitute(data))
  method <- match.arg(method)
  n <- dim(data)[1]
  p <- dim(data)[2]
  set.seed(123)
  covr <- cov.mcd(data, method = "mcd")
  mah <- mahalanobis(data,
                     center = covr$center,
                     cov = covr$cov)
  d <- mah
  sortMah <- data.frame(sort(mah, decreasing = TRUE))
  out <- cbind(rownames(sortMah), round(sortMah, 3), NA)
  colnames(out) <- c("Observation", "Mahalanobis Distance", "Outlier")
  if (method == "adj") {
    crt <- arw_adjustment(
      x = data,
      m0 = covr$center,
      c0 = covr$cov,
      alpha = 0.025
    )$cn
    for (i in 1:n) {
      {
        if (sortMah[i, ] > crt) {
          out[i, 3] <- "TRUE"
        }
        else {
          out[i, 3] <- "FALSE"
        }
      }
    }
    if (qqplot) {
      
      df <- data.frame(
        distance   = mah,
        chi2q      = qchisq((rank(mah) - 0.5) / n, df = p),
        is_outlier = factor(mah > crt, levels = c(FALSE, TRUE))
      )
      df$label <- rownames(df)  # for labeling
      
      qq_outlier_plot <- ggplot(df, aes(x = distance, y = chi2q, color = is_outlier)) +
        geom_point(aes(shape = is_outlier), size = 3, alpha = 0.7) +
        scale_color_manual(
          values = c("FALSE" = "#4C4C4C", "TRUE" = "#D62828"),
          labels = c("Non-outlier", "Outlier")
        ) +
        scale_shape_manual(
          values = c("FALSE" = 16, "TRUE" = 17),
          labels = c("Non-outlier", "Outlier")
        ) +
        
        {if (any(as.logical(as.character(df$is_outlier)))) {
        geom_vline(xintercept = crt, linetype = "dashed", size = 1, color = "#003049") }}+
        # Title & axis labels
        labs(
          title = main,
          x     = "Robust Squared Mahalanobis Distance",
          y     = "Chi-Square Quantile",
          color = NULL,   # drops legend title
          shape = NULL
        ) +
        # Optional outlier labels
        { if (label && any(df$is_outlier == "TRUE")) {
          geom_text(
            data    = subset(df, is_outlier == "TRUE"),
            aes(label = label),
            nudge_x = 0.05 * max(df$distance),
            size    = 3,
            color   = "#D62828"
          )
        }
        } +
        # Quantile annotation
        { if (max(df$distance) >= crt) {
          annotate(
            "text",
            x     = crt,
            y     = max(df$chi2q) * 0.05,
            label = paste0("Cut-off: ", round(crt, 3)),
            angle = 90, vjust = -0.5, hjust = 0,
            color = "#003049",
            size  = 3
          )
        }
        } +
        # A clean, modern look
        theme_minimal(base_family = "sans") +
        theme(
          plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title       = element_text(size = 13),
          axis.text        = element_text(size = 11),
          legend.position  = c(0.85, 0.15),
          legend.background = element_rect(fill = "transparent"),
          panel.grid.minor = element_blank()
        )

      
    }
    newData <- out[out$Outlier %in% "FALSE", ]
    ind <- sort(row.names(newData))
    newData <- data[ind, ]
  }
  if (method == "quan") {
    chiSq <- qchisq(0.975, p)
    for (i in 1:n) {
      {
        if (sortMah[i, ] > chiSq) {
          out[i, 3] <- "TRUE"
        }
        else {
          out[i, 3] <- "FALSE"
        }
      }
    }
    if (qqplot) {

      df2 <- data.frame(
        distance   = mah,
        chi2q      = qchisq((rank(mah) - 0.5) / n, df = p),
        is_outlier = factor(mah > chiSq, levels = c(FALSE, TRUE))
      )
      df2$label <- rownames(out)   # for labeling outliers
      
      qq_outlier_plot <- ggplot(df2, aes(x = distance, y = chi2q, color = is_outlier, shape = is_outlier)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_manual(
          values = c("FALSE" = "#4C4C4C", "TRUE" = "#D62828"),
          labels = c("Non-outlier", "Outlier")
        ) +
        scale_shape_manual(
          values = c("FALSE" = 16, "TRUE" = 17),
          labels = c("Non-outlier", "Outlier")
        ) +
        geom_vline(xintercept = chiSq, linetype = "dashed", size = 1, color = "#D62828") +
        labs(
          title = main,
          x     = "Robust Squared Mahalanobis Distance",
          y     = "Chi-Square Quantile",
          color = NULL,
          shape = NULL
        ) +
        # Outlier labels
        { if (label && any(df2$is_outlier == "TRUE")) {
          geom_text(
            data    = subset(df2, is_outlier == "TRUE"),
            aes(label = label),
            nudge_x = 0.05 * max(df2$distance),
            size    = 3,
            color   = "#D62828"
          )
        }
        } +
        # Chi-square cutoff annotation
        { if (max(df2$distance) >= chiSq) {
          annotate(
            "text",
            x     = chiSq,
            y     = max(df2$chi2q) * 0.05,
            label = paste0("Cut-off: ", round(chiSq, 3)),
            angle = 90,
            vjust = -0.5,
            hjust = 0,
            color = "#D62828",
            size  = 3
          )
        }
        } +
        theme_minimal(base_family = "sans") +
        theme(
          plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title        = element_text(size = 13),
          axis.text         = element_text(size = 11),
          legend.position   = c(0.85, 0.15),
          legend.background = element_rect(fill = "transparent"),
          panel.grid.minor  = element_blank()
        )
      
    }
    newData <- out[out$Outlier %in% "FALSE", ]
    ind <- sort(row.names(newData))
    newData <- data[ind, ]
  }
  result <- list(
    qq_outlier_plot = qq_outlier_plot,  # your ggplot object
    outlier             = out,                    # a data.frame
    newData             = newData                 # another data.frame
  )
  return(result)
  
}