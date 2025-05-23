#' Plot Multivariate Normal Diagnostics and Bivariate Kernel Density
#'
#' Generates either a Mahalanobis Q-Q plot, an interactive 3D kernel density surface plot,
#' or a 2D kernel density contour plot for exactly two numeric variables. The function is intended
#' for assessing multivariate normality or exploring the bivariate distribution of the input data.
#'
#' @param data A numeric vector, matrix, or data frame. Non-numeric columns are dropped
#' with a warning; incomplete rows are removed. The input must contain exactly two numeric variables.
#' @param type Character string specifying the type of plot to generate.
#' Must be one of \code{"qq"} (Mahalanobis Q-Q plot), \code{"persp"} (3D KDE surface),
#' or \code{"contour"} (2D KDE contour). Default is \code{"qq"}.
#' @param tol Numeric tolerance for matrix inversion passed to \code{solve()}. Default is \code{1e-25}.
#' @param use_population Logical; if \code{TRUE}, uses the population covariance estimator \eqn{\frac{n-1}{n} \times \Sigma}; otherwise uses the sample covariance. Default is \code{TRUE}.
#'
#' @return If \code{type = "qq"}, returns a \code{ggplot2} object representing a Mahalanobis Q-Q plot.
#' If \code{type = "persp"} or \code{"contour"}, returns an interactive \code{plotly} widget
#' displaying the KDE surface or contour, respectively.
#'
#' @examples
#' \dontrun{
#' library(MASS)
#' data(iris)
#'
#' # Mahalanobis Q-Q plot
#' multivariate_diagnostic_plot(iris[, 1:2], type = "qq")
#'
#' # 3D KDE surface
#' multivariate_diagnostic_plot(iris[, 1:2], type = "persp")
#'
#' # 2D KDE contour
#' multivariate_diagnostic_plot(iris[, 1:2], type = "contour")
#' }
#'
#' @importFrom graphics persp contour
#' @importFrom stats complete.cases cov qchisq
#' @importFrom MASS kde2d
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_minimal theme element_text element_line element_rect margin
#' @importFrom plotly plot_ly layout
#' @importFrom viridis viridis
#' @export

multivariate_diagnostic_plot <- function(data,
                     type = c("qq", "persp", "contour"),
                     tol = 1e-25,
                     use_population = TRUE) {
  # Coerce and validate data
  df <- as.data.frame(data)
  nums <- vapply(df, is.numeric, logical(1))
  if (!all(nums)) {
    warning("Dropping non-numeric columns: ",
            paste(names(df)[!nums], collapse = ", "))
    df <- df[, nums, drop = FALSE]
  }

  # Remove missing values
  keep <- stats::complete.cases(df)
  if (sum(!keep) > 0) {
    warning(sprintf("Removed %d incomplete rows.", sum(!keep)))
    df <- df[keep, , drop = FALSE]
  }
  
  # Compute kernel density estimate
  kde <- MASS::kde2d(df[[1]], df[[2]], n = 100)
  
  type <- match.arg(type)
  
  if (type == "qq") {
    
    # 1. Compute distances & quantiles (same as before)
    n <- nrow(df)
    p <- ncol(df)
    
    S <- if (use_population) {
      ((n - 1) / n) * cov(df)
    } else {
      cov(df)
    }
    
    dif   <- scale(df, scale = FALSE)
    d     <- diag(dif %*% solve(S, tol = tol) %*% t(dif))
    chi2q <- qchisq((rank(d) - 0.5) / n, df = p)
    
    # 2. Tidy data
    plot_df <- data.frame(
      distance = d,
      chi2q    = chi2q
    )
    
    # 3. Prettier ggplot
    ggplot(plot_df, aes(x = distance, y = chi2q)) +
      geom_point(color = "#2a9d8f", size = 3, alpha = 0.7) +           # teal points
      geom_abline(intercept = 0, slope = 1,
                  linetype = "dashed", linewidth = 1, color = "#264653") +  # slate reference line
      labs(
        title    = "Mahalanobis Q-Q Plot",
        subtitle = "Squared distances vs. chi-square quantiles",
        x        = "Squared Mahalanobis Distance",
        y        = "Chi-Square Quantile"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title       = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
        axis.title       = element_text(size = 12),
        axis.text        = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        panel.background = element_rect(fill = "white", colour = NA)
      )
    
    
  }
  
  else if (type == "persp") {
    
    # Build the plotly surface
    fig <- plot_ly(
      x      = ~kde$x,
      y      = ~kde$y,
      z      = ~kde$z,
      type   = "surface",
      colors = viridis(100),
      showscale = TRUE,
      colorbar   = list(title = ""),        # ⬅ remove legend title
      
    ) %>%
      layout(
        title = "3D KDE Surface",
        scene = list(
          xaxis = list(title = names(df)[1]),
          yaxis = list(title = names(df)[2]),
          zaxis = list(title = "Density"),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 0.7)
          ),
          aspectmode = "auto"
        ),
        margin = list(l = 0, r = 0, b = 0, t = 50)
      )
    
    # Display
    fig
    
  } 
  
  else if (type == "contour") {
  
    
    fig_contour <- plot_ly(
      x          = ~kde$x,
      y          = ~kde$y,
      z          = ~kde$z,
      type       = "contour",
      colorscale = viridis(20),
      showscale  = TRUE,
      colorbar   = list(title = ""),        # ⬅ remove legend title
      contours   = list(
        coloring   = "heatmap",
        showlabels = TRUE,
        labelfont  = list(size = 12, color = "white")
      ),
      hoverinfo  = "x+y+z"
    ) %>%
      layout(
        title = "Kernel Density Estimate (Contour)",
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = names(df)[2]),
        margin= list(l = 60, r = 20, t = 60, b = 60)
      )
    
    fig_contour
    
  }
  
}


