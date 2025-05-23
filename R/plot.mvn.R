#' Plot Diagnostics for Multivariate Normality Analysis
#'
#' Generates diagnostic plots for objects of class \code{mvn}, including multivariate Q-Q plots,
#' 3D or contour kernel density plots, univariate plots (e.g., Q-Q, histograms, boxplots),
#' and multivariate outlier detection plots. If a grouping variable (subset) was used in the
#' \code{\link{mvn}} function, plots will be generated separately for each group.
#'
#' @param x An object of class \code{mvn}, as returned by the \code{\link{mvn}} function.
#' @param ... Additional arguments passed to internal plotting functions:
#'   \code{diagnostic} (\code{"multivariate"}, \code{"univariate"}, \code{"outlier"}),
#'   \code{type} (e.g., \code{"qq"}, \code{"boxplot"}, \code{"persp"}),
#'   \code{interactive} (logical; use \pkg{plotly}), and
#'   \code{outlier_method} (\code{"adj"}, \code{"quan"}).
#'
#' @return This function is called for its side effect of producing plots. It does not return a value.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:4]
#' result <- mvn(data)
#'
#' plot(result, diagnostic = "multivariate", type = "qq")
#' plot(result, diagnostic = "univariate", type = "boxplot")
#' plot(result, diagnostic = "outlier", outlier_method = "adj")
#' }
#'
#' @importFrom ggplot2 ggtitle
#' @importFrom stringr str_to_upper str_to_title
#' @importFrom plotly layout
#' @method plot mvn
#' @export
plot.mvn <- function(x, ...) {
  args <- list(...)
  
  diagnostic <- match.arg(if (is.null(args$diagnostic)) "multivariate" else args$diagnostic,
                          c("multivariate", "univariate", "outlier"))
  type <- if (is.null(args$type)) NULL else args$type
  interactive <- if (is.null(args$interactive)) FALSE else args$interactive
  outlier_method <- if (is.null(args$outlier_method)) "adj" else args$outlier_method
  
  df = x$data
  
  # ---- MULTIVARIATE PLOT ----
  if (diagnostic == "multivariate") {
    
    if (is.null(x$subset)) {
      print(multivariate_diagnostic_plot(x$data, type = type))  # also needs print()
    } else {
      splitData <- split(x$data, x$data[[x$subset]])
      splitData <- lapply(splitData, function(df) df[, names(df) != x$subset])
      group_names <- names(splitData)
      
      if(type == "qq"){
        
        title = "Mahalanobis Q-Q plot for"
        
        invisible(
          mapply(
            function(df, group) {
              p <- multivariate_diagnostic_plot(df, type = type)
              p <- p + ggtitle(paste(title, group))  # modify title here
              print(p)
            },
            splitData,
            group_names
          )
        )
        
      }else{
        
        if(type == "persp"){
          
          title = "3D perspective plot for"
          
        }
        
        if(type == "contour"){
          
          title = "Contour plot for"
          
        }
        
        invisible(
          mapply(
            function(df, group) {
              p <- multivariate_diagnostic_plot(df, type = type)
              p <- plotly::layout(p, title = list(text = paste(title, group)))
              print(p)
            },
            splitData,
            group_names
          )
        )
        
        
      }
      
    }
    
    # return(invisible(NULL))  # nothing visible printed to console
    
  }
  
  # ---- UNIVARIATE PLOT ----
  if (diagnostic == "univariate") {
    if (is.null(x$subset)) {
      univariate_diagnostic_plot(x$data, type = type, interactive = interactive)
    } else{
      
      splitData = split(x$data, x$data[[x$subset]])
      splitData <- lapply(splitData, function(df) df[, names(df) != x$subset])
      group_names <- names(splitData)
      
      if(type == "qq"){
        
        plot_name <- paste0(stringr::str_to_upper(type)," plots")
        
      }
      
      else if(type == "scatter"){
        
        plot_name <- paste0(stringr::str_to_title(type)," plots")
        
      }else{
        
        plot_name <- paste0(stringr::str_to_title(type),"s")
        
      }
      
      invisible(
        mapply(
          function(df, group) {
            univariate_diagnostic_plot(df, type = type, title = paste(plot_name, "for", group), interactive = interactive)
          },
          splitData,
          group_names
        )
      )
      
      
      
    }
    
    return(invisible(NULL))  # nothing visible printed to console
    
  }
  
  # ---- OUTLIER PLOT ----
  if (diagnostic == "outlier") {
    
    if (outlier_method == "quan") {
      title = "Chi-Square Q-Q Plot"
      
    } else{
      title = "Adjusted Chi-Square Q-Q Plot"
      
    }
    
    
    if (is.null(x$subset)) {
      p <- mv_outlier(x$data, method = outlier_method, title = title, outlier = FALSE)
      invisible(print(p$qq_outlier_plot))
    } else {
      splitData <- split(x$data, x$data[[x$subset]])
      splitData <- lapply(splitData, function(df) df[, names(df) != x$subset])
      group_names <- names(splitData)
      
      invisible(
        mapply(
          function(df, group) {
            p <- mv_outlier(df, method = outlier_method, title = paste(title, "for", group), outlier = FALSE)
            if (!is.null(p$qq_outlier_plot)) print(p$qq_outlier_plot)
          },
          splitData,
          group_names
        )
      )
    }
    
    
  }
  
  
  
}

