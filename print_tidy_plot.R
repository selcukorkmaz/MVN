library(MVN)


result = mvn(
  data = iris[-4],
  subset = "Species",
  mvn_test = "hz",
  univariate_test = "AD",
  multivariate_outlier_method = "adj",
  show_outliers = TRUE,
  show_new_data = TRUE
)


result$descriptives %>%
  mutate(Variable = rownames(result$descriptives))








print.mvn <- function(x, ...) {
  cat("Multivariate Normality Test Results:\n")
  print(x$multivariate_normality)
  cat("\nUnivariate Normality Test Results:\n")
  print(x$univariate_normality)
  invisible(x)
}

print.mvn(result)

#' @export
tidy.mvn <- function(x,
                     type = c("multivariate", "univariate", "descriptives"),
                     ...) {
  type <- match.arg(type)
  switch(
    type,
    multivariate = x$multivariate_normality,
    univariate = x$univariate_normality,
    descriptives = x$descriptives
  )
}

tidy.mvn(result, type = c("multivariate", "univariate", "descriptives"))



#' @export
plot.mvn <- function(x,
                     diagnostic = c("multivariate", "univariate", "outlier"),
                     type = NULL, interactive = FALSE, outlier_method = "adj"
                     ) {
  df = x$data
  
  diagnostic <- match.arg(diagnostic)
  
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
        
        plot_name <- paste0(str_to_upper(type)," plots")
        
      }
      
      else if(type == "scatter"){
        
        plot_name <- paste0(str_to_title(type)," plots")
        
      }else{
        
        plot_name <- paste0(str_to_title(type),"s")
        
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
    
    
    # return(invisible(NULL))  # nothing visible printed to console
    
  }
  
  
  
}



plot.mvn(result, diagnostic = "outlier", outlier_method = "adj", interactive = TRUE)


