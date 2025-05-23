#' Diagnostic Plots for Univariate and Multivariate Data
#'
#' Generates QQ plots, histograms with density overlays, boxplots, or scatterplot matrices
#' for numeric data (vector, matrix, or data frame).
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' @param type Character; type of plot. One of:
#'   \describe{
#'     \item{"qq"}{Normal Q-Q plot}
#'     \item{"histogram"}{Histogram with normal density overlay}
#'     \item{"boxplot"}{Boxplots of (scaled) variables}
#'     \item{"scatter"}{Scatterplot matrix (for p ≥ 2)}
#'   }. Default selects the first.
#' @param mfrow Numeric vector of length 2 to set the plotting layout via \code{par(mfrow=mfrow)}. If \code{NULL}, layout is chosen automatically for multiple variables.
#' @param ... Additional graphical parameters passed to the plotting functions.
#' @examples
#' \dontrun{
#' data = iris[1:4]
#' uni_plot(data, type = "histogram")
#' uni_plot(rnorm(100), type = "qq")
#' }
#' @importFrom graphics hist curve boxplot pairs par
#' @importFrom stats complete.cases dnorm sd qqnorm qqline
#' @export
univariate_diagnostic_plot <- function(data,
                     type  = c("qq", "histogram", "boxplot", "scatter"),
                     mfrow = NULL,
                     ...) {
  type <- match.arg(type)
  
  # Coerce input to data frame
  if (is.vector(data)) {
    df <- data.frame(value = data)
    var_names <- deparse(substitute(data))
    names(df) <- var_names
  } else if (is.matrix(data) || is.data.frame(data)) {
    df <- as.data.frame(data)
    var_names <- if (!is.null(colnames(df))) colnames(df) else paste0("V", seq_len(ncol(df)))
    names(df) <- var_names
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  
  # Drop non-numeric columns
  is_num <- vapply(df, is.numeric, logical(1))
  if (!all(is_num)) {
    warning("Dropping non-numeric columns: ", paste(names(df)[!is_num], collapse=", "))
    df <- df[, is_num, drop = FALSE]
    var_names <- names(df)
  }
  p <- ncol(df)
  if (p < 1) stop("No numeric variables to plot.")
  
  # Remove rows with any NA
  complete_rows <- complete.cases(df)
  if (sum(!complete_rows) > 0) {
    warning(sprintf("Removed %d rows with missing values.", sum(!complete_rows)))
    df <- df[complete_rows, , drop = FALSE]
  }
  
  
  df_long <- df %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  

  num_vars <- ncol(df)
  ncol_plot <- ceiling(sqrt(num_vars))
  nrow_plot <- ceiling(num_vars / ncol_plot)
  
  # Plot according to type
  if (type == "histogram") {
    
    # Step 2: Compute density curves per variable
    density_data <- df_long %>%
      group_by(Variable) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SD   = sd(Value, na.rm = TRUE),
        Min  = min(Value, na.rm = TRUE),
        Max  = max(Value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rowwise() %>%
      mutate(Density = list({
        x_vals <- seq(Min, Max, length.out = 100)
        y_vals <- dnorm(x_vals, mean = Mean, sd = SD)
        tibble(Value = x_vals, Density = y_vals)
      })) %>%
      unnest(Density)
  
    
    
    
    # Step 4: Plot histograms with normal density overlays
    ggplot(df_long, aes(x = Value)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#E0E0E0", color = "black") +
      geom_line(data = density_data, aes(y = Density), color = "#D62828", linewidth = 1) +
      facet_wrap(~ Variable, scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
      labs(x = "Value", y = "Density") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    
  } else if (type == "qq") {
    
    # Step 1: Convert to long format
    df_long <- df %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    
    # Step 2: Compute theoretical quantiles and sample quantiles
    qq_data <- df_long %>%
      group_by(Variable) %>%
      mutate(
        Theoretical = qqnorm(Value, plot.it = FALSE)$x,
        Sample = sort(Value)
      ) %>%
      ungroup()
    
    # Step 3: Compute slope and intercept for each Q-Q line (like qqline())
    line_params <- qq_data %>%
      group_by(Variable) %>%
      reframe(
        q_sample = quantile(Sample, probs = c(0.25, 0.75), na.rm = TRUE),
        q_theoretical = quantile(Theoretical, probs = c(0.25, 0.75), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(Variable) %>%
      reframe(
        slope = diff(q_sample) / diff(q_theoretical),
        intercept = q_sample[1] - (diff(q_sample) / diff(q_theoretical)) * q_theoretical[1],
        .groups = "drop"
      )
    
    # Step 4: Merge slope/intercept into qq_data
    qq_data <- left_join(qq_data, line_params, by = "Variable")
    
    
    # Step 5: Plot with proper qqline per facet and dynamic layout
    ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
      geom_point(color = "#4C4C4C", size = 1.5, alpha = 0.7) +
      geom_abline(aes(slope = slope, intercept = intercept), color = "#D62828", linewidth = 0.8) +
      facet_wrap(~ Variable, scales = "free", nrow = nrow_plot, ncol = ncol_plot) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    
    
  } else if (type == "boxplot") {
    
    # Step 2: Determine layout automatically
    num_vars <- ncol(df)
    ncol_plot <- ceiling(sqrt(num_vars))
    nrow_plot <- ceiling(num_vars / ncol_plot)
    
    # Step 3: Create faceted boxplots (unscaled)
    ggplot(df_long, aes(x = Variable, y = Value)) +
      geom_boxplot(fill = "#A6CEE3", color = "#333333", outlier.color = "#D62828", outlier.size = 1.5, width = 0.5) +
      facet_wrap(~ Variable, scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
      labs(x = NULL, y = "Value") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text        = element_text(size = 12, face = "bold"),
        axis.text.x       = element_blank(),
        axis.ticks.x      = element_blank(),
        axis.title.y      = element_text(size = 11),
        axis.text.y       = element_text(size = 10),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    
    
  } else if (type == "scatter") {
    if (p < 2) stop("Scatterplot matrix requires at least two variables.")
    graphics::pairs(df, labels = var_names, ...)
  }
  
  invisible(NULL)
}
