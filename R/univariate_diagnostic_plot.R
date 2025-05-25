utils::globalVariables(c(
  "everything", "Variable", "Value", "Min", "Max", "Mean", "SD", 
  "Density", "density", "Sample", "Theoretical", 
  "q_sample", "q_theoretical", "slope", "intercept", 
  "x", "y", "x_var", "y_var"
))
#' Diagnostic Plots for Univariate and Multivariate Data
#'
#' Generates QQ plots, histograms with density overlays, boxplots, or scatterplot matrices
#' for numeric data (vector, matrix, or data frame).
#'
#' @param data A numeric vector, matrix, or data frame with observations in rows and variables in columns.
#' @param type Character; type of plot. One of: "qq", "histogram", "boxplot", "scatter". Default selects the first.
#' @param title Character; plot title.
#' @param interactive Logical; if TRUE, renders the plot interactively using plotly.
#'
#' @examples
#' \dontrun{
#' data <- iris[1:50, 1:3]
#' univariate_diagnostic_plot(data, type = "histogram")
#' univariate_diagnostic_plot(data, type = "qq")
#' univariate_diagnostic_plot(data, type = "boxplot")
#' univariate_diagnostic_plot(data, type = "scatter", interactive = TRUE)
#' }
#' @importFrom graphics hist curve boxplot pairs par
#' @importFrom stats complete.cases dnorm sd qqnorm qqline quantile
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line geom_point geom_boxplot
#'             facet_wrap facet_grid labs theme_minimal theme element_text element_blank
#'             element_rect unit margin geom_abline after_stat vars ggtitle
#' @importFrom tidyr pivot_longer unnest
#' @importFrom dplyr group_by summarise rowwise mutate reframe ungroup left_join select
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom plotly ggplotly
#' @export

univariate_diagnostic_plot <- function(data,
                                       type  = c("qq", "histogram", "boxplot", "scatter"), 
                                       title = NULL,
                                       interactive = FALSE) {
  type <- match.arg(type)
  
  # Coerce input to data frame
  if (is.vector(data)) {
    df <- data.frame(value = data)
    var_names <- deparse(substitute(data))
    names(df) <- var_names
  } else if (is.matrix(data) || is.data.frame(data)) {
    df <- as.data.frame(data)
    var_names <- if (!is.null(colnames(df)))
      colnames(df)
    else
      paste0("V", seq_len(ncol(df)))
    names(df) <- var_names
  } else {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  
  # Drop non-numeric columns
  is_num <- vapply(df, is.numeric, logical(1))
  if (!all(is_num)) {
    warning("Dropping non-numeric columns: ",
            paste(names(df)[!is_num], collapse = ", "))
    df <- df[, is_num, drop = FALSE]
    var_names <- names(df)
  }
  p <- ncol(df)
  if (p < 1)
    stop("No numeric variables to plot.")
  
  # Remove rows with any NA
  complete_rows <- complete.cases(df)
  if (sum(!complete_rows) > 0) {
    warning(sprintf("Removed %d rows with missing values.", sum(!complete_rows)))
    df <- df[complete_rows, , drop = FALSE]
  }
  
  
  df_long <- df %>%
    pivot_longer(cols = everything(),
                 names_to = "Variable",
                 values_to = "Value")
  
  
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
    p <- ggplot(df_long, aes(x = Value)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "#E0E0E0",
        color = "black"
      ) +
      geom_line(
        data = density_data,
        aes(y = Density),
        color = "#D62828",
        linewidth = 1
      ) +
      facet_wrap( ~ Variable,
                  scales = "free",
                  ncol = ncol_plot,
                  nrow = nrow_plot) +
      labs(x = "Value", y = "Density") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        )
      )+
      ggtitle(title)  
    
    
  } else if (type == "qq") {
    # Step 1: Convert to long format
    df_long <- df %>%
      pivot_longer(cols = everything(),
                   names_to = "Variable",
                   values_to = "Value")
    
    # Step 2: Compute theoretical quantiles and sample quantiles
    qq_data <- df_long %>%
      group_by(Variable) %>%
      mutate(Theoretical = qqnorm(Value, plot.it = FALSE)$x,
             Sample = sort(Value)) %>%
      ungroup()
    
    # Step 3: Compute slope and intercept for each Q-Q line (like qqline())
    line_params <- qq_data %>%
      group_by(Variable) %>%
      reframe(
        q_sample = quantile(Sample, probs = c(0.25, 0.75), na.rm = TRUE),
        q_theoretical = quantile(
          Theoretical,
          probs = c(0.25, 0.75),
          na.rm = TRUE
        ),
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
    p <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
      geom_point(color = "#4C4C4C",
                 size = 1.5,
                 alpha = 0.7) +
      geom_abline(
        aes(slope = slope, intercept = intercept),
        color = "#D62828",
        linewidth = 0.8
      ) +
      facet_wrap( ~ Variable,
                  scales = "free",
                  nrow = nrow_plot,
                  ncol = ncol_plot) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        )
      )+
      ggtitle(title)  
    
    
  } else if (type == "boxplot") {
    # Step 2: Determine layout automatically
    num_vars <- ncol(df)
    ncol_plot <- ceiling(sqrt(num_vars))
    nrow_plot <- ceiling(num_vars / ncol_plot)
    
    # Step 3: Create faceted boxplots (unscaled)
    p <- ggplot(df_long, aes(x = Variable, y = Value)) +
      geom_boxplot(
        fill = "#A6CEE3",
        color = "#333333",
        outlier.color = "#D62828",
        outlier.size = 1.5,
        width = 0.5
      ) +
      facet_wrap( ~ Variable,
                  scales = "free",
                  ncol = ncol_plot,
                  nrow = nrow_plot) +
      labs(x = NULL, y = "Value") +
      theme_minimal(base_family = "sans") +
      theme(
        strip.text        = element_text(size = 12, face = "bold"),
        axis.text.x       = element_blank(),
        axis.ticks.x      = element_blank(),
        axis.title.y      = element_text(size = 11),
        axis.text.y       = element_text(size = 10),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        )
      )+
      ggtitle(title)  
    
    
    
  } else if (type == "scatter") {
    
    var_names <- names(df)
    
    if (length(var_names) < 2)
      stop("Scatterplot matrix requires at least two variables.")
    
    # Prepare all variable combinations
    combinations <- expand.grid(x_var = var_names,
                                y_var = var_names,
                                stringsAsFactors = FALSE)
    
    # Build long-format data manually
    plot_data <- purrr::map_dfr(seq_len(nrow(combinations)), function(i) {
      xv <- combinations$x_var[i]
      yv <- combinations$y_var[i]
      tibble(
        x_var = xv,
        y_var = yv,
        x = df[[xv]],
        y = df[[yv]]
      )
    })
    
    # Plot
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(alpha = 0.7,
                 size = 1.2,
                 color = "#3366CC") +
      facet_grid(rows = vars(y_var),
                 cols = vars(x_var),
                 scales = "free") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "sans") +
      theme(
        strip.background = element_rect(fill = "#f0f0f0", color = NA),
        strip.text = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(
          size = 7,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(size = 7),
        panel.spacing = unit(0.8, "lines"),
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        )
      )+
      ggtitle(title)  
    
    
  }
  
  if (interactive) {
    p <- ggplotly(p)
  } else{
    p
  }
  
  print(p)  # <-- this is essential
  invisible(p)
  
  # invisible(NULL)
}
