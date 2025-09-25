mod_graphs_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::card(
      bslib::card_header("Plot controls"),
      shiny::selectInput(
        ns("plot_type"),
        label = "Plot type",
        choices = c(
          "Pairs plot" = "pairs",
          "Correlation heatmap" = "correlation",
          "Q-Q plot" = "qq",
          "Density" = "density"
        ),
        selected = "pairs"
      ),
      shiny::uiOutput(ns("variable_ui"))
    ),
    bslib::card(
      bslib::card_header("Visualization"),
      shiny::plotOutput(ns("plot"), height = "520px")
    )
  )
}

mod_graphs_server <- function(id, processed_data) {
  stopifnot(is.function(processed_data))

  shiny::moduleServer(
    id,
    function(input, output, session) {
      observeEvent(processed_data(), {
        data <- processed_data()
        if (is.null(data)) {
          return()
        }
        vars <- names(data)
        shiny::updateSelectInput(session, "plot_variable", choices = vars)
      }, ignoreNULL = FALSE)

      output$variable_ui <- shiny::renderUI({
        if (input$plot_type %in% c("qq", "density")) {
          data <- processed_data()
          if (is.null(data)) {
            return(NULL)
          }
          shiny::selectInput(
            session$ns("plot_variable"),
            label = "Variable",
            choices = names(data)
          )
        } else {
          NULL
        }
      })

      output$plot <- shiny::renderPlot({
        df <- processed_data()
        shiny::req(df)
        plot_type <- input$plot_type

        if (plot_type == "pairs") {
          shiny::validate(shiny::need(ncol(df) >= 2, "At least two variables are required for a pairs plot."))
          pairs(df, main = "Pairs plot of prepared data", pch = 19, col = grDevices::adjustcolor("#0d6efd", alpha.f = 0.4))
          return(invisible(NULL))
        }

        if (plot_type == "correlation") {
          shiny::validate(shiny::need(ncol(df) >= 2, "At least two variables are required to compute correlations."))
          corr <- stats::cor(df, use = "pairwise.complete.obs")
          corr_df <- data.frame(
            Var1 = rep(colnames(corr), times = nrow(corr)),
            Var2 = rep(rownames(corr), each = ncol(corr)),
            value = as.vector(corr),
            stringsAsFactors = FALSE
          )
          plot_obj <- ggplot2::ggplot(corr_df, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_gradient2(limits = c(-1, 1), low = "#dc3545", mid = "#f8f9fa", high = "#198754") +
            ggplot2::labs(fill = "Correlation", x = NULL, y = NULL) +
            ggplot2::coord_fixed() +
            ggplot2::theme_minimal()
          return(plot_obj)
        }

        var <- input$plot_variable
        shiny::req(var)
        shiny::validate(shiny::need(var %in% names(df), "Select a variable to continue."))
        plot_df <- data.frame(value = df[[var]])

        if (plot_type == "qq") {
          ggplot2::ggplot(plot_df, ggplot2::aes(sample = value)) +
            ggplot2::stat_qq(color = "#0d6efd") +
            ggplot2::stat_qq_line(color = "#6c757d") +
            ggplot2::labs(title = paste("Q-Q plot for", var), x = "Theoretical quantiles", y = "Sample quantiles") +
            ggplot2::theme_minimal()
        } else if (plot_type == "density") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = value)) +
            ggplot2::geom_density(fill = "#0d6efd", alpha = 0.4, color = "#0d6efd") +
            ggplot2::labs(title = paste("Density for", var), x = var, y = "Density") +
            ggplot2::theme_minimal()
        }
      })
    }
  )
}
