mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::card(
      bslib::card_header("Export"),
      shiny::textInput(ns("base_name"), label = "Base file name", value = "mvn_analysis"),
      shiny::downloadButton(ns("download_data"), label = "Prepared data (CSV)", class = "btn-primary"),
      shiny::downloadButton(ns("download_results"), label = "Analysis object (RDS)")
    ),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Analysis snapshot"),
        shiny::uiOutput(ns("report_summary"))
      ),
      bslib::card(
        bslib::card_header("Multivariate test table"),
        shiny::tableOutput(ns("report_table"))
      )
    )
  )
}

mod_report_server <- function(id, processed_data, analysis_result, settings) {
  stopifnot(is.function(processed_data), is.function(analysis_result), is.function(settings))

  shiny::moduleServer(
    id,
    function(input, output, session) {
      base_name <- shiny::reactive({
        name <- input$base_name
        if (is.null(name) || !nzchar(trimws(name))) {
          return("mvn_analysis")
        }
        gsub("\\s+", "_", trimws(name))
      })

      output$report_summary <- shiny::renderUI({
        data <- processed_data()
        res <- analysis_result()
        opts <- settings()

        if (is.null(data) || is.null(res) || is.null(opts)) {
          return(shiny::div(class = "text-muted", "Run the analysis to generate a report preview."))
        }

        sample_n <- nrow(data)
        sample_p <- ncol(data)
        test_name <- opts$test_label
        if (is.null(test_name) || is.na(test_name)) {
          test_name <- opts$mvn_test
        }

        tbl <- res$multivariate_normality
        if (!is.null(tbl)) {
          p_col <- intersect(c("p.value", "p_value", "pvalue", "p.value.skew"), names(tbl))
          p_val <- if (length(p_col) > 0) suppressWarnings(as.numeric(tbl[[p_col[1]]][1])) else NA_real_
        } else {
          p_val <- NA_real_
        }

        shiny::tagList(
          shiny::p(
            sprintf(
              "Prepared dataset contains %d observations and %d numeric variables.",
              sample_n,
              sample_p
            )
          ),
          shiny::p(
            sprintf(
              "Latest multivariate normality test: %s (alpha = %s).",
              test_name,
              format(opts$alpha, digits = 3)
            )
          ),
          if (isTRUE(is.finite(p_val))) {
            shiny::p(sprintf("Reported p-value: %s", formatC(p_val, digits = 3, format = "g")))
          },
          if (isTRUE(opts$descriptives)) {
            shiny::p("Descriptive statistics were included in the analysis output.")
          } else {
            shiny::p("Descriptive statistics were skipped to streamline computation.")
          }
        )
      })

      output$report_table <- shiny::renderTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_normality
        shiny::req(tbl)
        as.data.frame(tbl)
      }, rownames = FALSE)

      output$download_data <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_data.csv")
        },
        content = function(file) {
          data <- processed_data()
          shiny::req(data)
          utils::write.csv(data, file, row.names = FALSE)
        }
      )

      output$download_results <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_analysis.rds")
        },
        content = function(file) {
          res <- analysis_result()
          shiny::req(res)
          saveRDS(res, file)
        }
      )
    }
  )
}
