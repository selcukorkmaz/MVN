mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1/2,
    bslib::card(
      bslib::card_header("Multivariate normality"),
      shiny::uiOutput(ns("results_message")),
      shiny::tableOutput(ns("test_table"))
    ),
    bslib::card(
      bslib::card_header("Descriptive statistics"),
      shiny::tableOutput(ns("descriptives_table"))
    ),
    bslib::card(
      bslib::card_header("Analysis details"),
      shiny::verbatimTextOutput(ns("analysis_summary"))
    )
  )
}

mod_results_server <- function(id, processed_data, settings) {
  stopifnot(is.function(processed_data), is.function(settings))

  shiny::moduleServer(
    id,
    function(input, output, session) {
      analysis_result <- shiny::reactive({
        df <- processed_data()
        opts <- settings()
        shiny::req(df, opts)

        df <- as.data.frame(df)
        if (ncol(df) < 2) {
          shiny::showNotification("At least two numeric variables are required for multivariate analysis.", type = "warning")
          return(NULL)
        }

        tryCatch({
          MVN::mvn(
            data = df,
            mvn_test = opts$mvn_test,
            univariate_test = opts$univariate_test,
            multivariate_outlier_method = opts$outlier_method,
            descriptives = isTRUE(opts$descriptives),
            bootstrap = isTRUE(opts$bootstrap),
            alpha = opts$alpha,
            tidy = TRUE
          )
        }, error = function(e) {
          shiny::showNotification(paste("Analysis failed:", e$message), type = "error")
          NULL
        })
      })

      output$results_message <- shiny::renderUI({
        res <- analysis_result()
        if (is.null(res)) {
          return(shiny::div(class = "text-muted", "Run the analysis to view results."))
        }

        opts <- settings()
        data <- processed_data()
        shiny::req(opts, data)

        sample_n <- nrow(data)
        sample_p <- ncol(data)
        test_name <- opts$test_label
        if (is.null(test_name) || is.na(test_name)) {
          test_name <- opts$mvn_test
        }
        alpha <- opts$alpha

        tbl <- res$multivariate_normality
        if (is.null(tbl)) {
          p_val <- NA_real_
        } else {
          p_col <- intersect(c("p.value", "p_value", "pvalue", "p.value.skew"), names(tbl))
          if (length(p_col) > 0) {
            p_val <- suppressWarnings(as.numeric(tbl[[p_col[1]]][1]))
          } else {
            p_val <- NA_real_
          }
        }

        base_message <- sprintf(
          "Analyzed %d observations across %d variables using the %s test.",
          sample_n,
          sample_p,
          test_name
        )

        if (isTRUE(is.finite(p_val))) {
          detail <- paste("Reported p-value:", formatC(p_val, digits = 3, format = "g"))
          alert_class <- if (p_val < alpha) "alert-warning" else "alert-success"
          shiny::div(class = paste("alert", alert_class), base_message, detail)
        } else {
          shiny::div(class = "alert alert-info", base_message, "Review the table below for details.")
        }
      })

      output$test_table <- shiny::renderTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_normality
        shiny::req(tbl)
        as.data.frame(tbl)
      }, rownames = FALSE)

      output$descriptives_table <- shiny::renderTable({
        res <- analysis_result()
        opts <- settings()
        shiny::req(res, opts)
        if (!isTRUE(opts$descriptives)) {
          shiny::validate(shiny::need(FALSE, "Enable descriptive statistics in the Analysis Settings tab to view this table."))
        }
        tbl <- res$descriptives
        shiny::req(tbl)
        utils::head(as.data.frame(tbl), n = 12)
      }, rownames = FALSE)

      output$analysis_summary <- shiny::renderPrint({
        res <- analysis_result()
        shiny::req(res)
        MVN::summary(res, select = "mvn")
      })

      list(result = analysis_result)
    }
  )
}
