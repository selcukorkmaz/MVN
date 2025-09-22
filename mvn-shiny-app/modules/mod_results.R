mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1,
    bslib::layout_column_wrap(
      width = 1/5,
      bslib::value_box(
        title = "Observations",
        value = shiny::textOutput(ns("summary_n"), inline = TRUE)
      ),
      bslib::value_box(
        title = "Variables",
        value = shiny::textOutput(ns("summary_p"), inline = TRUE)
      ),
      bslib::value_box(
        title = "MVN test",
        value = shiny::textOutput(ns("summary_test"), inline = TRUE)
      ),
      bslib::value_box(
        title = "p-value",
        value = shiny::textOutput(ns("summary_pvalue"), inline = TRUE)
      ),
      bslib::value_box(
        title = "Normality",
        value = shiny::uiOutput(ns("summary_decision"))
      )
    ),
    bslib::card(
      bslib::card_header("Multivariate normality"),
      shiny::uiOutput(ns("results_message"))
    ),
    bslib::card(
      bslib::card_header("Detailed tables"),
      shiny::tabsetPanel(
        type = "pills",
        shiny::tabPanel("Multivariate tests", DT::dataTableOutput(ns("multivariate_table"))),
        shiny::tabPanel("Univariate tests", DT::dataTableOutput(ns("univariate_table"))),
        shiny::tabPanel("Descriptive statistics", DT::dataTableOutput(ns("descriptives_table"))),
        shiny::tabPanel("Outliers", DT::dataTableOutput(ns("outliers_table"))),
        shiny::tabPanel("Cleaned data", DT::dataTableOutput(ns("clean_data_table")))
      )
    ),
    bslib::card(
      bslib::card_header("Analysis details"),
      shiny::verbatimTextOutput(ns("analysis_summary"))
    )
  )
}

mod_results_server <- function(id, processed_data, settings, analysis_data = NULL, subset = NULL) {
  stopifnot(is.function(processed_data), is.function(settings))
  data_for_analysis <- if (is.null(analysis_data)) processed_data else analysis_data
  stopifnot(is.function(data_for_analysis))
  subset_var <- if (is.null(subset)) {
    function() NULL
  } else {
    stopifnot(is.function(subset))
    subset
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      analysis_result <- shiny::reactiveVal(NULL)
      analysis_needs_run <- shiny::reactiveVal(TRUE)

      prepare_analysis_data <- function(df) {
        df <- as.data.frame(df)
        group <- subset_var()
        if (!is.null(group) && !nzchar(group)) {
          group <- NULL
        }
        if (!is.null(group) && !(group %in% names(df))) {
          shiny::showNotification(sprintf("Grouping variable '%s' not found in the prepared data.", group), type = "error")
          group <- NULL
        }
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        if (length(numeric_cols) < 2) {
          shiny::showNotification("At least two numeric variables are required for multivariate analysis.", type = "warning")
          return(NULL)
        }

        list(
          data = if (is.null(group)) {
            df[, numeric_cols, drop = FALSE]
          } else {
            df[, c(numeric_cols, group), drop = FALSE]
          },
          group = group,
          numeric_cols = numeric_cols
        )
      }

      observeEvent(settings(), {
        opts <- settings()
        df <- data_for_analysis()
        if (is.null(df) || is.null(opts)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          return()
        }

        prepared <- prepare_analysis_data(df)
        if (is.null(prepared)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          return()
        }

        vars_count <- length(prepared$numeric_cols)
        obs_count <- nrow(prepared$data)
        if (vars_count > obs_count && !identical(opts$mvn_test, "hw")) {
          shiny::showNotification(
            "Number of variables exceeds the number of observations. Henze–Wagner test is recommended in this scenario.",
            type = "warning"
          )
        }

        result <- tryCatch({
          MVN::mvn(
            data = prepared$data,
            subset = prepared$group,
            mvn_test = opts$mvn_test,
            univariate_test = opts$univariate_test,
            multivariate_outlier_method = opts$outlier_method,
            descriptives = isTRUE(opts$descriptives),
            bootstrap = isTRUE(opts$bootstrap),
            alpha = opts$alpha,
            B = opts$B,
            cores = opts$cores,
            show_new_data = TRUE,
            tidy = TRUE
          )
        }, error = function(e) {
          shiny::showNotification(paste("Analysis failed:", e$message), type = "error")
          NULL
        })

        analysis_result(result)
        analysis_needs_run(is.null(result))
      }, ignoreNULL = FALSE)

      data_initialized <- shiny::reactiveVal(FALSE)
      observeEvent(data_for_analysis(), {
        df <- data_for_analysis()
        if (!isTRUE(data_initialized())) {
          data_initialized(TRUE)
        } else {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
        }
        if (is.null(df)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
        }
      }, ignoreNULL = FALSE)

      subset_initialized <- shiny::reactiveVal(FALSE)
      observeEvent(subset_var(), {
        if (!isTRUE(subset_initialized())) {
          subset_initialized(TRUE)
          return()
        }
        analysis_result(NULL)
        analysis_needs_run(TRUE)
      }, ignoreNULL = FALSE)

      extract_p_value <- function(tbl) {
        if (is.null(tbl)) {
          return(list(display = "\u2014", numeric = NA_real_))
        }
        df <- as.data.frame(tbl)
        if (!nrow(df)) {
          return(list(display = "\u2014", numeric = NA_real_))
        }
        p_col <- intersect(c("p.value", "p_value", "pvalue", "p.value.skew"), names(df))
        if (!length(p_col)) {
          return(list(display = "\u2014", numeric = NA_real_))
        }
        raw_value <- df[[p_col[1]]][1]
        if (length(raw_value) == 0) {
          return(list(display = "\u2014", numeric = NA_real_))
        }
        display <- if (is.numeric(raw_value)) {
          formatC(raw_value, digits = 3, format = "g")
        } else {
          as.character(raw_value)
        }
        numeric_value <- suppressWarnings(as.numeric(raw_value))
        if (!is.finite(numeric_value) && is.character(raw_value)) {
          if (grepl("^\\s*<", raw_value)) {
            numeric_value <- suppressWarnings(as.numeric(sub("^\\s*<\\s*", "", raw_value)))
          } else if (grepl("^\\s*>", raw_value)) {
            numeric_value <- suppressWarnings(as.numeric(sub("^\\s*>\\s*", "", raw_value)))
          }
        }
        list(
          display = if (is.null(display) || !nzchar(display)) "\u2014" else display,
          numeric = numeric_value
        )
      }

      summary_info <- shiny::reactive({
        res <- analysis_result()
        opts <- settings()
        if (is.null(res) || is.null(opts)) {
          return(NULL)
        }
        data <- res$data
        if (is.null(data)) {
          return(NULL)
        }
        data <- as.data.frame(data)
        group <- res$subset
        if (is.null(group) || !nzchar(group) || !(group %in% names(data))) {
          group <- NULL
        }
        numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        p_info <- extract_p_value(res$multivariate_normality)
        outlier_tbl <- res$multivariate_outliers
        outlier_count <- if (is.null(outlier_tbl)) 0L else nrow(as.data.frame(outlier_tbl))
        test_name <- opts$test_label
        if (is.null(test_name) || is.na(test_name)) {
          test_name <- opts$mvn_test
        }
        list(
          n = nrow(data),
          p = length(numeric_cols),
          group = group,
          group_levels = if (!is.null(group)) length(unique(data[[group]][!is.na(data[[group]])])) else NULL,
          test_label = test_name,
          alpha = opts$alpha,
          p_display = p_info$display,
          p_value = p_info$numeric,
          outlier_label = opts$outlier_label,
          outlier_count = outlier_count,
          cleaned_available = !is.null(res$new_data)
        )
      })

      output$summary_n <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        format(info$n, big.mark = ",", trim = TRUE)
      })

      output$summary_p <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        format(info$p, big.mark = ",", trim = TRUE)
      })

      output$summary_test <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        sprintf("%s (\u03b1 = %s)", info$test_label, format(info$alpha, digits = 3, trim = TRUE))
      })

      output$summary_pvalue <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        info$p_display
      })

      output$summary_decision <- shiny::renderUI({
        info <- summary_info()
        if (is.null(info)) {
          return(shiny::tags$span(class = "badge bg-secondary", "Awaiting analysis"))
        }
        if (!is.null(info$p_value) && is.finite(info$p_value)) {
          if (info$p_value < info$alpha) {
            badge_class <- "badge bg-danger"
            badge_text <- "Not normal"
          } else {
            badge_class <- "badge bg-success"
            badge_text <- "Normal"
          }
        } else {
          badge_class <- "badge bg-info text-dark"
          badge_text <- "Review details"
        }
        shiny::tags$span(class = badge_class, badge_text)
      })

      output$results_message <- shiny::renderUI({
        res <- analysis_result()
        if (is.null(res)) {
          if (isTRUE(analysis_needs_run())) {
            return(shiny::div(
              class = "alert alert-info",
              "Click Run analysis to compute results with the current configuration."
            ))
          }
          return(shiny::div(class = "text-muted", "Run the analysis to view results."))
        }

        info <- summary_info()
        if (is.null(info)) {
          return(shiny::div(class = "alert alert-info", "Run the analysis to view results."))
        }

        base_text <- sprintf(
          "Analyzed %s observations across %s variables using the %s test.",
          format(info$n, big.mark = ",", trim = TRUE),
          format(info$p, big.mark = ",", trim = TRUE),
          info$test_label
        )

        details <- character(0)
        if (!is.null(info$group)) {
          details <- c(
            details,
            sprintf(
              "Grouping variable: %s (%d levels).",
              info$group,
              info$group_levels
            )
          )
        }
        if (!is.null(info$outlier_label)) {
          outlier_sentence <- if (info$outlier_count > 0) {
            sprintf(
              "%d multivariate outlier%s flagged.",
              info$outlier_count,
              ifelse(info$outlier_count == 1, "", "s")
            )
          } else {
            "No multivariate outliers were flagged."
          }
          details <- c(
            details,
            sprintf("Outlier method: %s. %s", info$outlier_label, outlier_sentence)
          )
        }
        if (isTRUE(info$cleaned_available)) {
          details <- c(
            details,
            "A cleaned dataset excluding flagged outliers is available in the Cleaned data tab."
          )
        }

        if (!is.null(info$p_value) && is.finite(info$p_value)) {
          details <- c(details, sprintf("Reported p-value: %s.", info$p_display))
          alert_class <- if (info$p_value < info$alpha) {
            "alert alert-warning"
          } else {
            "alert alert-success"
          }
        } else {
          details <- c(details, "Reported p-value is unavailable. Review the tables below for additional details.")
          alert_class <- "alert alert-info"
        }

        detail_tags <- lapply(details, shiny::tags$p)
        shiny::div(
          class = alert_class,
          shiny::tags$p(base_text),
          detail_tags
        )
      })

      render_results_table <- function(data) {
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            lengthMenu = c(5, 10, 25, 50, 100)
          ),
          rownames = FALSE,
          filter = "top",
          class = "display nowrap"
        )
      }

      output$multivariate_table <- DT::renderDataTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_normality
        shiny::validate(shiny::need(!is.null(tbl), "Multivariate test results are unavailable."))
        render_results_table(as.data.frame(tbl))
      })

      output$univariate_table <- DT::renderDataTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$univariate_normality
        shiny::validate(shiny::need(!is.null(tbl), "Univariate test results are unavailable."))
        render_results_table(as.data.frame(tbl))
      })

      output$descriptives_table <- DT::renderDataTable({
        res <- analysis_result()
        opts <- settings()
        shiny::req(res, opts)
        if (!isTRUE(opts$descriptives)) {
          shiny::validate(shiny::need(FALSE, "Enable descriptive statistics in the Analysis Settings tab to view this table."))
        }
        tbl <- res$descriptives
        shiny::validate(shiny::need(!is.null(tbl), "Descriptive statistics were not returned."))
        render_results_table(as.data.frame(tbl))
      })

      output$outliers_table <- DT::renderDataTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_outliers
        if (is.null(tbl)) {
          shiny::validate(shiny::need(FALSE, "No multivariate outliers were detected."))
        }
        df <- as.data.frame(tbl)
        if (!nrow(df)) {
          shiny::validate(shiny::need(FALSE, "No multivariate outliers were detected."))
        }
        render_results_table(df)
      })

      output$clean_data_table <- DT::renderDataTable({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$new_data
        if (is.null(tbl)) {
          shiny::validate(shiny::need(FALSE, "Cleaned dataset is generated when multivariate outliers are removed."))
        }
        render_results_table(as.data.frame(tbl))
      })

      output$analysis_summary <- shiny::renderPrint({
        res <- analysis_result()
        shiny::req(res)
        MVN::summary(res, select = "mvn")
      })

      list(result = analysis_result)
    }
  )
}
