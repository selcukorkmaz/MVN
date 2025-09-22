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

        opts <- settings()
        data <- data_for_analysis()
        shiny::req(opts, data)

        data <- as.data.frame(data)
        group <- subset_var()
        if (!is.null(group) && !nzchar(group)) {
          group <- NULL
        }
        numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        sample_n <- nrow(data)
        sample_p <- length(numeric_cols)
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

        if (!is.null(group) && group %in% names(data)) {
          group_levels <- length(unique(data[[group]][!is.na(data[[group]])]))
          base_message <- paste0(
            base_message,
            sprintf(" Grouping variable: %s (%d levels).", group, group_levels)
          )
        }

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
