mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::card(
      bslib::card_header("Export"),
      shiny::textInput(ns("base_name"), label = "Base file name", value = "mvn_analysis"),
      shiny::div(
        class = "d-grid gap-2",
        shiny::downloadButton(ns("download_data"), label = "Prepared data (CSV)", class = "btn-primary"),
        shiny::downloadButton(ns("download_results"), label = "Analysis object (RDS)", class = "btn-outline-primary"),
        shiny::downloadButton(ns("download_results_csv"), label = "Analysis tables (CSV)", class = "btn-outline-secondary"),
        shiny::downloadButton(ns("download_parameters_yaml"), label = "Parameters (YAML)", class = "btn-outline-secondary"),
        shiny::downloadButton(ns("download_parameters_json"), label = "Parameters (JSON)", class = "btn-outline-secondary")
      )
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

mod_report_server <- function(id, processed_data, analysis_result, settings, analysis_data = NULL) {
  stopifnot(is.function(processed_data), is.function(analysis_result), is.function(settings))
  data_for_export <- if (is.null(analysis_data)) processed_data else analysis_data
  stopifnot(is.function(data_for_export))

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

      build_analysis_csv <- function(res) {
        tables <- list(
          multivariate_tests = res$multivariate_normality,
          univariate_tests = res$univariate_normality,
          descriptives = res$descriptives,
          multivariate_outliers = res$multivariate_outliers,
          cleaned_data = res$new_data
        )
        tables <- tables[!vapply(tables, is.null, logical(1))]
        if (!length(tables)) {
          return(NULL)
        }
        combined <- lapply(names(tables), function(name) {
          df <- as.data.frame(tables[[name]])
          if (!nrow(df)) {
            return(NULL)
          }
          rows <- seq_len(nrow(df))
          columns <- names(df)
          column_frames <- lapply(columns, function(col) {
            values <- df[[col]]
            data.frame(
              table = name,
              row = rows,
              column = col,
              value = as.character(values),
              stringsAsFactors = FALSE
            )
          })
          do.call(rbind, column_frames)
        })
        combined <- Filter(Negate(is.null), combined)
        if (!length(combined)) {
          return(NULL)
        }
        result <- do.call(rbind, combined)
        rownames(result) <- NULL
        result
      }

      collect_parameters <- function() {
        opts <- settings()
        params <- list()
        if (!is.null(opts)) {
          params$tests <- list(
            mvn_test = opts$mvn_test,
            mvn_test_label = opts$test_label,
            univariate_test = opts$univariate_test,
            univariate_test_label = opts$univariate_label,
            outlier_method = opts$outlier_method,
            outlier_method_label = opts$outlier_label
          )
          params$significance <- list(alpha = opts$alpha)
          params$descriptives <- isTRUE(opts$descriptives)
          params$bootstrap <- list(
            enabled = isTRUE(opts$bootstrap),
            replicates = opts$B,
            cores = opts$cores
          )
        }

        res <- analysis_result()
        if (!is.null(res)) {
          dataset_info <- list()
          data <- res$data
          if (!is.null(data)) {
            data <- as.data.frame(data)
            group <- res$subset
            if (is.null(group) || !nzchar(group) || !(group %in% names(data))) {
              group <- NULL
            }
            numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
            if (!is.null(group)) {
              numeric_cols <- setdiff(numeric_cols, group)
            }
            dataset_info$observations <- nrow(data)
            dataset_info$numeric_variables <- length(numeric_cols)
            if (!is.null(group)) {
              dataset_info$grouping_variable <- group
              dataset_info$group_levels <- length(unique(data[[group]][!is.na(data[[group]])]))
            }
          }
          outlier_tbl <- res$multivariate_outliers
          dataset_info$outliers_flagged <- if (is.null(outlier_tbl)) 0L else nrow(as.data.frame(outlier_tbl))
          if (!is.null(res$new_data)) {
            dataset_info$cleaned_observations <- nrow(as.data.frame(res$new_data))
          }
          if (length(dataset_info)) {
            params$dataset <- dataset_info
          }
          if (!is.null(opts)) {
            p_info <- extract_p_value(res$multivariate_normality)
            params$decision <- list(
              alpha = opts$alpha,
              p_value = p_info$numeric,
              p_value_display = p_info$display,
              normal = if (!is.null(p_info$numeric) && is.finite(p_info$numeric)) p_info$numeric >= opts$alpha else NULL
            )
          }
        }

        params
      }

      output$report_summary <- shiny::renderUI({
        data <- processed_data()
        res <- analysis_result()
        opts <- settings()

        if (is.null(data) || is.null(res) || is.null(opts)) {
          return(shiny::div(class = "text-muted", "Run the analysis to generate a report preview."))
        }

        data <- as.data.frame(data)
        numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
        sample_n <- nrow(data)
        sample_p <- length(numeric_cols)

        analysis_df <- res$data
        if (!is.null(analysis_df)) {
          analysis_df <- as.data.frame(analysis_df)
        } else {
          analysis_df <- data
        }

        group <- res$subset
        if (is.null(group) || !nzchar(group) || !(group %in% names(analysis_df))) {
          group <- NULL
        }

        test_name <- opts$test_label
        if (is.null(test_name) || is.na(test_name)) {
          test_name <- opts$mvn_test
        }

        p_info <- extract_p_value(res$multivariate_normality)
        outlier_tbl <- res$multivariate_outliers
        outlier_count <- if (is.null(outlier_tbl)) 0L else nrow(as.data.frame(outlier_tbl))
        cleaned_rows <- if (!is.null(res$new_data)) nrow(as.data.frame(res$new_data)) else NULL

        shiny::tagList(
          shiny::p(
            sprintf(
              "Prepared dataset contains %s observations and %s numeric variables.",
              format(sample_n, big.mark = ",", trim = TRUE),
              format(sample_p, big.mark = ",", trim = TRUE)
            )
          ),
          if (!is.null(group)) {
            shiny::p(
              sprintf(
                "Grouping variable: %s (%d levels).",
                group,
                length(unique(analysis_df[[group]][!is.na(analysis_df[[group]])]))
              )
            )
          },
          shiny::p(
            sprintf(
              "Latest multivariate normality test: %s (\u03b1 = %s).",
              test_name,
              format(opts$alpha, digits = 3, trim = TRUE)
            )
          ),
          if (!is.null(opts$outlier_label)) {
            shiny::p(
              sprintf(
                "Outlier method: %s (%d flagged).",
                opts$outlier_label,
                outlier_count
              )
            )
          },
          if (!is.null(cleaned_rows)) {
            shiny::p(
              sprintf(
                "Cleaned dataset contains %s observations after excluding flagged outliers.",
                format(cleaned_rows, big.mark = ",", trim = TRUE)
              )
            )
          },
          if (isTRUE(opts$bootstrap) || identical(opts$mvn_test, "energy")) {
            shiny::p(
              sprintf(
                "Bootstrap settings: B = %d, cores = %d.",
                opts$B,
                opts$cores
              )
            )
          },
          if (!identical(p_info$display, "\u2014")) {
            shiny::p(sprintf("Reported p-value: %s", p_info$display))
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
          data <- data_for_export()
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

      output$download_results_csv <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_analysis_tables.csv")
        },
        content = function(file) {
          res <- analysis_result()
          shiny::req(res)
          csv_data <- build_analysis_csv(res)
          if (is.null(csv_data) || !nrow(csv_data)) {
            utils::write.csv(
              data.frame(message = "No tabular results were produced for this analysis.", stringsAsFactors = FALSE),
              file,
              row.names = FALSE
            )
          } else {
            utils::write.csv(csv_data, file, row.names = FALSE)
          }
        }
      )

      output$download_parameters_yaml <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_parameters.yaml")
        },
        content = function(file) {
          params <- collect_parameters()
          if (is.null(params) || !length(params)) {
            params <- list(message = "No analysis parameters are available.")
          }
          yaml::write_yaml(params, file)
        }
      )

      output$download_parameters_json <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_parameters.json")
        },
        content = function(file) {
          params <- collect_parameters()
          if (is.null(params) || !length(params)) {
            params <- list(message = "No analysis parameters are available.")
          }
          jsonlite::write_json(params, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
        }
      )
    }
  )
}
