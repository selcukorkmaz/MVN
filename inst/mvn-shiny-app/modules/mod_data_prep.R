mod_data_prep_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = shiny::tagList(
      bslib::card(
        bslib::card_header("Data source"),
        shiny::radioButtons(
          ns("data_source"),
          label = NULL,
          choices = c("Sample dataset" = "sample", "Upload file" = "upload"),
          selected = "sample"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'sample'", ns("data_source")),
          shiny::selectInput(
            ns("sample_dataset"),
            label = "Choose a dataset",
            choices = c(
              "Iris" = "iris",
              "Motor Trend Cars" = "mtcars",
              "Swiss" = "swiss",
              "US Arrests" = "USArrests"
            ),
            selected = "iris"
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
          shiny::fileInput(
            ns("file_upload"),
            label = "Upload CSV, text, Excel, SPSS, or R data",
            accept = c(
              ".csv", ".tsv", ".txt",
              ".rds", ".rda", ".RData",
              ".xls", ".xlsx", ".xlsm",
              ".sav", ".por"
            )
          ),
          shiny::checkboxInput(ns("file_header"), label = "First row has column names", value = TRUE),
          shiny::selectInput(
            ns("file_sep"),
            label = "Column separator",
            choices = c("," = ",", ";" = ";", "Tab" = "\t"),
            selected = ","
          ),
          shiny::helpText("Excel files load the first worksheet by default.")
        )
      ),
      bslib::card(
        bslib::card_header("Variables"),
        shiny::selectizeInput(
          ns("selected_columns"),
          label = "Numeric variables for analysis",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Select numeric columns"
          )
        ),
        shiny::helpText("Columns left unselected will be excluded from downstream analysis."),
        shiny::selectInput(
          ns("group_variable"),
          label = "Grouping variable (optional)",
          choices = c("None" = ""),
          selected = ""
        )
      ),
      bslib::card(
        bslib::card_header("Missing data"),
        shiny::selectInput(
          ns("impute_method"),
          label = "Handling method",
          choices = c("None" = "none", "Mean" = "mean", "Median" = "median", "MICE" = "mice"),
          selected = "none"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'mice'", ns("impute_method")),
          shiny::numericInput(ns("mice_m"), label = "Number of imputations (m)", value = 5, min = 1, step = 1),
          shiny::numericInput(ns("mice_seed"), label = "Random seed", value = 123, min = 1, step = 1)
        )
      ),
      bslib::card(
        bslib::card_header("Transformations"),
        shiny::selectInput(
          ns("transform_family"),
          label = "Power transformation",
          choices = c(
            "None" = "none",
            "Box-Cox" = "bcPower",
            "Box-Cox (allow negatives)" = "bcnPower",
            "Yeo-Johnson" = "yjPower"
          ),
          selected = "none"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] != 'none'", ns("transform_family")),
          shiny::radioButtons(
            ns("transform_type"),
            label = "Lambda type",
            choices = c("Optimal" = "optimal", "Rounded" = "rounded"),
            inline = TRUE,
            selected = "optimal"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Standardization"),
        shiny::checkboxInput(
          ns("standardize"),
          label = "Center and scale numeric columns",
          value = FALSE
        )
      )
    ),
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Data summary"),
        shiny::verbatimTextOutput(ns("data_info")),
        shiny::uiOutput(ns("sampling_warning")),
        shiny::uiOutput(ns("validation_messages")),
        shiny::uiOutput(ns("excluded_ui")),
        shiny::tableOutput(ns("missing_overview"))
      ),
      bslib::card(
        bslib::card_header("Prepared data preview"),
        shiny::tableOutput(ns("data_preview")),
        shiny::uiOutput(ns("preview_notice")),
        shiny::uiOutput(ns("lambda_header")),
        shiny::tableOutput(ns("lambda_table"))
      )
    )
  )
}

mod_data_prep_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      lambda_vals <- shiny::reactiveVal(NULL)

      sample_dataset <- shiny::reactive({
        dataset_name <- input$sample_dataset
        shiny::req(dataset_name)
        data <- get(dataset_name, "package:datasets")
        as.data.frame(data)
      })

      uploaded_dataset <- shiny::reactive({
        file <- input$file_upload
        shiny::req(file)
        ext <- tolower(tools::file_ext(file$datapath))

        tryCatch({
          if (ext %in% c("csv", "txt", "tsv")) {
            sep <- input$file_sep
            if (identical(sep, "\t")) {
              sep <- "\t"
            }
            utils::read.table(
              file$datapath,
              header = isTRUE(input$file_header),
              sep = sep,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          } else if (ext %in% c("xls", "xlsx", "xlsm")) {
            if (!requireNamespace("readxl", quietly = TRUE)) {
              stop("Package 'readxl' is required to read Excel files.")
            }
            readxl::read_excel(file$datapath, .name_repair = "minimal")
          } else if (ext %in% c("sav", "por")) {
            if (!requireNamespace("haven", quietly = TRUE)) {
              stop("Package 'haven' is required to read SPSS files.")
            }
            haven::read_sav(file$datapath)
          } else if (ext == "rds") {
            readRDS(file$datapath)
          } else if (ext %in% c("rda", "rdata")) {
            env <- new.env(parent = emptyenv())
            load(file$datapath, envir = env)
            obj_name <- ls(env)
            if (length(obj_name) == 0) {
              stop("No objects found in the uploaded file.")
            }
            as.data.frame(env[[obj_name[1]]])
          } else {
            stop("Unsupported file type: ", ext)
          }
        }, error = function(e) {
          shiny::showNotification(paste("Failed to read uploaded data:", e$message), type = "error")
          NULL
        })
      })

      raw_data <- shiny::reactive({
        if (identical(input$data_source, "sample")) {
          sample_dataset()
        } else {
          uploaded_dataset()
        }
      })

      observeEvent(raw_data(), {
        df <- raw_data()
        if (is.null(df)) {
          shiny::updateSelectizeInput(session, "selected_columns", choices = character(), selected = character(), server = TRUE)
          shiny::updateSelectInput(session, "group_variable", choices = c("None" = ""), selected = "")
          return()
        }
        df <- as.data.frame(df)
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        shiny::updateSelectizeInput(
          session,
          "selected_columns",
          choices = numeric_cols,
          selected = numeric_cols,
          server = TRUE
        )
        group_choices <- setdiff(names(df), numeric_cols)
        shiny::updateSelectInput(
          session,
          "group_variable",
          choices = c("None" = "", stats::setNames(group_choices, group_choices)),
          selected = ""
        )
      }, ignoreNULL = FALSE)

      group_variable <- shiny::reactive({
        value <- input$group_variable
        if (is.null(value) || !nzchar(value)) {
          return(NULL)
        }
        value
      })

      numeric_data <- shiny::reactive({
        df <- raw_data()
        shiny::req(df)
        df <- as.data.frame(df)
        numeric_cols <- vapply(df, is.numeric, logical(1))
        selected <- input$selected_columns
        if (is.null(selected)) {
          selected <- character()
        }
        invalid <- setdiff(selected, names(df)[numeric_cols])
        if (length(invalid) > 0) {
          shiny::showNotification(
            sprintf(
              "Non-numeric column%s excluded from the analysis: %s",
              ifelse(length(invalid) == 1, "", "s"),
              paste(invalid, collapse = ", ")
            ),
            type = "warning"
          )
        }
        selected <- intersect(selected, names(df)[numeric_cols])
        if (!any(numeric_cols)) {
          shiny::showNotification("No numeric columns detected in the selected data set.", type = "error")
          return(NULL)
        }
        if (length(selected) == 0) {
          shiny::showNotification("Select at least one numeric column to continue.", type = "warning")
          return(NULL)
        }
        df[, selected, drop = FALSE]
      })

      imputed_data <- shiny::reactive({
        df <- numeric_data()
        shiny::req(df)
        method <- input$impute_method
        if (!anyNA(df) || identical(method, "none")) {
          return(df)
        }

        tryCatch({
          MVN::impute_missing(df, method = method, m = input$mice_m, seed = input$mice_seed)
        }, error = function(e) {
          shiny::showNotification(paste("Imputation failed:", e$message), type = "error")
          df
        })
      })

      transformed_data <- shiny::reactive({
        df <- imputed_data()
        shiny::req(df)
        lambda_vals(NULL)
        family <- input$transform_family
        if (identical(family, "none")) {
          return(df)
        }
        type <- input$transform_type
        result <- tryCatch({
          MVN::power_transform(df, family = family, type = type)
        }, error = function(e) {
          shiny::showNotification(paste("Power transformation failed:", e$message), type = "error")
          NULL
        })
        if (is.null(result)) {
          return(df)
        }
        lambda_vals(result$lambda)
        result$data
      })

      processed_data <- shiny::reactive({
        df <- transformed_data()
        shiny::req(df)
        if (isTRUE(input$standardize)) {
          scaled <- scale(df)
          df <- as.data.frame(scaled)
        }
        df
      })

      analysis_data <- shiny::reactive({
        df <- processed_data()
        if (is.null(df)) {
          return(NULL)
        }
        group <- group_variable()
        if (is.null(group)) {
          return(df)
        }
        original <- raw_data()
        if (is.null(original) || !(group %in% names(original))) {
          shiny::showNotification(sprintf("Grouping variable '%s' is not available in the data.", group), type = "error")
          return(df)
        }
        group_col <- original[[group]]
        if (length(group_col) != nrow(df)) {
          shiny::showNotification(sprintf("Grouping variable '%s' could not be aligned with the prepared data.", group), type = "error")
          return(df)
        }
        df[[group]] <- group_col
        df
      })

      output$data_info <- shiny::renderPrint({
        df <- raw_data()
        shiny::req(df)
        df <- as.data.frame(df)
        numeric_flags <- vapply(df, is.numeric, logical(1))
        numeric_df <- numeric_data()
        detected_numeric <- sum(numeric_flags)
        selected_numeric <- if (!is.null(numeric_df)) ncol(numeric_df) else 0
        cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
        cat("Numeric columns detected:", detected_numeric, "\n")
        cat("Numeric columns selected:", selected_numeric, "\n")
        group <- group_variable()
        if (!is.null(group)) {
          valid_group <- df[[group]]
          distinct_groups <- length(unique(valid_group[!is.na(valid_group)]))
          cat("Grouping variable:", group, sprintf("(%d levels)", distinct_groups), "\n")
        }
        if (is.null(numeric_df)) {
          cat("No numeric columns were selected for analysis.\n")
          return(invisible(NULL))
        }
        missing_counts <- colSums(is.na(numeric_df))
        if (any(missing_counts > 0)) {
          cat("Missing values before imputation:\n")
          print(missing_counts)
        } else {
          cat("No missing values in numeric columns before imputation.\n")
        }
      })

      output$sampling_warning <- shiny::renderUI({
        df <- raw_data()
        if (is.null(df)) {
          return(NULL)
        }
        df <- as.data.frame(df)
        n <- nrow(df)
        p <- ncol(df)
        preview_limit <- 10L
        large_row_threshold <- 5000L
        large_col_threshold <- 100L
        notices <- list()
        if (is.numeric(n) && is.numeric(p) && (n > large_row_threshold || p > large_col_threshold)) {
          notices[[length(notices) + 1]] <- shiny::div(
            class = "alert alert-warning",
            sprintf(
              "Large dataset detected (%s rows, %s columns). Sampling and transformations may take additional time.",
              format(n, big.mark = ",", trim = TRUE),
              format(p, big.mark = ",", trim = TRUE)
            )
          )
        }
        if (is.numeric(n) && n > preview_limit) {
          notices[[length(notices) + 1]] <- shiny::div(
            class = "alert alert-info",
            sprintf("Only the first %d rows are displayed in the preview.", preview_limit)
          )
        }
        if (!length(notices)) {
          return(NULL)
        }
        do.call(shiny::tagList, notices)
      })

      output$validation_messages <- shiny::renderUI({
        df <- numeric_data()
        shiny::req(df)
        has_data <- is.data.frame(df) && ncol(df) > 0 && nrow(df) > 0
        shiny::validate(shiny::need(has_data, "Select at least one numeric column with data."))
        has_non_missing <- any(!vapply(df, function(col) all(is.na(col)), logical(1)))
        shiny::validate(shiny::need(has_non_missing, "All selected numeric columns contain only missing values."))
        if (identical(input$transform_family, "bcPower")) {
          non_positive <- df <= 0
          if (length(non_positive) && any(non_positive, na.rm = TRUE)) {
            shiny::validate(
              shiny::need(
                FALSE,
                "Box-Cox transformation requires strictly positive values. Choose a different transformation or adjust the data."
              )
            )
          }
        }
        NULL
      })

      output$excluded_ui <- shiny::renderUI({
        df <- raw_data()
        if (is.null(df)) {
          return(NULL)
        }
        df <- as.data.frame(df)
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        auto_excluded <- setdiff(names(df), numeric_cols)
        selected <- input$selected_columns
        if (is.null(selected)) {
          selected <- character()
        }
        manual_excluded <- setdiff(numeric_cols, selected)
        alerts <- list()
        if (length(auto_excluded) > 0) {
          alerts[[length(alerts) + 1]] <- shiny::div(
            class = "alert alert-info",
            sprintf("Automatically excluded non-numeric columns: %s", paste(auto_excluded, collapse = ", "))
          )
        }
        if (length(manual_excluded) > 0) {
          alerts[[length(alerts) + 1]] <- shiny::div(
            class = "alert alert-secondary",
            sprintf("Manually excluded numeric columns: %s", paste(manual_excluded, collapse = ", "))
          )
        }
        if (length(alerts) == 0) {
          return(NULL)
        }
        do.call(shiny::tagList, alerts)
      })

      output$missing_overview <- shiny::renderTable({
        df <- numeric_data()
        shiny::req(df)
        shiny::validate(
          shiny::need(ncol(df) > 0, "No numeric columns available for summary."),
          shiny::need(nrow(df) > 0, "No data available to summarise.")
        )
        data.frame(
          Variable = names(df),
          Missing = colSums(is.na(df)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })

      output$data_preview <- shiny::renderTable({
        df <- analysis_data()
        shiny::req(df)
        shiny::validate(
          shiny::need(nrow(df) > 0, "Prepared data is empty."),
          shiny::need(ncol(df) > 0, "Prepared data does not contain any columns.")
        )
        utils::head(df, n = 10)
      }, rownames = TRUE)

      output$preview_notice <- shiny::renderUI({
        df <- analysis_data()
        if (is.null(df)) {
          return(NULL)
        }
        df <- as.data.frame(df)
        if (nrow(df) <= 10) {
          return(NULL)
        }
        shiny::div(
          class = "text-muted fst-italic mt-2",
          "Preview truncated to the first 10 rows."
        )
      })

      output$lambda_header <- shiny::renderUI({
        lambda <- lambda_vals()
        if (is.null(lambda)) {
          return(NULL)
        }
        shiny::h4("Estimated lambda values")
      })

      output$lambda_table <- shiny::renderTable({
        lambda <- lambda_vals()
        shiny::req(lambda)
        data.frame(
          Variable = names(lambda),
          Lambda = lambda,
          row.names = NULL,
          check.names = FALSE
        )
      })

      list(
        raw_data = raw_data,
        numeric_data = numeric_data,
        processed_data = processed_data,
        analysis_data = analysis_data,
        subset = group_variable,
        lambda = shiny::reactive(lambda_vals())
      )
    }
  )
}
