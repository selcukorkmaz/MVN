mod_data_prep_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          ns("data_source"),
          label = "Data source",
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
            label = "Upload CSV or RDS",
            accept = c(".csv", ".tsv", ".txt", ".rds", ".rda", ".RData")
          ),
          shiny::checkboxInput(ns("file_header"), label = "First row has column names", value = TRUE),
          shiny::selectInput(
            ns("file_sep"),
            label = "Column separator",
            choices = c("," = ",", ";" = ";", "Tab" = "\t"),
            selected = ","
          )
        ),
        shiny::hr(),
        shiny::selectInput(
          ns("impute_method"),
          label = "Missing data handling",
          choices = c("None" = "none", "Mean" = "mean", "Median" = "median", "MICE" = "mice"),
          selected = "none"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'mice'", ns("impute_method")),
          shiny::numericInput(ns("mice_m"), label = "Number of imputations (m)", value = 5, min = 1, step = 1),
          shiny::numericInput(ns("mice_seed"), label = "Random seed", value = 123, min = 1, step = 1)
        ),
        shiny::hr(),
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
        ),
        shiny::checkboxInput(
          ns("standardize"),
          label = "Center and scale numeric columns",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        shiny::h4("Data summary"),
        shiny::verbatimTextOutput(ns("data_info")),
        shiny::uiOutput(ns("excluded_ui")),
        shiny::tableOutput(ns("missing_overview")),
        shiny::h4("Preview of prepared data"),
        shiny::tableOutput(ns("data_preview")),
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
      excluded_cols <- shiny::reactiveVal(character())
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

      numeric_data <- shiny::reactive({
        df <- raw_data()
        shiny::req(df)
        df <- as.data.frame(df)
        numeric_cols <- vapply(df, is.numeric, logical(1))
        excluded_cols(names(df)[!numeric_cols])
        if (!any(numeric_cols)) {
          shiny::showNotification("No numeric columns detected in the selected data set.", type = "error")
          return(NULL)
        }
        df[, numeric_cols, drop = FALSE]
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

      output$data_info <- shiny::renderPrint({
        df <- raw_data()
        shiny::req(df)
        numeric_df <- numeric_data()
        if (is.null(numeric_df)) {
          cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
          cat("No numeric columns were found.")
        } else {
          cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
          cat("Numeric columns used:", ncol(numeric_df), "\n")
          missing_counts <- colSums(is.na(numeric_df))
          if (any(missing_counts > 0)) {
            cat("Missing values before imputation:\n")
            print(missing_counts)
          } else {
            cat("No missing values in numeric columns before imputation.\n")
          }
        }
      })

      output$excluded_ui <- shiny::renderUI({
        excluded <- excluded_cols()
        if (length(excluded) == 0) {
          return(NULL)
        }
        shiny::div(
          class = "alert alert-info",
          sprintf("Excluded non-numeric columns: %s", paste(excluded, collapse = ", "))
        )
      })

      output$missing_overview <- shiny::renderTable({
        df <- numeric_data()
        shiny::req(df)
        data.frame(
          Variable = names(df),
          Missing = colSums(is.na(df)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })

      output$data_preview <- shiny::renderTable({
        df <- processed_data()
        shiny::req(df)
        utils::head(df, n = 10)
      }, rownames = TRUE)

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
        lambda = shiny::reactive(lambda_vals())
      )
    }
  )
}
