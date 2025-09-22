mod_analysis_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  available_cores <- tryCatch(
    suppressWarnings(parallel::detectCores(logical = FALSE)),
    error = function(e) NA_integer_
  )
  if (!is.numeric(available_cores) || !is.finite(available_cores) || available_cores < 1) {
    available_cores <- 1
  }

  bslib::layout_sidebar(
    sidebar = bslib::card(
      bslib::card_header("Configure analysis"),
      shiny::selectInput(
        ns("mvn_test"),
        label = "Multivariate normality test",
        choices = c(
          "Mardia (skewness & kurtosis)" = "mardia",
          "Henze-Zirkler" = "hz",
          "Henze-Wagner" = "hw",
          "Royston" = "royston",
          "Doornik-Hansen" = "doornik_hansen",
          "Energy" = "energy"
        ),
        selected = "hz"
      ),
      shiny::uiOutput(ns("mvn_warning")),
      shiny::selectInput(
        ns("univariate_test"),
        label = "Univariate normality test",
        choices = c(
          "Anderson-Darling" = "AD",
          "Shapiro-Wilk" = "SW",
          "Cramér–von Mises" = "CVM",
          "Lilliefors" = "Lillie",
          "Shapiro-Francia" = "SF"
        ),
        selected = "AD"
      ),
      shiny::selectInput(
        ns("outlier_method"),
        label = "Multivariate outliers",
        choices = c(
          "Robust quantile (quan)" = "quan",
          "Adjusted robust weights (adj)" = "adj"
        ),
        selected = "quan"
      ),
      shiny::checkboxInput(ns("descriptives"), label = "Compute descriptive statistics", value = TRUE),
      shiny::checkboxInput(ns("bootstrap"), label = "Bootstrap p-values (where available)", value = FALSE),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] || input['%s'] == 'energy'", ns("bootstrap"), ns("mvn_test")),
        shiny::numericInput(
          ns("bootstrap_B"),
          label = "Bootstrap replicates (B)",
          value = 1000,
          min = 100,
          max = 5000,
          step = 100
        ),
        shiny::numericInput(
          ns("bootstrap_cores"),
          label = "Number of cores",
          value = 1,
          min = 1,
          max = available_cores,
          step = 1
        ),
        shiny::helpText("Energy test always uses bootstrap replicates. Adjust settings to control computation time.")
      ),
      shiny::numericInput(ns("alpha"), label = "Significance level (alpha)", value = 0.05, min = 0.0001, max = 0.25, step = 0.01),
      shiny::actionButton(ns("run_analysis"), label = "Run analysis", class = "btn-primary")
    ),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Current configuration"),
        shiny::helpText("Adjust options in the sidebar and click Run analysis to update the downstream tabs."),
        shiny::verbatimTextOutput(ns("settings_summary"))
      )
    )
  )
}

mod_analysis_settings_server <- function(id, processed_data = NULL) {
  if (!is.null(processed_data)) {
    stopifnot(is.function(processed_data))
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      mvn_labels <- c(
        mardia = "Mardia (skewness & kurtosis)",
        hz = "Henze-Zirkler",
        hw = "Henze-Wagner",
        royston = "Royston",
        doornik_hansen = "Doornik-Hansen",
        energy = "Energy"
      )

      univariate_labels <- c(
        AD = "Anderson-Darling",
        SW = "Shapiro-Wilk",
        CVM = "Cramér–von Mises",
        Lillie = "Lilliefors",
        SF = "Shapiro-Francia"
      )

      outlier_labels <- c(
        quan = "Robust quantile",
        adj = "Adjusted robust weights"
      )

      collect_settings <- function() {
        alpha_value <- suppressWarnings(as.numeric(input$alpha))
        if (length(alpha_value) != 1 || !is.finite(alpha_value) || alpha_value <= 0) {
          alpha_value <- 0.05
        }
        mvn_method <- input$mvn_test
        if (is.null(mvn_method) || !(mvn_method %in% names(mvn_labels))) {
          mvn_method <- "hz"
        }
        univariate_method <- input$univariate_test
        if (is.null(univariate_method) || !(univariate_method %in% names(univariate_labels))) {
          univariate_method <- "AD"
        }
        outlier_method <- input$outlier_method
        if (is.null(outlier_method) || !(outlier_method %in% names(outlier_labels))) {
          outlier_method <- "quan"
        }
        bootstrap_input <- input$bootstrap_B
        if (is.null(bootstrap_input) || length(bootstrap_input) != 1) {
          bootstrap_B <- 1000L
        } else {
          bootstrap_B <- suppressWarnings(as.integer(round(bootstrap_input)))
          if (length(bootstrap_B) != 1 || !is.finite(bootstrap_B) || is.na(bootstrap_B) || bootstrap_B <= 0) {
            bootstrap_B <- 1000L
          }
        }
        cores_input <- input$bootstrap_cores
        if (is.null(cores_input) || length(cores_input) != 1) {
          cores_value <- 1L
        } else {
          cores_value <- suppressWarnings(as.integer(round(cores_input)))
          if (length(cores_value) != 1 || !is.finite(cores_value) || is.na(cores_value) || cores_value <= 0) {
            cores_value <- 1L
          }
        }
        max_cores <- tryCatch(
          suppressWarnings(parallel::detectCores(logical = FALSE)),
          error = function(e) NA_integer_
        )
        if (is.numeric(max_cores) && is.finite(max_cores) && max_cores >= 1) {
          cores_value <- min(cores_value, max_cores)
        }
        list(
          mvn_test = mvn_method,
          test_label = mvn_labels[[mvn_method]],
          univariate_test = univariate_method,
          univariate_label = univariate_labels[[univariate_method]],
          outlier_method = outlier_method,
          outlier_label = outlier_labels[[outlier_method]],
          descriptives = isTRUE(input$descriptives),
          bootstrap = isTRUE(input$bootstrap),
          B = bootstrap_B,
          cores = cores_value,
          alpha = alpha_value
        )
      }

      settings <- shiny::eventReactive(
        input$run_analysis,
        collect_settings(),
        ignoreNULL = FALSE
      )

      run_trigger <- shiny::reactive({
        count <- input$run_analysis
        if (is.null(count)) 0L else as.integer(count)
      })

      data_dims <- shiny::reactive({
        if (is.null(processed_data)) {
          return(NULL)
        }
        df <- processed_data()
        if (is.null(df)) {
          return(NULL)
        }
        df <- as.data.frame(df)
        list(n = nrow(df), p = ncol(df))
      })

      output$mvn_warning <- shiny::renderUI({
        dims <- data_dims()
        if (is.null(dims) || !is.numeric(dims$n) || !is.numeric(dims$p)) {
          return(NULL)
        }
        if (dims$p <= dims$n) {
          return(NULL)
        }
        if (identical(input$mvn_test, "hw")) {
          shiny::div(
            class = "alert alert-info",
            sprintf(
              "High-dimensional data detected (%d variables, %d observations). Henze–Wagner test is suitable for this scenario.",
              dims$p,
              dims$n
            )
          )
        } else {
          shiny::div(
            class = "alert alert-warning",
            sprintf(
              "%d variables and %d observations detected. Consider switching to the Henze–Wagner test for better performance.",
              dims$p,
              dims$n
            )
          )
        }
      })

      output$settings_summary <- shiny::renderPrint({
        opts <- settings()
        cat("Multivariate test:", opts$test_label, "\n")
        cat("Univariate test:", opts$univariate_label, "\n")
        cat("Outlier detection:", opts$outlier_label, "\n")
        cat("Descriptives:", if (isTRUE(opts$descriptives)) "Yes" else "No", "\n")
        bootstrap_text <- if (isTRUE(opts$bootstrap)) {
          sprintf("Enabled (B = %d, cores = %d)", opts$B, opts$cores)
        } else if (identical(opts$mvn_test, "energy")) {
          sprintf("Energy test uses B = %d, cores = %d", opts$B, opts$cores)
        } else {
          "Disabled"
        }
        cat("Bootstrap:", bootstrap_text, "\n")
        cat("Alpha:", format(opts$alpha, digits = 3), "\n")
      })

      list(
        settings = settings,
        run_analysis = run_trigger
      )
    }
  )
}
