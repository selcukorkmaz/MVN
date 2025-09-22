mod_analysis_settings_ui <- function(id) {
  ns <- shiny::NS(id)

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
          "None" = "none",
          "Robust quantile (quan)" = "quan",
          "Adjusted robust weights (adj)" = "adj"
        ),
        selected = "none"
      ),
      shiny::checkboxInput(ns("descriptives"), label = "Compute descriptive statistics", value = TRUE),
      shiny::checkboxInput(ns("bootstrap"), label = "Bootstrap p-values (where available)", value = FALSE),
      shiny::numericInput(ns("alpha"), label = "Significance level (alpha)", value = 0.05, min = 0.0001, max = 0.25, step = 0.01),
      shiny::actionButton(ns("apply_settings"), label = "Run analysis", class = "btn-primary")
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

mod_analysis_settings_server <- function(id) {
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
        none = "None",
        quan = "Robust quantile",
        adj = "Adjusted robust weights"
      )

      collect_settings <- function() {
        alpha_value <- suppressWarnings(as.numeric(input$alpha))
        if (!is.finite(alpha_value) || alpha_value <= 0) {
          alpha_value <- 0.05
        }
        list(
          mvn_test = input$mvn_test,
          test_label = mvn_labels[[input$mvn_test]],
          univariate_test = input$univariate_test,
          univariate_label = univariate_labels[[input$univariate_test]],
          outlier_method = input$outlier_method,
          outlier_label = outlier_labels[[input$outlier_method]],
          descriptives = isTRUE(input$descriptives),
          bootstrap = isTRUE(input$bootstrap),
          alpha = alpha_value
        )
      }

      settings <- shiny::eventReactive(
        input$apply_settings,
        collect_settings(),
        ignoreNULL = FALSE
      )

      output$settings_summary <- shiny::renderPrint({
        opts <- settings()
        cat("Multivariate test:", opts$test_label, "\n")
        cat("Univariate test:", opts$univariate_label, "\n")
        cat("Outlier detection:", opts$outlier_label, "\n")
        cat("Descriptives:", if (isTRUE(opts$descriptives)) "Yes" else "No", "\n")
        cat("Bootstrap:", if (isTRUE(opts$bootstrap)) "Enabled" else "Disabled", "\n")
        cat("Alpha:", format(opts$alpha, digits = 3), "\n")
      })

      list(settings = settings)
    }
  )
}
