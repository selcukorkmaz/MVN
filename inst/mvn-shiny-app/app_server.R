app_server <- function(input, output, session) {
  data_prep <- mod_data_prep_server("data_prep")

  analysis_settings <- mod_analysis_settings_server(
    "analysis",
    processed_data = data_prep$processed_data
  )

  analysis <- mod_results_server(
    "results",
    processed_data = data_prep$processed_data,
    settings = analysis_settings$settings,
    run_analysis = analysis_settings$run_analysis,
    analysis_data = data_prep$analysis_data,
    subset = data_prep$subset
  )

  mod_report_server(
    "report",
    processed_data = data_prep$processed_data,
    analysis_result = analysis$result,
    settings = analysis_settings$settings,
    analysis_data = data_prep$analysis_data
  )

  mod_about_server("about")

  shiny::observeEvent(analysis_settings$run_analysis(), {
    shiny::req(analysis_settings$run_analysis() > 0)
    bslib::nav_select(id = "main_nav", selected = "Results")
  }, ignoreNULL = TRUE)

  observeEvent(data_prep$processed_data(), {
    prepared <- data_prep$processed_data()
    if (!is.null(prepared)) {
      cat("Prepared dataset with", ncol(prepared), "columns and", nrow(prepared), "rows.\n")
    }
  }, ignoreNULL = TRUE)
}
