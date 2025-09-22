app_server <- function(input, output, session) {
  data_prep <- mod_data_prep_server("data_prep")

  analysis_settings <- mod_analysis_settings_server("analysis")

  analysis <- mod_results_server(
    "results",
    processed_data = data_prep$processed_data,
    settings = analysis_settings$settings
  )

  mod_graphs_server(
    "graphs",
    processed_data = data_prep$processed_data
  )

  mod_report_server(
    "report",
    processed_data = data_prep$processed_data,
    analysis_result = analysis$result,
    settings = analysis_settings$settings
  )

  observeEvent(data_prep$processed_data(), {
    prepared <- data_prep$processed_data()
    if (!is.null(prepared)) {
      cat("Prepared dataset with", ncol(prepared), "columns and", nrow(prepared), "rows.\n")
    }
  }, ignoreNULL = TRUE)
}
