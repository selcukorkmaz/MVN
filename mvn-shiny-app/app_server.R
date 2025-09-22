app_server <- function(input, output, session) {
  data_prep <- mod_data_prep_server("data_prep")

  observeEvent(data_prep$processed_data(), {
    prepared <- data_prep$processed_data()
    if (!is.null(prepared)) {
      cat("Prepared dataset with", ncol(prepared), "columns and", nrow(prepared), "rows.\n")
    }
  }, ignoreNULL = TRUE)
}
