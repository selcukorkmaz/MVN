app_ui <- function(request) {
  bslib::page_navbar(
    title = "MVN Shiny App",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    header = shiny::tags$head(
      shiny::tags$style(
        "
        .selectize-control {
          position: relative;
          z-index: 2000 !important;
        }

        .selectize-dropdown,
        .dropdown-menu {
          z-index: 2001 !important;
        }

        .selectize-dropdown-content {
          position: relative;
          z-index: 2002 !important;
        }
        "
      )
    ),
    bslib::nav_panel("Data & Preprocessing", mod_data_prep_ui("data_prep")),
    bslib::nav_panel("Analysis Settings", mod_analysis_settings_ui("analysis")),
    bslib::nav_panel("Results", mod_results_ui("results")),
    bslib::nav_panel("Report / Download", mod_report_ui("report"))
  )
}
