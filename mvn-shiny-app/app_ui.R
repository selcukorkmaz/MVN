app_ui <- function(request) {
  bslib::page_navbar(
    title = "MVN Shiny App",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    bslib::nav_panel("Data & Preprocessing", mod_data_prep_ui("data_prep")),
    bslib::nav_panel("Analysis Settings", mod_analysis_settings_ui("analysis")),
    bslib::nav_panel("Results", mod_results_ui("results")),
    bslib::nav_panel("Graphs", mod_graphs_ui("graphs")),
    bslib::nav_panel("Report / Download", mod_report_ui("report"))
  )
}
