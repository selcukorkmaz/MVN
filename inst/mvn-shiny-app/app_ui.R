app_ui <- function(request) {
  bslib::page_navbar(
    title = "MVN Shiny App",
    theme = bslib::bs_theme(
      version = 5,
      bg = "#ecf0f1",
      fg = "#2c3e50",
      primary = "#18bc9c",
      secondary = "#2c3e50",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Inter")
    ),
    header = shiny::tags$head(
      shiny::tags$style(
        "
        .selectize-control,
        .dropdown,
        .dropup,
        .dropend,
        .dropstart {
          position: relative;
          z-index: 9998 !important;
          overflow: visible !important;
        }

        .selectize-dropdown,
        .selectize-dropdown-content,
        .dropdown-menu,
        .dropdown-menu.show {
          z-index: 9999 !important;
        }

        .bslib-card,
        .bslib-card > .card,
        .bslib-card .card,
        .bslib-card .card-body,
        .card,
        .card-body,
        .layout-sidebar,
        .layout-sidebar .sidebar,
        .layout-sidebar .main,
        .layout-column-wrap,
        .layout-column-wrap > * {
          overflow: visible !important;
        }
        "
      )
    ),
    bslib::nav_panel("Data & Preprocessing", mod_data_prep_ui("data_prep")),
    bslib::nav_panel("Analysis Settings", mod_analysis_settings_ui("analysis")),
    bslib::nav_panel("Results", mod_results_ui("results")),
    bslib::nav_panel("Report / Download", mod_report_ui("report")),
    bslib::nav_panel("About", mod_about_ui("about"))
  )
}
