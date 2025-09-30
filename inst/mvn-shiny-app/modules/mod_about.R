mod_about_ui <- function(id) {
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::h2("About MVN Shiny App"),
        shiny::p(
          "This interactive interface wraps the capabilities of the MVN R package,",
          "allowing you to explore, diagnose, and report on the multivariate",
          "normality of your data."
        )
      ),
      bslib::layout_column_wrap(
        width = NULL,
        bslib::card(
          bslib::card_body(
            shiny::h3("Getting Started"),
            shiny::p(
              "Upload or select a dataset on the Data & Preprocessing tab to prepare",
              "variables for analysis, then configure the statistical checks you",
              "would like to run."
            ),
            shiny::p(
              "Review computed measures and diagnostic plots in the Results tab,",
              "and export a comprehensive PDF report from the Report / Download tab"
            )
          )
        ),
        bslib::card(
          bslib::card_body(
            shiny::h3("Recommended Citation"),
            shiny::p(
              "If you use this application or the MVN package in your work, please cite:",
              shiny::tags$cite(
                "Selcuk Korkmaz, Dincer Goksuluk, and Gokmen Zararsiz (2014).",
                " MVN: An R Package for Assessing Multivariate Normality. The R Journal",
                " 6(2), 151--162.",
                " doi:10.32614/RJ-2014-031."
              )
            ),
            shiny::tags$pre(
              "@article{RJ-2014-031,\n",
              "  author = {Selcuk Korkmaz and Dincer Goksuluk and Gokmen Zararsiz},\n",
              "  title = {MVN: An R Package for Assessing Multivariate Normality},\n",
              "  year = {2014},\n",
              "  journal = {The R Journal},\n",
              "  doi = {10.32614/RJ-2014-031},\n",
              "  url = {https://doi.org/10.32614/RJ-2014-031},\n",
              "  pages = {151--162},\n",
              "  volume = {6},\n",
              "  number = {2}\n",
              "}"
            )
          )
        )
      )
    )
  )
}

mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
