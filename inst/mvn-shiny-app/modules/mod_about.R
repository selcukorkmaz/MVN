mod_about_ui <- function(id) {
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h1("About MVN Shiny App"),
          shiny::tags$p(
            "MVN Shiny App is an interactive interface built on top of the ",
            shiny::tags$b("MVN R package"),
            ", designed to help you ",
            shiny::tags$b("explore, diagnose, and report on the multivariate normality of your data"),
            ". It provides an intuitive workflow for researchers and data analysts, combining statistical rigor with a user-friendly interface."
          ),
          shiny::tags$hr()
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h2("Getting Started"),
          shiny::tags$ol(
            shiny::tags$li(shiny::tags$b("Upload or select a dataset"), " from the Data & Preprocessing tab."),
            shiny::tags$li(shiny::tags$b("Choose statistical checks"), " in the Analysis Settings tab."),
            shiny::tags$li(shiny::tags$b("View results"), " with computed measures and diagnostic plots in the Results tab."),
            shiny::tags$li(shiny::tags$b("Export a comprehensive report"), " in PDF format from the Report / Download tab.")
          ),
          shiny::tags$hr()
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h2("Recommended Citation"),
          shiny::tags$p(
            "If you use this application or the MVN package in your research, please cite:"
          ),
          shiny::tags$p(
            shiny::tags$b("Selcuk Korkmaz, Dincer Goksuluk, and Gokmen Zararsiz (2014)."),
            shiny::tags$br(),
            shiny::tags$i("MVN: An R Package for Assessing Multivariate Normality."),
            " The R Journal, 6(2), 151–162.",
            shiny::tags$br(),
            "doi:",
            shiny::tags$a(href = "https://doi.org/10.32614/RJ-2014-031", "10.32614/RJ-2014-031")
          ),
          shiny::tags$pre(
            "@article{RJ-2014-031,\n",
            "  author  = {Selcuk Korkmaz and Dincer Goksuluk and Gokmen Zararsiz},\n",
            "  title   = {MVN: An R Package for Assessing Multivariate Normality},\n",
            "  year    = {2014},\n",
            "  journal = {The R Journal},\n",
            "  volume  = {6},\n",
            "  number  = {2},\n",
            "  pages   = {151--162},\n",
            "  doi     = {10.32614/RJ-2014-031}\n",
            "}"
          ),
          shiny::tags$hr()
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h2("Why Use MVN Shiny App?"),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("No coding required"), " – all MVN functionality accessible through an interactive interface."),
            shiny::tags$li(shiny::tags$b("Clear diagnostics"), " – visual and statistical checks in one place."),
            shiny::tags$li(shiny::tags$b("Publication-ready reports"), " – generate complete reports in just a few clicks.")
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
