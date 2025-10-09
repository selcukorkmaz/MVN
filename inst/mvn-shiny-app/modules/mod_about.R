mod_about_ui <- function(id) {
  shiny::tagList(
    shiny::fluidPage(
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Inter"),
        heading_font = bslib::font_google("Inter"),
        bootswatch = "flatly"
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$div(
            style = "max-width: 800px; margin: 0 auto; padding: 2rem;",
            shiny::tags$h1(
              "About MVN Shiny App",
              style = "text-align: center; margin-bottom: 1rem;"
            ),
            shiny::tags$p(
              "The ",
              shiny::tags$b("MVN Shiny App"),
              " is an interactive interface built on top of the ",
              shiny::tags$b("MVN R package"),
              ". It helps you ",
              shiny::tags$b("explore, diagnose, and report on multivariate normality"),
              " in a structured and intuitive way."
            ),
            shiny::tags$hr(style = "margin: 2rem 0;")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$div(
            style = "max-width: 800px; margin: 0 auto; padding: 0 2rem;",
            shiny::tags$h2("Getting Started"),
            shiny::tags$ol(
              shiny::tags$li(shiny::tags$b("Upload or select a dataset"), " in the Data & Preprocessing tab."),
              shiny::tags$li(shiny::tags$b("Choose statistical checks"), " in the Analysis Settings tab."),
              shiny::tags$li(shiny::tags$b("View results"), " with measures and diagnostic plots in the Results tab."),
              shiny::tags$li(shiny::tags$b("Export a PDF report"), " from the Report / Download tab.")
            ),
            shiny::tags$hr(style = "margin: 2rem 0;")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$div(
            style = "max-width: 800px; margin: 0 auto; padding: 0 2rem;",
            shiny::tags$h2("Recommended Citation"),
            shiny::tags$p(
              "If you use this app or the MVN package in your research, please cite:"
            ),
            shiny::tags$p(
              shiny::tags$b("Korkmaz S, Goksuluk D, Zararsiz G (2014)."),
              shiny::tags$br(),
              shiny::tags$i("MVN: An R Package for Assessing Multivariate Normality."),
              " The R Journal, 6(2), 151–162.",
              shiny::tags$br(),
              "doi: ",
              shiny::tags$a(
                href = "https://doi.org/10.32614/RJ-2014-031",
                "10.32614/RJ-2014-031",
                target = "_blank"
              )
            ),
            shiny::tags$pre(
              style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px;",
              "@article{RJ-2014-031,
  author  = {Selcuk Korkmaz and Dincer Goksuluk and Gokmen Zararsiz},
  title   = {MVN: An R Package for Assessing Multivariate Normality},
  year    = {2014},
  journal = {The R Journal},
  volume  = {6},
  number  = {2},
  pages   = {151--162},
  doi     = {10.32614/RJ-2014-031}
}"
            ),
            shiny::tags$hr(style = "margin: 2rem 0;")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$div(
            style = "max-width: 800px; margin: 0 auto; padding: 0 2rem 3rem 2rem;",
            shiny::tags$h2("Why Use MVN Shiny App?"),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("No coding required"), " – full MVN functionality in a GUI."),
              shiny::tags$li(shiny::tags$b("Clear diagnostics"), " – visual and statistical checks together."),
              shiny::tags$li(shiny::tags$b("Publication-ready reports"), " – generate comprehensive outputs in a few clicks.")
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
