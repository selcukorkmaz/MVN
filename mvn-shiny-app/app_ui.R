app_ui <- function(request) {
  shiny::fluidPage(
    shiny::titlePanel("MVN Shiny App"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Data Preparation",
        mod_data_prep_ui("data_prep")
      )
    )
  )
}
