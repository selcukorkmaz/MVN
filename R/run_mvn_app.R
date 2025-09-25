#' Launch the MVN Shiny application
#'
#' @description Opens the interactive Shiny interface bundled with the
#'   package, allowing users to explore MVN's functionality without writing
#'   code.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `host` or `port`.
#'
#' @return This function is called for its side effects of launching the
#'   Shiny application.
#'
#' @examples
#' \dontrun{
#' run_mvn_app()
#' }
#'
#' @export
run_mvn_app <- function(...) {
  app_dir <- system.file("mvn-shiny-app", package = "MVN")

  if (!nzchar(app_dir)) {
    stop("Could not locate the MVN Shiny application directory. Please reinstall MVN.", call. = FALSE)
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to launch the MVN Shiny app. Please install it.", call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, ...)
}
