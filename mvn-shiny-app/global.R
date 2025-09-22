if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is required to run this application.")
}

library(shiny)

if (!requireNamespace("MVN", quietly = TRUE)) {
  stop("The 'MVN' package must be installed to run this application.")
}

library(MVN)

modules_dir <- file.path(getwd(), "modules")
if (dir.exists(modules_dir)) {
  module_files <- list.files(modules_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  for (module_file in module_files) {
    source(module_file, local = FALSE)
  }
}

source("app_ui.R")
source("app_server.R")
