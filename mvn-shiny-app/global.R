if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is required to run this application.")
}

library(shiny)

if (!requireNamespace("bslib", quietly = TRUE)) {
  stop("The 'bslib' package is required to run this application.")
}

if (!requireNamespace("MVN", quietly = TRUE)) {
  stop("The 'MVN' package must be installed to run this application.")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("The 'ggplot2' package is required to run this application.")
}

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("The 'plotly' package is required to render interactive visualisations.")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 'jsonlite' package is required to export analysis parameters as JSON.")
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("The 'yaml' package is required to export analysis parameters as YAML.")
}

if (!requireNamespace("promises", quietly = TRUE)) {
  stop("The 'promises' package is required for asynchronous processing in the Shiny application.")
}

if (!requireNamespace("future", quietly = TRUE)) {
  stop("The 'future' package is required for background execution in the Shiny application.")
}

library(MVN)
library(promises)

future::plan(future::multisession)

modules_dir <- file.path(getwd(), "modules")
if (dir.exists(modules_dir)) {
  module_files <- list.files(modules_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  for (module_file in module_files) {
    source(module_file, local = FALSE)
  }
}

source("app_ui.R")
source("app_server.R")
