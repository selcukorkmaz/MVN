mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::card(
      bslib::card_header("Export"),
      shiny::textInput(ns("base_name"), label = "Base file name", value = "mvn_analysis"),
      shiny::div(
        class = "d-grid gap-3",
        shiny::tags$div(
          class = "text-uppercase text-muted fw-semibold small",
          "Prepared data"
        ),
        shiny::downloadButton(ns("download_data"), label = "Prepared data (CSV)", class = "btn-primary"),
        # shiny::tags$div(
        #   class = "text-uppercase text-muted fw-semibold small",
        #   "Analysis tables"
        # ),
        # shiny::downloadButton(ns("download_tables_zip"), label = "All tables (ZIP)", class = "btn-primary"),
        shiny::tags$div(
          class = "text-uppercase text-muted fw-semibold small",
          "Analysis plots"
        ),
        shiny::downloadButton(ns("download_plots_zip"), label = "All plots (ZIP)", class = "btn-primary")
      )
    ),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header("Analysis R code"),
        shiny::uiOutput(ns("analysis_code"))
      )
    )
  )
}

mod_report_server <- function(id, processed_data, analysis_result, settings, analysis_data = NULL) {
  stopifnot(is.function(processed_data), is.function(analysis_result), is.function(settings))
  data_for_export <- if (is.null(analysis_data)) processed_data else analysis_data
  stopifnot(is.function(data_for_export))

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      base_name <- shiny::reactive({
        name <- input$base_name
        if (is.null(name) || !nzchar(trimws(name))) {
          return("mvn_analysis")
        }
        gsub("\\s+", "_", trimws(name))
      })
      
      
      sanitize_for_filename <- function(x) {
        if (is.null(x) || !length(x)) {
          return("value")
        }
        value <- tolower(paste(x, collapse = "_"))
        value <- gsub("[^A-Za-z0-9]+", "-", value)
        value <- gsub("-+", "-", value)
        value <- gsub("^-+|-+$", "", value)
        if (!nzchar(value)) "value" else value
      }

      format_group_label <- function(value) {
        if (is.null(value) || length(value) == 0) {
          return("Missing")
        }
        value <- value[1]
        if (is.na(value)) {
          return("Missing")
        }
        if (is.factor(value)) {
          value <- as.character(value)
        }
        formatted <- if (inherits(value, c("POSIXt", "Date"))) {
          format(value)
        } else if (is.numeric(value)) {
          format(value, trim = TRUE, scientific = FALSE)
        } else {
          as.character(value)
        }
        formatted <- trimws(formatted)
        if (!nzchar(formatted)) "Missing" else formatted
      }

      get_numeric_data <- function(res) {
        if (is.null(res)) {
          return(NULL)
        }
        data <- res$data
        if (is.null(data)) {
          return(NULL)
        }
        df <- as.data.frame(data)
        group <- res$subset
        if (!is.null(group) && nzchar(group) && group %in% names(df)) {
          df[[group]] <- NULL
        }
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (!length(numeric_cols)) {
          return(NULL)
        }
        df[, numeric_cols, drop = FALSE]
      }

      get_group_values <- function(res) {
        group <- res$subset
        if (is.null(group) || !nzchar(group)) {
          return(NULL)
        }
        data <- res$data
        if (is.null(data)) {
          return(NULL)
        }
        df <- as.data.frame(data)
        if (!(group %in% names(df))) {
          return(NULL)
        }
        df[[group]]
      }

      get_grouped_numeric_data <- function(res) {
        if (is.null(res)) {
          return(NULL)
        }
        data <- res$data
        if (is.null(data)) {
          return(NULL)
        }
        df <- as.data.frame(data)
        group <- res$subset
        if (is.null(group) || !nzchar(group) || !(group %in% names(df))) {
          return(NULL)
        }
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        numeric_cols <- setdiff(numeric_cols, group)
        if (!length(numeric_cols)) {
          return(NULL)
        }
        group_vals <- df[[group]]
        unique_vals <- unique(group_vals)
        if (!length(unique_vals)) {
          return(NULL)
        }
        entries <- list()
        counter <- 0L
        for (val in unique_vals) {
          if (is.na(val)) {
            mask <- is.na(group_vals)
            label <- "Missing"
          } else {
            mask <- group_vals == val
            label <- format_group_label(val)
          }
          subset_df <- df[mask, numeric_cols, drop = FALSE]
          if (!nrow(subset_df) || !ncol(subset_df)) {
            next
          }
          counter <- counter + 1L
          key <- sprintf("group_%03d", counter)
          entries[[key]] <- list(
            key = key,
            label = label,
            data = subset_df
          )
        }
        if (!length(entries)) {
          return(NULL)
        }
        entries
      }

      build_analysis_code <- function() {
        res <- analysis_result()
        opts <- settings()
        data <- data_for_export()
        if (is.null(res) || is.null(opts) || is.null(data)) {
          return(NULL)
        }

        df <- res$data
        if (is.null(df)) {
          df <- data
        }
        df <- as.data.frame(df)

        group <- res$subset
        if (is.null(group) || !nzchar(group) || !(group %in% names(df))) {
          group <- NULL
        }

        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (!length(numeric_cols)) {
          return(NULL)
        }
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        if (!length(numeric_cols)) {
          return(NULL)
        }

        quote_vector <- function(x) {
          if (!length(x)) {
            return("character(0)")
          }
          quoted <- vapply(x, function(val) {
            val <- gsub("\\\\", "\\\\\\\\", val)
            val <- gsub("\"", "\\\\\"", val)
            sprintf("\"%s\"", val)
          }, character(1))
          paste(quoted, collapse = ", ")
        }

        format_scalar <- function(x, digits = 6) {
          if (is.null(x) || length(x) == 0) {
            return("NA")
          }
          value <- suppressWarnings(as.numeric(x[1]))
          if (!is.finite(value)) {
            return("NA")
          }
          format(value, digits = digits, trim = TRUE)
        }

        dataset_file <- paste0(base_name(), "_data.csv")

        code_lines <- c(
          "# Load required package and the prepared dataset exported from the app",
          "library(MVN)",
          sprintf("prepared_data <- read.csv(\"%s\")", dataset_file),
          "",
          sprintf("numeric_vars <- c(%s)", quote_vector(numeric_cols))
        )

        if (!is.null(group)) {
          code_lines <- c(
            code_lines,
            sprintf("group_variable <- \"%s\"", group),
            "analysis_input <- prepared_data[, c(numeric_vars, group_variable), drop = FALSE]"
          )
        } else {
          code_lines <- c(
            code_lines,
            "analysis_input <- prepared_data[, numeric_vars, drop = FALSE]"
          )
        }

        call_lines <- c(
          "",
          "analysis <- MVN::mvn(",
          "  data = analysis_input,"
        )
        if (!is.null(group)) {
          call_lines <- c(call_lines, sprintf("  subset = \"%s\",", group))
        }
        call_lines <- c(
          call_lines,
          sprintf("  mvn_test = \"%s\",", opts$mvn_test),
          sprintf("  univariate_test = \"%s\",", opts$univariate_test),
          sprintf("  multivariate_outlier_method = \"%s\",", opts$outlier_method),
          sprintf("  descriptives = %s,", if (isTRUE(opts$descriptives)) "TRUE" else "FALSE"),
          sprintf("  bootstrap = %s,", if (isTRUE(opts$bootstrap)) "TRUE" else "FALSE"),
          sprintf("  alpha = %s,", format_scalar(opts$alpha)),
          sprintf("  B = %s,", format_scalar(opts$B, digits = 8)),
          sprintf("  cores = %s,", format_scalar(opts$cores, digits = 3)),
          "  show_new_data = TRUE,",
          "  tidy = TRUE",
          ")",
          "",
          "# Inspect the core multivariate test results",
          "summary(analysis, select = \"mvn\")"
        )
        
        # --- ADD THIS BLOCK ---
        lines <- c(code_lines, call_lines)
        lines <- gsub("&lt;", "<", lines, fixed = TRUE)
        lines <- gsub("&gt;", ">", lines, fixed = TRUE)
        lines <- gsub("&amp;", "&", lines, fixed = TRUE)
        return(lines)
        
      }
      
      prepare_code_lines_for_export <- function(lines) {
        if (is.null(lines)) {
          return(NULL)
        }
        # Decode common HTML entities back to R syntax
        lines <- gsub("&lt;", "<", lines, fixed = TRUE)
        lines <- gsub("&gt;", ">", lines, fixed = TRUE)
        lines <- gsub("&amp;", "&", lines, fixed = TRUE)
        lines
      }

      write_table_csv <- function(df, path) {
        df <- as.data.frame(df)
        row_ids <- rownames(df)
        if (!is.null(row_ids) && any(nzchar(row_ids))) {
          df <- cbind(Row = row_ids, df, stringsAsFactors = FALSE, check.names = FALSE)
        }
        utils::write.csv(df, path, row.names = FALSE)
      }

      collect_analysis_tables <- function(res) {
        tables <- list(
          multivariate_normality = res$multivariate_normality,
          univariate_normality = res$univariate_normality,
          descriptive_statistics = res$descriptives,
          multivariate_outliers = res$multivariate_outliers,
          cleaned_data = res$new_data
        )
        tables <- Filter(function(tbl) {
          if (is.null(tbl)) {
            return(FALSE)
          }
          df <- tryCatch(as.data.frame(tbl), error = function(e) NULL)
          if (is.null(df)) {
            return(FALSE)
          }
          nrow(df) > 0 || ncol(df) > 0
        }, tables)
        lapply(tables, as.data.frame)
      }

      ensure_zip_archive <- function(source_dir, destination) {
        files_to_zip <- list.files(source_dir, recursive = TRUE, include.dirs = TRUE)
        if (!length(files_to_zip)) {
          stop("Failed to create ZIP archive: no files were written to the staging directory.")
        }

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(source_dir)

        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zipr(zipfile = destination, files = files_to_zip)
        } else {
          utils::zip(zipfile = destination, files = files_to_zip)
        }

        archive_info <- file.info(destination)
        if (!is.finite(archive_info$size) || archive_info$size <= 0) {
          stop("Failed to create ZIP archive: the resulting file is empty.")
        }
      }

      write_tables_archive <- function(res, destination) {
        tables <- collect_analysis_tables(res)
        if (!length(tables)) {
          stop("No tables were produced for the current analysis.")
        }
        tmpdir <- tempfile(pattern = "mvn_tables_")
        dir.create(tmpdir)
        on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
        tables_dir <- file.path(tmpdir, "tables")
        dir.create(tables_dir, recursive = TRUE)

        manifest <- c()
        friendly_names <- c(
          multivariate_normality = "multivariate-normality",
          univariate_normality = "univariate-normality",
          descriptive_statistics = "descriptive-statistics",
          multivariate_outliers = "multivariate-outliers",
          cleaned_data = "cleaned-data"
        )

        for (name in names(tables)) {
          file_stub <- friendly_names[[name]]
          if (is.null(file_stub)) {
            file_stub <- sanitize_for_filename(name)
          }
          out_path <- file.path(tables_dir, paste0(file_stub, ".csv"))
          write_table_csv(tables[[name]], out_path)
          manifest <- c(manifest, sprintf("tables/%s.csv", file_stub))
        }

        readme <- c(
          "Analysis tables exported from the MVN Shiny application.",
          "Each CSV file contains one of the tabular outputs shown in the Results tab.",
          "",
          "Files included:"
        )
        readme <- c(readme, paste0("- ", manifest))
        writeLines(readme, con = file.path(tmpdir, "README.txt"))

        ensure_zip_archive(tmpdir, destination)
      }

      create_multivariate_scatter <- function(numeric_data, group_values, subset_label) {
        if (is.null(numeric_data) || ncol(numeric_data) < 2) {
          return(NULL)
        }
        if (ncol(numeric_data) >= 3 && nrow(numeric_data) >= 3) {
          pcs <- tryCatch(stats::prcomp(numeric_data, center = TRUE, scale. = TRUE), error = function(e) NULL)
          if (!is.null(pcs) && ncol(pcs$x) >= 3) {
            coords <- pcs$x[, 1:3, drop = FALSE]
            plt <- plotly::plot_ly(
              x = coords[, 1],
              y = coords[, 2],
              z = coords[, 3],
              type = "scatter3d",
              mode = "markers",
              marker = list(size = 4, opacity = 0.7),
              color = if (!is.null(group_values)) as.factor(group_values) else NULL,
              colors = "Viridis"
            )
            label <- if (!is.null(subset_label) && nzchar(subset_label)) subset_label else "Group"
            return(plotly::layout(
              plt,
              scene = list(
                xaxis = list(title = "PC1"),
                yaxis = list(title = "PC2"),
                zaxis = list(title = "PC3")
              ),
              legend = list(title = list(text = label))
            ))
          }
        }
        plt <- plotly::plot_ly(
          x = numeric_data[[1]],
          y = numeric_data[[2]],
          type = "scatter",
          mode = "markers",
          marker = list(size = 8, opacity = 0.7),
          color = if (!is.null(group_values)) as.factor(group_values) else NULL,
          colors = "Viridis"
        )
        label <- if (!is.null(subset_label) && nzchar(subset_label)) subset_label else "Group"
        plotly::layout(
          plt,
          xaxis = list(title = colnames(numeric_data)[1]),
          yaxis = list(title = colnames(numeric_data)[2]),
          legend = list(title = list(text = label))
        )
      }

      `%||%` <- function(x, y) {
        if (!is.null(x) && length(x) && !is.na(x[1])) x else y
      }

      collect_plot_specs <- function(res, opts) {
        numeric_data <- get_numeric_data(res)
        if (is.null(numeric_data)) {
          return(list())
        }
        group_values <- get_group_values(res)
        grouped_entries <- get_grouped_numeric_data(res)

        specs <- list()
        add_spec <- function(path, object, type, description) {
          if (is.null(object)) {
            return(NULL)
          }
          specs[[length(specs) + 1]] <<- list(path = path, object = object, type = type, description = description)
          invisible(NULL)
        }

        scatter <- create_multivariate_scatter(numeric_data, group_values, res$subset)
        if (!is.null(scatter)) {
          scatter_type <- if (inherits(scatter, "plotly")) "html" else "png"
          scatter_path <- if (identical(scatter_type, "html")) "multivariate/scatter.html" else "multivariate/scatter.png"
          add_spec(scatter_path, scatter, scatter_type, "Interactive scatter plot of the analysed variables.")
        }

        if (ncol(numeric_data) >= 2 && nrow(numeric_data) >= 3) {
          overall_qq <- tryCatch(
            MVN::multivariate_diagnostic_plot(numeric_data, type = "qq"),
            error = function(e) NULL
          )
          if (!is.null(overall_qq)) {
            add_spec("multivariate/mahalanobis-qq_overall.png", overall_qq, "png", "Mahalanobis Q-Q plot for all observations.")
          }
          if (!is.null(grouped_entries) && length(grouped_entries)) {
            for (entry in grouped_entries) {
              df_group <- entry$data
              if (is.null(df_group) || ncol(df_group) < 2 || nrow(df_group) < 3) {
                next
              }
              plot_obj <- tryCatch(
                MVN::multivariate_diagnostic_plot(df_group, type = "qq"),
                error = function(e) NULL
              )
              if (!is.null(plot_obj)) {
                file_name <- sprintf("multivariate/mahalanobis-qq_%s.png", sanitize_for_filename(entry$label))
                description <- sprintf("Mahalanobis Q-Q plot for group %s.", entry$label)
                add_spec(file_name, plot_obj, "png", description)
              }
            }
          }
        }

        diag_titles <- c(
          histogram = "Histograms with normal overlay",
          qq = "Q-Q plots",
          boxplot = "Boxplots by variable",
          scatter = "Scatter plots by variable"
        )
        if (ncol(numeric_data) >= 1) {
          for (type in names(diag_titles)) {
            plot_obj <- tryCatch(
              MVN::univariate_diagnostic_plot(numeric_data, type = type, title = diag_titles[[type]]),
              error = function(e) NULL
            )
            if (!is.null(plot_obj)) {
              file_name <- sprintf("univariate/%s_overall.png", sanitize_for_filename(type))
              description <- sprintf("%s for all selected numeric variables.", diag_titles[[type]])
              add_spec(file_name, plot_obj, "png", description)
            }
            if (!is.null(grouped_entries) && length(grouped_entries)) {
              for (entry in grouped_entries) {
                df_group <- entry$data
                if (is.null(df_group) || !nrow(df_group) || !ncol(df_group)) {
                  next
                }
                grouped_plot <- tryCatch(
                  MVN::univariate_diagnostic_plot(
                    df_group,
                    type = type,
                    title = sprintf("%s — %s", diag_titles[[type]], entry$label)
                  ),
                  error = function(e) NULL
                )
                if (!is.null(grouped_plot)) {
                  file_name <- sprintf(
                    "univariate/%s_%s.png",
                    sanitize_for_filename(type),
                    sanitize_for_filename(entry$label)
                  )
                  description <- sprintf("%s for group %s.", diag_titles[[type]], entry$label)
                  add_spec(file_name, grouped_plot, "png", description)
                }
              }
            }
          }
        }

        if (ncol(numeric_data) >= 2) {
          method <- opts$outlier_method %||% "quan"
          alpha <- opts$alpha %||% 0.05
          overall_outlier <- tryCatch({
            res_out <- MVN::mv_outlier(numeric_data, method = method, alpha = alpha, outlier = FALSE)
            res_out$qq_outlier_plot
          }, error = function(e) NULL)
          if (!is.null(overall_outlier)) {
            add_spec("outliers/outlier-qq_overall.png", overall_outlier, "png", "Outlier diagnostics Q-Q plot for all observations.")
          }
          if (!is.null(grouped_entries) && length(grouped_entries)) {
            for (entry in grouped_entries) {
              df_group <- entry$data
              if (is.null(df_group) || ncol(df_group) < 2) {
                next
              }
              grouped_outlier <- tryCatch({
                res_out <- MVN::mv_outlier(df_group, method = method, alpha = alpha, outlier = FALSE)
                res_out$qq_outlier_plot
              }, error = function(e) NULL)
              if (!is.null(grouped_outlier)) {
                file_name <- sprintf("outliers/outlier-qq_%s.png", sanitize_for_filename(entry$label))
                description <- sprintf("Outlier diagnostics Q-Q plot for group %s.", entry$label)
                add_spec(file_name, grouped_outlier, "png", description)
              }
            }
          }
        }

        specs
      }

      save_plot_spec <- function(spec, root_dir) {
        dest <- file.path(root_dir, spec$path)
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (identical(spec$type, "html")) {
          htmlwidgets::saveWidget(spec$object, file = dest, selfcontained = TRUE)
        } else if (identical(spec$type, "png")) {
          grDevices::png(filename = dest, width = 1600, height = 1000, res = 150)
          on.exit(grDevices::dev.off(), add = TRUE)
          print(spec$object)
        } else {
          stop(sprintf("Unsupported plot export type: %s", spec$type))
        }
      }

      write_plots_archive <- function(res, opts, destination) {
        specs <- collect_plot_specs(res, opts)
        if (!length(specs)) {
          stop("No plots were produced for the current analysis.")
        }
        tmpdir <- tempfile(pattern = "mvn_plots_")
        dir.create(tmpdir)
        on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
        plots_dir <- file.path(tmpdir, "plots")
        dir.create(plots_dir, recursive = TRUE)

        manifest <- character()
        for (spec in specs) {
          save_plot_spec(spec, plots_dir)
          manifest <- c(manifest, sprintf("plots/%s", spec$path))
        }

        readme <- c(
          "Plots exported from the MVN Shiny application.",
          "PNG files contain static diagnostics while HTML files preserve interactive views.",
          "",
          "Files included:"
        )
        readme <- c(readme, paste0("- ", manifest))
        writeLines(readme, con = file.path(tmpdir, "README.txt"))

        ensure_zip_archive(tmpdir, destination)
      }

      output$analysis_code <- shiny::renderUI({
        code_lines <- build_analysis_code()
        if (is.null(code_lines)) {
          return(shiny::div(class = "text-muted", "Run the analysis to generate reproducible R code."))
        }
        code_text <- paste(code_lines, collapse = "\n")
        code_block <- htmltools::tags$pre(
          class = "bg-light border rounded p-3",
          style = "white-space: pre-wrap;",
          htmltools::htmlEscape(code_text)
        )
        
        shiny::tagList(
          shiny::div(
            class = "d-flex flex-column flex-md-row align-items-md-center justify-content-between gap-3 mb-3",
            shiny::p(
              class = "text-muted mb-0",
              "Copy the script below to recreate the analysis with MVN::mvn()."
            ),
            shiny::downloadButton(
              ns("download_analysis_code"),
              label = "Download R script",
              class = "btn btn-primary"
            )
          ),
          code_block
        )
      })

      output$download_analysis_code <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_analysis.R")
        },
        contentType = "text/plain",
        content = function(file) {
          # Rebuild the code lines freshly (unescaped)
          code_lines <- build_analysis_code()
          shiny::req(code_lines)
          
          # Decode any escaped HTML entities just in case
          code_lines <- gsub("&lt;", "<", code_lines, fixed = TRUE)
          code_lines <- gsub("&gt;", ">", code_lines, fixed = TRUE)
          code_lines <- gsub("&amp;", "&", code_lines, fixed = TRUE)
          
          # Write to file
          writeLines(code_lines, con = file, useBytes = TRUE)
        }
      )
      
      
      

      output$download_data <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_data.csv")
        },
        content = function(file) {
          data <- data_for_export()
          shiny::req(data)
          utils::write.csv(data, file, row.names = FALSE)
        }
      )

      output$download_tables_zip <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_tables.zip")
        },
        content = function(file) {
          res <- analysis_result()
          shiny::req(res)
          write_tables_archive(res, file)
        },
        contentType = "application/zip"
      )

      output$download_plots_zip <- shiny::downloadHandler(
        filename = function() {
          paste0(base_name(), "_plots.zip")
        },
        content = function(file) {
          res <- analysis_result()
          opts <- settings()
          shiny::req(res, opts)
          write_plots_archive(res, opts, file)
        },
        contentType = "application/zip"
      )
    }
  )
}



