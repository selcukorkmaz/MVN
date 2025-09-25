mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  create_expand_button <- function(card_id) {
    shiny::tags$button(
      type = "button",
      class = "btn btn-link btn-sm card-expand-btn",
      `data-card-target` = card_id,
      title = "Toggle full-screen view",
      shiny::tags$span(class = "card-expand-icon", `aria-hidden` = "true"),
      shiny::tags$span("Toggle full-screen view", class = "visually-hidden")
    )
  }

  summary_metric <- function(label, value_ui, icon = NULL) {
    shiny::tags$div(
      class = "summary-item d-flex align-items-center gap-3",
      if (!is.null(icon)) {
        shiny::tags$span(icon, class = "summary-icon", `aria-hidden` = "true")
      },
      shiny::tags$div(
        class = "summary-metric-text",
        shiny::tags$div(
          class = "summary-label text-uppercase text-muted fw-semibold",
          label
        ),
        shiny::tags$div(class = "summary-value", value_ui)
      )
    )
  }

  css_rules <- paste(
    ".results-summary {",
    "  display: flex;",
    "  flex-wrap: wrap;",
    "  gap: 1rem;",
    "  margin-bottom: 0.75rem;",
    "}",
    ".results-summary .summary-item {",
    "  flex: 1 1 200px;",
    "  background: rgba(248, 249, 250, 0.85);",
    "  border: 1px solid rgba(0, 0, 0, 0.05);",
    "  border-radius: 0.85rem;",
    "  padding: 0.75rem 1rem;",
    "  min-height: 72px;",
    "}",
    ".summary-item .summary-icon {",
    "  font-size: 1.5rem;",
    "}",
    ".summary-metric-text .summary-label {",
    "  font-size: 0.7rem;",
    "  letter-spacing: 0.08em;",
    "}",
    ".summary-metric-text .summary-value {",
    "  font-size: 1.25rem;",
    "  font-weight: 600;",
    "  color: var(--bs-body-color, #212529);",
    "}",
    ".summary-context {",
    "  margin-bottom: 1.25rem;",
    "}",
    ".results-card-grid {",
    "  gap: 1.5rem;",
    "  margin-bottom: 1.5rem;",
    "}",
    ".results-card {",
    "  min-height: 100%;",
    "  position: relative;",
    "}",
    ".results-card .card-header {",
    "  font-weight: 600;",
    "  letter-spacing: 0.06em;",
    "  text-transform: uppercase;",
    "  display: flex;",
    "  align-items: flex-start;",
    "  gap: 0.5rem;",
    "}",
    ".results-card .card-header-content {",
    "  display: flex;",
    "  align-items: center;",
    "  gap: 0.5rem;",
    "}",
    ".results-card .card-title-icon {",
    "  font-size: 1.25rem;",
    "}",
    ".results-card .card-title-text {",
    "  font-size: 0.85rem;",
    "  letter-spacing: 0.04em;",
    "}",
    ".results-card .card-header .card-title-text {",
    "  flex: 1 1 auto;",
    "}",
    ".card-expand-btn {",
    "  margin-left: auto;",
    "  padding: 0.25rem;",
    "  color: var(--bs-secondary-color, #6c757d);",
    "  text-decoration: none;",
    "}",
    ".card-expand-btn:hover,",
    ".card-expand-btn:focus {",
    "  color: var(--bs-primary, #0d6efd);",
    "  text-decoration: none;",
    "}",
    ".card-expand-btn:focus {",
    "  box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.25);",
    "  border-radius: 999px;",
    "}",
    ".card-expand-icon::before {",
    "  content: '⤢';",
    "  display: inline-block;",
    "  font-size: 1.1rem;",
    "  line-height: 1;",
    "}",
    ".results-card.card-fullscreen .card-expand-icon::before {",
    "  content: '⤡';",
    "}",
    "body.card-fullscreen-open {",
    "  overflow: hidden;",
    "}",
    "body.card-fullscreen-open::before {",
    "  content: '';",
    "  position: fixed;",
    "  inset: 0;",
    "  background: rgba(15, 23, 42, 0.45);",
    "  z-index: 1040;",
    "}",
    ".results-card.card-fullscreen {",
    "  position: fixed;",
    "  inset: 1.5rem;",
    "  z-index: 1050;",
    "  width: auto;",
    "  max-width: none;",
    "  height: calc(100vh - 3rem);",
    "  overflow: auto;",
    "  box-shadow: 0 1.25rem 3rem rgba(15, 23, 42, 0.35);",
    "}",
    ".results-card.card-fullscreen .card-body {",
    "  display: flex;",
    "  flex-direction: column;",
    "  gap: 1.5rem;",
    "  align-items: center;",
    "  padding: clamp(1rem, 3vh, 2rem);",
    "}",
    ".results-card.card-fullscreen .card-body > * {",
    "  width: min(100%, 1100px);",
    "}",
    ".results-card.card-fullscreen .visual-block,",
    ".results-card.card-fullscreen .results-table-wrapper,",
    ".results-card.card-fullscreen details {",
    "  width: 100%;",
    "}",
    ".results-card.card-fullscreen .results-table-wrapper {",
    "  max-height: 75vh;",
    "  overflow: auto;",
    "  border-radius: 0.85rem;",
    "  box-shadow: inset 0 0 0 1px rgba(15, 23, 42, 0.06);",
    "  background: rgba(255, 255, 255, 0.92);",
    "  padding: 0.75rem;",
    "}",
    ".results-card.card-fullscreen .visual-block .shiny-plot-output,",
    ".results-card.card-fullscreen .visual-block .plotly.html-widget {",
    "  width: 100%;",
    "  height: auto !important;",
    "  min-height: 320px;",
    "  border-radius: 0.85rem;",
    "  box-shadow: inset 0 0 0 1px rgba(15, 23, 42, 0.05);",
    "  background: rgba(248, 249, 250, 0.65);",
    "  padding: 0.5rem;",
    "  box-sizing: border-box;",
    "}",
    ".results-card.card-fullscreen .visual-block .plotly.html-widget {",
    "  padding: 0;",
    "  background: transparent;",
    "}",
    ".grouped-plot-stack {",
    "  display: flex;",
    "  flex-direction: column;",
    "  gap: 1rem;",
    "}",
    ".grouped-plot-block {",
    "  background: rgba(248, 249, 250, 0.65);",
    "  border: 1px solid rgba(0, 0, 0, 0.05);",
    "  border-radius: 0.85rem;",
    "  padding: 0.85rem;",
    "}",
    ".grouped-plot-block .grouped-plot-label {",
    "  font-size: 0.75rem;",
    "  letter-spacing: 0.08em;",
    "  text-transform: uppercase;",
    "  color: var(--bs-secondary-color, #6c757d);",
    "  margin-bottom: 0.5rem;",
    "  display: inline-flex;",
    "  align-items: center;",
    "  gap: 0.35rem;",
    "}",
    "@media (max-width: 576px) {",
    "  .results-card.card-fullscreen {",
    "    inset: 0.75rem;",
    "    height: calc(100vh - 1.5rem);",
    "  }",
    "}",
    ".results-card details {",
    "  border-top: 1px solid rgba(0, 0, 0, 0.05);",
    "  margin-top: 1rem;",
    "  padding-top: 0.75rem;",
    "}",
    ".results-card details > summary {",
    "  cursor: pointer;",
    "  font-weight: 600;",
    "  color: var(--bs-primary, #0d6efd);",
    "}",
    ".results-table {",
    "  width: 100%;",
    "  border-collapse: collapse;",
    "  font-size: 0.95rem;",
    "}",
    ".results-table-wrapper {",
    "  width: 100%;",
    "  overflow-x: auto;",
    "}",
    ".results-table th,",
    ".results-table td {",
    "  padding: 0.5rem 0.75rem;",
    "  border-bottom: 1px solid rgba(0, 0, 0, 0.05);",
    "  vertical-align: middle;",
    "}",
    ".results-table th {",
    "  text-transform: uppercase;",
    "  font-size: 0.7rem;",
    "  letter-spacing: 0.08em;",
    "  color: var(--bs-secondary-color, #6c757d);",
    "}",
    ".results-table tbody tr:nth-child(odd) {",
    "  background-color: rgba(240, 242, 245, 0.65);",
    "}",
    ".results-table tbody tr:hover {",
    "  background-color: rgba(13, 110, 253, 0.08);",
    "}",
    ".p-value-low {",
    "  color: var(--bs-danger, #dc3545);",
    "  font-weight: 600;",
    "}",
    ".p-value-ok {",
    "  font-weight: 600;",
    "}",
    ".p-value-mixed {",
    "  font-weight: 600;",
    "  color: var(--bs-primary, #0d6efd);",
    "}",
    ".badge-decision {",
    "  display: inline-flex;",
    "  align-items: center;",
    "  gap: 0.35rem;",
    "  padding: 0.35rem 0.75rem;",
    "  border-radius: 999px;",
    "  font-weight: 600;",
    "}",
    ".badge-decision.bg-warning {",
    "  color: #343a40;",
    "}",
    ".badge-decision.badge-decision-mixed {",
    "  background-color: #0a6ebd;",
    "  color: #ffffff;",
    "}",
    ".badge-decision .fa,",
    ".badge-decision .fas {",
    "  color: inherit;",
    "}",
    ".visual-block {",
    "  margin-bottom: 1.25rem;",
    "}",
    ".spread-bar {",
    "  position: relative;",
    "  height: 0.55rem;",
    "  border-radius: 999px;",
    "  background-color: rgba(33, 37, 41, 0.08);",
    "  overflow: hidden;",
    "}",
    ".spread-bar-range {",
    "  position: absolute;",
    "  top: 0;",
    "  bottom: 0;",
    "  border-radius: inherit;",
    "  background: linear-gradient(90deg, rgba(13, 110, 253, 0.75), rgba(25, 135, 84, 0.75));",
    "}",
    ".spread-bar-mean {",
    "  position: absolute;",
    "  top: -25%;",
    "  bottom: -25%;",
    "  width: 2px;",
    "  background-color: #0d1b2a;",
    "}",
    "@media (max-width: 576px) {",
    "  .results-summary .summary-item {",
    "    flex: 1 1 100%;",
    "  }",
    "}",
    sep = "\n"
  )

  card_ids <- list(
    multivariate = ns("card_multivariate"),
    univariate = ns("card_univariate"),
    descriptives = ns("card_descriptives"),
    outlier = ns("card_outlier")
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML(css_rules)),
    shiny::div(
      class = "results-summary",
      summary_metric("Observations", shiny::textOutput(ns("summary_n"), inline = TRUE), icon = "👥"),
      summary_metric("Variables", shiny::textOutput(ns("summary_p"), inline = TRUE), icon = "🔢"),
      summary_metric("MVN p-value", shiny::uiOutput(ns("summary_pvalue")), icon = "📈"),
      summary_metric("Decision", shiny::uiOutput(ns("summary_decision")))
    ),
    shiny::div(
      class = "summary-context text-muted small",
      shiny::uiOutput(ns("summary_context"))
    ),
    shiny::uiOutput(ns("analysis_status")),
    bslib::layout_column_wrap(
      width = 1/2,
      class = "results-card-grid",
      bslib::card(
        class = "results-card",
        id = card_ids$multivariate,
        bslib::card_header(
          class = "results-card-header",
          shiny::tags$div(
            class = "card-header-content",
            shiny::tags$span("🌐", class = "card-title-icon", `aria-hidden` = "true"),
            shiny::tags$span("Multivariate normality", class = "card-title-text")
          ),
          create_expand_button(card_ids$multivariate)
        ),
        shiny::uiOutput(ns("multivariate_content"))
      ),
      bslib::card(
        class = "results-card",
        id = card_ids$univariate,
        bslib::card_header(
          class = "results-card-header",
          shiny::tags$div(
            class = "card-header-content",
            shiny::tags$span("📊", class = "card-title-icon", `aria-hidden` = "true"),
            shiny::tags$span("Univariate normality", class = "card-title-text")
          ),
          create_expand_button(card_ids$univariate)
        ),
        shiny::uiOutput(ns("univariate_content"))
      ),
      bslib::card(
        class = "results-card",
        id = card_ids$descriptives,
        bslib::card_header(
          class = "results-card-header",
          shiny::tags$div(
            class = "card-header-content",
            shiny::tags$span("🧮", class = "card-title-icon", `aria-hidden` = "true"),
            shiny::tags$span("Descriptive statistics", class = "card-title-text")
          ),
          create_expand_button(card_ids$descriptives)
        ),
        shiny::uiOutput(ns("descriptives_content"))
      ),
      bslib::card(
        class = "results-card",
        id = card_ids$outlier,
        bslib::card_header(
          class = "results-card-header",
          shiny::tags$div(
            class = "card-header-content",
            shiny::tags$span("🚨", class = "card-title-icon", `aria-hidden` = "true"),
            shiny::tags$span("Outlier diagnostics", class = "card-title-text")
          ),
          create_expand_button(card_ids$outlier)
        ),
        shiny::uiOutput(ns("outlier_content"))
      )
    ),
    shiny::tags$script(
      shiny::HTML(paste(
        "const resizeFullscreenCard = function(card) {",
        "  if (!card) return;",
        "  const body = card.querySelector('.card-body');",
        "  if (!body) return;",
        "  const plots = body.querySelectorAll('.shiny-plot-output, .plotly.html-widget');",
        "  const tables = body.querySelectorAll('.results-table-wrapper');",
        "  const header = card.querySelector('.card-header');",
        "  const headerHeight = header ? header.offsetHeight : 0;",
        "  const styles = window.getComputedStyle(body);",
        "  const padding = parseFloat(styles.paddingTop || 0) + parseFloat(styles.paddingBottom || 0);",
        "  const availableHeight = card.clientHeight - headerHeight - padding;",
        "  const blocks = plots.length + tables.length;",
        "  const spacing = blocks > 1 ? (blocks - 1) * 24 : 0;",
        "  const share = blocks > 0 ? Math.max((availableHeight - spacing) / blocks, 280) : 0;",
        "  plots.forEach(function(el) {",
        "    const elementWidth = el.clientWidth || body.clientWidth;",
        "    const preferred = elementWidth * 0.65;",
        "    const limit = share > 0 ? Math.min(share, window.innerHeight * 0.8) : Math.min(window.innerHeight * 0.8, preferred);",
        "    const height = Math.max(320, Math.min(preferred, limit));",
        "    el.style.height = height + 'px';",
        "  });",
        "  tables.forEach(function(el) {",
        "    const limit = share > 0 ? Math.min(share, window.innerHeight * 0.8) : window.innerHeight * 0.8;",
        "    el.style.maxHeight = Math.max(320, limit) + 'px';",
        "  });",
        "  window.dispatchEvent(new Event('resize'));",
        "};",
        "const clearFullscreenSizing = function(card) {",
        "  if (!card) return;",
        "  card.querySelectorAll('.shiny-plot-output, .plotly.html-widget').forEach(function(el) {",
        "    el.style.height = '';",
        "  });",
        "  card.querySelectorAll('.results-table-wrapper').forEach(function(el) {",
        "    el.style.maxHeight = '';",
        "  });",
        "};",
        "document.addEventListener('click', function (event) {",
        "  const btn = event.target.closest('.card-expand-btn');",
        "  if (!btn) return;",
        "  const targetId = btn.getAttribute('data-card-target');",
        "  if (!targetId) return;",
        "  const card = document.getElementById(targetId);",
        "  if (!card) return;",
        "  event.preventDefault();",
        "  const isActive = card.classList.contains('card-fullscreen');",
        "  document.querySelectorAll('.results-card.card-fullscreen').forEach(function (el) {",
        "    if (el !== card) {",
        "      el.classList.remove('card-fullscreen');",
        "      clearFullscreenSizing(el);",
        "    }",
        "  });",
        "  if (isActive) {",
        "    card.classList.remove('card-fullscreen');",
        "    clearFullscreenSizing(card);",
        "  } else {",
        "    card.classList.add('card-fullscreen');",
        "    resizeFullscreenCard(card);",
        "    window.setTimeout(function () { resizeFullscreenCard(card); }, 150);",
        "  }",
        "  const hasActive = document.querySelector('.results-card.card-fullscreen') !== null;",
        "  document.body.classList.toggle('card-fullscreen-open', hasActive);",
        "});",
        "window.addEventListener('resize', function () {",
        "  const activeCard = document.querySelector('.results-card.card-fullscreen');",
        "  if (activeCard) {",
        "    resizeFullscreenCard(activeCard);",
        "  }",
        "});",
        "document.addEventListener('keydown', function (event) {",
        "  if (event.key === 'Escape') {",
        "    const activeCard = document.querySelector('.results-card.card-fullscreen');",
        "    if (activeCard) {",
        "      activeCard.classList.remove('card-fullscreen');",
        "      clearFullscreenSizing(activeCard);",
        "      document.body.classList.remove('card-fullscreen-open');",
        "    }",
        "  }",
        "});",
        sep = "\n"
      ))
    )
    # bslib::accordion(
    #   id = ns("results_details"),
    #   open = character(0),
    #   bslib::accordion_panel(
    #     title = "Analysis details",
    #     value = "analysis-details",
    #     shiny::verbatimTextOutput(ns("analysis_summary"))
    #   )
    # )
  )
}

mod_results_server <- function(id, processed_data, settings, run_analysis = NULL, analysis_data = NULL, subset = NULL) {
  stopifnot(is.function(processed_data), is.function(settings))
  if (!is.null(run_analysis)) {
    stopifnot(is.function(run_analysis))
  }
  data_for_analysis <- if (is.null(analysis_data)) processed_data else analysis_data
  stopifnot(is.function(data_for_analysis))
  subset_var <- if (is.null(subset)) {
    function() NULL
  } else {
    stopifnot(is.function(subset))
    subset
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      analysis_result <- shiny::reactiveVal(NULL)
      analysis_needs_run <- shiny::reactiveVal(TRUE)
      analysis_in_progress <- shiny::reactiveVal(FALSE)

      bootstrap_async_threshold <- 500L

      prepare_analysis_data <- function(df) {
        df <- as.data.frame(df)
        group <- subset_var()
        if (!is.null(group) && !nzchar(group)) {
          group <- NULL
        }
        if (!is.null(group) && !(group %in% names(df))) {
          shiny::showNotification(sprintf("Grouping variable '%s' not found in the prepared data.", group), type = "error")
          group <- NULL
        }
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        if (length(numeric_cols) < 2) {
          shiny::showNotification("At least two numeric variables are required for multivariate analysis.", type = "warning")
          return(NULL)
        }

        list(
          data = if (is.null(group)) {
            df[, numeric_cols, drop = FALSE]
          } else {
            df[, c(numeric_cols, group), drop = FALSE]
          },
          group = group,
          numeric_cols = numeric_cols
        )
      }

      run_mvn_analysis <- function(prepared, opts) {
        df <- as.data.frame(prepared$data)
        group <- prepared$group

        # Ensure only the numeric variables (and optional grouping variable) are
        # passed to the MVN::mvn() call. The grouping column must remain in the
        # data so that the subset argument can be resolved correctly.
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        keep_cols <- if (is.null(group)) {
          numeric_cols
        } else {
          unique(c(numeric_cols, group))
        }
        df <- df[, keep_cols, drop = FALSE]

        MVN::mvn(
          data = df,
          subset = prepared$group,
          mvn_test = opts$mvn_test,
          univariate_test = opts$univariate_test,
          multivariate_outlier_method = opts$outlier_method,
          descriptives = isTRUE(opts$descriptives),
          bootstrap = isTRUE(opts$bootstrap),
          alpha = opts$alpha,
          B = opts$B,
          cores = opts$cores,
          show_new_data = TRUE,
          tidy = TRUE
        )
      }
      
      execute_analysis <- function(prepared, opts, asynchronous = FALSE) {
        analysis_result(NULL)
        analysis_in_progress(TRUE)
        analysis_needs_run(FALSE)

        run_call <- function() {
          run_mvn_analysis(prepared, opts)
        }

        # if (!isTRUE(asynchronous)) {
        #   result <- NULL
        #   tryCatch({
        #     shiny::withProgress(message = "Running analysis...", {
        #       result <<- run_call()
        #     })
        #   }, error = function(e) {
        #     shiny::showNotification(paste("Analysis failed:", e$message), type = "error")
        #     result <<- NULL
        #   })
        #   analysis_result(result)
        #   analysis_in_progress(FALSE)
        #   analysis_needs_run(is.null(result))
        #   return(invisible(NULL))
        # }
        
        if (!isTRUE(asynchronous)) {
          shiny::withProgress(message = "Running analysis...", {
            result <- run_call()   # hata olursa Shiny konsoluna düşecek
          })
          analysis_result(result)
          analysis_in_progress(FALSE)
          analysis_needs_run(is.null(result))
          return(invisible(NULL))
        }
        

        detail_message <- sprintf(
          "Running %d bootstrap replicate%s using %d core%s.",
          opts$B,
          ifelse(opts$B == 1, "", "s"),
          opts$cores,
          ifelse(opts$cores == 1, "", "s")
        )

        progress <- shiny::Progress$new(session = session)
        progress$set(message = "Bootstrapping analysis...", detail = detail_message, value = 0)

        promises::future_promise({
          run_call()
        }) %...>%
          (function(result) {
            progress$set(value = 1)
            progress$close()
            result
          }) %...>%
          (function(result) {
            analysis_result(result)
            analysis_in_progress(FALSE)
            analysis_needs_run(is.null(result))
            NULL
          }) %...!%
          (function(err) {
            progress$close()
            analysis_result(NULL)
            analysis_in_progress(FALSE)
            analysis_needs_run(TRUE)
            shiny::showNotification(paste("Analysis failed:", conditionMessage(err)), type = "error")
            NULL
          })

        shiny::showNotification(
          "Bootstrap analysis started in the background. Results will appear when computation finishes.",
          type = "message"
        )

        invisible(NULL)
      }

      analysis_trigger <- shiny::reactive({
        current_settings <- settings()
        trigger_val <- if (!is.null(run_analysis)) run_analysis() else NULL
        list(settings = current_settings, trigger = trigger_val)
      })
      

      shiny::observeEvent(analysis_trigger(), {
        opts <- settings()
        df <- data_for_analysis()
        if (is.null(df) || is.null(opts)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          analysis_in_progress(FALSE)
          return()
        }

        prepared <- prepare_analysis_data(df)
        if (is.null(prepared)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          analysis_in_progress(FALSE)
          return()
        }

        vars_count <- length(prepared$numeric_cols)
        obs_count <- nrow(prepared$data)
        if (vars_count > obs_count && !identical(opts$mvn_test, "hw")) {
          shiny::showNotification(
            "Number of variables exceeds the number of observations. Henze–Wagner test is recommended in this scenario.",
            type = "warning"
          )
        }

        asynchronous <- (isTRUE(opts$bootstrap) || identical(opts$mvn_test, "energy")) && opts$B >= bootstrap_async_threshold
        execute_analysis(prepared, opts, asynchronous = asynchronous)
      }, ignoreNULL = FALSE)

      data_initialized <- shiny::reactiveVal(FALSE)
      observeEvent(data_for_analysis(), {
        df <- data_for_analysis()
        if (!isTRUE(data_initialized())) {
          data_initialized(TRUE)
        } else {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          analysis_in_progress(FALSE)
        }
        if (is.null(df)) {
          analysis_result(NULL)
          analysis_needs_run(TRUE)
          analysis_in_progress(FALSE)
        }
      }, ignoreNULL = FALSE)

      subset_initialized <- shiny::reactiveVal(FALSE)
      observeEvent(subset_var(), {
        if (!isTRUE(subset_initialized())) {
          subset_initialized(TRUE)
          return()
        }
        analysis_result(NULL)
        analysis_needs_run(TRUE)
        analysis_in_progress(FALSE)
      }, ignoreNULL = FALSE)

      parse_p_value <- function(x) {
        if (is.null(x) || length(x) == 0) {
          return(NA_real_)
        }
        if (is.numeric(x)) {
          return(x[1])
        }
        value <- trimws(as.character(x[1]))
        if (!nzchar(value)) {
          return(NA_real_)
        }
        numeric_value <- suppressWarnings(as.numeric(value))
        if (is.na(numeric_value)) {
          if (grepl("^<\\s*", value)) {
            numeric_value <- suppressWarnings(as.numeric(sub("^<\\s*", "", value)))
          } else if (grepl("^>\\s*", value)) {
            numeric_value <- suppressWarnings(as.numeric(sub("^>\\s*", "", value)))
          }
        }
        numeric_value
      }

      format_number <- function(x, digits = 3, format = "g") {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) {
          return("\u2014")
        }
        value <- x[1]
        if (is.numeric(value)) {
          return(formatC(value, digits = digits, format = format))
        }
        as.character(value)
      }

      format_integer <- function(x) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) {
          return("\u2014")
        }
        value <- suppressWarnings(as.integer(x[1]))
        if (is.na(value)) {
          return(format_number(x))
        }
        format(value, big.mark = ",", trim = TRUE)
      }

      classify_decision <- function(p, alpha) {
        if (is.null(p) || !is.finite(p)) {
          return(list(status = "unknown", class = "badge-decision bg-secondary text-white", icon = shiny::icon("info-circle"), label = "Review details"))
        }
        tolerance <- max(0.005, alpha * 0.1)
        if (abs(p - alpha) <= tolerance) {
          return(list(status = "borderline", class = "badge-decision bg-warning text-dark", icon = shiny::icon("exclamation-triangle"), label = "Borderline"))
        }
        if (p < alpha) {
          return(list(status = "not_normal", class = "badge-decision bg-danger text-white", icon = shiny::icon("times-circle"), label = "Non-normal"))
        }
        list(status = "normal", class = "badge-decision bg-success text-white", icon = shiny::icon("check-circle"), label = "Normal")
      }

      build_spread_bar <- function(min_val, max_val, mean_val, sd_val) {
        values <- c(min_val, max_val, mean_val, sd_val)
        if (!all(is.finite(values[1:2])) || max_val <= min_val) {
          return(shiny::tags$div(class = "spread-bar"))
        }
        lower <- if (is.finite(sd_val)) mean_val - sd_val else mean_val
        upper <- if (is.finite(sd_val)) mean_val + sd_val else mean_val
        start <- max(min(lower, max_val), min_val)
        end <- min(max(upper, min_val), max_val)
        range_span <- max_val - min_val
        start_pct <- max(min((start - min_val) / range_span, 1), 0)
        end_pct <- max(min((end - min_val) / range_span, 1), start_pct)
        mean_pct <- max(min((mean_val - min_val) / range_span, 1), 0)
        width_pct <- max((end_pct - start_pct) * 100, 4)
        shiny::tags$div(
          class = "spread-bar",
          shiny::tags$div(
            class = "spread-bar-range",
            style = sprintf("left: %.2f%%; width: %.2f%%;", start_pct * 100, width_pct)
          ),
          shiny::tags$div(
            class = "spread-bar-mean",
            style = sprintf("left: %.2f%%;", mean_pct * 100)
          )
        )
      }

      format_table_cell <- function(value, column, alpha, highlight_p, badge_columns) {
        if (inherits(value, "shiny.tag")) {
          return(value)
        }
        if (is.null(value) || (length(value) == 1 && (is.na(value) || !nzchar(as.character(value))))) {
          return(shiny::HTML("&mdash;"))
        }
        if (column %in% badge_columns) {
          text <- as.character(value)
          lower_text <- tolower(text)
          class <- if (grepl("not", lower_text, fixed = TRUE)) {
            "badge text-bg-danger"
          } else if (grepl("normal", lower_text, fixed = TRUE)) {
            "badge text-bg-success"
          } else if (grepl("true", lower_text, fixed = TRUE)) {
            "badge text-bg-danger"
          } else {
            "badge text-bg-secondary"
          }
          return(shiny::tags$span(class = class, text))
        }
        if (highlight_p && grepl("p", column, ignore.case = TRUE)) {
          display <- format_number(value)
          numeric_value <- parse_p_value(value)
          cls <- if (!is.na(numeric_value) && numeric_value < alpha) "p-value-low" else "p-value-ok"
          return(shiny::tags$span(class = cls, display))
        }
        format_number(value)
      }

      build_results_table <- function(data, alpha, highlight_p = TRUE, caption = NULL, badge_columns = c("MVN", "Normality", "Outlier")) {
        df <- as.data.frame(data, stringsAsFactors = FALSE)
        if (!nrow(df)) {
          return(shiny::div(class = "alert alert-secondary", "No results available."))
        }
        columns <- names(df)
        header <- shiny::tags$tr(lapply(columns, shiny::tags$th))
        body <- lapply(seq_len(nrow(df)), function(i) {
          shiny::tags$tr(lapply(columns, function(col) {
            shiny::tags$td(format_table_cell(df[[col]][i], col, alpha, highlight_p, badge_columns))
          }))
        })
        table <- shiny::tags$table(
          class = "results-table table table-sm",
          shiny::tags$thead(header),
          shiny::tags$tbody(body)
        )
        table_wrapper <- shiny::div(class = "results-table-wrapper", table)
        if (!is.null(caption)) {
          return(shiny::tagList(shiny::tags$p(class = "text-muted small mb-2", caption), table_wrapper))
        }
        table_wrapper
      }

      render_descriptive_table <- function(desc_df) {
        df <- as.data.frame(desc_df, stringsAsFactors = FALSE)
        if (!nrow(df)) {
          return(shiny::div(class = "alert alert-secondary", "No descriptive statistics available."))
        }
        has_group <- "Group" %in% names(df)
        rows <- lapply(seq_len(nrow(df)), function(i) {
          group_val <- if (has_group) df$Group[i] else NULL
          cells <- list()
          if (has_group) {
            cells <- append(cells, list(shiny::tags$td(htmltools::htmlEscape(as.character(group_val)))))
          }
          mean_val <- suppressWarnings(as.numeric(df$Mean[i]))
          sd_val <- suppressWarnings(as.numeric(df$Std.Dev[i]))
          min_val <- suppressWarnings(as.numeric(df$Min[i]))
          max_val <- suppressWarnings(as.numeric(df$Max[i]))
          cells <- append(
            cells,
            list(
              shiny::tags$td(htmltools::htmlEscape(as.character(df$Variable[i]))),
              shiny::tags$td(format_integer(df$n[i])),
              shiny::tags$td(shiny::tags$span(class = "fw-semibold", sprintf("%s ± %s", format_number(mean_val), format_number(sd_val)))),
              shiny::tags$td(format_number(df$Median[i])),
              shiny::tags$td(format_number(min_val)),
              shiny::tags$td(format_number(max_val)),
              shiny::tags$td(build_spread_bar(min_val, max_val, mean_val, sd_val)),
              shiny::tags$td(format_number(df$Skew[i])),
              shiny::tags$td(format_number(df$Kurtosis[i]))
            )
          )
          shiny::tags$tr(cells)
        })
        header_labels <- c(if (has_group) "Group", "Variable", "n", "Mean ± SD", "Median", "Min", "Max", "Spread", "Skew", "Kurtosis")
        header <- shiny::tags$tr(lapply(header_labels, shiny::tags$th))
        shiny::div(
          class = "results-table-wrapper",
          shiny::tags$table(
            class = "results-table table table-sm",
            shiny::tags$thead(header),
            shiny::tags$tbody(rows)
          )
        )
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

      sanitize_for_id <- function(x) {
        if (is.null(x) || !nzchar(x)) {
          x <- "group"
        }
        x <- gsub("[^A-Za-z0-9]+", "_", x)
        x <- gsub("_+", "_", x)
        x <- gsub("^_+|_+$", "", x)
        if (!nzchar(x)) {
          x <- "group"
        }
        tolower(x)
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

      grouped_numeric_data <- shiny::reactive({
        res <- analysis_result()
        if (is.null(res)) {
          return(NULL)
        }
        get_grouped_numeric_data(res)
      })

      compute_plot_height <- function(num_vars, base = 260, per_row = 140) {
        if (is.null(num_vars) || !is.finite(num_vars) || num_vars <= 0) {
          return(paste0(base, "px"))
        }
        cols <- ceiling(sqrt(num_vars))
        rows <- ceiling(num_vars / cols)
        height <- base + (rows - 1) * per_row
        paste0(min(height, 900), "px")
      }
      summary_info <- shiny::reactive({
        res <- analysis_result()
        opts <- settings()
        if (is.null(res) || is.null(opts)) {
          return(NULL)
        }
        data <- res$data
        if (is.null(data)) {
          return(NULL)
        }
        df <- as.data.frame(data)
        group <- res$subset
        group_levels <- NULL
        if (!is.null(group) && nzchar(group) && group %in% names(df)) {
          group_levels <- length(unique(stats::na.omit(df[[group]])))
        } else {
          group <- NULL
        }
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (!is.null(group)) {
          numeric_cols <- setdiff(numeric_cols, group)
        }
        multivariate_tbl <- res$multivariate_normality
        p_display <- "\u2014"
        p_value <- NA_real_
        p_class <- NULL
        p_tooltip <- NULL
        decision_overall <- NULL
        decision_tooltip <- NULL
        group_summaries <- NULL
        if (!is.null(multivariate_tbl)) {
          df_p <- as.data.frame(multivariate_tbl, stringsAsFactors = FALSE)
          if (nrow(df_p)) {
            p_col <- intersect(c("p.value", "p_value", "p.value.skew"), names(df_p))
            if (length(p_col)) {
              value_column <- p_col[1]
              if (!is.null(group) && "Group" %in% names(df_p)) {
                groups_vec <- df_p$Group
                unique_groups <- unique(groups_vec)
                if (length(unique_groups)) {
                  group_entries <- list()
                  for (grp in unique_groups) {
                    idx <- which(groups_vec == grp)[1]
                    if (!length(idx) || is.na(idx)) {
                      next
                    }
                    raw_value <- df_p[[value_column]][idx]
                    display_value <- format_number(raw_value)
                    numeric_value <- parse_p_value(raw_value)
                    label <- format_group_label(grp)
                    decision_value <- classify_decision(numeric_value, opts$alpha)
                    group_entries[[length(group_entries) + 1L]] <- list(
                      group = label,
                      display = display_value,
                      numeric = numeric_value,
                      decision = decision_value
                    )
                  }
                  group_entries <- Filter(Negate(is.null), group_entries)
                  if (length(group_entries)) {
                    group_summaries <- group_entries
                    numeric_values <- vapply(group_entries, function(entry) entry$numeric, numeric(1))
                    finite_idx <- which(is.finite(numeric_values))
                    if (length(finite_idx)) {
                      min_idx <- finite_idx[which.min(numeric_values[finite_idx])]
                      max_idx <- finite_idx[which.max(numeric_values[finite_idx])]
                      min_display <- group_entries[[min_idx]]$display
                      max_display <- group_entries[[max_idx]]$display
                      min_value <- numeric_values[min_idx]
                      max_value <- numeric_values[max_idx]
                      tolerance <- max(0.001, opts$alpha * 0.05)
                      if (length(finite_idx) == 1 || abs(max_value - min_value) <= tolerance) {
                        p_value <- min_value
                        p_display <- min_display
                      } else {
                        p_value <- NA_real_
                        p_display <- sprintf("%s \u2013 %s", min_display, max_display)
                      }
                      if (!is.null(p_display) && !nzchar(p_display)) {
                        p_display <- "\u2014"
                      }
                      if (is.finite(p_value)) {
                        p_class <- if (p_value < opts$alpha) "p-value-low" else "p-value-ok"
                      } else {
                        decision_statuses <- vapply(group_entries[finite_idx], function(entry) entry$decision$status, character(1))
                        unique_statuses <- unique(decision_statuses)
                        if (length(unique_statuses) == 1) {
                          if (identical(unique_statuses, "not_normal")) {
                            p_class <- "p-value-low"
                          } else if (identical(unique_statuses, "normal")) {
                            p_class <- "p-value-ok"
                          } else {
                            p_class <- "p-value-mixed"
                          }
                        } else {
                          p_class <- "p-value-mixed"
                        }
                      }
                    } else {
                      first_raw <- df_p[[value_column]][1]
                      p_display <- format_number(first_raw)
                      p_value <- parse_p_value(first_raw)
                      if (is.finite(p_value)) {
                        p_class <- if (p_value < opts$alpha) "p-value-low" else "p-value-ok"
                      }
                    }
                    p_tooltip <- paste(
                      vapply(
                        group_entries,
                        function(entry) sprintf("%s: %s", entry$group, entry$display),
                        character(1)
                      ),
                      collapse = "\n"
                    )
                    decision_tooltip <- paste(
                      vapply(
                        group_entries,
                        function(entry) sprintf("%s: %s", entry$group, entry$decision$label),
                        character(1)
                      ),
                      collapse = "\n"
                    )
                    decision_statuses <- vapply(group_entries, function(entry) entry$decision$status, character(1))
                    known_idx <- which(decision_statuses != "unknown")
                    if (length(known_idx)) {
                      known_statuses <- unique(decision_statuses[known_idx])
                      if (length(known_statuses) == 1) {
                        decision_overall <- group_entries[[known_idx[1]]]$decision
                      } else {
                        decision_overall <- list(
                          status = "mixed",
                          class = "badge-decision badge-decision-mixed",
                          icon = shiny::icon("random"),
                          label = "Mixed results"
                        )
                      }
                    } else {
                      decision_overall <- list(
                        status = "unknown",
                        class = "badge-decision bg-secondary text-white",
                        icon = shiny::icon("info-circle"),
                        label = "Review details"
                      )
                    }
                  }
                }
              }
              if (is.null(group_summaries)) {
                raw <- df_p[[value_column]][1]
                p_display <- format_number(raw)
                p_value <- parse_p_value(raw)
                if (is.finite(p_value)) {
                  p_class <- if (p_value < opts$alpha) "p-value-low" else "p-value-ok"
                }
              }
            }
          }
        }
        if (is.null(decision_overall)) {
          decision_overall <- classify_decision(p_value, opts$alpha)
        }
        if (is.null(p_class)) {
          if (is.finite(p_value)) {
            p_class <- if (p_value < opts$alpha) "p-value-low" else "p-value-ok"
          }
        }
        outlier_tbl <- res$multivariate_outliers
        outlier_count <- if (is.null(outlier_tbl)) 0L else nrow(as.data.frame(outlier_tbl))
        test_label <- opts$test_label
        if (is.null(test_label) || is.na(test_label)) {
          test_label <- opts$mvn_test
        }
        list(
          n = nrow(df),
          p = length(numeric_cols),
          alpha = opts$alpha,
          p_value = p_value,
          p_display = p_display,
          p_class = p_class,
          p_tooltip = p_tooltip,
          decision = decision_overall,
          decision_tooltip = decision_tooltip,
          test_label = test_label,
          group = group,
          group_levels = group_levels,
          outlier_label = opts$outlier_label,
          outlier_count = outlier_count,
          cleaned_available = !is.null(res$new_data),
          group_summaries = group_summaries
        )
      })

      output$summary_n <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        format(info$n, big.mark = ",", trim = TRUE)
      })

      output$summary_p <- shiny::renderText({
        info <- summary_info()
        if (is.null(info)) {
          return("\u2014")
        }
        format(info$p, big.mark = ",", trim = TRUE)
      })

      output$summary_pvalue <- shiny::renderUI({
        info <- summary_info()
        if (is.null(info)) {
          return(shiny::tags$span(class = "text-muted", "\u2014"))
        }
        cls <- info$p_class
        if (is.null(cls) || !nzchar(cls)) {
          cls <- if (!is.null(info$p_value) && is.finite(info$p_value) && info$p_value < info$alpha) "p-value-low" else "p-value-ok"
        }
        span <- shiny::tags$span(info$p_display)
        span$attribs$class <- cls
        if (!is.null(info$p_tooltip) && nzchar(info$p_tooltip)) {
          span$attribs$title <- info$p_tooltip
        }
        span
      })

      output$summary_decision <- shiny::renderUI({
        info <- summary_info()
        if (is.null(info)) {
          return(shiny::tags$span(class = "badge-decision bg-secondary text-white", "Awaiting analysis"))
        }
        decision <- info$decision
        if (is.null(decision)) {
          decision <- classify_decision(info$p_value, info$alpha)
        }
        label <- decision$label
        if (identical(decision$status, "borderline") && is.finite(info$p_value)) {
          direction <- if (info$p_value < info$alpha) "below" else "above"
          label <- sprintf("Borderline (%s \u03b1)", direction)
        }
        badge <- shiny::tags$span(
          class = decision$class,
          shiny::tags$span(decision$icon, `aria-hidden` = "true"),
          shiny::tags$span(label)
        )
        if (!is.null(info$decision_tooltip) && nzchar(info$decision_tooltip)) {
          badge$attribs$title <- info$decision_tooltip
        }
        badge
      })

      output$summary_context <- shiny::renderUI({
        info <- summary_info()
        if (is.null(info)) {
          return(shiny::tags$span("Results will display after running the analysis."))
        }
        parts <- c(sprintf("%s test", info$test_label), sprintf("\u03b1 = %s", format(info$alpha, digits = 3)))
        if (!is.null(info$group)) {
          group_text <- if (!is.null(info$group_levels)) {
            sprintf("Grouping: %s (%d levels)", info$group, info$group_levels)
          } else {
            sprintf("Grouping: %s", info$group)
          }
          parts <- c(parts, group_text)
        }
        shiny::tags$span(htmltools::htmlEscape(paste(parts, collapse = " \u00b7 ")))
      })

      output$analysis_status <- shiny::renderUI({
        if (isTRUE(analysis_in_progress())) {
          return(shiny::div(
            class = "alert alert-info d-flex align-items-center gap-2",
            shiny::tags$span("\u23f3", `aria-hidden` = "true"),
            shiny::tags$span("Analysis in progress. Results will refresh automatically when complete.")
          ))
        }
        res <- analysis_result()
        if (is.null(res)) {
          if (isTRUE(analysis_needs_run())) {
            return(shiny::div(
              class = "alert alert-primary d-flex align-items-center gap-2",
              shiny::tags$span("\ud83e\uddea", `aria-hidden` = "true"),
              shiny::tags$span("Click Run analysis in the Analysis Settings tab to generate results.")
            ))
          }
          return(shiny::div(
            class = "alert alert-warning",
            "Results are unavailable for the current configuration. Re-run the analysis to refresh the output."
          ))
        }
        NULL
      })
      output$multivariate_content <- shiny::renderUI({
        res <- analysis_result()
        if (is.null(res)) {
          message <- if (isTRUE(analysis_needs_run())) {
            "Run the analysis to view multivariate normality diagnostics."
          } else {
            "Multivariate normality results are unavailable. Re-run the analysis to generate them."
          }
          return(shiny::div(class = "alert alert-secondary", message))
        }
        numeric_data <- get_numeric_data(res)
        if (is.null(numeric_data) || ncol(numeric_data) < 2) {
          return(shiny::div(class = "alert alert-warning", "At least two numeric variables are required to display multivariate diagnostics."))
        }
        info <- summary_info()
        shiny::tagList(
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Test statistics"),
            shiny::uiOutput(ns("multivariate_table"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Mahalanobis Q-Q plot"),
            shiny::uiOutput(ns("multivariate_qq_panel"))
          ),
          shiny::tags$details(
            shiny::tags$summary("Interpretation notes"),
            shiny::tags$p(sprintf(
              "%s at \u03b1 = %s. P-values below the threshold indicate departures from multivariate normality.",
              info$test_label,
              format(info$alpha, digits = 3)
            )),
            shiny::tags$p("Use the Q-Q plot to assess the overall fit before reviewing the numerical tests.")
          )
        )
      })
      
      output$multivariate_table <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_normality
        shiny::req(!is.null(tbl))
        info <- summary_info()
        alpha <- if (is.null(info)) 0.05 else info$alpha
        build_results_table(tbl, alpha = alpha, badge_columns = c("MVN"))
      })

      output$multivariate_scatter <- plotly::renderPlotly({
        res <- analysis_result()
        shiny::req(res)
        df <- get_numeric_data(res)
        shiny::req(df)
        shiny::req(ncol(df) >= 2)
        groups <- get_group_values(res)
        if (ncol(df) >= 3 && nrow(df) >= 3) {
          pcs <- tryCatch(stats::prcomp(df, center = TRUE, scale. = TRUE), error = function(e) NULL)
          if (!is.null(pcs) && ncol(pcs$x) >= 3) {
            coords <- pcs$x[, 1:3, drop = FALSE]
            plt <- plotly::plot_ly(
              x = coords[, 1],
              y = coords[, 2],
              z = coords[, 3],
              type = "scatter3d",
              mode = "markers",
              marker = list(size = 4, opacity = 0.7),
              color = if (!is.null(groups)) as.factor(groups) else NULL,
              colors = "Viridis"
            )
            return(plotly::layout(
              plt,
              scene = list(
                xaxis = list(title = "PC1"),
                yaxis = list(title = "PC2"),
                zaxis = list(title = "PC3")
              ),
              legend = list(title = list(text = res$subset %||% "Group"))
            ))
          }
        }
        plt <- plotly::plot_ly(
          x = df[[1]],
          y = df[[2]],
          type = "scatter",
          mode = "markers",
          marker = list(size = 8, opacity = 0.7),
          color = if (!is.null(groups)) as.factor(groups) else NULL,
          colors = "Viridis"
        )
        plotly::layout(
          plt,
          xaxis = list(title = colnames(df)[1]),
          yaxis = list(title = colnames(df)[2]),
          legend = list(title = list(text = res$subset %||% "Group"))
        )
      })

      output$multivariate_qq_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        df <- get_numeric_data(res)
        shiny::req(df)
        shiny::req(ncol(df) >= 2, nrow(df) >= 3)
        plot_obj <- tryCatch(
          MVN::multivariate_diagnostic_plot(df, type = "qq"),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, paste("Unable to generate Mahalanobis Q-Q plot:", e$message)))
          }
        )
        shiny::validate(shiny::need(!is.null(plot_obj), "Mahalanobis Q-Q plot is unavailable for the current dataset."))
        plot_obj
      })

      output$multivariate_qq_panel <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        numeric_data <- get_numeric_data(res)
        shiny::req(numeric_data)
        shiny::req(ncol(numeric_data) >= 2)
        groups <- grouped_numeric_data()
        plot_height <- "320px"
        if (is.null(groups) || !length(groups)) {
          return(shiny::plotOutput(ns("multivariate_qq_plot"), height = plot_height))
        }
        valid_groups <- Filter(function(entry) {
          is.list(entry) &&
            !is.null(entry$data) &&
            is.data.frame(entry$data) &&
            nrow(entry$data) >= 3 &&
            ncol(entry$data) >= 2
        }, groups)
        if (!length(valid_groups)) {
          return(shiny::plotOutput(ns("multivariate_qq_plot"), height = plot_height))
        }
        group_ui <- lapply(valid_groups, function(entry) {
          safe_id <- paste(entry$key, sanitize_for_id(entry$label), sep = "_")
          plot_id <- paste0("multivariate_qq_plot_", safe_id)
          local({
            id <- plot_id
            key <- entry$key
            output[[id]] <- shiny::renderPlot({
              groups_latest <- grouped_numeric_data()
              shiny::req(groups_latest)
              entry_latest <- groups_latest[[key]]
              shiny::req(entry_latest)
              df_group <- entry_latest$data
              shiny::req(ncol(df_group) >= 2, nrow(df_group) >= 3)
              plot_obj <- tryCatch(
                MVN::multivariate_diagnostic_plot(df_group, type = "qq"),
                error = function(e) {
                  shiny::validate(shiny::need(FALSE, paste("Unable to generate Mahalanobis Q-Q plot:", e$message)))
                }
              )
              shiny::validate(shiny::need(!is.null(plot_obj), "Mahalanobis Q-Q plot is unavailable for this subgroup."))
              plot_obj
            })
          })
          shiny::tags$div(
            class = "grouped-plot-block",
            shiny::tags$span(
              class = "grouped-plot-label",
              htmltools::htmlEscape(paste("Group:", entry$label))
            ),
            shiny::plotOutput(ns(plot_id), height = plot_height)
          )
        })
        shiny::tags$div(class = "grouped-plot-stack", group_ui)
      })

      `%||%` <- function(x, y) {
        if (!is.null(x) && length(x) == 1 && nzchar(x)) x else y
      }
      output$univariate_content <- shiny::renderUI({
        res <- analysis_result()
        if (is.null(res)) {
          message <- if (isTRUE(analysis_needs_run())) {
            "Run the analysis to view univariate diagnostics."
          } else {
            "Univariate results are unavailable. Re-run the analysis to generate them."
          }
          return(shiny::div(class = "alert alert-secondary", message))
        }
        numeric_data <- get_numeric_data(res)
        if (is.null(numeric_data) || ncol(numeric_data) < 1) {
          return(shiny::div(class = "alert alert-warning", "Select at least one numeric variable to display univariate diagnostics."))
        }
        shiny::tagList(
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Test statistics"),
            shiny::uiOutput(ns("univariate_table"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Histograms with normal curve"),
            shiny::uiOutput(ns("univariate_hist_panel"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Q-Q plots by variable"),
            shiny::uiOutput(ns("univariate_qq_panel"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Boxplots by variable"),
            shiny::uiOutput(ns("univariate_boxplot_panel"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Scatter plots by variable"),
            shiny::uiOutput(ns("univariate_scatter_panel"))
          ),
          shiny::tags$details(
            shiny::tags$summary("Interpretation notes"),
            shiny::tags$p("Histograms, Q-Q plots, and boxplots reveal departures from normality before you inspect test statistics."),
            shiny::tags$p("Scatter plots help spot trends or runs in the data, while highlighted p-values flag variables needing transformation or closer inspection.")
          )
        )
      })

      output$univariate_hist_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        data <- get_numeric_data(res)
        shiny::req(data)
        MVN::univariate_diagnostic_plot(data, type = "histogram", title = "Histograms with normal overlay")
      })

      output$univariate_qq_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        data <- get_numeric_data(res)
        shiny::req(data)
        MVN::univariate_diagnostic_plot(data, type = "qq", title = "Q-Q plots")
      })

      output$univariate_boxplot_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        data <- get_numeric_data(res)
        shiny::req(data)
        MVN::univariate_diagnostic_plot(data, type = "boxplot", title = "Boxplots by variable")
      })

      output$univariate_scatter_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        data <- get_numeric_data(res)
        shiny::req(data)
        MVN::univariate_diagnostic_plot(data, type = "scatter", title = "Scatter plots by variable")
      })

      render_grouped_univariate_panel <- function(panel_id, base_plot_id, plot_type, base_title) {
        output[[panel_id]] <- shiny::renderUI({
          res <- analysis_result()
          shiny::req(res)
          numeric_data <- get_numeric_data(res)
          shiny::req(numeric_data)
          plot_height <- compute_plot_height(ncol(numeric_data))
          groups <- grouped_numeric_data()
          if (is.null(groups) || !length(groups)) {
            return(shiny::plotOutput(ns(base_plot_id), height = plot_height))
          }
          valid_groups <- Filter(function(entry) {
            is.list(entry) &&
              !is.null(entry$data) &&
              is.data.frame(entry$data) &&
              nrow(entry$data) > 0 &&
              ncol(entry$data) > 0
          }, groups)
          if (!length(valid_groups)) {
            return(shiny::plotOutput(ns(base_plot_id), height = plot_height))
          }
          group_ui <- lapply(valid_groups, function(entry) {
            safe_id <- paste(entry$key, sanitize_for_id(entry$label), sep = "_")
            plot_id <- paste0(base_plot_id, "_", safe_id)
            local({
              id <- plot_id
              key <- entry$key
              output[[id]] <- shiny::renderPlot({
                groups_latest <- grouped_numeric_data()
                shiny::req(groups_latest)
                entry_latest <- groups_latest[[key]]
                shiny::req(entry_latest)
                MVN::univariate_diagnostic_plot(
                  entry_latest$data,
                  type = plot_type,
                  title = sprintf("%s \u2014 %s", base_title, entry_latest$label)
                )
              })
            })
            shiny::tags$div(
              class = "grouped-plot-block",
              shiny::tags$span(
                class = "grouped-plot-label",
                htmltools::htmlEscape(paste("Group:", entry$label))
              ),
              shiny::plotOutput(ns(plot_id), height = plot_height)
            )
          })
          shiny::tags$div(class = "grouped-plot-stack", group_ui)
        })
      }

      render_grouped_univariate_panel("univariate_hist_panel", "univariate_hist_plot", "histogram", "Histograms with normal overlay")
      render_grouped_univariate_panel("univariate_qq_panel", "univariate_qq_plot", "qq", "Q-Q plots")
      render_grouped_univariate_panel("univariate_boxplot_panel", "univariate_boxplot_plot", "boxplot", "Boxplots by variable")
      render_grouped_univariate_panel("univariate_scatter_panel", "univariate_scatter_plot", "scatter", "Scatter plots by variable")

      output$univariate_table <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$univariate_normality
        shiny::req(!is.null(tbl))
        info <- summary_info()
        alpha <- if (is.null(info)) 0.05 else info$alpha
        build_results_table(tbl, alpha = alpha, badge_columns = c("Normality"))
      })

      output$descriptives_content <- shiny::renderUI({
        res <- analysis_result()
        opts <- settings()
        if (is.null(res) || is.null(opts)) {
          return(shiny::div(class = "alert alert-secondary", "Run the analysis to compute descriptive statistics."))
        }
        if (!isTRUE(opts$descriptives)) {
          return(shiny::div(class = "alert alert-info", "Enable descriptive statistics in the Analysis Settings tab to view this section."))
        }
        tbl <- res$descriptives
        if (is.null(tbl)) {
          return(shiny::div(class = "alert alert-warning", "Descriptive statistics were not returned. Re-run the analysis to compute them."))
        }
        info <- summary_info()
        shiny::tagList(
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Variable summary"),
            shiny::uiOutput(ns("descriptives_table"))
          ),
          shiny::tags$details(
            shiny::tags$summary("Interpretation notes"),
            shiny::tags$p("Bars illustrate mean ± SD within the observed range. Skew and kurtosis help detect asymmetry and heavy tails."),
            if (isTRUE(info$cleaned_available)) {
              shiny::tags$p("A cleaned dataset excluding flagged multivariate outliers is available in the Outlier diagnostics section.")
            }
          )
        )
      })

      output$descriptives_table <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$descriptives
        shiny::req(!is.null(tbl))
        render_descriptive_table(tbl)
      })

      output$outlier_content <- shiny::renderUI({
        res <- analysis_result()
        if (is.null(res)) {
          message <- if (isTRUE(analysis_needs_run())) {
            "Run the analysis to compute outlier diagnostics."
          } else {
            "Outlier diagnostics are unavailable. Re-run the analysis to generate them."
          }
          return(shiny::div(class = "alert alert-secondary", message))
        }
        numeric_data <- get_numeric_data(res)
        if (is.null(numeric_data) || ncol(numeric_data) < 1) {
          return(shiny::div(class = "alert alert-warning", "Select numeric variables to review outlier diagnostics."))
        }
        info <- summary_info()
        outlier_plot_title <- if (!is.null(info) && !is.null(info$outlier_label)) {
          paste(info$outlier_label, "Q-Q plot")
        } else {
          "Outlier Q-Q plot"
        }
        shiny::tagList(
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", outlier_plot_title),
            shiny::uiOutput(ns("outlier_distance_panel"))
          ),
          shiny::div(
            class = "visual-block",
            shiny::tags$h6(class = "fw-semibold text-muted mb-2", "Flagged observations"),
            shiny::uiOutput(ns("outlier_table"))
          ),
          shiny::tags$details(
            shiny::tags$summary("Interpretation notes"),
            shiny::tags$p(sprintf("Outlier detection uses the %s method.", info$outlier_label)),
            shiny::tags$p("Review the Q-Q plot for points deviating from the reference line and examine flagged cases below.")
          )
        )
      })

      output$outlier_distance_plot <- shiny::renderPlot({
        res <- analysis_result()
        shiny::req(res)
        df <- get_numeric_data(res)
        shiny::req(df)
        shiny::req(ncol(df) >= 2)
        opts <- settings()
        shiny::req(opts)
        method <- opts$outlier_method %||% "quan"
        alpha <- opts$alpha %||% 0.05
        plot_result <- tryCatch(
          MVN::mv_outlier(
            df,
            method = method,
            alpha = alpha,
            outlier = FALSE
          ),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, paste("Unable to generate outlier diagnostics plot:", e$message)))
          }
        )
        plot_obj <- plot_result$qq_outlier_plot
        shiny::validate(shiny::need(!is.null(plot_obj), "Outlier diagnostics plot is unavailable for the current dataset."))
        plot_obj
      })

      output$outlier_distance_panel <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        numeric_data <- get_numeric_data(res)
        shiny::req(numeric_data)
        shiny::req(ncol(numeric_data) >= 2)
        groups <- grouped_numeric_data()
        plot_height <- "320px"
        if (is.null(groups) || !length(groups)) {
          return(shiny::plotOutput(ns("outlier_distance_plot"), height = plot_height))
        }
        valid_groups <- Filter(function(entry) {
          is.list(entry) &&
            !is.null(entry$data) &&
            is.data.frame(entry$data) &&
            nrow(entry$data) >= 2 &&
            ncol(entry$data) >= 2
        }, groups)
        if (!length(valid_groups)) {
          return(shiny::plotOutput(ns("outlier_distance_plot"), height = plot_height))
        }
        opts <- settings()
        shiny::req(opts)
        method <- opts$outlier_method %||% "quan"
        alpha <- opts$alpha %||% 0.05
        group_ui <- lapply(valid_groups, function(entry) {
          safe_id <- paste(entry$key, sanitize_for_id(entry$label), sep = "_")
          plot_id <- paste0("outlier_distance_plot_", safe_id)
          local({
            id <- plot_id
            key <- entry$key
            output[[id]] <- shiny::renderPlot({
              groups_latest <- grouped_numeric_data()
              shiny::req(groups_latest)
              entry_latest <- groups_latest[[key]]
              shiny::req(entry_latest)
              df_group <- entry_latest$data
              shiny::req(ncol(df_group) >= 2)
              plot_result <- tryCatch(
                MVN::mv_outlier(
                  df_group,
                  method = method,
                  alpha = alpha,
                  outlier = FALSE
                ),
                error = function(e) {
                  shiny::validate(shiny::need(FALSE, paste("Unable to generate outlier diagnostics plot:", e$message)))
                }
              )
              plot_obj <- plot_result$qq_outlier_plot
              shiny::validate(shiny::need(!is.null(plot_obj), "Outlier diagnostics plot is unavailable for this subgroup."))
              plot_obj
            })
          })
          shiny::tags$div(
            class = "grouped-plot-block",
            shiny::tags$span(
              class = "grouped-plot-label",
              htmltools::htmlEscape(paste("Group:", entry$label))
            ),
            shiny::plotOutput(ns(plot_id), height = plot_height)
          )
        })
        shiny::tags$div(class = "grouped-plot-stack", group_ui)
      })

      output$outlier_table <- shiny::renderUI({
        res <- analysis_result()
        shiny::req(res)
        tbl <- res$multivariate_outliers
        if (is.null(tbl)) {
          return(shiny::div(class = "alert alert-info", "Enable a multivariate outlier method to view flagged cases."))
        }
        df <- as.data.frame(tbl)
        if (!nrow(df)) {
          return(shiny::div(class = "alert alert-success", "No multivariate outliers were detected."))
        }
        build_results_table(df, alpha = 0.05, highlight_p = FALSE, badge_columns = c("Outlier"))
      })
      # output$analysis_summary <- shiny::renderPrint({
      #   res <- analysis_result()
      #   shiny::req(res)
      #   summary(res, select = "mvn")
      # })

      list(result = analysis_result)
    }
  )
}
