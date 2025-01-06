# UI ===========================================================================
#' Beta Diversity UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [diversity_beta_server()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_beta_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Principal Coordinates Analysis"),
      selectInput(
        inputId = ns("method"),
        label = "Dissimilarity measure",
        choices = c(`Bray-Curtis` = "bray", `Morisita-Horn` = "morisita"),
        multiple = FALSE
      ),
      bslib::input_task_button(id = ns("go"), label = "(Re)Compute"),
      downloadButton(
        outputId = ns("download_beta"),
        label = "Download dissimilarity matrix"
      ),
      downloadButton(
        outputId = ns("download_pcoa"),
        label = "Download PCoA results"
      ),
      hr(),
      ## Input: quantitative variable mapping
      selectizeInput(
        inputId = ns("extra_quanti"),
        label = "Alpha diversity",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
      checkboxInput(
        inputId = ns("pcoa_labels"),
        label = "Display labels",
        value = FALSE
      )
    ), # sidebar
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      output_plot(
        id = ns("plot_diss"),
        title = "Dissimilarity",
        tools = list(
          select_color(inputId = ns("col_diss"), type = "sequential", default = "YlOrBr")
        ),
        height = "100%"
      ),
      output_plot(
        id = ns("plot_pcoa"),
        title = "PCoA",
        tools = list(
          select_color(inputId = ns("col_pcoa"), type = "sequential", default = "YlOrBr"),
          select_cex(inputId = ns("cex_pcoa"), default = c(1, 1))
        ),
        height = "100%"
      )
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' Beta Diversity Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param y A reactive `data.frame` returned by [diversity_alpha_server()].
#' @return A reactive [`data.frame`].
#' @seealso [diversity_beta_ui()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_beta_server <- function(id, x, y) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    observe({
      freezeReactiveValue(input, "extra_quanti")
      updateSelectInput(session, inputId = "extra_quanti",
                        choices = c(None = "", colnames(y())))
    }) |>
      bindEvent(y())

    ## Bookmark -----
    onRestored(function(state) {
      updateSelectizeInput(session, inputId = "extra_quanti",
                           selected = state$input$extra_quanti)
    })

    ## Compute similarity -----
    compute_beta <- ExtendedTask$new(
      function(x, method) {
        promises::future_promise({
          1 - tabula::similarity(x, method = method)
        })
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_beta$invoke(x(), input$method)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_beta$result(), title = "Beta Diversity")
    })

    ## Compute PCoA -----
    analysis <- reactive({
      req(results())
      validate_na(results())
      notify(dimensio::pcoa(results(), rank = 2))
    })

    ## Plot -----
    plot_diss <- reactive({
      req(results())
      function() {
        tabula::plot_heatmap(
          object = results(),
          color = get_color(input$col_diss),
          diag = FALSE,
          upper = FALSE,
          fixed_ratio = TRUE
        )
      }
    })

    plot_pcoa <- reactive({
      req(analysis(), y())

      extra_quanti <- NULL
      if (isTruthy(input$extra_quanti))
        extra_quanti <- y()[[input$extra_quanti]]

      function() {
        dimensio::plot(
          x = analysis(),
          labels = input$pcoa_labels,
          extra_quanti = extra_quanti,
          color = get_color(input$col_pcoa),
          size = get_value(input$cex_pcoa)
        )
      }
    })

    ## Render plot -----
    render_plot("plot_diss", x = plot_diss)
    render_plot("plot_pcoa", x = plot_pcoa)

    ## Download -----
    output$download_beta <- export_table(results, "beta")
  })
}
