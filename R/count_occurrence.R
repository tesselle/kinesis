# UI ===========================================================================
#' Co-Occurrence UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [occurrence_server()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Co-Occurrence"),
      radioButtons(
        inputId = ns("method"),
        label = "Method",
        choices = c(`Absolute frequency` = "absolute",
                    `Relative frequency` = "relative",
                    `Binomial assessment` = "binomial"),
      ),
      radioButtons(
        inputId = ns("plot_type"),
        label = "Plot type",
        choices = c("Heatmap", "Spot"),
      ),
      bslib::input_task_button(id = ns("go"), label = "(Re)Compute"),
      downloadButton(
        outputId = ns("download"),
        label = "Download results"
      )
    ), # sidebar
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      output_plot(
        id = ns("plot"),
        tools = select_color(
          inputId = ns("color"),
          type = c("sequential", "diverging"),
          default = "YlOrBr"
        )
      ),
      card(
        gt::gt_output(outputId = ns("table"))
      )
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' Co-Occurrence Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive [`data.frame`].
#' @seealso [occurrence_ui()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Compute index -----
    compute_occur <- ExtendedTask$new(
      function(x, method) {
        promises::future_promise({
          tabula::occurrence(x, method = tolower(method))
        })
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_occur$invoke(x(), input$method)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_occur$result(), title = "Co-Occurrence")
    })

    ## Plot -----
    map <- reactive({
      req(results())
      fun <- switch(
        input$plot_type,
        color = "",
        Heatmap = function(x, ...) tabula::plot_heatmap(x, ...),
        Spot = function(x, ...) tabula::plot_spot(x, ...)
      )

      function() fun(results(), color = khroma::color(input$color))
    })

    ## Render table -----
    output$table <- gt::render_gt({
      req(results())
      results() |>
        as.matrix() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 2) |>
        gt::sub_missing()
    })

    ## Render plot -----
    render_plot("plot", x = map)

    ## Download -----
    output$download <- export_table(results, "occurrence")
  })
}
