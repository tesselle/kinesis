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
        choices = c("Count", "Binomial"),
      ),
      radioButtons(
        inputId = ns("plot_type"),
        label = "Plot type",
        choices = c("Heatmap", "Spot"),
      ),
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
    occur <- reactive({
      req(x())
      validate_na(x())
      tabula::occurrence(x(), method = tolower(input$method))
    })

    ## Plot -----
    map <- reactive({
      req(occur())
      fun <- switch(
        input$plot_type,
        color = "",
        Heatmap = function(x, ...) tabula::plot_heatmap(x, ...),
        Spot = function(x, ...) tabula::plot_spot(x, ...)
      )

      function() fun(occur(), color = khroma::color(input$color))
    })

    ## Render table -----
    output$table <- gt::render_gt({
      req(occur())
      occur() |>
        as.matrix() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 2) |>
        gt::sub_missing()
    })

    ## Render plot -----
    render_plot("plot", x = map)

    ## Download -----
    output$download <- export_table(occur, "occurrence")
  })
}
