
# UI ===========================================================================
#' Seriate UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [seriate_server()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      ## Input: checkbox if permute rows
      checkboxInput(
        inputId = ns("margin_row"),
        label = "Permute rows",
        value = TRUE
      ),
      ## Input: checkbox if permute columns
      checkboxInput(
        inputId = ns("margin_col"),
        label = "Permute columns",
        value = TRUE
      ),
      ## Input: select CA axes
      numericInput(
        inputId = ns("axes"),
        label = "CA dimension",
        value = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      ## Input: select plot
      checkboxInput(
        inputId = ns("eppm"),
        label = "EPPM",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("weights"),
        label = "weights",
        value = FALSE
      ),
      ## Output: download
      downloadButton(outputId = ns("export_table"),
                     label = "Export matrix")
    ), # sidebar
    ## Output: plot reordered matrix
    output_plot(id = ns("plot_permute"), height = "100%", title = "Rearranged matrix")
  ) # layout_sidebar
}

# Server =======================================================================
#' Seriate Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive [`kairos::AveragePermutationOrder-class`] object.
#' @seealso [seriate_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_server  <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Seriate -----
    data_seriate <- reactive({
      req(x())
      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)

      run_with_notification(
        {
          kairos::seriate_average(
            object = x(),
            margin = margin,
            axes = input$axes
          )
        },
        what = "Seriate"
      )
    })

    ## Permute -----
    data_permute <- reactive({
      req(data_seriate())
      kairos::permute(x(), data_seriate())
    })

    ## Plot -----
    plot_permute <- reactive({
      req(data_permute())
      tabula::plot_ford(data_permute(), weights = input$weights, EPPM = input$eppm)
      grDevices::recordPlot()
    })

    ## Render plot -----
    render_plot("plot_permute", x = plot_permute)

    ## Download -----
    output$export_table <- export_table(data_permute, name = "permuted")

    data_seriate
  })
}
