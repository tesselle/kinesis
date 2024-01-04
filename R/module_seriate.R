
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

  sidebarLayout(
    sidebarPanel(
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
    ), # sidebarPanel
    mainPanel(
      ## Output: permutation summary
      # verbatimTextOutput(outputId = ns("summary")),
      tabsetPanel(
        id = ns("plot"),
        type = "tabs",
        tabPanel(
          title = "Rearranged matrix",
          ## Output: plot reordered matrix
          output_plot(id = ns("plot_permute"), height = "auto", title = "Ford plot")
        ),
        tabPanel(
          title = "Raw data",
          ## Output: plot raw matrix
          output_plot(id = ns("plot_raw"), height = "auto", title = "Ford plot")
        )
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Seriate Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [data_server()]).
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
    fun_plot <- reactive({
      function(x) tabula::plot_ford(x, weights = input$weights, EPPM = input$eppm)
    })
    plot_raw <- reactive({
      req(x())
      fun_plot()(x())
      grDevices::recordPlot()
    })
    plot_permute <- reactive({
      req(data_permute())
      fun_plot()(data_permute())
      grDevices::recordPlot()
    })

    ## Render plot -----
    render_plot("plot_raw", x = plot_raw, ratio = 0.5)
    render_plot("plot_permute", x = plot_permute, ratio = 0.5)

    ## Download -----
    output$export_table <- export_table(permuted = data_permute, name = "matrix_permuted")

    data_seriate
  })
}
