# UI ===========================================================================
#' Bar Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_coda_plot_server()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_plot_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("order"),
        label = "Order",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      checkboxInput(
        inputId = ns("decreasing"),
        label = "Decreasing order",
        value = FALSE
      ),
      select_color(
        inputId = ns("col"),
        type = "qualitative"
      ),
      downloadButton(outputId = ns("export"), label = "Export plot")
    ), # sidebarPanel
    mainPanel(
      plotOutput(outputId = ns("plot"), height = "auto")
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Bar Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [module_coda_plot_ui()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_plot_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Observe -----------------------------------------------------------------
    observeEvent(x(), {
      updateSelectInput(session, inputId = "order", choices = colnames(x()))
    })

    ## Reactive ----------------------------------------------------------------
    plot_bar <- reactive({
      col <- get_color(input$col, n = ncol(x()))

      nexus::barplot(x(), order = input$order, decreasing = input$decreasing)
      grDevices::recordPlot()
    })

    ## Output ------------------------------------------------------------------
    output$plot <- renderPlot({
      grDevices::replayPlot(plot_bar())
    }, height = function() { getCurrentOutputInfo(session)$width() / 2 } )

    ## Download ----------------------------------------------------------------
    output$export <- export_plot(plot_bar, name = "barplot")
  })
}
