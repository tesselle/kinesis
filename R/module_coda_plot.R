# UI ===========================================================================
#' Bar Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_plot_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_plot_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h5("Compositional barplot"),
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
      )
    ), # sidebarPanel
    mainPanel(
      output_plot(
        id = ns("plot"),
        tools = select_color(
          inputId = ns("pal_qualitative"),
          type = "qualitative"
        ),
        height = "auto",
        title = "Barplot"
      )
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Bar Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_plot_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_plot_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build barplot -----
    bindEvent(
      observe({
        freezeReactiveValue(input, "order")
        updateSelectInput(inputId = "order", choices = colnames(x()))
      }),
      x()
    )
    plot_pal <- reactive({
      get_color(input$pal_qualitative, NCOL(x()))
    })
    plot_bar <- reactive({
      req(x())
      nexus::barplot(x(), order = input$order, decreasing = input$decreasing,
                     col = plot_pal())
      grDevices::recordPlot()
    })

    ## Render barplot -----
    render_plot("plot", x = plot_bar, ratio = 0.5)
  })
}
