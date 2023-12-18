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
      ),
      select_color(
        inputId = ns("col"),
        type = "qualitative"
      )
    ), # sidebarPanel
    mainPanel(
      output_plot(id = ns("plot"), height = "auto", title = "Barplot")
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

    ## Build barplot -----
    plot_bar <- reactive({
      col <- get_color(input$col, n = ncol(x()))

      nexus::barplot(x(), order = input$order, decreasing = input$decreasing)
      grDevices::recordPlot()
    })

    ## Render barplot -----
    render_plot("plot", x = plot_bar, height = function() { getCurrentOutputInfo(session)$width() / 2 })
  })
}
