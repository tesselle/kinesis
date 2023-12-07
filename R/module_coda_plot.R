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
#' @param user_data A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [module_coda_plot_ui()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_plot_server <- function(id, user_data) {
  stopifnot(is.reactive(user_data))

  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    plot_bar <- reactive({
      nexus::barplot(user_data())
      grDevices::recordPlot()
    })

    ## Output ------------------------------------------------------------------
    output$plot <- renderPlot({
      grDevices::replayPlot(plot_bar())
    }, height = function() { getCurrentOutputInfo(session)$width() / 2 } )
  })
}
