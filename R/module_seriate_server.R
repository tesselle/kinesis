#' Seriate Server
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_seriate_ui()]
#' @family server modules
#' @export
module_seriate_server <- function(input, output, session,
                                  user_data, user_settings) {
  ## Reactive ------------------------------------------------------------------
  data_seriate <- eventReactive(input$go_seriate, {
    req(user_data$data)
    margin <- NULL
    if (input$margin_row) margin <- c(margin, 1)
    if (input$margin_col) margin <- c(margin, 2)
    tabula::seriate_average(user_data$data, margin = margin, axes = input$axes)
  })
  data_permute <- reactive({
    req(user_data$data)
    req(data_seriate())
    tabula::permute(user_data$data, data_seriate())
  })
  plot_data <- reactive({
    req(user_data$data)
    tabula::plot_ford(user_data$data)
  })
  plot_permute <- reactive({
    req(data_permute())
    tabula::plot_ford(data_permute())
  })
  observeEvent(data_permute(), {
    updateTabsetPanel(session, inputId = "plot", selected = "panel_permute")
  })
  ## Render --------------------------------------------------------------------
  output$summary <- renderPrint({
    data_seriate()
  })
  output$plot_data <- renderPlot({
    plot_data()
  })
  output$plot_permute <- renderPlot({
    plot_permute()
  })
}
