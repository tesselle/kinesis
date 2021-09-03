#' Visualize Server
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_visualize_ui()]
#' @family Server modules
#' @export
module_visualize_server <- function(input, output, session,
                                    user_data, user_settings) {
  ## Observe -------------------------------------------------------------------
  observeEvent(input$data, {
    req(input$data)
    types <- c(Ford = "ford", Heatmap = "heatmap")
    choices <- switch(
      input$data,
      count = types,
      composition = types[2],
      incidence = types[2],
      occurrence = types[2]
    )
    updateSelectInput(inputId = "select", choices = choices)
  })
  observeEvent(input$select, {
    req(input$select)
    updateTabsetPanel(inputId = "parameters", selected = input$select)
  })
  ## Reactive ------------------------------------------------------------------
  data <- reactive({
    req(user_data$data)
    count <- user_data$data
    switch(
      input$data,
      composition = arkhe::as_composition(count),
      incidence = arkhe::as_incidence(count),
      occurrence = arkhe::as_occurrence(count),
      count
    )
  })
  plot_data <- reactive({
    req(user_data$data)
    req(input$select)

    fun_plot <- switch(
      input$select,
      ford = tabula::plot_ford,
      heatmap = tabula::plot_heatmap
    )
    fun_plot(data())
  })
  ## Output --------------------------------------------------------------------
  output$plot <- renderPlot(plot_data())
}
