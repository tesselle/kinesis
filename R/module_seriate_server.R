#' Seriate Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_seriate_ui()]
#' @family server modules
#' @export
module_seriate_server  <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    data_seriate <- eventReactive(input$go_seriate, {
      req(user_data$data)
      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)
      tabula::seriate_average(user_data$data, margin = margin,
                              axes = input$axes)
    })
    data_permute <- reactive({
      req(user_data$data)
      req(data_seriate())
      tabula::permute(user_data$data, data_seriate())
    })
    plot_data <- reactive({
      req(user_data$data)
      req(input$plot_type)
      mtx_plot <- switch(
        input$plot_type,
        heat = function(x) tabula::plot_heatmap(arkhe::as_composition(x)),
        ford = tabula::plot_ford,
        bertin = tabula::plot_bertin
      )
      mtx_plot(user_data$data)
    })
    plot_permute <- reactive({
      req(data_permute())
      req(input$plot_type)
      mtx_plot <- switch(
        input$plot_type,
        heat = function(x) tabula::plot_heatmap(arkhe::as_composition(x)),
        ford = tabula::plot_ford,
        bertin = tabula::plot_bertin
      )
      mtx_plot(data_permute())
    })
    observeEvent(data_permute(), {
      updateTabsetPanel(session, inputId = "plot", selected = "panel_permute")
    })
    ## Render ------------------------------------------------------------------
    output$summary <- renderPrint({
      data_seriate()
    })
    output$plot_data <- renderPlot({
      plot_data()
    })
    output$plot_permute <- renderPlot({
      plot_permute()
    })
    ## Download ----------------------------------------------------------------
    output$export_plot_data <- module_export_plot(
      "plot_matrix_raw", "matrix_raw", plot_data(), user_settings)
    output$export_plot_perm <- module_export_plot(
      "plot_matrix_permuted", "matrix_permuted", plot_permute(), user_settings)
    output$export_table <- module_export_table(
      "matrix_permuted", "matrix_permuted", data_permute())
  })
}
