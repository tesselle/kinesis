#' Correspondence Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_ca_ui()]
#' @family server modules
#' @export
module_ca_server <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    data_ca <- reactive({
      req(user_data$data)
      dimensio::ca(user_data$data, rank = input$rank,
                   sup_row = input$sup_row, sup_col = input$sup_col)
    })
    boot_ca <- eventReactive(input$go_bootrstrap, {
      req(data_ca())
      req(input$replicates)

      dimensio::bootstrap(data_ca(), n = input$replicates)
    })
    plot_results <- reactive({
      req(data_ca())
      req(input$axis1)
      req(input$axis2)
      ca_plot <- switch(
        input$margin,
        "1" = dimensio::plot_rows,
        "2" = dimensio::plot_columns
      )
      ca_plot(data_ca(), axes = as.numeric(c(input$axis1, input$axis2)))
    })
    plot_variance <- reactive({
      req(data_ca())
      dimensio::plot_variance(data_ca())
    })
    plot_contrib1 <- reactive({
      req(data_ca())
      req(input$axis1)
      dimensio::plot_contributions(data_ca(), margin = as.numeric(input$margin),
                                   axes = as.numeric(input$axis1))
    })
    plot_contrib2 <- reactive({
      req(data_ca())
      req(input$axis2)
      dimensio::plot_contributions(data_ca(), margin = as.numeric(input$margin),
                                   axes = as.numeric(input$axis2))
    })
    ## Observe -----------------------------------------------------------------
    observeEvent(data_ca(), {
      choices <- seq_len(dim(data_ca()))
      names(choices) <- unique(rownames(dimensio::get_eigenvalues(data_ca())))
      updateSelectizeInput(session, inputId = "axis1", choices = choices)
    })
    observeEvent(input$axis1, {
      choices <- seq_len(dim(data_ca()))
      names(choices) <- unique(rownames(dimensio::get_eigenvalues(data_ca())))

      choices <- choices[-as.numeric(input$axis1)]
      updateSelectizeInput(session, inputId = "axis2", choices = choices)
    })
    ## Render ------------------------------------------------------------------
    output$variance <- renderTable({
      dimensio::get_eigenvalues(data_ca())
    }, striped = TRUE, hover = FALSE, rownames = TRUE, colnames = TRUE)
    output$contrib <- renderTable({
      dimensio::get_contributions(data_ca())
    }, striped = TRUE, hover = FALSE, rownames = TRUE, colnames = TRUE)
    output$summary <- renderPrint({
      dimensio::summary(data_ca(), margin = as.numeric(input$margin))
    })
    output$plot_results <- renderPlot({
      plot_results() +
        ggplot2::theme_bw() +
        scale_picker(user_settings$col_qualitative, "colour")
    })
    output$plot_contrib1 <- renderPlot({
      plot_contrib1() +
        ggplot2::theme_bw()
    })
    output$plot_contrib2 <- renderPlot({
      plot_contrib2() +
        ggplot2::theme_bw()
    })
    output$plot_variance <- renderPlot({
      plot_variance() +
        ggplot2::theme_bw()
    })
  })
}
