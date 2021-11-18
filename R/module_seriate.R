
# UI ===========================================================================
#' Seriate UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_seriate_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Seriate",
    icon = icon("sort-amount-up"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
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
            ## Input: seriate
            actionButton(
              inputId = ns("go_seriate"),
              label = "Seriate",
              style = "margin-bottom: 15px;"
            ),
            ## Input: select plot
            radioButtons(
              inputId = ns("plot_type"),
              label = "Display",
              choices = c(Bertin = "bertin", Ford = "ford", Heatmap = "heat"),
              selected = "heat"
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          ## Output: permutation summary
          # verbatimTextOutput(outputId = ns("summary")),
          fluidRow(
            tabsetPanel(
              id = ns("plot"),
              type = "tabs",
              tabPanel(
                title = "Raw data",
                value = "panel_raw",
                style = "margin-top: 15px;",
                ## Output: download
                downloadButton(outputId = ns("export_plot_data"),
                               label = "Export plot"),
                ## Output: plot raw matrix
                plotOutput(outputId = ns("plot_data"))
              ),
              tabPanel(
                title = "Rearranged matrix",
                value = "panel_permute",
                style = "margin-top: 15px;",
                ## Output: download
                downloadButton(outputId = ns("export_plot_perm"),
                               label = "Export plot"),
                downloadButton(outputId = ns("export_table"),
                               label = "Export matrix"),
                ## Output: plot reordered matrix
                plotOutput(outputId = ns("plot_permute"))
              )
            ) # tabsetPanel
          )
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}

# Server =======================================================================
#' Seriate Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_seriate_ui()]
#' @family server modules
#' @keywords internal
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
      plot_data() + scale_picker(user_settings$col_sequential, "fill")
    })
    output$plot_permute <- renderPlot({
      plot_permute() + scale_picker(user_settings$col_sequential, "fill")
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
