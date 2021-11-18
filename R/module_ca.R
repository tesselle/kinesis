
# UI ===========================================================================
#' Correspondence Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_ca_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_ca_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Analyse",
    icon = icon("chart-bar"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            ## Input: select CA axes
            selectizeInput(
              inputId = ns("axis1"),
              label = "Horizontal axis",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
            ),
            selectizeInput(
              inputId = ns("axis2"),
              label = "Vertical axis",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
            ),
            ## Input: select margin
            radioButtons(
              inputId = ns("margin"),
              label = "Display",
              choices = c(Rows = 1, Columns = 2),
              selected = 1
            )
          ), # wellPanel
          wellPanel(
            ## Input: select bootstrat replicates
            numericInput(
              inputId = ns("replicates"),
              label = "Replicates",
              value = 500,
              min = 10,
              max = NA,
              step = 10
            ),
            ## Input: run bootstrap
            actionButton(
              inputId = ns("go_bootrstrap"),
              label = "Bootstrap"
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          tabsetPanel(
            id = ns("results"),
            type = "tabs",
            tabPanel(
              title = "Results",
              value = "panel_results",
              style = "margin-top: 15px;",
              ## Output: plot coordinates
              plotOutput(outputId = ns("plot_results"))
            ),
            tabPanel(
              title = "Contributions",
              value = "panel_contrib",
              style = "margin-top: 15px;",
              fluidRow(
                ## Output: contribution
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  plotOutput(outputId = ns("plot_contrib1")),
                  plotOutput(outputId = ns("plot_contrib2"))
                )
              )
            ),
            tabPanel(
              title = "Variance",
              value = "panel_variance",
              style = "margin-top: 15px;",
              fluidRow(
                ## Output: eigenvalues and variance
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  plotOutput(outputId = ns("plot_variance")),
                  tableOutput(outputId = ns("variance"))
                )
              )
            ),
            tabPanel(
              title = "Summary",
              value = "panel_summary",
              style = "margin-top: 15px;",
              ## Output: plot reordered matrix
              verbatimTextOutput(outputId = ns("summary"))
            )
          ) # tabsetPanel
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}

# Server =======================================================================
#' Correspondence Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_ca_ui()]
#' @family server modules
#' @keywords internal
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
