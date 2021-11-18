
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
    "CA",
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
              plotOutput(outputId = ns("plot_ca"))
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
            ),
            tabPanel(
              title = "Bootstrap",
              value = "panel_bootstrap",
              style = "margin-top: 15px;",
              ## Input: set bootstrap replicates
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
              ),
              ## Output: plot bootstrap replicates
              plotOutput(outputId = ns("plot_boot"))
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
    ca_axis1 <- reactive({
      req(input$axis1)
      as.numeric(input$axis1)
    })
    ca_axis2 <- reactive({
      req(input$axis2)
      as.numeric(input$axis2)
    })
    ca_margin <- reactive({
      req(input$margin)
      as.numeric(input$margin)
    })
    ca_plot <- reactive({
      switch(
        ca_margin(),
        `1` = dimensio::plot_rows,
        `2` = dimensio::plot_columns
      )
    })
    ca_results <- reactive({
      req(user_data$data)
      dimensio::ca(
        user_data$data,
        rank = input$rank,
        sup_row = input$sup_row,
        sup_col = input$sup_col
      )
    })
    ca_boot <- eventReactive(input$go_bootrstrap, {
      req(ca_results())
      req(input$replicates)

      id <- showNotification(
        ui = "Bootstraping...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      dimensio::bootstrap(ca_results(), n = input$replicates)
    })
    plot_ca <- reactive({
      req(ca_results())
      ca_plot()(ca_results(), axes = c(ca_axis1(), ca_axis2()))
    })
    plot_boot <- reactive({
      req(ca_boot())
      ca_plot()(ca_boot(), axes = c(ca_axis1(), ca_axis2()))
    })
    plot_variance <- reactive({
      req(ca_results())
      dimensio::plot_variance(ca_results())
    })
    plot_contrib1 <- reactive({
      req(ca_results())
      dimensio::plot_contributions(
        ca_results(),
        margin = ca_margin(),
        axes = ca_axis1()
      )
    })
    plot_contrib2 <- reactive({
      req(ca_results())
      dimensio::plot_contributions(
        ca_results(),
        margin = ca_margin(),
        axes = ca_axis2()
      )
    })
    ## Observe -----------------------------------------------------------------
    observeEvent(ca_results(), {
      choices <- seq_len(dim(ca_results()))
      names(choices) <- unique(rownames(dimensio::get_eigenvalues(ca_results())))
      updateSelectizeInput(session, inputId = "axis1", choices = choices)
    })
    observeEvent(input$axis1, {
      choices <- seq_len(dim(ca_results()))
      names(choices) <- unique(rownames(dimensio::get_eigenvalues(ca_results())))

      choices <- choices[-as.numeric(input$axis1)]
      updateSelectizeInput(session, inputId = "axis2", choices = choices)
    })
    ## Render ------------------------------------------------------------------
    output$variance <- renderTable({
      req(ca_results())
      dimensio::get_eigenvalues(ca_results())
    }, striped = TRUE, hover = FALSE, rownames = TRUE, colnames = TRUE)
    output$contrib <- renderTable({
      req(ca_results())
      dimensio::get_contributions(ca_results())
    }, striped = TRUE, hover = FALSE, rownames = TRUE, colnames = TRUE)
    output$summary <- renderPrint({
      req(ca_results())
      dimensio::summary(ca_results(), margin = as.numeric(input$margin))
    })
    output$plot_ca <- renderPlot({
      plot_ca()
    })
    output$plot_boot <- renderPlot({
      plot_boot()
    })
    output$plot_contrib1 <- renderPlot({
      plot_contrib1()
    })
    output$plot_contrib2 <- renderPlot({
      plot_contrib2()
    })
    output$plot_variance <- renderPlot({
      plot_variance()
    })
  })
}
