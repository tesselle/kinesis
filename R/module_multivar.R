# UI ===========================================================================
#' Mutlivariate Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return A tab that can be passed to [shiny::tabsetPanel()].
#' @seealso [module_multivar_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_multivar_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(outputId = ns("title")),
      ## Input: display options
      selectizeInput(
        inputId = ns("axis1"),
        label = "Horizontal axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        inputId = ns("axis2"),
        label = "Vertical axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      checkboxInput(
        inputId = ns("supplementary"),
        label = "Supplementary observations",
        value = TRUE
      ),
      checkboxInput(
        inputId = ns("lab_row"),
        label = "Label individuals",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("lab_col"),
        label = "Label variables",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("ellipses"),
        label = "Ellipses (95%)",
        value = FALSE
      ),
      hr(),
      selectInput(
        inputId = ns("highlight"),
        label = "Symbol size",
        choices = c("none" = "", "contribution", "cos2"),
        selected = "observation",
        multiple = FALSE,
      )
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        module_multivar_results(id),
        module_multivar_individuals(id),
        module_multivar_variables(id),
        module_multivar_screeplot(id)
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

module_multivar_screeplot <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Screeplot",
    ## Output: download
    downloadButton(outputId = ns("export_screeplot"),
                   label = "Export plot"),
    hr(),
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        plotOutput(outputId = ns("screeplot"), height = "auto")
      ),
      div(
        class = "col-lg-6 col-md-1",
        DT::dataTableOutput(outputId = ns("variance"))
      )
    )
  ) # tabPanel
}

module_multivar_results <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Results",
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        h5("Individuals factor map"),
        hr(),
        scatterD3::scatterD3Output(outputId = ns("plot_ind"))
      ),
      div(
        class = "col-lg-6 col-md-1",
        h5("Variables factor map"),
        hr(),
        scatterD3::scatterD3Output(outputId = ns("plot_var"))
      )
    )
  ) # tabPanel
}

module_multivar_individuals <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Individuals",
    DT::dataTableOutput(outputId = ns("info_ind"))
  ) # tabPanel
}

module_multivar_variables <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Variables",
    DT::dataTableOutput(outputId = ns("info_var"))
  ) # tabPanel
}

# Server =======================================================================
#' Multivariate Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`dimensio::MultivariateAnalysis-class`] object.
#' @seealso [module_multivar_ui]
#' @family server modules
#' @keywords internal
#' @export
module_multivar_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ranges_ind <- reactiveValues(x = NULL, y = NULL)
    ranges_var <- reactiveValues(x = NULL, y = NULL)

    ## Observe -----------------------------------------------------------------
    observeEvent(axes(), {
      updateSelectInput(session, inputId = "axis1", choices = axes())
    })
    observeEvent(axis1(), {
      choices <- axes()[-axis1()]
      updateSelectInput(session, inputId = "axis2", choices = choices)
    })

    ## Reactive ----------------------------------------------------------------
    eigen <- reactive({
      req(x())
      dimensio::get_eigenvalues(x())
    })

    axes <- reactive({
      choices <- seq_len(nrow(eigen()))
      names(choices) <- unique(rownames(eigen()))
      choices
    })
    axis1 <- reactive({
      req(input$axis1)
      as.numeric(input$axis1)
    })
    axis2 <- reactive({
      req(input$axis2)
      as.numeric(input$axis2)
    })

    plot_eigen <- reactive({
      req(x())
      dimensio::screeplot(
        x = x(),
        cumulative = TRUE,
        labels = FALSE,
        limit = sum(eigen()[, 3] <= 99)
      )
      grDevices::recordPlot()
    })

    info_ind <- reactive({
      req(x())
      z <- dimensio::augment(x = x(), margin = 1, axes = c(axis1(), axis2()))
      z <- arkhe::assign_rownames(z, 3)
      if (any(z$supplementary)) {
        z$supplementary <- ifelse(z$supplementary, "Suppl.", "Active")
      } else {
        z$supplementary <- NULL
      }
      z
    })
    info_var <- reactive({
      req(x())
      z <- dimensio::augment(x = x(), margin = 2, axes = c(axis1(), axis2()))
      z <- arkhe::assign_rownames(z, 3)
      if (any(z$supplementary)) {
        z$supplementary <- ifelse(z$supplementary, "Suppl.", "Active")
      } else {
        z$supplementary <- NULL
      }
      z
    })

    plot_ind <- reactive({
      req(x())
      req(info_ind())
      scatterD3::scatterD3(
        x = info_ind()[[1]], y = info_ind()[[2]],
        xlab = dimensio:::print_variance(x(), axis1()),
        ylab = dimensio:::print_variance(x(), axis2()),
        # col_var = info_ind()[[input$highlight]],
        # col_lab = input$highlight,
        size_var = info_ind()[[input$highlight]],
        size_lab = input$highlight,
        symbol_var = info_ind()$supplementary,
        symbol_lab = "observation",
        lab = if (input$lab_row) rownames(info_ind()) else NULL,
        labels_positions = "auto",
        ellipses = input$ellipses,
        ellipses_level = 0.95,
        fixed = TRUE
      )
    })
    plot_var <- reactive({
      req(x())
      req(info_var())
      scatterD3::scatterD3(
        x = info_var()[[1]], y = info_var()[[2]],
        xlab = dimensio:::print_variance(x(), axis1()),
        ylab = dimensio:::print_variance(x(), axis2()),
        # col_var = info_var()[[input$highlight]],
        # col_lab = input$highlight,
        size_var = info_var()[[input$highlight]],
        size_lab = input$highlight,
        symbol_var = info_var()$supplementary,
        symbol_lab = "observation",
        lab = if (input$lab_col) rownames(info_var()) else NULL,
        labels_positions = "auto",
        fixed = TRUE
      )
    })

    ## Render ------------------------------------------------------------------
    output$title <- renderUI({
      txt <- ""
      if (inherits(x(), "PCA")) txt <- "Principal Components Analysis"
      if (inherits(x(), "CA")) txt <- "Correspondence Analysis"
      h5(txt)
    })
    output$variance <- DT::renderDataTable({
      dt <- eigen()
      dt$cumulative <- dt$cumulative / 100

      dt <- DT::datatable(dt)
      dt <- DT::formatRound(dt, columns = c(1, 2), digits = 3)
      dt <- DT::formatPercentage(dt, columns = 3, digits = 2)
      dt
    })
    output$screeplot <- renderPlot(
      { grDevices::replayPlot(plot_eigen()) },
      height = function() { getCurrentOutputInfo(session)$width() }
    )
    output$plot_ind <- scatterD3::renderScatterD3({ plot_ind() })
    output$plot_var <- scatterD3::renderScatterD3({ plot_var() })
    output$info_ind <- DT::renderDataTable({ DT::datatable(info_ind()) })
    output$info_var <- DT::renderDataTable({ DT::datatable(info_var()) })

    ## Download ----------------------------------------------------------------
    output$export_screeplot <- export_plot(plot_eigen, name = "screeplot")
  })
}
