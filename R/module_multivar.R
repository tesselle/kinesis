# UI ===========================================================================
#' Mutlivariate Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return A tab that can be passed to [shiny::tabsetPanel()].
#' @seealso [multivariate_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(outputId = ns("title")),
      ## Input: display options
      selectInput(
        inputId = ns("axis1"),
        label = "Horizontal axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectInput(
        inputId = ns("axis2"),
        label = "Vertical axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
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
      selectizeInput(
        inputId = ns("highlight"),
        label = "Highlight",
        choices = c("none" = "", "contribution", "cos2"),
        selected = "observation",
        multiple = FALSE,
        options = list(plugins = "clear_button")
      )
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        multivariate_results(id),
        multivariate_individuals(id),
        multivariate_variables(id),
        multivariate_screeplot(id)
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

multivariate_results <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Results",
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        bslib::card(
          bslib::card_header(
            "Individuals factor map",
            class = "d-flex justify-content-between"
          ),
          scatterD3::scatterD3Output(outputId = ns("plot_ind"))
        )
      ),
      div(
        class = "col-lg-6 col-md-1",
        bslib::card(
          bslib::card_header(
            "Variables factor map",
            class = "d-flex justify-content-between"
          ),
          scatterD3::scatterD3Output(outputId = ns("plot_var"))
        )
      )
    )
  ) # tabPanel
}

multivariate_individuals <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Individuals",
    output_plot(id = ns("contrib_ind"), height = "auto", title = "Contributions"),
    DT::dataTableOutput(outputId = ns("info_ind"))
  ) # tabPanel
}

multivariate_variables <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Variables",
    output_plot(id = ns("contrib_var"), height = "auto", title = "Contributions"),
    DT::dataTableOutput(outputId = ns("info_var"))
  ) # tabPanel
}

multivariate_screeplot <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Screeplot",
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        output_plot(id = ns("screeplot"), height = "auto", title = "Screeplot")
      ),
      div(
        class = "col-lg-6 col-md-1",
        DT::dataTableOutput(outputId = ns("variance"))
      )
    )
  ) # tabPanel
}

# Server =======================================================================
#' Multivariate Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`dimensio::MultivariateAnalysis-class`] object.
#' @seealso [multivariate_ui]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    observeEvent(axes(), {
      freezeReactiveValue(input, "axis1")
      updateSelectInput(inputId = "axis1", choices = axes())
    })
    observeEvent(axis1(), {
      choices <- axes()[-axis1()]
      freezeReactiveValue(input, "axis2")
      updateSelectInput(inputId = "axis2", choices = choices)
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

    ## Eigenvalues -----
    eigen <- reactive({
      req(x())
      dimensio::get_eigenvalues(x())
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

    ## Individuals -----
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

    plot_ind <- reactive({
      req(x())
      req(info_ind())

      grp <- x()@rows@groups
      if (length(grp) == 0 || all(is.na(grp))) grp <- NULL

      scatterD3::scatterD3(
        x = info_ind()[[1]], y = info_ind()[[2]],
        xlab = dimensio:::print_variance(x(), axis1()),
        ylab = dimensio:::print_variance(x(), axis2()),
        col_var = grp,
        col_lab = "Group",
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

    contrib_ind <- reactive({
      req(x())

      graphics::par(mfrow = c(1, 2))
      dimensio::viz_contributions(x = x(), margin = 1, axes = axis1())
      dimensio::viz_contributions(x = x(), margin = 1, axes = axis2())
      grDevices::recordPlot()
    })

    ## Variables -----
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

    plot_var <- reactive({
      req(x())
      req(info_var())

      high_lab <- input$highlight
      high_var <- info_var()[[high_lab]]

      scatterD3::scatterD3(
        x = info_var()[[1]], y = info_var()[[2]],
        xlab = dimensio:::print_variance(x(), axis1()),
        ylab = dimensio:::print_variance(x(), axis2()),
        col_var = if (inherits(x(), "PCA")) high_var else NULL,
        col_lab = high_lab,
        size_var = if (inherits(x(), "CA")) high_var else NULL,
        size_lab = high_lab,
        symbol_var = info_var()$supplementary,
        symbol_lab = "observation",
        type_var = if (inherits(x(), "PCA")) "arrow" else NULL,
        unit_circle = inherits(x(), "PCA") && dimensio:::is_scaled(x()),
        lab = if (input$lab_col) rownames(info_var()) else NULL,
        labels_positions = "auto",
        fixed = TRUE
      )
    })

    contrib_var <- reactive({
      req(x())

      graphics::par(mfrow = c(1, 2))
      dimensio::viz_contributions(x = x(), margin = 2, axes = axis1())
      dimensio::viz_contributions(x = x(), margin = 2, axes = axis2())
      grDevices::recordPlot()
    })

    ## Render title -----
    output$title <- renderUI({
      title <- ""
      if (inherits(x(), "PCA")) title <- "Principal Components Analysis"
      if (inherits(x(), "CA")) title <- "Correspondence Analysis"
      h5(title)
    })

    ## Render plots -----
    render_plot("contrib_ind", x = contrib_ind, ratio = 0.5)
    render_plot("contrib_var", x = contrib_var, ratio = 0.5)
    render_plot("screeplot", x = plot_eigen)

    ## Render factor maps -----
    output$plot_ind <- scatterD3::renderScatterD3({ plot_ind() })
    output$plot_var <- scatterD3::renderScatterD3({ plot_var() })

    ## Render tables -----
    output$variance <- DT::renderDataTable({
      dt <- eigen()
      dt$cumulative <- dt$cumulative / 100

      dt <- DT::datatable(dt)
      dt <- DT::formatRound(dt, columns = c(1, 2), digits = 3)
      dt <- DT::formatPercentage(dt, columns = 3, digits = 2)
      dt
    })
    output$info_ind <- DT::renderDataTable({
      req(x())
      info <- dimensio::summary(x(), margin = 1)@results
      dt <- DT::datatable(info)
      dt <- DT::formatRound(dt, columns = seq_len(ncol(info)), digits = 3)
      dt
    })
    output$info_var <- DT::renderDataTable({
      req(x())
      info <- dimensio::summary(x(), margin = 2)@results
      dt <- DT::datatable(info)
      dt <- DT::formatRound(dt, columns = seq_len(ncol(info)), digits = 3)
      dt
    })
  })
}
