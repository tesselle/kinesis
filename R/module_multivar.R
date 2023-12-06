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
        inputId = ns("supplement"),
        label = "Supplementary observations",
        value = TRUE
      ),
      # checkboxInput(
      #   inputId = ns("labels"),
      #   label = "Draw labels",
      #   value = FALSE
      # ),
      selectizeInput(
        inputId = ns("highlight"),
        label = "Highlight",
        choices = c("observation", "contribution", "cos2"),
        selected = "observation",
        multiple = FALSE,
      ),
      hr(),
      sliderInput(
        inputId = ns("cex"),
        label = "Symbol size",
        min = 1, max = 6, value = c(1, 3),
        step = 0.5
      ),
      color_picker(id, "col")
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        type = "tabs",
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

module_multivar_individuals <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Individuals",
    ## Output: download
    downloadButton(outputId = ns("export_plot_ind"),
                   label = "Export plot"),
    hr(),
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        plotOutput(outputId = ns("plot_ind"),
                   click = ns("click_ind"), dblclick = ns("dblclick_ind"),
                   brush = brushOpts(id = ns("brush_ind"), resetOnNew = TRUE),
                   height = "auto")
      ),
      div(
        class = "col-lg-6 col-md-1",
        tableOutput(outputId = ns("info_ind"))
      )
    )
  ) # tabPanel
}

module_multivar_variables <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Variables",
    ## Output: download
    downloadButton(outputId = ns("export_plot_var"),
                   label = "Export plot"),
    hr(),
    fluidRow(
      div(
        class = "col-lg-6 col-md-1",
        plotOutput(outputId = ns("plot_var"),
                   click = ns("click_var"), dblclick = ns("dblclick_var"),
                   brush = brushOpts(id = ns("brush_var"), resetOnNew = TRUE),
                   height = "auto")
      ),
      div(
        class = "col-lg-6 col-md-1",
        tableOutput(outputId = ns("info_var"))
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
    observeEvent(input$dblclick_ind, {
      brush <- input$brush_ind
      if (!is.null(brush)) {
        ranges_ind$x <- c(brush$xmin, brush$xmax)
        ranges_ind$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges_ind$x <- NULL
        ranges_ind$y <- NULL
      }
    })
    observeEvent(input$dblclick_var, {
      brush <- input$brush_var
      if (!is.null(brush)) {
        ranges_var$x <- c(brush$xmin, brush$xmax)
        ranges_var$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges_var$x <- NULL
        ranges_var$y <- NULL
      }
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
    plot_ind <- reactive({
      req(x())
      dimensio::viz_rows(
        x = x(),
        axes = c(axis1(), axis2()),
        active = TRUE,
        sup = input$supplement,
        labels = FALSE,
        highlight = input$highlight,
        xlim = ranges_ind$x,
        ylim = ranges_ind$y,
        legend = list(x = "topleft"),
        ## FIXME: temporary workaround (must be fixed in dimensio)
        cex = if (input$highlight == "observation") input$cex[1] else input$cex,
        pch = if (input$highlight == "observation") c(16, 17) else 16,
        col = khroma::color(input$col, names = FALSE, force = TRUE)(256)
      )
      grDevices::recordPlot()
    })
    plot_var <- reactive({
      req(x())
      dimensio::viz_columns(
        x = x(),
        axes = c(axis1(), axis2()),
        active = TRUE,
        sup = input$supplement,
        labels = FALSE,
        highlight = input$highlight,
        xlim = ranges_var$x,
        ylim = ranges_var$y,
        legend = list(x = "topleft"),
        ## FIXME: temporary workaround (must be fixed in dimensio)
        cex = if (input$highlight == "observation") input$cex[1] else input$cex,
        pch = if (input$highlight == "observation") c(16, 17) else 16,
        col = khroma::color(input$col, names = FALSE, force = TRUE)(256)
      )
      grDevices::recordPlot()
    })
    info_ind <- reactive({
      req(x())
      dimensio::augment(
        x = x(),
        margin = 1,
        axes = c(axis1(), axis2())
      )
    })
    info_var <- reactive({
      req(x())
      dimensio::augment(
        x = x(),
        margin = 2,
        axes = c(axis1(), axis2())
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
    output$plot_ind <- renderPlot(
      { grDevices::replayPlot(plot_ind()) },
      height = function() { getCurrentOutputInfo(session)$width() }
    )
    output$plot_var <- renderPlot(
      { grDevices::replayPlot(plot_var()) },
      height = function() { getCurrentOutputInfo(session)$width() }
    )
    output$info_ind <- renderTable({
      xy <- names(info_ind())
      tbl <- nearPoints(
        df = info_ind(),
        coordinfo = input$click_ind,
        xvar = xy[1], yvar = xy[2],
        threshold = 10, maxpoints = 5,
        addDist = FALSE
      )
    }, striped = TRUE, width = "100%", rownames = FALSE)
    output$info_var <- renderTable({
      xy <- names(info_var())
      tbl <- nearPoints(
        df = info_var(),
        coordinfo = input$click_var,
        xvar = xy[1], yvar = xy[2],
        threshold = 10, maxpoints = 5,
        addDist = FALSE
      )
    }, striped = TRUE, width = "100%", rownames = FALSE)

    ## Download ----------------------------------------------------------------
    output$export_plot_ind <- export_plot(plot_ind, name = "map_individuals")
    output$export_plot_var <- export_plot(plot_var, name = "map_variables")
    output$export_screeplot <- export_plot(plot_eigen, name = "screeplot")
  })
}
