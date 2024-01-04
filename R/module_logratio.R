# UI ===========================================================================
#' Log-Ratio UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [logratio_server()]
#' @family coda modules
#' @keywords internal
#' @export
logratio_ui <- function() {
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "CLR",
      value = "panel_clr",
      logratioUI("clr")
    ), # tabPanel
    tabPanel(
      title = "ALR",
      value = "panel_alr",
      logratioUI("alr")
    ), # tabPanel
    tabPanel(
      title = "ILR",
      value = "panel_ilr",
      logratioUI("ilr")
    ), # tabPanel
    tabPanel(
      title = "PLR",
      value = "panel_plr",
      logratioUI("plr")
    )
  ) # tabsetPanel
}

logratioUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      wellPanel(
        uiOutput(outputId = ns("title")),
        uiOutput(outputId = ns("settings")),
        downloadButton(outputId = ns("download_table"), "Download table")
      ), # wellPanel
      ## Output: graph
      plotOutput(outputId = ns("graph"), height = "auto")
    ), # column
    column(
      width = 8,
      tabsetPanel(
        tabPanel(
          title = "Plot",
          ## Output: plot
          output_plot(id = ns("plot"), height = "auto", title = "Density")
        ),
        tabPanel(
          title = "Table",
          ## Output: table
          DT::dataTableOutput(outputId = ns("table"))
        )
      ) # mainPanel
    ) # column
  ) # fluidRow
}

# Server =======================================================================
#' Log-Ratio Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @param method A [`character`] string specifying the log-ratio to be computed.
#' @return A reactive [`nexus::LogRatio-class`] object.
#' @seealso [logratio_ui()]
#' @family coda modules
#' @keywords internal
#' @export
logratio_server <- function(id, x, method) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Render settings -----
    output$settings <- renderUI({
      if (!(method == "alr" || method == "plr")) return(NULL)
      label <- switch (
        method,
        alr = "Rationing part:",
        plr = "Pivotal variable:"
      )
      selectizeInput(
        inputId = session$ns("pivot"),
        label = label,
        choices = colnames(x()),
        selected = NULL,
        multiple = FALSE,
      )
    })

    ## Compute -----
    logratio <- reactive({
      req(x())
      trans <- switch (
        method,
        clr = function(x) nexus::transform_clr(x),
        alr = function(x) nexus::transform_alr(x, j = input$pivot),
        ilr = function(x) nexus::transform_ilr(x),
        plr = function(x) nexus::transform_plr(x, pivot = input$pivot)
      )

      ratio <- try(trans(x()), silent = get_option("verbose"))
      if (inherits(ratio, "try-error")) return(NULL)
      ratio
    })

    ## Plot -----
    plot_log <- reactive({
      req(logratio())
      plot(logratio())
      grDevices::recordPlot()
    })

    ## Graph -----
    plot_graph <- reactive({
      req(logratio())
      if (inherits(logratio(), "CLR")) return(NULL)
      graph <- nexus::as_graph(logratio())
      plot(graph)
      grDevices::recordPlot()
    })

    ## Render title -----
    output$title <- renderUI({
      title <- ""
      title <- switch(
        method,
        clr = "Centered Log-Ratio",
        alr = "Additive Log-Ratio",
        ilr = "Isometric Log-Ratio",
        plr = "Pivot Log-Ratio"
      )
      h5(title)
    })

    ## Render table -----
    output$table <- DT::renderDataTable({
      req(logratio())
      dt <- DT::datatable(logratio())
      dt <- DT::formatRound(dt, columns = seq_len(ncol(logratio())), digits = 3)
      dt
    })

    ## Render plot -----
    render_plot("plot", x = plot_log, ratio = 0.5)

    ## Render graph -----
    output$graph <- renderPlot({
      req(plot_graph())
      grDevices::replayPlot(plot_graph())
    }, height = function() { getCurrentOutputInfo(session)$width() } )

    ## Download -----
    output$download_table <- export_table(
      log_ratio = logratio,
      name = method
    )

    logratio
  })
}
