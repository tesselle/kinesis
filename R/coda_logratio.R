# UI ===========================================================================
#' Log-Ratio UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [logratio_server()]
#' @family coda modules
#' @keywords internal
#' @export
logratio_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      title = uiOutput(outputId = ns("title")),
      helpText("Compute log-ratio transformation of compositional data."),
      checkboxInput(
        inputId = ns("weights"),
        label = "Weighted log-ratio",
        value = FALSE
      ),
      uiOutput(outputId = ns("settings")),
      downloadButton(outputId = ns("download_table"), "Download log-ratio"),
      ## Output: graph
      plotOutput(outputId = ns("graph"))
    ), # sidebar
    ## Output: plot
    output_plot(
      id = ns("plot"),
      tools = list(
        select_color(inputId = ns("col")),
        select_pch(inputId = ns("pch"), default = NULL)
      ),
      height = "100%",
      title = "Density"
    )
  ) # layout_sidebar
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

      pivot <- get_value(input$pivot, default = 1)
      trans <- switch (
        method,
        clr = function(x) nexus::transform_clr(x, weights = input$weights),
        alr = function(x) nexus::transform_alr(x, j = pivot, weights = input$weights),
        ilr = function(x) nexus::transform_ilr(x),
        plr = function(x) nexus::transform_plr(x, pivot = pivot)
      )

      notify(trans(x()), title = toupper(method))
    })

    ## Plot -----
    plot_log <- reactive({
      req(logratio())
      col <- get_color(input$col)
      pch <- get_value(as.integer(input$pch))

      function() {
        plot(
          logratio(),
          palette_color = khroma::palette_color_discrete(col),
          palette_symbol = khroma::palette_shape(pch),
          pch = get_value(pch, 1)
        )
      }
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
      title <- switch(
        method,
        clr = "Centered Log-Ratio",
        alr = "Additive Log-Ratio",
        ilr = "Isometric Log-Ratio",
        plr = "Pivot Log-Ratio",
        ""
      )
      tags$header(title, class = "sidebar-title")
    })

    ## Render table -----
    # TODO?

    ## Render plot -----
    render_plot("plot", x = plot_log)

    ## Render graph -----
    output$graph <- renderPlot({
      req(plot_graph())
      grDevices::replayPlot(plot_graph())
    })

    ## Download -----
    output$download_table <- export_table(logratio, name = paste0("coda_", method))

    logratio
  })
}
