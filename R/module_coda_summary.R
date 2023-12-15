# UI ===========================================================================
#' Compositional Data Summary UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_coda_summary_server()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_summary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      wellPanel(
        style = "margin-bottom: 15px;",
        h5("Compositional statistics"),
        downloadButton(outputId = ns("download"), "Download tables")
      ), # wellPanel
      output_plot(id = ns("dendrogram"), height = "auto", title = "Variation matrix")
    ), # column
    column(
      width = 8,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Summary statistics",
          h5("Center"),
          tableOutput(outputId = ns("location")),
          h5("Percentile table"),
          tableOutput(outputId = ns("quantile"))
        ),
        tabPanel(
          title = "Univariate statistics",
          actionButton(inputId = ns("hist_back"), label = "Back"),
          actionButton(inputId = ns("hist_next"), label = "Next"),
          output_plot(id = ns("hist"), height = "auto", title = "Histogram")
        ),
        tabPanel(
          title = "CLR covariance",
          tableOutput(outputId = ns("covariance"))
        ),
        tabPanel(
          title = "Variation matrix",
          tableOutput(outputId = ns("variation"))
        )
      ) # tabsetPanel
    ) # column
  ) # fluidRow
}

# Server =======================================================================
#' Compositional Data Summary Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [module_coda_summary_ui()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_summary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Location -----
    data_loc <- reactive({
      req(x())
      if (nexus::any_assigned(x())) {
        index <- nexus::get_groups(x())
        nexus::aggregate(x(), by = index, FUN = nexus::mean, na.rm = FALSE)
      } else {
        m <- nexus::mean(x(), na.rm = FALSE)
        matrix(m, nrow = 1, dimnames = list("center", names(m)))
      }
    })

    ## Percentiles -----
    data_quant <- reactive({
      req(x())
      nexus::quantile(x(), probs = seq(0, 1, 0.25))
    })

    ## Histograms -----
    max <- 6
    chunk <- reactive({
      d <- seq_len(ncol(x()))
      split(d, ceiling(d / max))
    })
    index <- reactiveVal(1) # Set the initial subset index
    observeEvent(input$hist_next, {
      index((index() %% length(chunk())) + 1)
    })
    observeEvent(input$hist_back, {
      index(((index() - 2) %% length(chunk())) + 1)
    })

    plot_hist <- reactive({
      req(x())
      nexus::hist(x()[, chunk()[[index()]], drop = FALSE], ncol = 3)
      grDevices::recordPlot()
    })

    ## CLR covariance -----
    data_cov <- reactive({
      req(x())
      nexus::covariance(x(), center = TRUE)
    })

    ## Variation matrix -----
    data_var <- reactive({
      req(x())
      nexus::variation(x())
    })

    ## Clustering -----
    # plot_clust <- reactive({
    #   d <- stats::as.dist(data_var())
    #   h <- stats::hclust(d, method = "ward.D2")
    #   plot(h, main = "", sub = "", xlab = "", ylab = "Total variation", las = 1)
    #   grDevices::recordPlot()
    # })

    ## Heatmap -----
    plot_heatmap <- reactive({
      stats::heatmap(
        data_var(),
        distfun = stats::as.dist,
        hclustfun = function(x) stats::hclust(x, method = "ward.D2"),
        symm = TRUE,
        scale = "none"
      )
      grDevices::recordPlot()
    })

    ## Render table -----
    output$location <- renderTable({data_loc()}, striped = TRUE, width = "100%", rownames = TRUE, digits = 3)
    output$quantile <- renderTable({data_quant()}, striped = TRUE, width = "100%", rownames = TRUE, digits = 3)
    output$covariance <- renderTable({data_cov()}, striped = TRUE, width = "100%", rownames = TRUE, digits = 3)
    output$variation <- renderTable({data_var()}, striped = TRUE, width = "100%", rownames = TRUE, digits = 3)

    ## Render plot -----
    render_plot("dendrogram", x = plot_heatmap)
    render_plot("hist", x = plot_hist, height = function() { getCurrentOutputInfo(session)$width() / 2 })

    ## Download -----
    output$download <- export_table(
      location = data_loc,
      quantiles = data_quant,
      covariance = data_cov,
      variation = data_var,
      name = "coda_summary"
    )
  })
}
