# UI ===========================================================================
#' Compositional Data Summary UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_summary_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_ui <- function(id) {
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
          h5("Spread"),
          helpText("See", cite_article("Hron & Kubacek", "2011", "10.1007/s00184-010-0299-3", TRUE)),
          tableOutput(outputId = ns("spread")),
          h5("Percentile table"),
          tableOutput(outputId = ns("quantile"))
        ),
        tabPanel(
          title = "Univariate statistics",
          fluidRow(
            div(
              class = "col-lg-6 col-md-1",
              output_plot(id = ns("hist"), height = "auto", title = "Histogram")
            ),
            div(
              class = "col-lg-6 col-md-1",
              tags$p("See", cite_article("Filzmoser et al.", "2009", "10.1016/j.scitotenv.2009.08.008", TRUE)),
              selectInput(
                inputId = ns("hist_select"),
                label = "Select a part",
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              )
            )
          )
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
#' @seealso [coda_summary_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    data <- reactive({
      validate(need(!anyNA(x()), "Your data must not contain missing values."))
      x()
    })
    ## Location -----
    data_loc <- reactive({
      req(data())
      if (nexus::any_assigned(data())) {
        index <- nexus::get_groups(data())
        nexus::aggregate(data(), by = index, FUN = nexus::mean, na.rm = FALSE)
      } else {
        m <- nexus::mean(data(), na.rm = FALSE)
        matrix(m, nrow = 1, dimnames = list("center", names(m)))
      }
    })

    ## Spread -----
    data_spread <- reactive({
      req(data())
      ## Metric variance by group
      if (nexus::any_assigned(data())) {
        index <- nexus::get_groups(data())
        s <- nexus::aggregate(data(), by = index, FUN = nexus::metric_var)
      } else {
        m <- nexus::metric_var(data())
        s <- matrix(m, nrow = 1, dimnames = list("", NULL))
      }
      colnames(s) <- c("metric variance")
      s
    })

    ## Percentiles -----
    data_quant <- reactive({
      req(data())
      nexus::quantile(data(), probs = seq(0, 1, 0.25))
    })

    ## Histogram -----
    observeEvent(data(), {
      choices <- colnames(data())
      freezeReactiveValue(input, "hist_select")
      updateSelectInput(inputId = "hist_select", choices = choices)
    })
    plot_hist <- reactive({
      req(data())
      nexus::hist(data()[, input$hist_select, drop = FALSE], ncol = 1)
      grDevices::recordPlot()
    })

    ## CLR covariance -----
    data_cov <- reactive({
      req(data())
      nexus::covariance(data(), center = TRUE)
    })

    ## Variation matrix -----
    data_var <- reactive({
      req(data())
      nexus::variation(data())
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
    output$location <- render_table(data_loc)
    output$spread <- render_table(data_spread, width = "auto")
    output$quantile <- render_table(data_quant)
    output$covariance <- render_table(data_cov)
    output$variation <- render_table(data_var)

    ## Render plot -----
    render_plot("dendrogram", x = plot_heatmap)
    render_plot("hist", x = plot_hist)

    ## Download -----
    output$download <- export_table(
      location = data_loc,
      spread = data_spread,
      quantiles = data_quant,
      covariance = data_cov,
      variation = data_var,
      name = "coda_summary"
    )
  })
}
