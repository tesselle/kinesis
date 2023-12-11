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

  sidebarLayout(
    sidebarPanel(
      downloadButton(outputId = ns("download"), "Download")
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Summary statistics",
          h5("Center"),
          tableOutput(outputId = ns("location"))
        ),
        tabPanel(
          title = "CLR covariance",
          tableOutput(outputId = ns("covariance"))
        ),
        tabPanel(
          title = "Variation matrix",
          fluidRow(
            div(
              class = "col-lg-6 col-md-1",
              plotOutput(outputId = ns("dendrogram"), height = "auto")
            ),
            div(
              class = "col-lg-6 col-md-1"
            )
          ),
          fluidRow(
            tableOutput(outputId = ns("variation"))
          )
        )
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
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
    ## Location
    data_loc <- reactive({
      req(x())
      if (nexus::any_assigned(x())) {
        nexus::aggregate(x(), by = "groups", FUN = mean, na.rm = FALSE)
      } else {
        moy <- colMeans(x(), na.rm = FALSE)
        matrix(moy, ncol = length(moy), dimnames = list("", names(moy)))
      }
    })

    ## CLR covariance
    data_cov <- reactive({
      req(x())
      nexus::covariance(x(), center = TRUE)
    })

    ## Variation matrix
    data_var <- reactive({
      req(x())
      nexus::variation(x())
    })

    ## Clustering
    # plot_clust <- reactive({
    #   d <- stats::as.dist(data_var())
    #   h <- stats::hclust(d, method = "ward.D2")
    #   plot(h, main = "", sub = "", xlab = "", ylab = "Total variation", las = 1)
    #   grDevices::recordPlot()
    # })

    plot_heatmap <- reactive({
      stats::heatmap(data_var(), distfun = as.dist,
                     hclustfun = function(x) hclust(x, method = "ward.D2"),
                     symm = TRUE, scale = "none")
      grDevices::recordPlot()
    })

    ## Render table
    output$location <- renderTable({data_loc()}, striped = TRUE, width = "100%", rownames = TRUE)
    output$covariance <- renderTable({data_cov()}, striped = TRUE, width = "100%", rownames = TRUE)
    output$variation <- renderTable({data_var()}, striped = TRUE, width = "100%", rownames = TRUE)

    ## Render plot
    output$dendrogram <- renderPlot({
      grDevices::replayPlot(plot_heatmap())
    }, height = function() { getCurrentOutputInfo(session)$width() } )

    ## Download
    output$download <- export_table(
      location = data_loc,
      covariance = data_cov,
      variation = data_var,
      name = "coda_summary"
    )
  })
}
