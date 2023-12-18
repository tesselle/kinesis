# UI ===========================================================================
#' Data Summary UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_summary_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_summary_ui <- function(id) {
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
          h5("Location"),
          tableOutput(outputId = ns("location")),
          h5("Spread"),
          tableOutput(outputId = ns("spread")),
          h5("Percentile table"),
          tableOutput(outputId = ns("quantile"))
        ),
        tabPanel(
          title = "Covariance",
          tableOutput(outputId = ns("cov"))
        )
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Data Summary Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_prepare_server()]).
#' @seealso [module_summary_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_summary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Location -----
    data_loc <- reactive({
      moy <- colMeans(x(), na.rm = FALSE)
      matrix(moy, ncol = length(moy), dimnames = list("mean", names(moy)))
    })

    ## Spread -----
    data_spread <- reactive({
      z <- apply(
        X = x(),
        MARGIN = 2,
        FUN = function(x, na.rm) {
          c(stats::sd(x, na.rm = na.rm), stats::IQR(x, na.rm = na.rm),
            stats::mad(x, na.rm = na.rm))
        },
        na.rm = FALSE,
        simplify = TRUE
      )
      colnames(z) <- colnames(x())
      rownames(z) <- c("SD", "IQR", "MAD")
      z
    })

    ## Percentiles -----
    data_percentiles <- reactive({
      probs <- seq(0, 1, 0.25)
      pc <- apply(
        X = x(),
        MARGIN = 2,
        FUN = stats::quantile,
        probs = probs,
        na.rm = TRUE
      )
      pc
    })

    ## Covariance -----
    data_cov <- reactive({
      stats::cov(x())
    })

    ## Render table -----
    output$location <- render_table(data_loc)
    output$spread <- render_table(data_spread)
    output$quantile <- render_table(data_percentiles)
    output$cov <- render_table(data_cov)

    ## Download -----
    output$download <- export_table(
      location = data_loc,
      spread = data_spread,
      quantiles = data_percentiles,
      name = "summary"
    )
  })
}
