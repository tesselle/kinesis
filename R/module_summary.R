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

  fluidRow(
    column(
      width = 8,
      offset = 2,
      downloadButton(outputId = ns("download"), "Download"),
      ## Output: display data
      h4("Mean"),
      tableOutput(outputId = ns("mean")),
      h4("Standard deviation"),
      tableOutput(outputId = ns("sd")),
      h4("Percentile table"),
      tableOutput(outputId = ns("quantile")),
      h4("Covariance"),
      tableOutput(outputId = ns("cov"))
    ) # column
  ) # fluidRow
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
    ## Percentiles
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
    ## Mean
    data_mean <- reactive({
      moy <- colMeans(x(), na.rm = FALSE)
      matrix(moy, ncol = length(moy), dimnames = list(NULL, names(moy)))
    })
    ## Standard deviation
    data_sd <- reactive({
      apply(
        X = x(),
        MARGIN = 2,
        FUN = stats::sd,
        na.rm = FALSE,
        simplify = FALSE
      )
    })
    ## Covariance
    data_cov <- reactive({
      stats::cov(x())
    })

    ## Render table
    output$quantile <- renderTable({data_percentiles()}, striped = TRUE, width = "100%", rownames = TRUE)
    output$mean <- renderTable({data_mean()}, striped = TRUE, width = "100%")
    output$sd <- renderTable({data_sd()}, striped = TRUE, width = "100%")
    output$cov <- renderTable({data_cov()}, width = "100%", rownames = TRUE)

    ## Download
    output$download <- export_table(
      quantiles = data_percentiles,
      mean = data_mean,
      standard_deviation = data_sd,
      name = "summary"
    )
  })
}
