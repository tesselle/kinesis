# UI ===========================================================================
#' Chi-squared Test UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_chi_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_chi2_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h5("Pearson's Chi-squared"),
      uiOutput(outputId = ns("results")),
      downloadButton(outputId = ns("download"), "Download")
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Expected",
          tableOutput(outputId = ns("expected"))
        ),
        tabPanel(
          title = "Residuals",
          tableOutput(outputId = ns("residuals"))
        ),
        tabPanel(
          title = "Standardized residuals",
          tableOutput(outputId = ns("stdres"))
        )
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Chi-squared Test Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_prepare_server()]).
#' @seealso [module_chi2_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_chi_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Chi-squared Test
    chi2_test <- reactive({
      z <- stats::chisq.test(x = x())

      # Adjusted Cramer's V [95%CI]
      k <- ncol(x())
      r <- nrow(x())
      n <- sum(x())
      phi <- max(0, (z$statistic / n) - (k - 1) * (r - 1) / (n - 1))

      k_bias <- k - (k - 1)^2 / (n - 1)
      r_bias <- r - (r - 1)^2 / (n - 1)
      V <- sqrt(phi / min(k_bias - 1, r_bias - 1))

      z$cramer <- V
      z
    })
    ## Observed counts
    chi2_obs <- reactive({
      chi2_test()$observed
    })
    ## Expected counts
    chi2_exp <- reactive({
      chi2_test()$expected
    })
    ## Pearson residuals
    chi2_res <- reactive({
      chi2_test()$residuals
    })
    ## Standardized residuals
    chi2_std <- reactive({
      chi2_test()$stdres
    })

    ## Render results
    output$results <- renderUI({
      tags$ul(
        tags$li(sprintf("Statistic: %.0f", chi2_test()$statistic)),
        tags$li(sprintf("p-value: %s", format.pval(chi2_test()$p.value, eps = .001))),
        tags$li(sprintf("Cramer's V: %.2f", chi2_test()$cramer))
      )
    })

    ## Render table
    output$expected <- renderTable({chi2_exp()}, striped = TRUE, width = "100%", rownames = TRUE)
    output$residuals <- renderTable({chi2_res()}, striped = TRUE, width = "100%", rownames = TRUE)
    output$stdres <- renderTable({chi2_std()}, striped = TRUE, width = "100%", rownames = TRUE)

    ## Download
    output$download <- export_table(
      expected = chi2_exp,
      residuals = chi2_res,
      name = "chi_squared"
    )
  })
}
