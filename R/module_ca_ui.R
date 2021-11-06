#' Correspondence Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_ca_server()]
#' @family UI modules
#' @export
module_ca_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Analyse",
    icon = icon("chart-bar"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            ## Input: select CA axes
            selectizeInput(
              inputId = ns("axis1"),
              label = "Horizontal axis",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
            ),
            selectizeInput(
              inputId = ns("axis2"),
              label = "Vertical axis",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
            ),

            ## Input: select margin
            radioButtons(
              inputId = ns("margin"),
              label = "Display",
              choices = c(Rows = 1, Columns = 2),
              selected = 1
            )
          ), # wellPanel
          wellPanel(
            ## Input: select bootstrat replicates
            numericInput(
              inputId = ns("replicates"),
              label = "Replicates",
              value = 500,
              min = 10,
              max = NA,
              step = 10
            ),

            actionButton(
              inputId = ns("go_bootrstrap"),
              label = "Bootstrap"
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          tabsetPanel(
            id = ns("results"),
            type = "tabs",
            tabPanel(
              title = "Results",
              value = "panel_results",
              style = "margin-top: 15px;",
              ## Output: plot coordinates
              plotOutput(outputId = ns("plot_results"))
            ),
            tabPanel(
              title = "Contributions",
              value = "panel_contrib",
              style = "margin-top: 15px;",
              fluidRow(
                ## Output: contribution
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  plotOutput(outputId = ns("plot_contrib1")),
                  plotOutput(outputId = ns("plot_contrib2"))
                )
              )
            ),
            tabPanel(
              title = "Variance",
              value = "panel_variance",
              style = "margin-top: 15px;",
              fluidRow(
                ## Output: eigenvalues and variance
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  plotOutput(outputId = ns("plot_variance")),
                  tableOutput(outputId = ns("variance"))
                )
              )
            ),
            tabPanel(
              title = "Summary",
              value = "panel_summary",
              style = "margin-top: 15px;",
              ## Output: plot reordered matrix
              verbatimTextOutput(outputId = ns("summary"))
            )
          )
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}
