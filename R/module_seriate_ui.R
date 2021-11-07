#' Seriate UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_seriate_server()]
#' @family UI modules
#' @export
module_seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Seriate",
    icon = icon("sort-amount-up"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            ## Input: checkbox if permute rows
            checkboxInput(
              inputId = ns("margin_row"),
              label = "Permute rows",
              value = TRUE
            ),
            ## Input: checkbox if permute columns
            checkboxInput(
              inputId = ns("margin_col"),
              label = "Permute columns",
              value = TRUE
            ),
            ## Input: select CA axes
            numericInput(
              inputId = ns("axes"),
              label = "CA dimension",
              value = 1,
              min = 1,
              max = 10,
              step = 1
            ),
            ## Input: seriate
            actionButton(
              inputId = ns("go_seriate"),
              label = "Seriate",
              style = "margin-bottom: 15px;"
            ),
            ## Input: select plot
            radioButtons(
              inputId = ns("plot_type"),
              label = "Display",
              choices = c(Bertin = "bertin", Ford = "ford", Heatmap = "heat"),
              selected = "heat"
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          ## Output: permutation summary
          # verbatimTextOutput(outputId = ns("summary")),
          fluidRow(
            tabsetPanel(
              id = ns("plot"),
              type = "tabs",
              tabPanel(
                title = "Raw data",
                value = "panel_raw",
                style = "margin-top: 15px;",
                ## Output: download
                downloadButton(outputId = ns("export_plot_data"),
                               label = "Export plot"),
                ## Output: plot raw matrix
                plotOutput(outputId = ns("plot_data"))
              ),
              tabPanel(
                title = "Rearranged matrix",
                value = "panel_permute",
                style = "margin-top: 15px;",
                ## Output: download
                downloadButton(outputId = ns("export_plot_perm"),
                               label = "Export plot"),
                downloadButton(outputId = ns("export_table"),
                               label = "Export matrix"),
                ## Output: plot reordered matrix
                plotOutput(outputId = ns("plot_permute"))
              )
            ) # tabsetPanel
          )
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}
