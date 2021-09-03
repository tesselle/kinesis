#' Visualize UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_visualize_server()]
#' @family UI modules
#' @export
module_visualize_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Visualize",
    icon = icon("bar-chart"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            ## Input: select data transformation
            radioButtons(
              inputId = ns("data"),
              label = "Transform data",
              choices = c(`Absolute frequencies` = "count",
                          `Relative frequencies` = "composition",
                          Incidence = "incidence",
                          `Co-occurrence` = "occurrence"),
              selected = "count"
            ),

            ## Input: select plot type
            selectInput(
              inputId = ns("select"),
              label = "Plot",
              choices = c(Ford = "ford", Heatmap = "heatmap"),
              multiple = FALSE
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          tabsetPanel(
            id = ns("parameters"),
            type = "hidden",
            tabPanelBody("ford", ""),
            tabPanelBody("heatmap", "")
          ),
          plotOutput(
            outputId = ns("plot")
          )
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}
