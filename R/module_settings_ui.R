#' Settings UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_settings_server()]
#' @family UI modules
#' @export
module_settings_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Colour schemes
  col <- khroma::info()

  tabPanel(
    "Settings",
    icon = icon("cogs"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          h4("Bookmarking"),
          bookmarkButton(),
          tags$p(textOutput(outputId = ns("last_saved"))),
          tags$br(),
          h4("Graphical Output"),
          numericInput(
            inputId = ns("fig_width"),
            label = "Figure width",
            value = 7
          ),
          numericInput(
            inputId = ns("fig_height"),
            label = "Figure height",
            value = 5
          ),
          selectInput(
            inputId = ns("fig_units"),
            label = "Figure units",
            choices = c("in", "cm", "mm")
          ),
          numericInput(
            inputId = ns("fig_dpi"),
            label = "Figure resolution",
            value = 300,
            min = 72,
            max = 320
          ),
          tags$br(),
          h4("Colour Schemes"),
          selectInput(
            inputId = ns("col_qualitative"),
            label = "Qualitative",
            choices = col$palette[col$type == "qualitative"],
            selected = "okabe ito"
          ),
          selectInput(
            inputId = ns("col_sequential"),
            label = "Sequential",
            choices = col$palette[col$type == "sequential"],
            selected = "YlOrBr"
          ),
          selectInput(
            inputId = ns("col_diverging"),
            label = "Diverging",
            choices = col$palette[col$type == "diverging"],
            selected = "sunset"
          )
        ),
        column(
          width = 9,
          h4("Session information"),
          verbatimTextOutput(outputId = ns("session"))
        )
      )
    )
  )
}
