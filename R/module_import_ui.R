#' Import UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_import_server()]
#' @family UI modules
#' @export
module_import_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Import",
    icon = icon("upload"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            ## Input: select a file
            fileInput(
              inputId = ns("file"),
              label = "Choose CSV file",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain",
                         ".csv")
            ),

            tags$hr(),

            ## Input: checkbox if file has header
            checkboxInput(
              inputId = ns("header"),
              label = "Header",
              value = TRUE
            ),

            ## Input: checkbox if file has row names
            checkboxInput(
              inputId = ns("rownames"),
              label = "Row names",
              value = TRUE
            ),

            ## Input: select decimal
            radioButtons(
              inputId = ns("dec"),
              label = "Decimal",
              choices = c(Dot = ".", Comma = ","),
              selected = "."
            ),

            ## Input: select separator
            radioButtons(
              inputId = ns("sep"),
              label = "Separator",
              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
              selected = ","
            ),

            ## Input: select quotes
            radioButtons(
              inputId = ns("quote"),
              label = "Quote",
              choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
              selected = '"'
            )
          ) # wellPanel
        ),
        column(
          width = 8,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Table",
              style = "margin-top: 15px;",
              ## TODO: column selection

              ## Input: select number of rows to display
              radioButtons(
                inputId = ns("display"),
                label = "Display",
                choices = c(Head = "head", All = "all"),
                selected = "head"
              ),

              ## Output: display data
              tableOutput(outputId = ns("table"))
            ),
            tabPanel(
              title = "Summary",
              style = "margin-top: 15px;",

              ## Output: display data
              verbatimTextOutput(outputId = ns("summary"))
            )
          )
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}
