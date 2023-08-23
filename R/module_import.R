# UI ===========================================================================
#' Import Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_import_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_import_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      ## Input: select a file
      fileInput(
        inputId = ns("file"),
        label = tooltip(
          span("Choose a CSV file", icon("circle-question")),
          "Select the location of, and the CSV file you want to upload. Please check the default settings below and adjust them to your data.",
          placement = "auto"
        ),
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      helpText("This application only supports data encoded in UFT-8."),
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
    ), # sidebarPanel
    mainPanel(
      ## Output: display data
      DT::dataTableOutput(outputId = ns("table"))
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Import Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list.
#' @seealso [module_import_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_import_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ## Event -------------------------------------------------------------------
    observeEvent({
      input$file
      input$header
      input$sep
      input$dec
      input$quote
      input$rownames
    }, {
      req(input$file)

      tryCatch(
        {
          df <- utils::read.table(
            file = input$file$datapath,
            header = input$header,
            sep = input$sep,
            dec = input$dec,
            quote = input$quote,
            row.names = if (input$rownames) 1 else NULL
          )
        },
        error = function(e) {
          stop(safeError(e)) # Return a safeError if a parsing error occurs
        }
      )

      ## Store data
      user_data$file <- input$file
      user_data$data_raw <- df
    })
    ## Output ------------------------------------------------------------------
    output$table <- DT::renderDataTable({
      req(user_data$data_raw)
      user_data$data_raw
    })
  })
}
