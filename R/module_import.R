
# UI ===========================================================================
#' Import UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_import_server()]
#' @family UI modules
#' @keywords internal
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
          ) # tabsetPanel
        )
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}

# Server =======================================================================
#' Import Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the following
#'  elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @param mode A [`character`] string specifying the target data type.
#'  It must be one of `"count"`, `"composition"` or `"incidence"`.
#'  Any unambiguous substring can be given.
#' @seealso [module_import_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_import_server <- function(id, user_data, user_settings, mode) {
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

      ## Coerce to Matrix
      arkhe_coerce <- switch (
        mode,
        count = arkhe::as_count,
        composition = arkhe::as_composition,
        incidence = arkhe::as_incidence,
        stop("Don't what to do...", call. = FALSE)
      )

      ## Store data
      user_data$file <- input$file
      user_data$data <- arkhe_coerce(df)
    })
    ## Reactive ----------------------------------------------------------------
    data <- reactive({
      req(user_data$data)
      user_data$data
    })
    ## Output ------------------------------------------------------------------
    output$summary <- renderPrint({
      summary(data())
    })
    output$table <- renderTable(
      {
        switch(
          input$display,
          head = utils::head(data()),
          all = data()
        )
      },
      rownames = TRUE,
      colnames = TRUE
    )
  })
}

