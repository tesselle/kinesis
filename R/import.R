# UI ===========================================================================
#' Import Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [import_server()]
#' @family generic modules
#' @keywords internal
#' @export
import_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Import"),
      ## Input: select a file
      fileInput(
        inputId = ns("file"),
        label = "Choose a CSV file:",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      tags$p(
        helpText("Select the location of, and the CSV file you want to upload."),
        helpText("Please check the default settings below and adjust them to your data."),
        helpText("This application only supports data encoded in UFT-8."),
      ),
      tags$p(
        helpText("It assumes that you keep your data tidy: each variable must be saved in its own column and each observation (sample) must be saved in its own row.")
      ),
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
        value = FALSE
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
        choices = c(None = "", "Double quote" = '"', "Single quote" = "'"),
        selected = '"'
      ),
      ## Input: lines of the data to skip
      numericInput(
        inputId = ns("skip"),
        label = "Lines of the data file to skip:",
        value = 0,
        min = 0,
        step = 1
      ),
      ## Input: missing string
      textInput(
        inputId = ns("na.strings"),
        label = "String to be interpreted as missing value:",
        value = ""
      ),
      ## Input: comment
      textInput(
        inputId = ns("comment"),
        label = "Character to be interpreted as comment:",
        value = "#"
      )
    ), # sidebar
    prepare_ui(ns("prepare")),
    border_radius = FALSE,
    fillable = TRUE,
    class = "p-0"
  ) # layout_sidebar
}

# Server =======================================================================
#' Import Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return A reactive `data.frame`.
#' @seealso [import_ui()]
#' @family generic modules
#' @keywords internal
#' @export
import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Get file path -----
    file <- reactive({
      query <- parseQueryString(session$clientData$url_search)

      if (!is.null(query[['data']])) {
        url(as.character(query[['data']]))
      } else {
        input$file$datapath
      }
    })

    ## Read data file -----
    data <- reactive({
      assert_csv(file())

      id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      read_table(
        path = file(),
        header = input$header,
        sep = input$sep,
        dec = input$dec,
        quote = input$quote,
        rownames = if (input$rownames) 1 else NULL,
        na.strings = input$na.strings,
        skip = if (!is.na(input$skip)) input$skip else 0,
        comment.char = input$comment
      )
    })

    ## Send notification -----
    bindEvent(
      observe({
        if (is.data.frame(data()) && all(dim(data()) > 0)) {
          msg <- "Data successfully imported!"
          showNotification(ui = msg, type = "message")
        }
      }),
      data()
    )

    ## Prepare data -----
    clean <- prepare_server("prepare", data)

    clean
  })
}

# Utils ========================================================================
read_table <- function(path, header = TRUE, sep = ",", dec = ".", quote = "\"'",
                       rownames = NULL, na.strings = "NA", skip = 0,
                       comment.char = "#") {
  tryCatch(
    {
      utils::read.table(file = path, header = header, sep = sep, dec = dec,
                        quote = quote, row.names = rownames,
                        na.strings = na.strings, skip = skip,
                        comment.char = comment.char)
    },
    error = function(e) {
      showModal(modalDialog(
        conditionMessage(e),
        title = "Data import failed!",
        easyClose = TRUE
      ))
      return(NULL)
    }
  )
}
