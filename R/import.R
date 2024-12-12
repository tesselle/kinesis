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

  actionButton(
    inputId = ns("upload"),
    label = "Upload",
    icon = icon("upload")
  )
}

#' Import Data Modal
#'
#' @param ns A [namespace][shiny::NS()] function.
#' @seealso [import_server()]
#' @keywords internal
#' @noRd
import_modal <- function(ns) {
  modalDialog(
    size = "xl",
    easyClose = FALSE,
    fade = FALSE,
    title = "Import Data",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("go"), "OK")
    ),
    layout_column_wrap(
      width = 1/3,
      ## Input: select a file
      div(
        tags$p(
          helpText("Select the location of, and the CSV file you want to upload."),
          helpText("Please check the default settings and adjust them to your data."),
          helpText("This application only supports data encoded in UFT-8."),
        ),
        tags$p(
          helpText("It assumes that you keep your data tidy:"),
          helpText("each variable must be saved in its own column and each sample must be saved in its own row.")
        ),
        fileInput(
          inputId = ns("file"),
          label = "Choose a CSV file:",
          multiple = FALSE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        )
      ),
      div(
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
        )
      ),
      div(
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
      )
    )
  )
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
    ## Show modal dialog -----
    bindEvent(
      observe({ showModal(import_modal(session$ns)) }),
      input$upload
    )

    ## Read data file -----
    data <- bindEvent(
      reactive({
        id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)

        x <- run_with_notification({
          if (tolower(tools::file_ext(input$file$datapath)) != "csv") {
            stop("Import a CSV file.", call. = FALSE)
          }

          utils::read.table(
            file = input$file$datapath,
            header = input$header,
            sep = input$sep,
            dec = input$dec,
            quote = input$quote,
            row.names = if (input$rownames) 1 else NULL,
            na.strings = input$na.strings,
            skip = if (!is.na(input$skip)) input$skip else 0,
            comment.char = input$comment
          )},
          what = "Data import"
        )

        if (!is.null(x)) {
          removeModal()
          x
        }
      }),
      input$go
    )

    data
  })
}
