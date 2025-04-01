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

  list(
    helpText("Import your data and perform basic data cleansing and preparation steps."),
    actionButton(
      inputId = ns("upload"),
      label = tr_("Upload"),
      icon = icon("upload")
    )
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
    title = tr_("Import Data"),
    footer = tagList(
      modalButton(tr_("Cancel")),
      actionButton(inputId = ns("go"), label = "OK", class = "btn-primary")
    ),
    layout_column_wrap(
      width = 1/3,
      ## Input: select a file
      div(
        tags$p(
          helpText("Select the location of, and the file you want to upload."),
          helpText("Please check the default settings and adjust them to your data."),
          helpText("This application only supports data encoded in UFT-8."),
        ),
        tags$p(
          helpText("It assumes that you keep your data tidy:"),
          helpText("each variable must be saved in its own column and each sample must be saved in its own row.")
        ),
        fileInput(
          inputId = ns("file"),
          label = tr_("Choose a CSV or a TSV file:"),
          multiple = FALSE,
          accept = c(".csv", ".tsv", "text/csv", "text/tsv",
                     "text/comma-separated-values", "text/tab-separated-values")
        )
      ),
      div(
        ## Input: checkbox if file has header
        input_switch(
          id = ns("header"),
          label = tr_("Header"),
          value = TRUE
        ),
        ## Input: checkbox if file has row names
        input_switch(
          id = ns("rownames"),
          label = tr_("Row names"),
          value = FALSE
        ),
        ## Input: select decimal
        radioButtons(
          inputId = ns("dec"),
          label = tr_("Decimal"),
          choices = c(Dot = ".", Comma = ","),
          selected = "."
        ),
        ## Input: select separator
        radioButtons(
          inputId = ns("sep"),
          label = tr_("Separator"),
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        ),
        ## Input: select quotes
        radioButtons(
          inputId = ns("quote"),
          label = tr_("Quote"),
          choiceNames = c(tr_("None"), tr_("Double quote"), tr_("Single quote")),
          choiceValues = c("", '"', "'"),
          selected = '"'
        )
      ),
      div(
        ## Input: lines of the data to skip
        numericInput(
          inputId = ns("skip"),
          label = tr_("Lines of the data file to skip:"),
          value = 0,
          min = 0,
          step = 1
        ),
        ## Input: missing string
        textInput(
          inputId = ns("na.strings"),
          label = tr_("String to be interpreted as missing value:"),
          value = ""
        ),
        ## Input: comment
        textInput(
          inputId = ns("comment"),
          label = tr_("Character to be interpreted as comment:"),
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
    data <- reactiveValues(values = NULL)

    ## Show modal dialog -----
    observe({ showModal(import_modal(session$ns)) }) |>
      bindEvent(input$upload)

    ## Read from connection -----
    obs <- observe({
      params <- parseQueryString(session$clientData$url_search)
      query <- params[["data"]]

      if (!is.null(query)) {
        msg <- sprintf(tr_("Reading data from %s..."), query)
        id <- showNotification(msg, duration = NULL, closeButton = FALSE,
                               type = "message")
        on.exit(removeNotification(id), add = TRUE)

        data$values <- notify(utils::read.csv(file = url(query)), tr_("Data Input"))
      }
      obs$destroy()
    })

    ## Read data file -----
    observe({
      id <- showNotification(tr_("Reading data..."), duration = NULL,
                             closeButton = FALSE, type = "message")
      on.exit(removeNotification(id), add = TRUE)

      x <- notify({
        utils::read.table(
          file = input$file$datapath,
          header = input$header,
          sep = input$sep,
          dec = input$dec,
          quote = input$quote,
          row.names = if (isTRUE(input$rownames)) 1 else NULL,
          na.strings = input$na.strings,
          skip = if (!is.na(input$skip)) input$skip else 0,
          comment.char = input$comment
        )},
        title = "Data Upload"
      )

      if (!is.null(x)) removeModal()
      data$values <- x
    }) |>
      bindEvent(input$go)

    ## Bookmark -----
    setBookmarkExclude(c("upload", "go"))
    onBookmark(function(state) state$values$data <- data$values)
    onRestore(function(state) data$values <- state$values$data)

    reactive({
      validate_csv(data$values)
      data$values
    })
  })
}
