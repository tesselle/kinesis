# UI ===========================================================================
#' Prepare Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_prepare_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_prepare_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      accordion(
        id = ns("prepare_accordion"),
        open = FALSE,
        accordion_panel(
          title = tooltip(
            span("1. Select columns", icon("circle-question")),
            "Select the columns you would like to keep.",
            placement = "auto"
          ),
          value = "select",
          checkboxGroupInput(
            inputId = ns("select_extra"),
            label = "Select supplementary variables",
            width = "100%"
          ),
          helpText("Selected variables will be excluded from calculations.")
        ),
        accordion_panel(
          title = tooltip(
            span("2. Remove data", icon("circle-question")),
            "Remove any non informative data.",
            placement = "auto"
          ),
          value = "remove",
          ## Input: remove missing
          checkboxInput(
            inputId = ns("remove_missing_row"),
            label = "Remove rows with missing values",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("remove_missing_column"),
            label = "Remove columns with missing values",
            value = FALSE
          ),
          ## Input: remove empty
          checkboxInput(
            inputId = ns("remove_empty_row"),
            label = "Remove empty rows",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("remove_empty_column"),
            label = "Remove empty columns",
            value = FALSE
          ),
          ## Input: remove constant
          checkboxInput(
            inputId = ns("remove_constant_column"),
            label = "Remove constant columns",
            value = FALSE
          )
        ),
        accordion_panel(
          title = tooltip(
            span("3. Filter rows", icon("circle-question")),
            "Remove data points that fall outside a specification.",
            placement = "auto"
          ),
          value = "filter"
        )
      )
    ), # column
    column(
      width = 8,
      ## Output: display data
      DT::dataTableOutput(outputId = ns("table"))
    ) # column
  ) # fluidRow
}

# Server =======================================================================
#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list.
#' @seealso [module_prepare_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_prepare_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    data_raw <- reactive({
      req(user_data$data_raw)
      user_data$data_raw
    })
    ## Observe -----------------------------------------------------------------
    observe({
      i <- vapply(X = data_raw(), FUN = is.numeric, FUN.VALUE = logical(1))
      choices <- colnames(data_raw())
      selected <- choices[!i]
      updateCheckboxGroupInput(inputId = "select_extra", inline = TRUE,
                               choices = choices, selected = selected)
    })
    observe({
      j <- colnames(data_raw()) %in% input$select_extra
      quanti <- data_raw()[, !j, drop = FALSE]
      extra <- data_raw()[, j, drop = FALSE]

      ## Remove row/column
      if (isTRUE(input$remove_missing_row)) {
        miss <- apply(X = quanti, MARGIN = 1, FUN = function(x) any(is.na(x)))
        quanti <- quanti[!miss, , drop = FALSE]
        extra <- extra[!miss, , drop = FALSE]
      }
      if (isTRUE(input$remove_missing_col)) {
        quanti <- arkhe::remove_NA(quanti, margin = 2, all = FALSE)
      }
      if (isTRUE(input$remove_empty_row)) {
        zeros <- rowSums(quanti, na.rm = FALSE) == 0
        quanti <- quanti[!zeros, , drop = FALSE]
        extra <- extra[!zeros, , drop = FALSE]
      }
      if (isTRUE(input$remove_empty_col)) {
        quanti <- arkhe::remove_zero(quanti, margin = 2, all = TRUE, na.rm = FALSE)
      }
      if (isTRUE(input$remove_constant_column)) {
        quanti <- arkhe::remove_constant(quanti, na.rm = FALSE)
      }

      ## Filter rows
      # TODO

      user_data$data_quanti <- quanti
      user_data$data_extra <- extra
    })
    ## Output ------------------------------------------------------------------
    output$table <- DT::renderDataTable({
      req(user_data$data_quanti)
      x <- cbind(user_data$data_extra, user_data$data_quanti)
      x <- DT::datatable(x)

      if (ncol(req(user_data$data_extra)) > 0) {
        x <- DT::formatStyle(
          table = x,
          columns = seq_len(ncol(user_data$data_extra)),
          backgroundColor = "lightgrey"
        )
      }

      x
    })
  })
}
