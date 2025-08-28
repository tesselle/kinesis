# UI ===========================================================================
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @noRd
variables_ui <- function(id, label = "Variables") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  checkboxGroupInput(
    inputId = ns("names"),
    label = label,
    choices = NULL,
    selected = NULL,
    inline = TRUE
  )
}

# Server =======================================================================
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param detect A predicate [`function`] for column detection
#'  (see [arkhe::detect()]).
#' @param select A [`logical`] scalar: should all detected columns be selected?
#' @param min_row An [`integer`] specifying the expected minimum number of rows.
#' @param min_col An [`integer`] specifying the expected minimum number of columns.
#' @return A reactive [`data.frame`].
#' @noRd
variables_server <- function(id, x, detect = NULL, select = TRUE,
                             min_row = 1, min_col = 1) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      choices <- colnames(x())
      if (is.function(detect)) {
        found <- which(arkhe::detect(x = x(), f = detect, margin = 2))
        choices <- choices[found]
      }
      selected <- if (isTRUE(select)) choices else NULL

      freezeReactiveValue(input, "names")
      updateCheckboxGroupInput(
        inputId = "names",
        choices = choices,
        selected = selected
      )
    }) |>
      bindEvent(x())

    ## Select variables
    selected <- reactive({
      req(x())
      out <- arkhe::get_columns(x(), names = input$names)
      validate_dim(out, i = min_row, j = min_col)
      out
    }) |>
      debounce(500)


    selected
  })
}
