# UI ===========================================================================
#' Header UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_header_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_header_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  list(
    uiOutput(outputId = ns("alert_config")),
    uiOutput(outputId = ns("alert_missing"))
  )
}

# Server =======================================================================
#' Header Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_import_server()]).
#' @seealso [module_header_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_header_server  <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    alert_missing <- reactive({
      invalidateLater(100, session)
      if (!anyNA(x())) return(NULL)
      div(
        class = "alert alert-warning",
        role = "alert",
        "Missing values detected!"
      )
    })

    output$alert_config <- renderUI({
      if (getOption("janus.config") == "production") return(NULL)
      div(
        class = "alert alert-warning",
        role = "alert",
        "This application is under development, so you shouldn't use it for anything serious!"
      )
    })
    output$alert_missing <- renderUI(alert_missing())
  })
}
