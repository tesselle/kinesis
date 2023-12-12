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
    uiOutput(outputId = ns("alert_config"))
  )
}

# Server =======================================================================
#' Header Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [module_header_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_header_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$alert_config <- renderUI({
      if (get_option("production")) return(NULL)
      div(
        class = "alert alert-warning",
        role = "alert",
        "This application is under development, so you shouldn't use it for anything serious!"
      )
    })
  })
}
