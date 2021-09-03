#' Count Data Analysis Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  # Set reactive values
  user_data <- reactiveValues()
  user_settings <- reactiveValues()
  callModule(module_import_server, "import", user_data, user_settings)
  callModule(module_seriate_server, "seriate", user_data, user_settings)
  callModule(module_footer_server, "footer", user_data, user_settings)
  session$onSessionEnded(stopApp)
}
