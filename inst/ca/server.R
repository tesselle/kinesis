#' Correspondence Analysis Shiny App Server Function
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

  module_import_server("import", user_data)
  module_prepare_server("prepare", user_data)
  module_ca_server("ca", user_data, user_settings)
  module_settings_server("settings", user_settings)
  module_footer_server("footer", cite = "dimensio")
  session$onSessionEnded(stopApp)
}
