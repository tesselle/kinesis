#' Correspondence Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    navbarPage(
      "Correspondence Analysis",
      module_import_ui("import"),
      module_ca_ui("ca"),
      module_settings_ui("settings"),
      module_about_ui("about"),
      footer = module_footer_ui("footer", name = "ca")
    )
  )
}
