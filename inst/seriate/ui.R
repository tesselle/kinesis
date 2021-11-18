#' Matrix Seriation Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    navbarPage(
      "Matrix Seriation",
      module_import_ui("import"),
      module_ca_ui("ca"),
      module_seriate_ui("seriate"),
      module_settings_ui("settings"),
      module_about_ui("about", cite = "tabula"),
      footer = module_footer_ui("footer", name = "seriate")
    )
  )
}
