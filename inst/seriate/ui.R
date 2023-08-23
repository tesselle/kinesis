#' Matrix Seriation Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    includeCSS("www/style.css"),
    theme = bs_theme(),
    navbarPage(
      "Matrix Seriation",
      module_data_ui("data"),
      module_ca_ui("ca"),
      module_seriate_ui("seriate"),
      module_settings_ui("settings"),
      module_about_ui("about", cite = c("kairos", "tabula")),
      footer = module_footer_ui("footer", name = "seriate")
    )
  )
}
