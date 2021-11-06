#' Count Data Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    navbarPage(
      "Matrix Seriation",
      module_import_ui("import"),
      module_seriate_ui("seriate"),
      module_about_ui("about"),
      footer = module_footer_ui("footer", name = "seriate")
    )
  )
}
