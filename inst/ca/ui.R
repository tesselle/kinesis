#' CA Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  bslib::page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    bslib::page_navbar(
      title = toupper(kinesis::get_option("name")),
      kinesis::home_ui("home", package = "dimensio"),
      bslib::nav_panel(
        title = "Data",
        kinesis::import_ui("import")
      ),
      bslib::nav_panel(
        title = "CA",
        kinesis::ca_ui("ca")
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
