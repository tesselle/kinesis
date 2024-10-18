#' Ternary Plot Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  bslib::page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    bslib::page_navbar(
      title = "ternary",
      kinesis::home_ui("home"),
      bslib::nav_panel(
        title = "Data",
        kinesis::import_ui("import")
      ),
      bslib::nav_panel(
        title = "Plot",
        kinesis::ternary_ui("ternary")
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
