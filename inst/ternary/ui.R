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
      kinesis::home_ui("home", name = "isopleuros"),
      bslib::nav_panel(
        title = "Import",
        kinesis::import_ui("import")
      ),
      bslib::nav_panel(
        title = "Prepare",
        kinesis::prepare_ui("prepare")
      ),
      bslib::nav_panel(
        title = "Plot",
        kinesis::ternary_ui("ternary")
      ),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Links",
        align = "right",
        bslib::nav_item(link_tesselle),
        bslib::nav_item(link_github)
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
