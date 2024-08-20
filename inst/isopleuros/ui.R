#' Ternary Plot Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    page_navbar(
      title = "isopleuros",
      kinesis::home_ui("home", name = "isopleuros"),
      nav_panel(
        title = "Import",
        kinesis::import_ui("import")
      ),
      nav_panel(
        title = "Prepare",
        kinesis::prepare_ui("prepare")
      ),
      nav_panel(
        title = "Plot",
        kinesis::ternary_ui("ternary")
      ),
      nav_spacer(),
      nav_menu(
        title = "Links",
        align = "right",
        nav_item(link_tesselle),
        nav_item(link_github)
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
