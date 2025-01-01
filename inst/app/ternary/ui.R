#' Ternary Plot Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    kinesis::home_ui("home", package = "isopleuros"),
    bslib::nav_panel(
      title = "Data",
      kinesis::prepare_ui("prepare")
    ),
    bslib::nav_panel(
      title = "Plot",
      kinesis::ternary_ui("ternary")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    header = kinesis::header_ui("header"),
    footer = kinesis::footer_ui("footer"),
    theme = kinesis::theme_ui(),
    lang = "en",
    collapsible = TRUE
  )
}
