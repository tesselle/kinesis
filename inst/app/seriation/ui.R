#' Matrix Seriation Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    id = "main",
    kinesis::home_ui("home", package = "kairos"),
    bslib::nav_panel(
      title = "Data",
      kinesis::prepare_ui("prepare")
    ),
    bslib::nav_panel(
      title = "Analysis",
      kinesis::ca_ui("ca")
    ), # navbarMenu
    bslib::nav_panel(
      title = "Seriation",
      kinesis::seriate_ui("seriate")
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
