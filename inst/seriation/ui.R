#' Matrix Seriation Shiny App User Interface Object
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
      kinesis::home_ui("home", package = "kairos"),
      bslib::nav_panel(
        title = "Data",
        kinesis::prepare_ui("prepare")
      ),
      bslib::nav_panel(
        title = "Chi-squared",
        kinesis::chi2_ui("chi2")
      ),
      bslib::nav_panel(
        title = "Analysis",
        kinesis::ca_ui("ca")
      ), # navbarMenu
      bslib::nav_panel(
        title = "Seriation",
        kinesis::seriate_ui("seriate")
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
