#' Ternary Plot Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    theme = bslib::bs_theme(),
    lang = "en",
    navbarPage(
      "isopleuros",
      kinesis::home_ui("home", name = "isopleuros"),
      tabPanel(
        title = "Data",
        kinesis::data_ui("data")
      ), # tabPanel
      tabPanel(
        title = "Plot",
        kinesis::ternary_ui("ternary")
      ), # tabPanel
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
