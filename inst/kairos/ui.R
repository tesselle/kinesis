#' Matrix Seriation Shiny App User Interface Object
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
      "kairos",
      kinesis::home_ui("home", name = "kairos"),
      tabPanel(
        title = "Data",
        kinesis::data_ui("data")
      ), # tabPanel
      navbarMenu(
        title = "Statistics",
        tabPanel(
          title = "Summary",
          kinesis::summary_ui("summary")
        ),
        tabPanel(
          title = "Chi-squared",
          kinesis::chi2_ui("chi2")
        )
      ), # tabPanel
      tabPanel(
        title = "Analysis",
        kinesis::multivariate_ui("ca")
      ), # navbarMenu
      tabPanel(
        title = "Seriation",
        kinesis::seriate_ui("seriate")
      ), # tabPanel
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
