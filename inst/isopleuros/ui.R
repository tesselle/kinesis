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
      kinesis::module_home_ui("home", name = "isopleuros"),
      tabPanel(
        title = "Data",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Import",
            kinesis::module_import_ui("import")
          ), # tabPanel
          tabPanel(
            title = "Prepare",
            kinesis::module_prepare_ui("prepare")
          ), # tabPanel
          tabPanel(
            title = "Missing values",
            kinesis::module_missing_ui("missing")
          ) # tabPanel
        ) # tabsetPanel
      ), # tabPanel
      tabPanel(
        title = "Plot",
        kinesis::module_ternary_ui("ternary")
      ), # tabPanel
      header = kinesis::module_header_ui("header"),
      footer = kinesis::module_footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
