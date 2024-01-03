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
      kinesis::module_home_ui("home", name = "kairos"),
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
      navbarMenu(
        title = "Statistics",
        tabPanel(
          title = "Summary",
          kinesis::module_summary_ui("summary")
        ),
        tabPanel(
          title = "Chi-squared",
          kinesis::module_chi2_ui("chi2")
        )
      ), # tabPanel
      tabPanel(
        title = "Analysis",
        kinesis::module_multivar_ui("ca")
      ), # navbarMenu
      tabPanel(
        title = "Seriation",
        kinesis::module_seriate_ui("seriate")
      ), # tabPanel
      header = kinesis::module_header_ui("header"),
      footer = kinesis::module_footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
