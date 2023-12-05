#' Matrix Seriation Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    includeCSS(system.file("style.css", package = "janus")),
    theme = bslib::bs_theme(),
    lang = "en",
    navbarPage(
      "seriation",
      module_home_ui("home"),
      tabPanel(
        title = "Data",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Import",
            module_import_ui("import")
          ), # tabPanel
          tabPanel(
            title = "Prepare",
            module_prepare_ui("prepare")
          ), # tabPanel
          tabPanel(
            title = "Missing values",
            module_missing_ui("missing")
          ) # tabPanel
        ) # tabsetPanel
      ), # tabPanel
      tabPanel(
        title = "Statistics",
        module_summary_ui("summary")
      ), # tabPanel
      tabPanel(
        title = "Analysis",
        module_multivar_ui("ca")
      ), # navbarMenu
      tabPanel(
        title = "Seriation",
        icon = icon("sort-amount-up"),
        module_seriate_ui("seriate")
      ), # tabPanel
      header = module_header_ui("header"),
      footer = module_footer_ui("footer")
    )
  )
}
