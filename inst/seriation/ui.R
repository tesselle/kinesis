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
      janus::module_home_ui("home"),
      tabPanel(
        title = "Data",
        icon = icon("upload"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Import",
            janus::module_import_ui("import")
          ), # tabPanel
          tabPanel(
            title = "Prepare",
            janus::module_prepare_ui("prepare")
          ), # tabPanel
          tabPanel(
            title = "Missing values",
            janus::module_missing_ui("missing")
          ) # tabPanel
        ) # tabsetPanel
      ), # tabPanel
      tabPanel(
        title = "Statistics",
        icon = icon("line-chart"),
        janus::module_summary_ui("summary")
      ), # tabPanel
      tabPanel(
        title = "Analysis",
        icon = icon("magnifying-glass"),
        janus::module_multivar_ui("ca")
      ), # navbarMenu
      tabPanel(
        title = "Seriation",
        icon = icon("sort-amount-up"),
        janus::module_seriate_ui("seriate")
      ), # tabPanel
      header = janus::module_header_ui("header"),
      footer = janus::module_footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
