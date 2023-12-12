#' Ternary Plot Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    includeCSS(system.file("static", "style.css", package = "janus")),
    theme = bslib::bs_theme(),
    collapsible = TRUE,
    lang = "en",
    navbarPage(
      "ternary",
      janus::module_home_ui("home"),
      tabPanel(
        title = "Data",
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
        title = "Plot",
        janus::module_ternary_ui("ternary")
      ), # tabPanel
      header = janus::module_header_ui("header"),
      footer = janus::module_footer_ui("footer")
    )
  )
}
