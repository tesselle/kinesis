#' Matrix Seriation Shiny App User Interface Object
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
      "seriation",
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
      navbarMenu(
        title = "Statistics",
        tabPanel(
          title = "Summary",
          janus::module_summary_ui("summary")
        ),
        tabPanel(
          title = "Chi-squared",
          janus::module_chi2_ui("chi2")
        )
      ), # tabPanel
      tabPanel(
        title = "Analysis",
        janus::module_multivar_ui("ca")
      ), # navbarMenu
      tabPanel(
        title = "Seriation",
        janus::module_seriate_ui("seriate")
      ), # tabPanel
      header = janus::module_header_ui("header"),
      footer = janus::module_footer_ui("footer")
    )
  )
}
