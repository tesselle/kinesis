#' Compositional Data Analysis Shiny App User Interface Object
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
      "nexus",
      janus::module_home_ui("home", name = "nexus"),
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
        title = "Composition",
        janus::module_coda_ui("coda")
      ), # tabPanel
      tabPanel(
        title = "Statistics",
        janus::module_coda_summary_ui("coda_summary")
      ), # tabPanel
      navbarMenu(
        title = "Graph",
        tabPanel(
          title = "Bar plot",
          janus::module_coda_plot_ui("barplot")
        ), # tabPanel
        tabPanel(
          title = "Ternary plot",
          janus::module_ternary_ui("ternary")
        ) # tabPanel
      ), # navbarMenu
      tabPanel(
        title = "Transform",
        janus::module_logratio_ui()
      ), # tabPanel
      navbarMenu(
        title = "Analysis",
        tabPanel(
          title = "PCA",
          janus::module_pca_ui("pca")
        ), # tabPanel
        tabPanel(
          title = "Outlier detection"
        ) # tabPanel
      ), # navbarMenu
      header = janus::module_header_ui("header"),
      footer = janus::module_footer_ui("footer")
    )
  )
}
