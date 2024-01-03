#' Compositional Data Analysis Shiny App User Interface Object
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
      "nexus",
      kinesis::module_home_ui("home", name = "nexus"),
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
        title = "Composition",
        kinesis::module_coda_ui("coda")
      ), # tabPanel
      tabPanel(
        title = "Statistics",
        kinesis::module_coda_summary_ui("coda_summary")
      ), # tabPanel
      navbarMenu(
        title = "Graph",
        tabPanel(
          title = "Bar plot",
          kinesis::module_coda_plot_ui("barplot")
        ), # tabPanel
        tabPanel(
          title = "Ternary plot",
          kinesis::module_ternary_ui("ternary")
        ) # tabPanel
      ), # navbarMenu
      tabPanel(
        title = "Transform",
        kinesis::module_logratio_ui()
      ), # tabPanel
      navbarMenu(
        title = "Analysis",
        tabPanel(
          title = "PCA",
          kinesis::module_pca_ui("pca", scale = FALSE)
        ), # tabPanel
        tabPanel(
          title = "Outlier detection"
        ) # tabPanel
      ), # navbarMenu
      header = kinesis::module_header_ui("header"),
      footer = kinesis::module_footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
