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
      kinesis::home_ui("home", name = "nexus"),
      tabPanel(
        title = "Data",
        kinesis::data_ui("data")
      ), # tabPanel
      tabPanel(
        title = "Composition",
        kinesis::coda_ui("coda")
      ), # tabPanel
      navbarMenu(
        title = "Statistics",
        tabPanel(
          title = "Summary",
          kinesis::coda_summary_ui("coda_summary")
        ), # tabPanel
        tabPanel(
          title = "Outliers",
          kinesis::coda_outliers_ui("outliers")
        ) # tabPanel
      ), # navbarMenu
      navbarMenu(
        title = "Graph",
        tabPanel(
          title = "Bar plot",
          kinesis::coda_plot_ui("barplot")
        ), # tabPanel
        tabPanel(
          title = "Ternary plot",
          kinesis::ternary_ui("ternary")
        ) # tabPanel
      ), # navbarMenu
      tabPanel(
        title = "Transform",
        kinesis::logratio_ui()
      ), # tabPanel
      navbarMenu(
        title = "Analysis",
        tabPanel(
          title = "PCA",
          kinesis::pca_ui("pca", scale = FALSE)
        ) # tabPanel
      ), # navbarMenu
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
