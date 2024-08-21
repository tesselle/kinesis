#' Compositional Data Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  bslib::page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    bslib::page_navbar(
      title = "source",
      kinesis::home_ui("home", name = "nexus"),
      bslib::nav_panel(
        title = "Data",
        kinesis::import_ui("import")
      ),
      bslib::nav_panel(
        title = "Composition",
        kinesis::coda_ui("coda")
      ),
      bslib::nav_panel(
        title = "Statistics",
        kinesis::coda_summary_ui("coda_summary")
      ),
      bslib::nav_menu(
        title = "Plot",
        nav_panel(
          title = "Bar plot",
          kinesis::coda_plot_ui("barplot")
        ),
        bslib::nav_panel(
          title = "Ternary plot",
          kinesis::ternary_ui("ternary")
        )
      ),
      bslib::nav_panel(
        title = "Transform",
        kinesis::logratio_ui("logratio")
      ),
      bslib::nav_menu(
        title = "Analysis",
        bslib::nav_panel(
            title = "Outliers",
            kinesis::coda_outliers_ui("outliers")
          ),
        bslib::nav_panel(
            title = "PCA",
            kinesis::pca_ui("pca", scale = FALSE)
          )
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
