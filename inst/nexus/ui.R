#' Compositional Data Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    page_navbar(
      title = "nexus",
      kinesis::home_ui("home", name = "nexus"),
      nav_menu(
        title = "Data",
        nav_panel(
          title = "Import",
          kinesis::import_ui("import")
        ),
        nav_panel(
          title = "Prepare",
          kinesis::prepare_ui("prepare")
        )
      ),
      nav_panel(
        title = "Composition",
        kinesis::coda_ui("coda")
      ),
      nav_panel(
        title = "Statistics",
        kinesis::coda_summary_ui("coda_summary")
      ),
      nav_menu(
        title = "Plot",
        nav_panel(
          title = "Bar plot",
          kinesis::coda_plot_ui("barplot")
        ),
        nav_panel(
          title = "Ternary plot",
          kinesis::ternary_ui("ternary")
        )
      ),
      nav_panel(
        title = "Transform",
        kinesis::logratio_ui()
      ),
      nav_menu(
        title = "Analysis",
          nav_panel(
            title = "Outliers",
            kinesis::coda_outliers_ui("outliers")
          ),
          nav_panel(
            title = "PCA",
            kinesis::pca_ui("pca", scale = FALSE)
          )
      ),
      nav_spacer(),
      nav_menu(
        title = "Links",
        align = "right",
        nav_item(link_tesselle),
        nav_item(link_github)
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
