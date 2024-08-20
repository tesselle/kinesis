#' Matrix Seriation Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  page_fillable(
    includeCSS(system.file("static", "style.css", package = "kinesis")),
    lang = "en",
    page_navbar(
      title = "seriate",
      kinesis::home_ui("home", name = "kairos"),
      nav_panel(
        title = "Import",
        kinesis::import_ui("import")
      ),
      nav_panel(
        title = "Prepare",
        kinesis::prepare_ui("prepare")
      ),
      nav_menu(
        title = "Statistics",
        nav_panel(
          title = "Summary",
          kinesis::summary_ui("summary")
        ),
        nav_panel(
          title = "Chi-squared",
          kinesis::chi2_ui("chi2")
        )
      ),
      nav_panel(
        title = "Analysis",
        kinesis::multivariate_ui("ca")
      ), # navbarMenu
      nav_panel(
        title = "Seriation",
        kinesis::seriate_ui("seriate")
      ),
      header = kinesis::header_ui("header"),
      footer = kinesis::footer_ui("footer"),
      collapsible = TRUE
    )
  )
}
