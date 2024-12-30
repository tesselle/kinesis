#' Diversity Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    kinesis::home_ui("home", package = "tabula"),
    bslib::nav_panel(
      title = "Data",
      kinesis::prepare_ui("prepare")
    ),
    bslib::nav_panel(
      title = HTML("&#945; Diversity"),
      kinesis::diversity_alpha_ui("alpha")
    ),
    bslib::nav_panel(
      title = HTML("&#946; Diversity"),
      kinesis::diversity_beta_ui("beta")
    ),
    bslib::nav_panel(
      title = "Co-Occurrence",
      kinesis::occurrence_ui("occurrence")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    header = kinesis::header_ui("header"),
    footer = kinesis::footer_ui("footer"),
    theme = kinesis::theme_ui(),
    lang = "en",
    collapsible = TRUE
  )
}
