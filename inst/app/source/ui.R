#' Compositional Data Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    id = "main",
    bslib::nav_panel(
      title = "Home",
      kinesis::home_ui("home", package = "nexus")
    ),
    bslib::nav_panel(
      title = "Data",
      kinesis::coda_ui("coda")
    ),
    bslib::nav_menu(
      title = "Transform",
      bslib::nav_panel(
        title = "CLR",
        value = "panel_clr",
        kinesis::logratio_ui("clr")
      ),
      bslib::nav_panel(
        title = "ALR",
        value = "panel_alr",
        kinesis::logratio_ui("alr")
      ),
      bslib::nav_panel(
        title = "ILR",
        value = "panel_ilr",
        kinesis::logratio_ui("ilr")
      ),
      bslib::nav_panel(
        title = "PLR",
        value = "panel_plr",
        kinesis::logratio_ui("plr")
      )
    ),
    bslib::nav_panel(
      title = "Statistics",
      kinesis::coda_summary_ui("coda_summary")
    ),
    bslib::nav_menu(
      title = "Plot",
      bslib::nav_panel(
        title = "Bar plot",
        kinesis::coda_plot_ui("barplot")
      ),
      bslib::nav_panel(
        title = "Ternary plot",
        kinesis::ternary_ui("ternary")
      )
    ),
    bslib::nav_menu(
      title = "Analysis",
      # bslib::nav_panel(
      #   title = "Outliers",
      #   kinesis::coda_outliers_ui("outliers")
      # ),
      bslib::nav_panel(
        title = "PCA",
        kinesis::pca_ui("pca", scale = FALSE)
      ),
      bslib::nav_panel(
        title = "HCLUST",
        kinesis::coda_hclust_ui("clust")
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    footer = kinesis::footer_ui("footer"),
    theme = kinesis::theme_ui(),
    lang = "en",
    collapsible = TRUE
  )
}
