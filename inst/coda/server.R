#' Compositional Data Analysis Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  ## Data
  clean <- janus::module_import_server("import") |>
    janus::module_prepare_server("prepare", x = _) |>
    janus::module_missing_server("missing", x = _)

  coda <- janus::module_coda_server("coda", x = clean)

  ## Statistics
  janus::module_summary_server("summary", coda)
  janus::module_coda_summary_server("coda_summary", coda)

  ## Graphs
  janus::module_coda_plot_server("barplot", coda)
  janus::module_ternary_server("ternary", x = clean)

  ## Log-ratio
  clogratio <- janus::module_logratio_server("clr", coda, method = "clr")
  alogratio <- janus::module_logratio_server("alr", coda, method = "alr")
  ilogratio <- janus::module_logratio_server("ilr", coda, method = "ilr")
  plogratio <- janus::module_logratio_server("plr", coda, method = "plr")

  ## PCA
  pca_results <- janus::module_pca_server("pca", clogratio)
  janus::module_multivar_server("pca", pca_results)

  janus::module_home_server("home")
  janus::module_header_server("header", x = coda)
  janus::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
