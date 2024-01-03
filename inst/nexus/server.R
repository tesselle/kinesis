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
  clean <- kinesis::module_import_server("import") |>
    kinesis::module_prepare_server("prepare", x = _) |>
    kinesis::module_missing_server("missing", x = _)

  coda <- kinesis::module_coda_server("coda", x = clean)

  ## Statistics
  kinesis::module_coda_summary_server("coda_summary", coda)

  ## Graphs
  kinesis::module_coda_plot_server("barplot", x = coda)
  kinesis::module_ternary_server("ternary", x = clean)

  ## Log-ratio
  clogratio <- kinesis::module_logratio_server("clr", coda, method = "clr")
  alogratio <- kinesis::module_logratio_server("alr", coda, method = "alr")
  ilogratio <- kinesis::module_logratio_server("ilr", coda, method = "ilr")
  plogratio <- kinesis::module_logratio_server("plr", coda, method = "plr")
  ratio <- list(
    clr = clogratio,
    alr = alogratio,
    ilr = ilogratio,
    plr = plogratio
  )

  ## PCA
  pca_results <- kinesis::module_pca_server("pca", x = ratio)
  kinesis::module_multivar_server("pca", pca_results)

  kinesis::module_home_server("home")
  kinesis::module_header_server("header")
  kinesis::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
