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
  data <- kinesis::import_server("import")
  coda <- kinesis::coda_server("coda", x = data)

  ## Statistics
  kinesis::coda_summary_server("coda_summary", coda)
  kinesis::coda_outliers_server("outliers", coda)

  ## Graphs
  kinesis::coda_plot_server("barplot", x = coda)
  kinesis::ternary_server("ternary", x = data)

  ## Log-ratio
  ratio <- kinesis::logratio_server("logratio", coda)

  ## PCA
  pca_results <- kinesis::pca_server("pca", x = ratio)
  kinesis::multivariate_server("pca", pca_results)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
