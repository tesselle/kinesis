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
  # kinesis::coda_outliers_server("outliers", coda)

  ## Graphs
  kinesis::coda_plot_server("barplot", x = coda)
  kinesis::ternary_server("ternary", x = data)

  ## Log-ratio
  clogratio <- kinesis::logratio_server("clr", coda, method = "clr")
  alogratio <- kinesis::logratio_server("alr", coda, method = "alr")
  ilogratio <- kinesis::logratio_server("ilr", coda, method = "ilr")
  plogratio <- kinesis::logratio_server("plr", coda, method = "plr")

  ## PCA
  pca_results <- kinesis::pca_server("pca", x = clogratio)
  kinesis::multivariate_server("pca", pca_results)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
