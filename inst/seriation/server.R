#' Matrix Seriation Shiny App Server Function
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
  count <- kinesis::count_server("count", x = data)

  ## Statistics
  kinesis::summary_server("summary", x = count)
  kinesis::chi2_server("chi2", x = count)

  ## Seriation
  ca_results <- kinesis::seriate_server("seriate", x = count)

  ## CA results
  kinesis::multivariate_server("ca", x = ca_results)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
