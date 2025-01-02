#' PCA Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare")

  ## Switch tab (only happen once)
  observe({
    bslib::nav_select(id = "main", selected = "Data")
  }) |> bindEvent(data(), once = TRUE)

  ## PCA
  pca_results <- kinesis::pca_server("pca", x = data)
  kinesis::multivariate_server("pca", pca_results)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
