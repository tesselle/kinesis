#' Matrix Seriation Shiny App Server Function
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
  count <- kinesis::count_server("count", x = data) # Remove non-numeric columns

  ## Switch tab (only happen once)
  observe({
    bslib::nav_select(id = "main", selected = "Data")
  }) |> bindEvent(data(), once = TRUE)

  ## CA
  ca_results <- kinesis::ca_server("ca", x = count)
  kinesis::multivariate_server("ca", ca_results)

  ## Seriation
  seriation <- kinesis::seriate_server("seriate", x = count, order = ca_results)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
