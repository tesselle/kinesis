#' Diversity Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare")
  count <- kinesis::count_server("count", x = data) # Remove non-numeric columns

  ## Diversity
  alpha <- kinesis::diversity_alpha_server("alpha", x = count)
  kinesis::diversity_beta_server("beta", x = data, y = alpha)
  kinesis::occurrence_server("occurrence", x = count)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
