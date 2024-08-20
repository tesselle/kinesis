#' Ternary Plot Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  ## Data
  data <- kinesis::import_server("import") |>
    kinesis::prepare_server("prepare", x = _)

  ## Ternary Plot
  kinesis::ternary_server("ternary", x = data)

  kinesis::home_server("home")
  kinesis::header_server("header")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
