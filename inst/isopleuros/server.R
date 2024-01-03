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
  clean <- kinesis::module_import_server("import") |>
    kinesis::module_prepare_server("prepare", x = _) |>
    kinesis::module_missing_server("missing", x = _)

  ## Ternary Plot
  kinesis::module_ternary_server("ternary", x = clean)

  kinesis::module_home_server("home")
  kinesis::module_header_server("header")
  kinesis::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
