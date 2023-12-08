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
  clean <- janus::module_import_server("import") |>
    janus::module_prepare_server("prepare", x = _) |>
    janus::module_missing_server("missing", x = _)

  ## Ternary Plot
  janus::module_ternary_server("ternary", x = clean)

  janus::module_home_server("home")
  janus::module_header_server("header", x = clean)
  janus::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
