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
  clean <- janus::module_import_server("import") |>
    janus::module_prepare_server("prepare", x = _) |>
    janus::module_missing_server("missing", x = _)

  count <- janus::module_count_server("count", x = clean)

  ## Statistics
  janus::module_summary_server("summary", x = count)
  janus::module_chi_server("chi2", x = count)

  ## Seriation
  ca_results <- janus::module_seriate_server("seriate", x = count)

  ## CA
  janus::module_multivar_server("ca", x = ca_results)

  janus::module_home_server("about")
  janus::module_header_server("header", x = count)
  janus::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
