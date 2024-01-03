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
  clean <- kinesis::module_import_server("import") |>
    kinesis::module_prepare_server("prepare", x = _) |>
    kinesis::module_missing_server("missing", x = _)

  count <- kinesis::module_count_server("count", x = clean)

  ## Statistics
  kinesis::module_summary_server("summary", x = count)
  kinesis::module_chi_server("chi2", x = count)

  ## Seriation
  ca_results <- kinesis::module_seriate_server("seriate", x = count)

  ## CA
  kinesis::module_multivar_server("ca", x = ca_results)

  kinesis::module_home_server("home")
  kinesis::module_header_server("header")
  kinesis::module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
