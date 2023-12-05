#' Matrix Seriation Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  ## Get configuration
  config <- get_config("seriation")

  ## Data
  clean <- module_import_server("import") |>
    module_prepare_server("prepare", x = _) |>
    module_missing_server("missing", x = _)

  count <- module_count_server("count", x = clean)

  ## Statistics
  module_summary_server("summary", x = count)

  ## Seriation
  ca_results <- module_seriate_server("seriate", x = count)

  ## CA
  module_multivar_server("ca", x = ca_results)

  module_home_server("about")
  module_header_server("header", x = count)
  module_footer_server("footer")
  session$onSessionEnded(stopApp)
}
