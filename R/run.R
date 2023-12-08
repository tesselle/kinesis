# SHINY

#' Run an App
#'
#' A wrapper for [shiny::shinyAppDir()].
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see details). Any unambiguous substring can be given.
#' @param options A [`list`] of named options that should be passed to the
#'  [`shiny::runApp()`] call.
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Compositional Data Analysis \tab `coda` \cr
#'   Matrix Seriation \tab `seriation` \cr
#'   Ternary Plot \tab `ternary` \cr
#'  }
#' @examples
#' \dontrun{
#' run_app("seriation")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny apps
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("seriation", "coda", "ternary"),
                    options = list(launch.browser = interactive())) {
  app <- match.arg(app, several.ok = FALSE)
  appDir <- system.file(app, package = "janus")

  if (appDir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  obj <- shiny::shinyAppDir(appDir = appDir, options = options)

  ## Bundling the options inside the shinyApp object
  janus_options <- get_config()
  obj$appOptions$janus_options <- janus_options

  obj
}

#' Read Configuration Values
#'
#' @param app A [`character`] string specifying the Shiny application
#'  configuration to get . Any unambiguous substring can be given.
#' @param config A [`character`] string specifying the name of configuration to
#'  read from.
#' @param use_parent A [`logical`] scalar: should parent directories be scanned
#'  for configuration files if the specified config file isn't found?
#' @return A [`list`] of configuration values.
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_config <- function(config = Sys.getenv("JANUS_CONFIG_ACTIVE", "default"),
                       use_parent = TRUE) {

  file <- system.file("config.yml", package = "janus", mustWork = FALSE)
  config::get(value = NULL, config = config, file = file,
              use_parent = use_parent)
}

#' Get Option
#'
#' @param name A [`character`] string specifying the name of an option to get.
#'  If `NULL` (the default), all options are returned.
#' @param default A value to be returned if the option is not currently set.
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_option <- function(name = NULL, default = NULL) {
  if (is.null(name)) {
    shiny::getShinyOption("janus_options")
  } else {
    shiny::getShinyOption("janus_options")[[name]] %||% default
  }
}
