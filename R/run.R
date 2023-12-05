# SHINY

#' Run an App
#'
#' A wrapper for [shiny::shinyAppDir()].
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see details). Any unambiguous substring can be given.
#' @param browser A [`logical`] scalar: should the app be run in
#'  the browser?
#' @param display A [`character`] string specifying the mode in which
#'  to display the application (see [shiny::runApp()]).
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Matrix Seriation \tab `seriation` \cr
#'  }
#' @examples
#' \dontrun{
#' run_app("seriation")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny apps
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("seriation"),
                    browser = TRUE, display = "auto") {
  app <- match.arg(app, several.ok = FALSE)
  appDir <- system.file(app, package = "janus")

  if (appDir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  shiny::shinyAppDir(
    appDir = appDir,
    options = list(launch.browser = browser, display.mode = display)
  )
}

#' Read Configuration Values
#'
#' A wrapper for [config::get()].
#' @param app A [`character`] string specifying the Shiny application
#'  configuration to get . Any unambiguous substring can be given.
#' @param config A [`character`] string specifying the name of configuration to
#'  read from.
#' @return A [`list`] of configuration values.
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_config <- function(app = c("seriation"), config = getOption("janus.config")) {
  app <- match.arg(app, several.ok = FALSE)
  file <- system.file(app, "config.yml", package = "janus")

  if (file == "")
    stop(sprintf("Could not find %s configuration.", sQuote(app)), call. = FALSE)

  config::get(value = NULL, config = config %||% "default", file = file)
}
