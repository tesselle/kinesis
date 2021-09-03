# SHINY

#' Run a Shiny App
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
#'   Matrix Seriation \tab `seriate` \cr
#'  }
#' @examples
#' \dontrun{
#' run_app("seriation")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("seriate"),
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
