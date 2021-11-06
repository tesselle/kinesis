# HELPERS

#' Deploy an App
#'
#' Deploy a shiny application with \pkg{rsconnect}.
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see details). Any unambiguous substring can be given.
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Matrix Seriation \tab `seriate` \cr
#'  }
#' @examples
#' \dontrun{
#' deploy("seriate")
#' }
#' @family shiny
#' @author N. Frerebeau
deploy <- function(app = c("seriate", "ca")) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    msg <- "Package rsconnect needed for this function to work."
    stop(msg, call. = FALSE)
  }

  app <- match.arg(app, several.ok = FALSE)
  appDir <- system.file(app, package = "janus")

  if (appDir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  ## Deploy to shinyapps.io
  rsconnect::deployApp(appDir = appDir, appTitle = app)
}
