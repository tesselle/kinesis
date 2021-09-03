# HELPERS

#' Deploy an App to shinyapps.io
#'
#' FOR DEVELOPMENT ONLY.
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see details). Any unambiguous substring can be given.
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Matrix Seriation \tab `seriate` \cr
#'  }
#' @examples
#' \dontrun{
#' devtools::load_all(".")
#' deploy("seriate")
#' }
#' @family shiny
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
deploy <- function(app = c("seriate")) {
  app <- match.arg(app, several.ok = FALSE)
  appDir <- system.file(app, package = "janus")

  if (appDir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  ## Write app.R
  cat(
    "pkgload::load_all(\".\")",
    sprintf("run_app(\"%s\")", app),
    file = "app.R", sep = "\n"
  )
  ## Deploy to shinyapps.io
  rsconnect::deployApp(appTitle = app)
}
