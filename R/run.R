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
#'   **Application name**          \tab  **Keyword** \cr
#'   Correspondence Analysis       \tab `ca`         \cr
#'   Principal Components Analysis \tab `pca`        \cr
#'   Matrix Seriation              \tab `seriation`  \cr
#'   Compositional Data Analysis   \tab `source`     \cr
#'   Ternary Plot                  \tab `ternary`    \cr
#'  }
#' @examples
#' \dontrun{
#' run_app("seriation")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny apps
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("seriation", "source", "ternary", "ca", "pca"),
                    options = list(launch.browser = interactive())) {
  app <- match.arg(app, several.ok = FALSE)
  appDir <- system.file(app, package = "kinesis")

  if (appDir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  obj <- shiny::shinyAppDir(appDir = appDir, options = options)

  ## Bundling the options inside the shinyApp object
  kinesis_options <- get_config(app)
  obj$appOptions$kinesis_options <- kinesis_options

  obj
}
