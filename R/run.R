# SHINY

#' Run an App
#'
#' A wrapper for [shiny::shinyAppDir()].
#' @param app A [`character`] string specifying the \pkg{Shiny} application
#'  to run (see details). Any unambiguous substring can be given.
#' @param bookmark A [`character`] string specifying how to save the
#'  application's state. It must be one of "`disable`" or "`server`" (see
#'  [shiny::enableBookmarking()])
#' @param options A [`list`] of named options that should be passed to the
#'  [`shiny::shinyAppDir()`] call.
#' @details
#'  \tabular{ll}{
#'   **Application name**          \tab  **Keyword** \cr
#'   Correspondence Analysis       \tab `ca`         \cr
#'   Principal Components Analysis \tab `pca`        \cr
#'   Diversity Measures            \tab `diversity`  \cr
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
run_app <- function(app = c("diversity", "seriation", "source", "ternary", "ca", "pca"),
                    bookmark = c("disable", "server"),
                    options = list(launch.browser = interactive())) {
  app <- match.arg(app, several.ok = FALSE)
  bookmark <- match.arg(bookmark, several.ok = FALSE)

  app_dir <- system.file("app", app, package = "kinesis")
  if (app_dir == "")
    stop(sprintf("Could not find %s app.", sQuote(app)), call. = FALSE)

  ## Create a Shiny app object
  shiny::enableBookmarking(store = bookmark)
  obj <- shiny::shinyAppDir(appDir = app_dir, options = options)

  ## Bundle the options inside the shinyApp object
  opt <- get_config(app)
  opt$bookmark <- bookmark != "disable"
  obj$appOptions$kinesis_options <- opt

  obj
}
