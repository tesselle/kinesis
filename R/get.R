# GETTERS

#' Get a (Default) Value
#'
#' @param x An \R object.
#' @param default A default value to be used is `x` is not
#'  [truthy][shiny::isTruthy()].
#' @return `x` or `default`.
#' @keywords internal
#' @noRd
get_value <- function(x, default = NULL) {
  if (!isTruthy(x)) return(default)
  x
}

#' Read Configuration Values
#'
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see [run_app()]).
#' @param config A [`character`] string specifying the name of configuration to
#'  read from.
#' @param use_parent A [`logical`] scalar: should parent directories be scanned
#'  for configuration files if the specified config file isn't found?
#' @return A [`list`] of configuration values.
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_config <- function(app, config = Sys.getenv("KINESIS_CONFIG_ACTIVE", "default"),
                       use_parent = TRUE) {

  file <- system.file("app", app, "config.yml", package = "kinesis", mustWork = FALSE)
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
    shiny::getShinyOption("kinesis_options")
  } else {
    shiny::getShinyOption("kinesis_options")[[name]] %||% default
  }
}
