# HELPERS

#' Notification
#'
#' Shows a notification if an expression raises an error or a warning.
#' @param expr An expression to be evaluated.
#' @param what A [`character`] string giving the title of the notification.
#' @return The result of `expr` or `NULL`.
#' @keywords internal
#' @noRd
run_with_notification <- function(expr, title = NULL) {
  warn <- err <- NULL

  res <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        if (!inherits(e, "shiny.silent.error")) { # Ignore silent error
          err <<- conditionMessage(e)
        }
        return(NULL)
      }
    ),
    warning = function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  notif <- function(text, title = NULL, how = "default") {
    # text <- paste0(text, collapse = "\n")
    if (!is.null(title)) text <- sprintf("**%s**\n%s", title, text)
    showNotification(ui = markdown(text, hardbreaks = TRUE), type = how)
  }

  if (!is.null(err)) notif(text = err, title = title, how = "error")
  if (!is.null(warn)) notif(text = warn, title = title, how = "warning")

  res
}

#' Make File Name
#'
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param ext A [`character`] string specifying the file extension.
#' @param project A [`character`] string specifying the name of the project.
#' @family widgets
#' @keywords internal
#' @noRd
make_file_name <- function(name, ext, project = NULL) {
  project <- if (is.null(project)) "" else paste0(project, "_")
  time_stamp <- format(Sys.time(), "%y%m%d_%H%M%S")

  sprintf("%s%s_%s.%s", project, name, time_stamp, ext)
}
