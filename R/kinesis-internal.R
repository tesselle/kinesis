# HELPERS

#' Notification
#'
#' Shows a notification if an expression raises an error or a warning.
#' @param expr An expression to be evaluated.
#' @param what A [`character`] string giving the title of the notification.
#' @return The result of `expr` or `NULL`.
#' @keywords internal
#' @noRd
run_with_notification <- function(expr, what = NULL) {
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

  notif <- function(text, what = NULL, how = "default") {
    # text <- paste0(text, collapse = "\n")
    if (!is.null(what)) text <- sprintf("**%s**\n%s", what, text)
    showNotification(ui = markdown(text, hardbreaks = TRUE), type = how)
  }

  if (!is.null(err)) notif(text = err, what = what, how = "error")
  if (!is.null(warn)) notif(text = warn, what = what, how = "warning")

  res
}

#' Modal Dialog
#'
#' Shows a modal dialog if an expression raises an error.
#' @param expr An expression to be evaluated.
#' @param title A [`character`] string giving the title of the dialog.
#' @return The result of `expr` or `NULL`.
#' @keywords internal
#' @noRd
run_with_modal <- function(expr, title = NULL) {
  err <- NULL

  res <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        if (!inherits(e, "shiny.silent.error")) { # Ignore silent error
          err <<- conditionMessage(e)
        }
        return(NULL)
      }
    )
  )

  modal <- function(text, title = NULL) {
    dialog <- modalDialog(text, title = title, easyClose = TRUE)
    showModal(dialog)
  }

  if (!is.null(err)) modal(text = err, title = title)

  res
}

assert_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
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
