# HELPERS

validate_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
}
validate_dim <- function(x) {
  validate(need(isTruthy(x) && all(dim(x) > 0), "Select at least one row and one column."))
}
validate_na <- function(x) {
  validate(need(!anyNA(x), "Your data should not contain missing values."))
}
validate_zero <- function(x) {
  validate(need(all(x != 0), "Your data should not contain zeros."))
}

#' Bootstrap Theme
#'
#' @param version A [`character`] string specifying the major version of
#'  Bootstrap to use.
#' @param ... Extra parameters to be passed to [bslib::bs_theme()].
#' @seealso [bslib::bs_theme()]
#' @keywords internal
#' @export
theme_ui <- function(version = "5", ...) {
  path <- system.file("static", "custom.scss", package = "kinesis")
  scss <- sass::sass_file(path)

  bs <- bslib::bs_theme(version = version, ...)
  bslib::bs_add_rules(bs, scss)
}

#' Check If a Dataset Has Changed
#'
#' @param x A reactive object.
#' @param trigger An input to respond to.
#' @return An UI element or `NULL` (if no changes).
#' @keywords internal
#' @noRd
data_diff_ui <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = ns("warning"))
}

data_diff_server <- function(id, x, y) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    output$warning <- renderUI({
      req(x(), y())
      new_raw <- serialize(x(), NULL)
      old_raw <- serialize(y(), NULL)
      if (isTRUE(all.equal(new_raw, old_raw))) return(NULL)
      div(
        class = "alert alert-warning",
        role = "alert",
        "Your data seem to have changed.",
        "You should perform your analysis again."
      )
    })
  })
}

column_checkbox_ui <- function(id, label = "Select columns:") {
  ns <- NS(id)

  checkboxGroupInput(
    inputId = ns("select"),
    label = label,
    choices = NULL,
    selected = NULL,
    inline = TRUE,
    width = "100%"
  )
}

column_checkbox_server <- function(id, x, f) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      req(x())
      choices <- colnames(x())
      selected <- NULL
      if (is.function(f)) {
        selected <- which(arkhe::detect(x = x(), f = f, margin = 2))
      }

      freezeReactiveValue(input, "select")
      updateCheckboxGroupInput(
        inputId = "select",
        choices = choices,
        selected = choices[selected]
      )
    })

    ## Bookmark
    onRestored(function(state) {
      updateCheckboxGroupInput(session, "select", selected = state$input$select)
    })

    reactive({ input$select })
  })
}

#' Notification
#'
#' Shows a notification if an expression raises an error or a warning.
#' @param expr An expression to be evaluated.
#' @param what A [`character`] string giving the title of the notification.
#' @return The result of `expr` or `NULL`.
#' @keywords internal
#' @noRd
notify <- function(expr, title = NULL) {
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
