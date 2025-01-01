# HELPERS

validate_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
}
validate_dim <- function(x) {
  validate(need(all(dim(x) > 0), "Select at least one row and one column."))
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

#' Compute on Action UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [compute_server()]
#' @keywords internal
compute_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  list(
    uiOutput(outputId = ns("warning")),
    actionButton(
      inputId = ns("go"),
      label = "(Re)Compute",
      class = "btn btn-primary"
    )
  )
}

#' Compute on Action Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive object.
#' @param f A [`function`] that takes `x` as argument.
#' @return A reactive object (the result of `f(x)`).
#' @seealso [compute_ui()]
#' @keywords internal
compute_server <- function(id, x, f) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    old_data <- reactive({ x() }) |> bindEvent(input$go)

    results <- reactive({
      req(x())
      run_with_notification({ f(x()) }, title = toupper(id))
    }) |>
      bindEvent(input$go)

    output$warning <- renderUI({
      req(x(), old_data())
      new_raw <- serialize(x(), NULL)
      old_raw <- serialize(old_data(), NULL)
      if (!isTRUE(all.equal(new_raw, old_raw))) {
        div(
          class = "alert alert-warning",
          role = "alert",
          "Your data seems to have changed.",
          "You should perform your analysis again."
        )
      }
    })

    results
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
