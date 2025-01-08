# HELPERS

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

# Helpers ======================================================================
validate_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
}
validate_dim <- function(x, i = 1, j = 1) {
  rows <- ngettext(i, "Select at least %d row.", "Select at least %d rows.")
  cols <- ngettext(j, "Select at least %d column.", "Select at least %d columns.")
  validate(need(NROW(x) >= i, sprintf(rows, i)))
  validate(need(NCOL(x) >= j, sprintf(cols, j)))
}
validate_na <- function(x) {
  validate(need(!anyNA(x), "Your data should not contain missing values."))
}
validate_zero <- function(x) {
  validate(need(all(x != 0), "Your data should not contain zeros."))
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

# Widgets ======================================================================
checkboxgroup_ui <- function(id, label = "Select columns:") {
  ns <- NS(id)

  checkboxGroupInput(
    inputId = ns("checked"),
    label = label,
    choices = NULL,
    selected = NULL,
    inline = TRUE,
    width = "100%"
  )
}

selectize_ui <- function(id, label = "Choose", multiple = FALSE) {
  ns <- NS(id)
  plugins <- ifelse(isTRUE(multiple), "remove_button", "clear_button")
  options <- list(plugins = plugins)

  selectizeInput(
    inputId = ns("selected"),
    label = label,
    choices = NULL,
    selected = NULL,
    multiple = multiple,
    options = options
  )
}

#' Update a Select List with Column Names
#'
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param find_col A predicate [`function`] for column detection
#'  (see [arkhe::detect()]).
#' @param preserve A [`logical`] scalar: should existing selection be preserved
#'  on update?
#' @param none A [`logical`] scalar: should a placeholder be added as the first
#'  element?
#' @param server A [`logical`] scalar: should server-side selectize be used?
#' @return A reactive [`character`] vector of column names.
#' @seealso [selectize_ui()]
#' @keywords internal
#' @noRd
column_select_server <- function(id, x, find_col = NULL,
                                 preserve = TRUE, none = TRUE,
                                 server = TRUE) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      choices <- colnames(x())
      selected <- NULL
      if (!is.null(choices) && is.function(find_col)) {
        selection <- which(arkhe::detect(x = x(), f = find_col, margin = 2))
        choices <- choices[selection]
      }
      if (isTRUE(preserve)) {
        ## Try to keep previous selection, if any
        keep <- intersect(choices, input$selected)
        if (length(keep) > 0) selected <- keep
      }
      if (isTRUE(none)) {
        choices <- c(Choose = "", choices)
      }

      freezeReactiveValue(input, "selected")
      updateSelectizeInput(
        inputId = "selected",
        choices = choices,
        selected = selected,
        server = server
      )
    }) |>
      bindEvent(x())

    ## Bookmark
    onRestored(function(state) {
      updateSelectizeInput(session, "selected", selected = state$input$selected)
    })

    reactive({ input$selected })
  })
}

#' Update a Select List with a Vector
#'
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `vector` object.
#' @param exclude A reactive `vector` object or `NULL`.
#' @param preserve A [`logical`] scalar: should existing selection be preserved
#'  on update?
#' @param none A [`logical`] scalar: should a placeholder be added as the first
#'  element?
#' @param server A [`logical`] scalar: should server-side selectize be used?
#' @return A reactive [`character`] vector of column names.
#' @seealso [selectize_ui()]
#' @keywords internal
#' @noRd
vector_select_server <- function(id, x, exclude = NULL, preserve = TRUE,
                                 none = TRUE) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      choices <- x()
      selected <- NULL
      if (is.reactive(exclude) && isTruthy(exclude())) {
        choices <- setdiff(choices, exclude())
      }
      if (isTRUE(preserve)) {
        ## Try to keep previous selection, if any
        isolate({
          keep <- intersect(choices, input$selected)
          if (length(keep) > 0) selected <- keep
        })
      }
      if (isTRUE(none)) {
        choices <- c(Choose = "", choices)
      }

      freezeReactiveValue(input, "selected")
      updateSelectizeInput(
        inputId = "selected",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    })

    ## Bookmark
    onRestored(function(state) {
      updateSelectizeInput(session, "selected", selected = state$input$selected)
    })

    reactive({ input$selected })
  })
}

#' Update a Checkbox Group with Column Names
#'
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param find_col A predicate [`function`] for column detection
#'  (see [arkhe::detect()]).
#' @param use_col A predicate [`function`] for column selection
#'  (see [arkhe::detect()]).
#' @param preserve A [`logical`] scalar: should existing selection be preserved
#'  on update?
#' @return A reactive [`character`] vector of column names.
#' @seealso [checkboxgroup_ui()]
#' @keywords internal
#' @noRd
column_checkbox_server <- function(id, x, find_col = NULL, use_col = NULL,
                                   preserve = TRUE) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      choices <- colnames(x())
      selected <- NULL
      col <- rep(TRUE, length(choices))
      if (length(choices) > 0 && is.function(find_col)) {
        col <- arkhe::detect(x = x(), f = find_col, margin = 2)
        choices <- choices[which(col)]
      }
      if (length(choices) > 0 && is.function(use_col)) {
        ok <- arkhe::detect(x = x(), f = use_col, margin = 2)
        selected <- choices[which(col & ok)]
      }
      if (isTRUE(preserve)) {
        ## Try to keep previous selection, if any
        keep <- intersect(choices, input$checked)
        if (length(keep) > 0) selected <- keep
      }
      freezeReactiveValue(input, "checked")
      updateCheckboxGroupInput(
        inputId = "checked",
        choices = choices,
        selected = selected
      )
    }) |>
      bindEvent(x())

    ## Bookmark
    onRestored(function(state) {
      updateCheckboxGroupInput(session, "checked", selected = state$input$checked)
    })

    reactive({ input$checked })
  })
}

# Notification =================================================================
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
