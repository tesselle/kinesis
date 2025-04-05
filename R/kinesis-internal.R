# HELPERS

## https://michaelchirico.github.io/potools/articles/developers.html
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-kinesis"))
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

# Helpers ======================================================================
validate_csv <- function(x) {
  validate(need(x, message = tr_("Import a CSV file first.")))
}
validate_dim <- function(x, i = 1, j = 1) {
  rows <- ngettext(i, "Select at least %d row.", "Select at least %d rows.")
  cols <- ngettext(j, "Select at least %d column.", "Select at least %d columns.")
  validate(need(NROW(x) >= i, sprintf(rows, i)))
  validate(need(NCOL(x) >= j, sprintf(cols, j)))
}
validate_na <- function(x) {
  validate(need(!anyNA(x), tr_("Your data should not contain missing values.")))
}
validate_zero <- function(x) {
  validate(need(all(x != 0), tr_("Your data should not contain zeros.")))
}

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
select_calendar <- function(id, default = "CE") {
  ns <- NS(id)

  selectizeInput(
    inputId = ns("calendar"),
    label = tr_("Calendar"),
    choices = c("CE", "BCE", "BP", "AD", "BC"),
    selected = default,
    multiple = FALSE,
    options = list(plugins = "remove_button")
  )
}
get_calendar <- function(id) {
  moduleServer(id, function(input, output, session) {

    cal <- reactive({
      aion::calendar(input$calendar)
    })

    cal
  })
}
column_input_numeric_ui <- function(id) {
  uiOutput(NS(id, "controls"))
}
column_input_numeric_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get variable names
    vars <- reactive({ names(x()) })

    ## Build UI
    output$controls <- renderUI({
      lapply(
        X = vars(),
        FUN = function(var) {
          numericInput(
            inputId = session$ns(paste0("num_", var)),
            label = var,
            value = 0
          )
        }
      )
    })

    ## Get values
    values <- reactive({
      vapply(
        X = paste0("num_", vars()),
        FUN = function(var, input) input[[var]],
        FUN.VALUE = numeric(1),
        input = input
      )
    })

    values
  })
}

#' Updatable Select List
#'
#' @param id A [`character`] string specifying the namespace.
#' @keywords internal
#' @noRd
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
#' @param id A [`character`] string specifying the namespace (must match
#'  [selectize_ui()]).
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

    reactive({ input$selected }) |>
      debounce(500)
  })
}

#' Update a Select List with a Vector
#'
#' @param id A [`character`] string specifying the namespace (must match
#'  [selectize_ui()]).
#' @param x A reactive [`character`] vector.
#' @param exclude A reactive [`character`] vector of values to exclude.
#' @param preserve A [`logical`] scalar: should existing selection be preserved
#'  on update?
#' @param none A [`logical`] scalar: should a placeholder be added as the first
#'  element?
#' @param server A [`logical`] scalar: should server-side selectize be used?
#' @return A reactive [`character`] vector of column names.
#' @seealso [selectize_ui()]
#' @keywords internal
#' @noRd
vector_select_server <- function(id, x, exclude = reactive({ NULL }),
                                 preserve = TRUE, none = TRUE, server = TRUE) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(exclude))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      choices <- x()
      selected <- NULL
      if (!is.null(exclude())) {
        choices <- setdiff(choices, exclude())
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
      bindEvent(x(), exclude())

    ## Bookmark
    onRestored(function(state) {
      updateSelectizeInput(session, "selected", selected = state$input$selected)
    })

    reactive({ input$selected })
  })
}

# Notification =================================================================
show_notification <- function(text, title = NULL, id = NULL, duration = 5,
                              closeButton = TRUE, type = "default") {
  # text <- paste0(text, collapse = "\n")
  if (!is.null(title)) text <- sprintf("**%s**\n%s", title, text)
  id <- showNotification(
    ui = markdown(text, hardbreaks = TRUE),
    duration = duration,
    closeButton = closeButton,
    id = id,
    type = type
  )
  invisible(id)
}

#' Notify
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

  if (!is.null(err))
    show_notification(text = err, title = title, type = "error")
  if (!is.null(warn))
    show_notification(text = warn, title = title, type = "warning")

  res
}

#' Compare Two \R Objects
#'
#' Shows a notification if `x` and `y` are not [identical][identical()].
#' @param x A reactive object.
#' @param y A reactive object.
#' @param title A [`character`] string giving the title of the notification.
#' @return
#'  `notify_change()` is called for its side-effects.
#' @keywords internal
#' @noRd
notify_change <- function(id, x, y, title = "Important message") {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(y))

  moduleServer(id, function(input, output, session) {
    observe({
      if (identical(x(), y())) {
        removeNotification(id)
      } else {
        txt <- c(tr_("Your data seem to have changed."),
                 tr_("You should perform your analysis again."))
        show_notification(id = id, text = txt, title = title,
                          duration = NULL, closeButton = FALSE,
                          type = "warning")
      }
    }) |>
      bindEvent(x(), y())
  })
}
