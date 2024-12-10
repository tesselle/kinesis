# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

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

#' Get User Data
#'
#' @param session A \pkg{shiny} [session][shiny::session] object.
#' @param name A [`character`] string specifying the name of an option to get.
#'  If `NULL` (the default), all options are returned.
#' @param default A value to be returned if the option is not currently set.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
get_user <- function(session, name = NULL, default = NULL) {
  if (is.null(name)) {
    session$userData
  } else {
    session$userData[[name]] %||% default
  }
}

assert_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
}

url_tesselle <- function(package = NULL) {
  if (is.null(package)) return("https://www.tesselle.org/")
  sprintf("https://packages.tesselle.org/%s/", package)
}

cite_package <- function(x = NULL) {
  x <- c("kinesis", x)
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x), style = "text")
      txt <- paste0(vapply(X = bib, FUN = markdown, FUN.VALUE = character(1)))
      HTML(txt)
    }
  )
}

cite_article <- function(author, year, doi = NULL, text = TRUE,
                         before = "", after = "", html = TRUE) {
  right <- paste0(")", after)
  if (is.null(doi)) {
    link <- tags$span(year, .noWS = "outside")
  } else {
    url <- sprintf("https://doi.org/%s", doi)
    link <- tags$a(year, href = url, target = "_blank", .noWS = "outside")
  }

  if (text) {
    cite <- tags$span(before, author, "(", link, right)
  } else {
    cite <- tags$span(
      paste0("(", author, ", "), link, right,
      .noWS = c("after-begin", "before-end")
    )
  }
  if (!html) cite <- as.character(cite)
  cite
}

info_article <- function(...) {
  cite_article(..., before = icon("info-circle"), after = ".")
}

info_session <- function() {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}

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
