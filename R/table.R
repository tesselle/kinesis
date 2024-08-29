# Server =======================================================================
#' Table Output
#'
#' A wrapper around [shiny::renderTable()].
#' @param x A reactive [`data.frame`].
#' @param ... Further arguments to be passed to [shiny::renderTable()].
#' @importFrom shiny renderTable
#' @keywords internal
#' @noRd
render_table <- function(x, ..., striped = TRUE, hover = FALSE, bordered = FALSE,
                         width = "100%", rownames = TRUE, digits = 3) {
  stopifnot(is.reactive(x))

  renderTable(
    expr = x(),
    striped = striped,
    hover = hover,
    bordered = bordered,
    width = width,
    rownames = rownames,
    colnames = TRUE,
    digits = digits,
    ...
  )
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
file_name <- function(name, ext, project = NULL) {
  project <- if (is.null(project)) "" else paste0(project, "_")
  sprintf("%s%s_%s.%s", project, name, format(Sys.Date(), "%F"), ext)
}

#' Download a CSV File
#'
#' Save and Download a [`data.frame`] (csv).
#' @param x A reactive [`data.frame`] to be saved.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @family widgets
#' @keywords internal
#' @noRd
export_table <- function(x, name) {
  stopifnot(is.reactive(x))

  downloadHandler(
    filename = file_name(name, "csv"),
    content = function(file) {
      ## CSV file
      utils::write.csv(
        x = x(),
        file = file,
        fileEncoding = "utf-8"
      )
    },
    contentType = "text/csv"
  )
}

#' Download Multiple CSV Files
#'
#' Save and Download several [`data.frame`] (csv).
#' @param ... Further named arguments ([`data.frame`] to be saved).
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @family widgets
#' @keywords internal
#' @noRd
export_multiple <- function(..., name = "archive") {
  tbl <- list(...)
  stopifnot(!is.null(names(tbl)))

  downloadHandler(
    filename = file_name(name, "zip"),
    content = function(file) {
      tmpdir <- tempdir()

      ## CSV file
      fs <- vapply(
        X = names(tbl),
        FUN = function(f) {
          path <- file.path(tmpdir, paste0(f, ".csv"))
          utils::write.csv(
            x = tbl[[f]](),
            file = path,
            row.names = TRUE,
            fileEncoding = "utf-8"
          )
          return(path)
        },
        FUN.VALUE = character(1)
      )

      ## Create ZIP-file
      utils::zip(zipfile = file, files = fs, flags = "-j")
    },
    contentType = "application/zip"
  )
}
