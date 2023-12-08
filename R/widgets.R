# WIDGETS

select_color <- function(inputId, type = NULL) {
  x <- khroma::info()
  x <- tapply(X = x$palette, INDEX = x$type, FUN = function(x) as.list(x))
  if (!is.null(type)) x <- x[type]

  selectInput(
    inputId = inputId,
    label = "Color palette",
    choices = x,
    selected = "discreterainbow",
    multiple = FALSE,
  )
}
get_color <- function(palette, n) {
  khroma::color(palette, name = FALSE, force = TRUE)(n)
}

#' Download Plot
#'
#' Save and Download a graphic (pdf).
#' @param plot A reactive plot to be saved.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @keywords internal
#' @noRd
export_plot <- function(object, name = "plot", width = 7, height = 7) {
  stopifnot(is.reactive(object))

  downloadHandler(
    filename = paste0(name, ".pdf"),
    content = function(file) {
      grDevices::pdf(file, width = width, height = height)
      grDevices::replayPlot(object())
      grDevices::dev.off()
    },
    contentType = "application/pdf"
  )
}

#' Download Multiple CSV Files
#'
#' Save and Download a [`data.frame`] (csv).
#' @param ... Further named arguments ([`data.frame`] to be saved).
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @keywords internal
#' @noRd
export_table <- function(..., name = "file") {
  tbl <- list(...)
  stopifnot(!is.null(names(tbl)))

  downloadHandler(
    filename = sprintf("%s_%s.zip", format(Sys.Date(), "%F"), name),
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
