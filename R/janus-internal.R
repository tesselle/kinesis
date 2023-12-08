# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

url_tesselle <- function(package = NULL, campaign = TRUE) {
  mtm <- if (campaign) "?mtm_campaign=shiny" else ""
  if (is.null(package)) {
    sprintf("https://www.tesselle.org/%s", mtm)
  } else {
    sprintf("https://packages.tesselle.org/%s/%s", package, mtm)
  }
}

cite_markdown <- function(x = NULL) {
  x <- c("janus", x)
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x), style = "text")
      markdown(bib)
    }
  )
}

info_markdown <- function() {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}

color_picker <- function(namespace, id = "col") {
  x <- khroma::info()
  x <- tapply(X = x$palette, INDEX = x$type, FUN = function(x) as.list(x))

  selectInput(
    inputId = NS(namespace, id),
    label = "Color scale",
    choices = x[c(2, 3, 1)],
    selected = "discreterainbow",
    multiple = FALSE,
  )
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
