# WIDGETS

select_cex <- function(inputId, default = graphics::par("cex")) {
  sliderInput(
    inputId = inputId,
    label = "Symbol size",
    min = 0.5,
    max = 5,
    value = default,
    step = 0.5
  )
}
select_pch <- function(inputId, default = 16) {
  x <- c(square = 0, circle = 1, `triangle up` = 2, plus = 3, cross = 4,
         diamond = 5, `triangle down` = 6, `square cross` = 7, star = 8,
         `diamond plus` = 9, `circle plus` = 10, `triangles up and down` = 11,
         `square plus` = 12, `circle cross` = 13, `square triangle` = 14,
         `filled square` = 15, `filled circle` = 16, `filled triangle` = 17,
         `filled diamond` = 18, `solid circle` = 19, bullet = 20)

  selectInput(
    inputId = inputId,
    label = "Symbol",
    choices = x,
    selected = default,
    multiple = FALSE
  )
}
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

#' Download Multiple CSV Files
#'
#' Save and Download a [`data.frame`] (csv).
#' @param ... Further named arguments ([`data.frame`] to be saved).
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @family widgets
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
