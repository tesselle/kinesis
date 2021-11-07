# HELPERS

#' Save and Download a 'ggplot' Object (pdf)
#'
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param plot A [`ggplot2::ggplot`] object to be saved.
#' @param ... Extra parameters to be passed to [ggplot2:ggsave()].
#' @keywords internal
#' @noRd
export_plot <- function(name, plot, ...) {
  downloadHandler(
    filename = paste0(name, ".pdf"),
    content = function(file) ggplot2::ggsave(file, plot = plot, ...),
    contentType = "application/pdf"
  )
}

#' Save and Download a data.frame (csv)
#'
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param table A [`data.frame`] to be saved.
#' @param ... Currently not used.
#' @keywords internal
#' @noRd
export_table <- function(name, table, ...) {
  downloadHandler(
    filename = paste0(name, ".csv"),
    content = function(file) {
      utils::write.csv(table, file, row.names = TRUE, fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
}
