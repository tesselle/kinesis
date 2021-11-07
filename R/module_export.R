# EXPORT

#' Download Plot Module
#'
#' Save and Download a [`ggplot2::ggplot`] Object (pdf).
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param plot A [`ggplot2::ggplot`] object to be saved.
#' @param user_settings A [shiny::reactiveValues()] list specifying extra
#'  parameters to be passed to [ggplot2:ggsave()].
#' @keywords internal
#' @noRd
module_export_plot <- function(id, name, plot, user_settings) {
  moduleServer(id, function(input, output, session) {
    downloadHandler(
      filename = paste0(name, ".pdf"),
      content = function(file) ggplot2::ggsave(
        file, plot = plot,
        width = user_settings$fig_width,
        height = user_settings$fig_height,
        units = user_settings$fig_units,
        dpi = user_settings$fig_dpi,
      ),
      contentType = "application/pdf"
    )
  })
}

#' Download CSV Module
#'
#' Save and Download a [`data.frame`] (csv).
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param table A [`data.frame`] to be saved.
#' @keywords internal
#' @noRd
module_export_table <- function(id, name, table) {
  moduleServer(id, function(input, output, session) {
    downloadHandler(
      filename = paste0(name, ".csv"),
      content = function(file) {
        utils::write.csv(table, file, row.names = TRUE, fileEncoding = "utf-8")
      },
      contentType = "text/csv"
    )
  })
}
