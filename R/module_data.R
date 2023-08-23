# UI ===========================================================================
#' Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
# @seealso [module_data_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_data_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "Data",
    icon = icon("upload"),
    fluidPage(
      fluidRow(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "1. Import",
            icon = icon("upload"),
            module_import_ui("import")
          ), # tabPanel
          tabPanel(
            title = "Prepare",
            icon = icon("screwdriver-wrench"),
            module_prepare_ui("prepare")
          ) # tabPanel
        ) # tabsetPanel
      ) # fluidRow
    ) # fluidPage
  ) # tabPanel
}
