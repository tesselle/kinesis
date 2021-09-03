#' Footer Server
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_footer_ui()]
#' @family Server modules
#' @export
module_footer_server <- function(input, output, session,
                                 user_data, user_settings) {
  ## Event ---------------------------------------------------------------------
  observeEvent(input$cite, {
    bib <- format(utils::citation("tabula"), style = "text")

    showModal(
      modalDialog(
        title = "Citation",
        markdown(bib),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
}
