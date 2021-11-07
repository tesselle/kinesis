#' Footer Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_footer_ui()]
#' @family server modules
#' @export
module_footer_server  <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {
    ## Event -------------------------------------------------------------------
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
  })
}
