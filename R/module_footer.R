# UI ===========================================================================
#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param package A [`character`] string giving the name of the app.
#' @seealso [module_footer_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_footer_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tags$footer(
    style = "margin-top: 1em; width: 100%; text-align: center;",
    tags$p(
      actionLink(inputId = ns("session"), label = "Session info"),
      HTML(" &middot; "),
      tags$a(href = "https://github.com/tesselle/janus",
             target = "_blank", rel = "external", "Source code"),
      HTML(" &middot; "),
      tags$a(href = "https://github.com/tesselle/janus/issues",
             target = "_blank", rel = "external", "Report a bug or request")
    )
  )
}

# Server =======================================================================
#' Footer Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [module_footer_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_footer_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Event -------------------------------------------------------------------
    observeEvent(input$session, {
      showModal(
        modalDialog(
          title = "Session Info",
          info_session(),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}
