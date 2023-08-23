# UI ===========================================================================
#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param name A [`character`] string giving the name of the app.
#' @seealso [module_footer_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_footer_ui <- function(id, name) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  url <- sprintf("https://www.tesselle.org/?mtm_campaign=shiny&mtm_kwd=%s", name)
  tags$footer(
    style = "margin-top: 1em; width: 100%; text-align: center;",
    tags$p(
      actionLink(inputId = ns("citation"), label = "How to cite"),
      HTML(" &middot; "),
      tags$a(href = "https://github.com/tesselle/janus/issues",
             rel = "external", title = "Issue", "Report a bug or request")
    ),
    tags$p(
      "This app is a part of the", tags$strong("tesselle"), "project,",
      "a collection of packages for research and teaching in archaeology.",
      "Learn more at",
      tags$a(href = url, rel = "external", title = "tesselle", "tesselle.org"),
      "."
    )
  )
}

# Server =======================================================================
#' Footer Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param cite A [`character`] string giving the name of the package to be
#'  cited.
#' @seealso [module_footer_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_footer_server  <- function(id, cite = "janus") {
  moduleServer(id, function(input, output, session) {
    ## Event -------------------------------------------------------------------
    observeEvent(input$citation, {
      showModal(
        modalDialog(
          title = "Citation",
          cite_markdown(cite),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}
