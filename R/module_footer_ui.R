#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param name A [`character`] string giving the name of the app.
#' @seealso [module_footer_server()]
#' @family UI modules
#' @export
module_footer_ui <- function(id, name) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  url <- sprintf("https://www.tesselle.org/?mtm_campaign=shiny&mtm_kwd=%s", name)
  tags$footer(
    style = "margin-top: 1em; width: 100%; text-align: center;",
    tags$p(
      actionLink(inputId = ns("cite"), label = "How to cite"),
      HTML(" &middot; "),
      tags$a(href = "https://github.com/tesselle/janus/issues",
          rel = "external", title = "Issue", "Report a bug or request")
    ),
    tags$p(
      "This app is a part of the", tags$strong("tesselle"), "project,",
      "a collection of packages for research and teaching in archaeology.",
      "Learn more at",
      tags$a(href = url, rel = "external", title = "tesselle", "tesselle.org")
    )
  )
}
