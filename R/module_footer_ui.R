#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_footer_server()]
#' @family UI modules
#' @export
module_footer_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tags$footer(
    style = "margin-top: 1em; width: 100%; text-align: center;",

    actionLink(inputId = ns("cite"), label = "How to cite"),

    HTML(" &middot; "),

    a(href = "https://github.com/tesselle/janus/issues",
      rel = "external", title = "Issue", "Report a bug or request")
  )
}
