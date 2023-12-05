# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_home_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_home_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Home",
    sidebarLayout(
      sidebarPanel(
        tags$p(
          "This app is a part of the", tags$strong("tesselle"), "project,",
          "a collection of packages for research and teaching in archaeology.",
          "Learn more at",
          tags$a(href = url_tesselle(), rel = "external", title = "tesselle", "tesselle.org"),
          "."
        ),
        tags$p(
          "This program is free software: you can redistribute it and/or
              modify it under the terms of the GNU General Public License as
              published by the Free Software Foundation, either version 3 of
              the License, or (at your option) any later version."
        ),
        tags$p(
          "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
              GNU General Public License for more details."
        ),
        tags$br(),
        tags$p("To cite in publications use:"),
        cite_markdown()
        # h4("Bookmarking"),
        # bookmarkButton(),
        # tags$p(textOutput(outputId = ns("last_saved")))
      ), # sidebarPanel
      mainPanel(

      ) # mainPanel
    ) # sidebarLayout
  ) # tabPanel
}

# Server =======================================================================
#' Home Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [module_home_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Render ------------------------------------------------------------------
    output$session <- renderPrint({ utils::sessionInfo() })

    ## Bookmark ----------------------------------------------------------------
    # onBookmark(function(state) {
    #   saved_time <- Sys.time()
    #
    #   msg <- sprintf("Last saved at %s.", saved_time)
    #   showNotification(
    #     ui = msg,
    #     duration = 5,
    #     closeButton = TRUE,
    #     type = "message",
    #     session = session
    #   )
    #   cat(msg, "\n")
    #
    #   # state is a mutable reference object,
    #   # we can add arbitrary values to it.
    #   state$values$time <- saved_time
    # })
    ## Bookmark ----------------------------------------------------------------
    # onRestore(function(state) {
    #   msg <- sprintf("Restoring from state bookmarked at %s.", state$values$time)
    #   showNotification(
    #     ui = msg,
    #     duration = 5,
    #     closeButton = TRUE,
    #     type = "message",
    #     session = session
    #   )
    #   cat(msg, sep = "\n")
    # })
  })
}
