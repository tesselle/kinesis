#' Settings Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_settings_ui()]
#' @family server modules
#' @export
module_settings_server <- function(input, output, session, user_settings) {
  observe({
    ## Colour scheme
    scale_qualitative <- khroma::colour(input$col_qualitative, names = FALSE)
    scale_sequential <- khroma::colour(input$col_sequential, names = FALSE)
    scale_diverging <- khroma::colour(input$col_diverging, names = FALSE)

    user_settings$fig_width <- input$fig_width
    user_settings$fig_height <- input$fig_height
    user_settings$fig_units <- input$fig_units
    user_settings$scale_qualitative <- scale_qualitative
    user_settings$scale_sequential <- scale_sequential
    user_settings$scale_diverging <- scale_diverging
  })
  # Render
  output$last_saved <- renderText({
    req(user_settings$saved)
    paste("Last saved at", user_settings$saved)
  })
  output$session <- renderPrint({ utils::sessionInfo() })
  # Bookmark
  onBookmark(function(state) {
    user_settings$saved <- Sys.time()
    # state is a mutable reference object,
    # we can add arbitrary values to it.
    state$values$time <- user_settings$saved
  })
  onRestore(function(state) {
    user_settings$saved <- state$values$time
  })
}
