#' Settings Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_settings_ui()]
#' @family server modules
#' @export
module_settings_server <- function(id, user_settings) {
  moduleServer(id, function(input, output, session) {
    ## Observe -----------------------------------------------------------------
    observeEvent(input$col_qualitative, {
      user_settings$col_qualitative <- input$col_qualitative
    })
    observeEvent(input$col_sequential, {
      user_settings$col_sequential <- input$col_sequential
    })
    observeEvent(input$col_diverging, {
      user_settings$col_diverging <- input$col_diverging
    })
    observeEvent(input$fig_width, {
      user_settings$fig_width <- input$fig_width
    })
    observeEvent(input$fig_height, {
      user_settings$fig_height <- input$fig_height
    })
    observeEvent(input$fig_units, {
      user_settings$fig_units <- input$fig_units
    })
    observeEvent(input$fig_dpi, {
      user_settings$fig_dpi <- input$fig_dpi
    })
    ## Render ------------------------------------------------------------------
    output$last_saved <- renderText({
      req(user_settings$saved)
      paste("Last saved at", user_settings$saved)
    })
    output$session <- renderPrint({ utils::sessionInfo() })
    ## Bookmark ----------------------------------------------------------------
    onBookmark(function(state) {
      user_settings$saved <- Sys.time()
      # state is a mutable reference object,
      # we can add arbitrary values to it.
      state$values$time <- user_settings$saved
    })
    onRestore(function(state) {
      user_settings$saved <- state$values$time
    })
  })
}
