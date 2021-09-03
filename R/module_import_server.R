#' Import Server
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @param user_data A [shiny::reactiveValues()] list with the
#'  following elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @seealso [module_import_ui()]
#' @family Server modules
#' @export
module_import_server <- function(input, output, session,
                                 user_data, user_settings) {
  ## Event ---------------------------------------------------------------------
  observeEvent({
    input$file
    input$header
    input$sep
    input$dec
    input$quote
    input$rownames
  }, {
    req(input$file)

    tryCatch(
      {
        df <- utils::read.table(
          file = input$file$datapath,
          header = input$header,
          sep = input$sep,
          dec = input$dec,
          quote = input$quote,
          row.names = if (input$rownames) 1 else NULL
        )
      },
      error = function(e) {
        stop(safeError(e)) # Return a safeError if a parsing error occurs
      }
    )

    ## Coerce to count matrix
    count <- arkhe::as_count(df)

    # Store data
    user_data$data <- count
  })
  ## Reactive ------------------------------------------------------------------
  data <- reactive({
    req(user_data$data)
    user_data$data
  })
  ## Output --------------------------------------------------------------------
  output$summary <- renderPrint({
    summary(data())
  })
  output$table <- renderTable(
    {
      switch(
        input$display,
        head = utils::head(data()),
        all = data()
      )
    },
    rownames = TRUE,
    colnames = TRUE
  )
}
