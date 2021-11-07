#' Import Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param user_data A [shiny::reactiveValues()] list with the following
#'  elements: "`data`".
#' @param user_settings A [shiny::reactiveValues()] list.
#' @param mode A [`character`] string specifying the target data type.
#'  It must be one of `"count"`, `"composition"` or `"incidence"`.
#'  Any unambiguous substring can be given.
#' @seealso [module_import_ui()]
#' @family server modules
#' @export
module_import_server <- function(id, user_data, user_settings, mode) {
  moduleServer(id, function(input, output, session) {
    ## Event -------------------------------------------------------------------
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

      ## Coerce to Matrix
      arkhe_coerce <- switch (
        mode,
        count = arkhe::as_count,
        composition = arkhe::as_composition,
        incidence = arkhe::as_incidence,
        stop("Don't what to do...", call. = FALSE)
      )

      ## Store data
      user_data$file <- input$file
      user_data$data <- arkhe_coerce(df)
    })
    ## Reactive ----------------------------------------------------------------
    data <- reactive({
      req(user_data$data)
      user_data$data
    })
    ## Output ------------------------------------------------------------------
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
  })
}
