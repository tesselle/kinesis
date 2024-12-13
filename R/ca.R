# UI ===========================================================================
#' Correspondence Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [ca_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
ca_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Correspondence Analysis"),
      selectizeInput(
        inputId = ns("sup_row"),
        label = "Supplementary individuals",
        choices = NULL, selected = NULL, multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      selectizeInput(
        inputId = ns("sup_col"),
        label = "Supplementary variables",
        choices = NULL, selected = NULL, multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      uiOutput(
        outputId = ns("warning"),
      ),
      actionButton(
        inputId = ns("go"),
        label = "(Re)Compute",
        class = "btn btn-primary"
      ),
      downloadButton(
        outputId = ns("download"),
        label = "Download results"
      )
    ), # sidebar
    multivariate_ui(id),
    border_radius = FALSE,
    fillable = TRUE,
    class = "p-0"
  )
}

# Server =======================================================================
#' Correspondence Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A list of reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [ca_ui()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
ca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    bindEvent(
      observe({
        freezeReactiveValue(input, "sup_row")
        updateSelectizeInput(inputId = "sup_row", choices = rownames(x()))
        freezeReactiveValue(input, "sup_col")
        updateSelectizeInput(inputId = "sup_col", choices = colnames(x()))
      }),
      x()
    )

    ## Compute PCA -----
    results <- bindEvent(
      reactive({
        req(x())
        run_with_notification(
          {
            dimensio::ca(
              object = x(),
              rank = input$rank,
              sup_row = arkhe::seek_rows(x(), names = input$sup_row),
              sup_col = arkhe::seek_columns(x(), names = input$sup_col)
            )
          },
          title = "CA"
        )
      }),
      input$go
    )

    ## Warning -----
    old_data <- bindEvent(
      reactive({
        x()
      }),
      input$go
    )
    output$warning <- renderUI({
      req(x(), old_data())
      new_raw <- serialize(x(), NULL)
      old_raw <- serialize(old_data(), NULL)
      if (!isTRUE(all.equal(new_raw, old_raw))) {
        div(
          class = "alert alert-warning",
          role = "alert",
          "Your data seems to have changed.",
          "You should perform your analysis again."
        )
      }
    })

    ## Export -----
    output$download <- downloadHandler(
      filename = function() { make_file_name("ca", "zip") },
      content = function(file) {
        dimensio::export(results(), file = file, flags = "-r9Xj")
      },
      contentType = "application/zip"
    )

    results
  })
}
