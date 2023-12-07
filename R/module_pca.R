# UI ===========================================================================
#' Principal Components Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_pca_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_pca_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    fluidRow(
      div(
        style="display: inline-block; vertical-align:middle; width: 150px;",
        checkboxInput(
          inputId = ns("center"),
          label = "Center",
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("scale"),
          label = "Scale",
          value = TRUE
        )
      ),
      div(
        style="display: inline-block; vertical-align:middle; width: 150px;",
        actionButton(
          inputId = ns("go"),
          label = "Compute"
        )
      )
    ),
    fluidRow(
      module_multivar_ui(id)
    )
  )
}

# Server =======================================================================
#' Principal Components Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A list of `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [module_pca_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_pca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    results <- reactive({
      req(x())
      dimensio::pca(
        object = x(),
        center = input$center,
        scale = input$scale,
        rank = input$rank,
        sup_row = input$sup_row,
        sup_col = input$sup_col
      )
    }) |>
      bindEvent(input$go)

    results
  })
}
