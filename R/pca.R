# UI ===========================================================================
#' Principal Components Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @seealso [pca_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_ui <- function(id, center = TRUE, scale = TRUE) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Principal Components Analysis"),
      checkboxInput(
        inputId = ns("center"),
        label = "Center",
        value = center
      ),
      checkboxInput(
        inputId = ns("scale"),
        label = "Scale",
        value = scale
      ),
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
      compute_ui(id = ns("pca")),
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
#' Principal Components Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A list of reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [pca_ui()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    observe({
      freezeReactiveValue(input, "sup_row")
      updateSelectizeInput(inputId = "sup_row", choices = rownames(x()))
      freezeReactiveValue(input, "sup_col")
      updateSelectizeInput(inputId = "sup_col", choices = colnames(x()))
    }) |>
      bindEvent(x())

    ## Compute PCA -----
    results <- compute_server(
      id = "pca",
      x = x,
      f = function(x) {
        dimensio::pca(
          object = x,
          center = input$center,
          scale = input$scale,
          rank = input$rank,
          sup_row = arkhe::seek_rows(x, names = input$sup_row),
          sup_col = arkhe::seek_columns(x, names = input$sup_col)
        )
      }
    )

    ## Export -----
    output$download <- downloadHandler(
      filename = function() { make_file_name("pca", "zip") },
      content = function(file) {
        dimensio::export(results(), file = file, flags = "-r9Xj")
      },
      contentType = "application/zip"
    )

    results
  })
}
