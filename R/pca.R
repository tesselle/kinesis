# UI ===========================================================================
#' Principal Components Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @param help A [`character`] string giving a short help text.
#' @seealso [pca_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_ui <- function(id, center = TRUE, scale = TRUE, help = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      title = tr_("Principal Components Analysis"),
      helpText(help),
      checkboxInput(
        inputId = ns("center"),
        label = tr_("Center"),
        value = center
      ),
      checkboxInput(
        inputId = ns("scale"),
        label = tr_("Scale"),
        value = scale
      ),
      selectizeInput(
        inputId = ns("sup_row"),
        label = tr_("Supplementary individuals"),
        choices = NULL, selected = NULL, multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      selectizeInput(
        inputId = ns("sup_col"),
        label = tr_("Supplementary variables"),
        choices = NULL, selected = NULL, multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
      downloadButton(
        outputId = ns("download"),
        label = tr_("Download results")
      )
    ), # sidebar
    multivariate_ui(ns("pca")),
    border_radius = FALSE,
    fillable = TRUE
  )
}

# Server =======================================================================
#' Principal Components Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [pca_ui()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    observe({
      freezeReactiveValue(input, "sup_row")
      updateSelectizeInput(
        inputId = "sup_row",
        choices = c(Choose = "", rownames(x())),
        server = TRUE
      )
      freezeReactiveValue(input, "sup_col")
      updateSelectizeInput(
        inputId = "sup_col",
        choices = c(Choose = "", colnames(x())),
        server = TRUE
      )
    })

    ## Bookmark -----
    onRestored(function(state) {
      updateSelectizeInput(session, inputId = "sup_row",
                           selected = state$input$sup_row)
      updateSelectizeInput(session, inputId = "sup_col",
                           selected = state$input$sup_col)
    })

    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = "PCA")

    ## Compute PCA -----
    compute_pca <- ExtendedTask$new(
      function(x, center, scale, rank, sup_row, sup_col, sup_quali) {
        promises::future_promise({
          dimensio::pca(
            object = x,
            center = center,
            scale = scale,
            rank = rank,
            sup_row = arkhe::seek_rows(x, names = sup_row),
            sup_col = arkhe::seek_columns(x, names = sup_col)
          )
        })
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_pca$invoke(x(), input$center, input$scale, input$rank,
                         input$sup_row, input$sup_col, input$sup_quali)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_pca$result(), title = tr_("Principal Components Analysis"))
    })

    multivariate_server("pca", x = results, y = x)

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
