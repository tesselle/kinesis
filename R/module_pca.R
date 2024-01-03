# UI ===========================================================================
#' Principal Components Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @seealso [module_pca_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_pca_ui <- function(id, center = TRUE, scale = TRUE) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    fluidRow(
      div(
        style="display: inline-block; vertical-align:bottom; width: 150px;",
        selectInput(
          inputId = ns("data"),
          label = "Dataset",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        )
      ),
      div(
        style="display: inline-block; vertical-align:bottom; width: 150px;",
        checkboxInput(
          inputId = ns("center"),
          label = "Center",
          value = center
        ),
        checkboxInput(
          inputId = ns("scale"),
          label = "Scale",
          value = scale
        )
      ),
      div(
        style="display: inline-block; vertical-align:bottom; width: 150px;",
        selectizeInput(
          inputId = ns("sup_row"),
          label = "Supplementary individuals",
          choices = NULL, selected = NULL, multiple = TRUE,
          options = list(plugins = "remove_button")
        )
      ),
      div(
        style="display: inline-block; vertical-align:bottom; width: 150px;",
        selectizeInput(
          inputId = ns("sup_col"),
          label = "Supplementary variables",
          choices = NULL, selected = NULL, multiple = TRUE,
          options = list(plugins = "remove_button")
        )
      ),
      div(
        style="display: inline-block; vertical-align:bottom; width: 150px;",
        actionButton(
          inputId = ns("go"),
          label = "(Re)Compute"
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
#' @param x A list of reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [module_pca_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_pca_server <- function(id, x) {
  # stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    observe({
      freezeReactiveValue(input, "data")
      updateSelectInput(inputId = "data", choices = names(x))
    })
    bindEvent(
      observe({
        freezeReactiveValue(input, "sup_row")
        updateSelectInput(inputId = "sup_row", choices = rownames(data()))
        freezeReactiveValue(input, "sup_col")
        updateSelectInput(inputId = "sup_col", choices = colnames(data()))
      }),
      data()
    )

    ## Get data -----
    data <- reactive({
      validate(need(input$data, "Select a dataset."))
      x[[input$data]]()
    })
    sup_row <- reactive({
      i <- match(input$sup_row, rownames(data()))
      if (anyNA(i)) return(NULL)
      i
    })
    sup_col <- reactive({
      i <- match(input$sup_row, colnames(data()))
      if (anyNA(i)) return(NULL)
      i
    })

    ## Compute PCA -----
    results <- bindEvent(
      reactive({
        validate(need(data(), "Check your data."))

        dimensio::pca(
          object = data(),
          center = input$center,
          scale = input$scale,
          rank = input$rank,
          sup_row = sup_row(),
          sup_col = sup_col()
        )
      }),
      input$go
    )

    results
  })
}
