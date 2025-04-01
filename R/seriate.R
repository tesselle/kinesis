# UI ===========================================================================
#' CA Seriation UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [seriate_server()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Permutation"),
      ## Input: checkbox if permute rows
      checkboxInput(
        inputId = ns("margin_row"),
        label = tr_("Permute rows"),
        value = TRUE
      ),
      ## Input: checkbox if permute columns
      checkboxInput(
        inputId = ns("margin_col"),
        label = tr_("Permute columns"),
        value = TRUE
      ),
      ## Input: select CA axes
      numericInput(
        inputId = ns("axes"),
        label = tr_("CA dimension"),
        value = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      ## Output: download
      downloadButton(outputId = ns("export_table"),
                     label = tr_("Export matrix")),
      h5("Display"),
      ## Input: select plot
      checkboxInput(
        inputId = ns("eppm"),
        label = "EPPM",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("weights"),
        label = tr_("Weights"),
        value = FALSE
      ),
      h5("Significance"),
      uiOutput(outputId = ns("coef"))
    ), # sidebar
    ## Output: plot reordered matrix
    output_plot(
      id = ns("plot_permute"),
      height = "100%",
      title = tr_("Rearranged matrix")
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' CA Seriation Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param order A reactive \R object (coercible by [kairos::as_seriation()]).
#' @return A reactive [`kairos::AveragePermutationOrder-class`] object.
#' @seealso [seriate_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_server  <- function(id, x, order) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(order))

  moduleServer(id, function(input, output, session) {
    ## Seriate -----
    data_seriate <- reactive({
      validate(need(order(), tr_("Compute the seriation order first.")))

      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)

      kairos::as_seriation(order(), margin = margin, axes = input$axes)
    })

    coef_seriate <- reactive({
      req(data_seriate())
      kairos::assess(data_seriate(), axes = input$axes, n = 0)
    })

    ## Permute -----
    data_permute <- reactive({
      req(x())
      req(data_seriate())
      kairos::permute(x(), data_seriate())
    })

    ## Plot -----
    plot_permute <- reactive({
      req(data_permute())
      function() {
        tabula::plot_ford(
          object = data_permute(),
          weights = input$weights,
          EPPM = input$eppm
        )
      }
    })

    ## Render plot -----
    render_plot("plot_permute", x = plot_permute)

    ## Render values -----
    output$coef <- renderUI({
      tags$div(
        tags$ul(
          tags$li(sprintf(tr_("Goodness of fit: %.3f"), coef_seriate()$coef))
        ),
        info_article(author = "Porcic", year = "2013",
                     doi = "10.1016/j.jas.2013.07.013")
      )
    })

    ## Download -----
    output$export_table <- export_table(data_permute, name = "permuted")

    data_seriate
  })
}
