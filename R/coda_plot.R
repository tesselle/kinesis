# UI ===========================================================================
#' Compositional Bar Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_plot_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_plot_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      checkboxInput(
        inputId = ns("order_columns"),
        label = "Sort columns",
        value = FALSE
      ),
      selectInput(
        inputId = ns("order_rows"),
        label = "Row order",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      checkboxInput(
        inputId = ns("decreasing"),
        label = "Decreasing row order",
        value = FALSE
      ),
      hr(),
      checkboxInput(
        inputId = ns("select_major"),
        label = "Major elements",
        value = TRUE
      ),
      checkboxInput(
        inputId = ns("select_minor"),
        label = "Minor elements",
        value = TRUE
      ),
      checkboxInput(
        inputId = ns("select_trace"),
        label = "Trace elements",
        value = TRUE
      ),
    ), # sidebar
    output_plot(
      id = ns("plot"),
      tools = select_color(
        inputId = ns("color_qualitative"),
        type = "qualitative"
      ),
      height = "100%",
      title = "Barplot"
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' Bar Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_plot_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_plot_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build barplot -----
    bindEvent(
      observe({
        freezeReactiveValue(input, "order_rows")
        choices <- c("", colnames(x()))
        updateSelectInput(inputId = "order_rows", choices = choices)
      }),
      x()
    )
    plot_bar <- reactive({
      req(x())

      col <- khroma::color(input$color_qualitative)

      elements <- logical(ncol(x()))
      is_major <- nexus::is_element_major(x())
      is_minor <- nexus::is_element_minor(x())
      is_trace <- nexus::is_element_trace(x())

      elements[which(is_major)] <- input$select_major
      elements[which(is_minor)] <- input$select_minor
      elements[which(is_trace)] <- input$select_trace

      if (!any(elements)) elements <- NULL

      nexus::barplot(
        height = x(),
        select = elements,
        order_columns = input$order_columns,
        order_rows = input$order_rows,
        decreasing = input$decreasing,
        color = khroma::palette_color_discrete(col)
      )
      grDevices::recordPlot()
    })

    ## Render barplot -----
    render_plot("plot", x = plot_bar)
  })
}
