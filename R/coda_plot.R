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
      hr(),
      checkboxInput(
        inputId = ns("order_columns"),
        label = "Sort columns",
        value = FALSE
      ),
      selectizeInput(
        inputId = ns("order_rows"),
        label = "Row order",
        choices = NULL, selected = NULL, multiple = FALSE,
        options = list(plugins = "remove_button")
      ),
      checkboxInput(
        inputId = ns("decreasing"),
        label = "Decreasing row order",
        value = FALSE
      )
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
    bindEvent(
      observe({
        freezeReactiveValue(input, "order_rows")
        choices <- c("", colnames(data_bar()))
        updateSelectizeInput(inputId = "order_rows", choices = choices)
      }),
      data_bar()
    )

    ## Subset -----
    data_bar <- reactive({
      req(x())

      elements <- logical(ncol(x()))
      is_major <- nexus::is_element_major(x())
      is_minor <- nexus::is_element_minor(x())
      is_trace <- nexus::is_element_trace(x())

      elements[which(is_major)] <- input$select_major
      elements[which(is_minor)] <- input$select_minor
      elements[which(is_trace)] <- input$select_trace

      if (!any(elements)) return(x())
      x()[, elements, drop = FALSE]
    })

    ## Build barplot -----
    plot_bar <- reactive({
      req(data_bar())

      col <- khroma::color(input$color_qualitative)
      pal <- khroma::palette_color_discrete(col, domain = colnames(x()))

      function() {
        nexus::barplot(
          height = data_bar(),
          order_columns = input$order_columns,
          order_rows = get_value(input$order_rows),
          decreasing = input$decreasing,
          color = pal
        )
      }
    })

    ## Render barplot -----
    render_plot("plot", x = plot_bar)
  })
}
