# UI ===========================================================================
#' Ternary Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_ternary_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_ternary_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      ## Input: select variables
      selectizeInput(
        inputId = ns("axis1"),
        label = "Component X",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        inputId = ns("axis2"),
        label = "Component Y",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      selectizeInput(
        inputId = ns("axis3"),
        label = "Component Z",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      hr(),
      ## Input: add points
      checkboxInput(
        inputId = ns("points"),
        label = "Show points",
        value = TRUE
      ),
      ## Input: add a grid
      checkboxInput(
        inputId = ns("grid"),
        label = "Grid",
        value = FALSE
      ),
      downloadButton(outputId = ns("export"), label = "Export plot")
    ), # sidebarPanel
    mainPanel(
      plotOutput(outputId = ns("plot"), height = "auto")
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Ternary Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @seealso [module_ternary_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_ternary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Observe -----------------------------------------------------------------
    observeEvent(x(), {
      choices <- colnames(x())
      updateSelectInput(session, inputId = "axis1", choices = choices)
      updateSelectInput(session, inputId = "axis2", choices = choices)
      updateSelectInput(session, inputId = "axis3", choices = choices)
    })
    observeEvent(input$axis1, {
      choices <- setdiff(colnames(x()), input$axis1)
      updateSelectInput(session, inputId = "axis2", choices = choices)
      updateSelectInput(session, inputId = "axis3", choices = choices)
    })
    observeEvent(input$axis2, {
      choices <- setdiff(colnames(x()), c(input$axis1, input$axis2))
      updateSelectInput(session, inputId = "axis3", choices = choices)
    })

    ## Reactive ----------------------------------------------------------------
    plot_ternary <- reactive({
      req(input$axis1)
      req(input$axis2)
      req(input$axis3)

      ## Add grid?
      do_grid <- function(...) return(NULL)
      if (input$grid) {
        do_grid <- function(...) isopleuros::ternary_grid(...)
      }

      tern <- x()[, c(input$axis1, input$axis2, input$axis3)]
      isopleuros::ternary_plot(
        x = NULL,
        xlab = input$axis1,
        ylab = input$axis2,
        zlab = input$axis3,
        panel.first = do_grid()
      )
      if (input$points) isopleuros::ternary_points(tern)
      grDevices::recordPlot()
    })
    ## Output ------------------------------------------------------------------
    output$plot <- renderPlot({
      grDevices::replayPlot(plot_ternary())
    }, height = function() { getCurrentOutputInfo(session)$width() / 2 } )

    ## Download ----------------------------------------------------------------
    output$export <- export_plot(plot_ternary, name = "screeplot")
  })
}
