# UI ===========================================================================
#' Compositional Data Outliers UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_outliers_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_outliers_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Outliers detection"),
      helpText("See", cite_article("Filzmoser & Hron", "2008", "10.1007/s11004-007-9141-5", after = ";"),
               cite_article("Filzmoser, Hron & Reimann", "2012", "10.1016/j.cageo.2011.06.014", after = ".")),
      radioButtons(
        inputId = ns("method"),
        label = "Multivariate location estimation",
        choiceNames = c("Minimum volume ellipsoid", "Minimum covariance determinant"),
        choiceValues = c("mve", "mcd")
      ),
      sliderInput(
        inputId = ns("quantile"),
        label = "Quantile",
        min = 0.025, max = 0.995,
        value = 0.975, step = 0.005
      ),
      actionButton(inputId = ns("go"), label = "(Re)Detect"),
      downloadButton(outputId = ns("download"), "Download results")
    ), # sidebar
    layout_columns(
      col_widths = "50%",
      output_plot(
        id = ns("plot"),
        title = "Plot"
      ),
      div(
        selectInput(
          inputId = ns("group"),
          label = "Select a group",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        radioButtons(
          inputId = ns("type"),
          label = "Type of plot",
          choices = c("dotchart", "distance", "qqplot")
        ),
        tableOutput(outputId = ns("info"))
      )
    ) # layout_columns
  ) # layout_sidebar
}

# Server =======================================================================
#' Compositional Data Outliers Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return A reactive [`nexus::OutlierIndex-class`] object.
#' @seealso [coda_outliers_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_outliers_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Detect outliers -----
    out <- bindEvent(
      reactive({
        validate(need(x(), "Check your data."))

        run_with_notification(
          {
            nexus::outliers(
              x(),
              groups = nexus::groups(x()),
              method = input$method,
              quantile = input$quantile
            )
          },
          what = "Outliers detection"
        )
      }),
      input$go
    )

    ## Select group -----
    bindEvent(
      observe({
        grp <- nexus::groups(out())
        choices <- seq_along(grp)
        names(choices) <- names(grp)
        freezeReactiveValue(input, "group")
        updateSelectInput(inputId = "group", choices = choices)
      }),
      out()
    )

    ## Plot -----
    plot <- reactive({
      req(out())
      validate(need(input$group, "Please choose a group."))
      plot(out(), select = as.integer(input$group), type = input$type)
      grDevices::recordPlot()
    })
    render_plot("plot", x = plot)

    ## Get data -----
    results <- reactive({
      req(out())
      as.data.frame(out())
    })

    ## Download results -----
    output$download <- export_table(results, name = "coda_outliers")

    out
  })
}
