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

  sidebarLayout(
    sidebarPanel(
      h5("Outliers detection"),
      checkboxInput(
        inputId = ns("robust"),
        label = "Use robust estimators",
        value = FALSE
      ),
      sliderInput(
        inputId = ns("quantile"),
        label = "Quantile",
        min = 0.025, max = 0.995,
        value = 0.975, step = 0.005
      ),
      downloadButton(outputId = ns("download"), "Download results")
    ), # sidebarPanel
    mainPanel(
      fluidRow(
        div(
          class = "col-lg-6 col-md-1",
          output_plot(
            id = ns("plot"),
            height = "auto",
            click = ns("click"),
            title = "Plot"
          )
        ),
        div(
          class = "col-lg-6 col-md-1",
          helpText("See", cite_article("Filzmoser & Hron", "2008", "10.1007/s11004-007-9141-5", after = ";"),
                   cite_article("Filzmoser, Hron & Reimann", "2012", "10.1016/j.cageo.2011.06.014", after = ".")),
          selectInput(
            inputId = ns("group"),
            label = "Select a group",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          ),
          tableOutput(outputId = ns("info"))
        )
      ) # fluidRow
    ) # mainPanel
  ) # sidebarLayout
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
    out <- reactive({
      req(x())

      run_with_notification(
        {
          nexus::outliers(
            x(),
            groups = nexus::get_groups(x()),
            robust = input$robust,
            method = c("mve", "mcd"),
            quantile = input$quantile
          )
        },
        what = "Outliers detection"
      )
    })

    ## Select group -----
    bindEvent(
      observe({
        choices <- colnames(out())
        freezeReactiveValue(input, "group")
        updateSelectInput(inputId = "group", choices = choices)
      }),
      out()
    )
    grp <- reactive({
      req(out())
      validate(need(input$group, "Please choose a group."))
      out()[[input$group]]
    })

    ## Plot -----
    plot <- reactive({
      req(grp())
      nexus::plot(grp(), qq = FALSE)
      grDevices::recordPlot()
    })
    render_plot("plot", x = plot)

    ## Get data -----
    results <- reactive({
      req(out())
      as.data.frame(out())
    })
    info <- reactive({
      req(grp())
      z <- as.data.frame(grp())
      z <- nearPoints(z, input$click, xvar = "index",
                      yvar = paste0("dist_", input$group))
      z
    })
    output$info <- render_table(info, rownames = FALSE)

    ## Download results -----
    output$download <- export_table(
      outliers = results,
      name = "coda_outliers"
    )

    out
  })
}
