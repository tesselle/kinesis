
# UI ===========================================================================
#' Seriate UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_seriate_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      ## Input: checkbox if permute rows
      checkboxInput(
        inputId = ns("margin_row"),
        label = "Permute rows",
        value = TRUE
      ),
      ## Input: checkbox if permute columns
      checkboxInput(
        inputId = ns("margin_col"),
        label = "Permute columns",
        value = TRUE
      ),
      ## Input: select CA axes
      numericInput(
        inputId = ns("axes"),
        label = "CA dimension",
        value = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      ## Input: select plot
      radioButtons(
        inputId = ns("plot_type"),
        label = "Display",
        choices = c(`Bertin plot` = "bertin", `Ford plot` = "ford"),
        selected = "ford"
      ),
      conditionalPanel(
        condition = "input.plot_type == 'ford'",
        ns = ns,
        checkboxInput(
          inputId = ns("eppm"),
          label = "EPPM",
          value = FALSE
        ),
        checkboxInput(
          inputId = ns("weights"),
          label = "weights",
          value = FALSE
        )
      )
    ), # sidebarPanel
    mainPanel(
      ## Output: permutation summary
      # verbatimTextOutput(outputId = ns("summary")),
      tabsetPanel(
        id = ns("plot"),
        type = "tabs",
        tabPanel(
          title = "Rearranged matrix",
          style = "margin-top: 15px;",
          ## Output: download
          downloadButton(outputId = ns("export_plot_perm"),
                         label = "Export plot"),
          downloadButton(outputId = ns("export_table"),
                         label = "Export matrix"),
          hr(),
          ## Output: plot reordered matrix
          plotOutput(outputId = ns("plot_permute"), height = "auto")
        ),
        tabPanel(
          title = "Raw data",
          style = "margin-top: 15px;",
          ## Output: download
          downloadButton(outputId = ns("export_plot_data"),
                         label = "Export plot"),
          hr(),
          ## Output: plot raw matrix
          plotOutput(outputId = ns("plot_raw"), height = "auto")
        )
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Seriate Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_prepare_server()]).
#' @return A reactive [`kairos:: AveragePermutationOrder-class`] object.
#' @seealso [module_seriate_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_seriate_server  <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Reactive ----------------------------------------------------------------
    data_seriate <- reactive({
      req(x())
      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)
      kairos::seriate_average(x(), margin = margin, axes = input$axes)
    })
    data_permute <- reactive({
      req(x())
      kairos::permute(x(), data_seriate())
    })
    fun_plot <- reactive({
      req(input$plot_type)
      switch(
        input$plot_type,
        ford = function(x) tabula::plot_ford(x, weights = input$weights, EPPM = input$eppm),
        bertin = tabula::plot_bertin
      )
    })
    plot_raw <- reactive({
      req(x())
      fun_plot()(x())
      grDevices::recordPlot()
    })
    plot_permute <- reactive({
      req(data_permute())
      fun_plot()(data_permute())
      grDevices::recordPlot()
    })

    ## Render ------------------------------------------------------------------
    output$plot_raw <- renderPlot(
      { grDevices::replayPlot(plot_raw()) },
      height = function() { getCurrentOutputInfo(session)$width() / 2 }
    )
    output$plot_permute <- renderPlot(
      { grDevices::replayPlot(plot_permute()) },
      height = function() { getCurrentOutputInfo(session)$width() / 2 }
    )
    ## Download ----------------------------------------------------------------
    output$export_plot_raw <- export_plot(plot_raw, name = "matrix_raw")
    output$export_plot_perm <- export_plot(plot_permute, name = "matrix_permuted")
    output$export_table <- export_table(permuted = data_permute, name = "matrix_permuted")

    data_seriate
  })
}
