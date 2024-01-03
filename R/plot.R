# UI ===========================================================================
#' Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param tools A (list of) input elements.
#' @param ... Further parameters to be passed to [shiny::plotOutput()].
#' @family widgets
#' @keywords internal
output_plot <- function(id, ..., tools = NULL, title = "Card title") {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  gear <- bslib::popover(
    icon("gear"),
    title = "Tools",
    placement = "auto",
    tools,
    actionButton(
      inputId = ns("download"),
      label = "Download",
      icon = icon("download")
    )
  )

  bslib::card(
    bslib::card_header(
      title, gear,
      class = "d-flex justify-content-between"
    ),
    plotOutput(outputId = ns("plot"), ...)
    # bslib::card_footer()
  )
}

# Server =======================================================================
#' Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive recorded plot to be saved (see [grDevices::recordPlot()]).
#' @param width,height Height and width specification (see [shiny::renderPlot()]).
#' @param ratio A length-one [`numeric`] vector giving the \eqn{x/y} ratio.
#'  Only used if `height` is `NULL`.
#' @family widgets
#' @keywords internal
render_plot <- function(id, x, width = "auto", height = NULL, ratio = 1) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Plot
    output$plot <- renderPlot(
      grDevices::replayPlot(x()),
      width = width,
      height = height %||% function() { getCurrentOutputInfo(session)$width() * ratio }
    )

    ## Download modal
    bindEvent(
      observe({ showModal(export_plot_modal(session$ns("export"))) }),
      input$download
    )

    ## Download
    export_plot_server("export", x, format = "pdf")
    export_plot_server("export", x, format = "png")
  })
}

#' Plot Modal Dialog
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @family widgets
#' @keywords internal
#' @noRd
export_plot_modal <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  modalDialog(
    easyClose = FALSE,
    title = "Save plot",
    textInput(
      inputId = ns("name"),
      label = "File name",
      value = "plot"
    ),
    numericInput(
      inputId = ns("width"),
      label = "Width (in)",
      min = 0.5,
      value = 7
    ),
    numericInput(
      inputId = ns("height"),
      label = "Height (in)",
      min = 0.5,
      value = 7
    ),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton(
        outputId = ns("pdf"),
        label = "PDF",
        icon = shiny::icon("download")
      ),
      downloadButton(
        outputId = ns("png"),
        label = "PNG",
        icon = shiny::icon("download")
      )
    )
  )
}

#' Download Plot
#'
#' Save and Download a graphic.
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive recorded plot to be saved (see [grDevices::recordPlot()]).
#' @param format A [`character`] string specifying the file extension.
#' @family widgets
#' @keywords internal
#' @noRd
export_plot_server <- function(id, x, format) {
  ## Validation
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    name <- reactive({
      req(input$name)
      input$name
    })
    width <- reactive({
      req(input$width)
      input$width
    })
    height <- reactive({
      req(input$height)
      input$height
    })

    output[[format]] <- downloadHandler(
      filename = function() { sprintf("%s.%s", name(), format) },
      content = function(file) {
        device <- switch (
          format,
          pdf = function(x, ...) grDevices::pdf(x, ...),
          png = function(x, ...) grDevices::png(x, ..., units = "in", res = 300),
          stop("Unknown graphics device.", call. = FALSE)
        )

        device(file, width = width(), height = height())
        grDevices::replayPlot(x())
        grDevices::dev.off()
      }
    )
  })
}
