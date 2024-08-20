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

  gear <- popover(
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

  card(
    full_screen = TRUE,
    card_header(
      title, gear,
      class = "d-flex justify-content-between"
    ),
    card_body(
      plotOutput(outputId = ns("plot"), ...)
    )
    # card_footer()
  )
}

select_cex <- function(inputId, default = graphics::par("cex")) {
  sliderInput(
    inputId = inputId,
    label = "Symbol size",
    min = 0.5,
    max = 5,
    value = default,
    step = 0.5
  )
}

select_pch <- function(inputId, default = 16) {
  x <- c(square = 0, circle = 1, `triangle up` = 2, plus = 3, cross = 4,
         diamond = 5, `triangle down` = 6, `square cross` = 7, star = 8,
         `diamond plus` = 9, `circle plus` = 10, `triangles up and down` = 11,
         `square plus` = 12, `circle cross` = 13, `square triangle` = 14,
         `filled square` = 15, `filled circle` = 16, `filled triangle` = 17,
         `filled diamond` = 18, `solid circle` = 19, bullet = 20)

  selectInput(
    inputId = inputId,
    label = "Symbol",
    choices = x,
    selected = default,
    multiple = FALSE
  )
}

select_color <- function(inputId, type = NULL) {
  x <- khroma::info()
  x <- tapply(X = x$palette, INDEX = x$type, FUN = function(x) as.list(x))
  if (!is.null(type)) x <- x[type]

  selectInput(
    inputId = inputId,
    label = "Color palette",
    choices = x,
    selected = "discreterainbow",
    multiple = FALSE,
  )
}

get_color <- function(palette, n = NULL) {
  fun <- khroma::color(palette, name = FALSE, force = TRUE)
  if (is.null(n)) n <- attr(fun, "max")
  fun(n)
}

# Server =======================================================================
#' Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive recorded plot to be saved (see [grDevices::recordPlot()]).
#' @param ... Further parameters to be passed to [shiny::renderPlot()].
#' @family widgets
#' @keywords internal
render_plot <- function(id, x, ...) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Plot
    output$plot <- renderPlot(grDevices::replayPlot(x()), ...)

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
