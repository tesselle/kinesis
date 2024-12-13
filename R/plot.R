# UI ===========================================================================
#' Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param tools A (list of) input elements.
#' @param title A [`character`] string giving the card title.
#' @param note A [`character`] string giving a note to be placed in the footer.
#' @param ... Further parameters to be passed to [shiny::plotOutput()].
#' @family widgets
#' @keywords internal
output_plot <- function(id, ..., tools = NULL, title = NULL, note = NULL) {
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
    id = ns("card"),
    full_screen = TRUE,
    card_header(
      title, gear,
      class = "d-flex justify-content-between"
    ),
    card_body(
      plotOutput(outputId = ns("plot"), ...)
    ),
    if (!is.null(note)) card_footer(note)
  )
}

brush_xlim <- function(e) {
  if (is.null(e)) return(NULL)
  c(e$xmin, e$xmax)
}

brush_ylim <- function(e) {
  if (is.null(e)) return(NULL)
  c(e$ymin, e$ymax)
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

    ## Show modal dialog
    bindEvent(
      observe({ showModal(download_plot(session$ns)) }),
      input$download
    )

    ## Preview
    output$preview <- renderImage({
      req(x())
      req(input$width)
      req(input$height)

      res <- 72

      ## Write to a temporary PNG file
      outfile <- tempfile(fileext = ".png")

      grDevices::png(
        filename = outfile,
        width = input$width,
        height = input$height,
        units = "in",
        res = res
      )
      grDevices::replayPlot(x())
      grDevices::dev.off()

      ## Return a list containing information about the image
      list(
        src = outfile,
        contentType = "image/png",
        style = "height:300px; width:auto; max-width:100%;"
      )
    }, deleteFile = TRUE)

    ## Download
    output[["pdf"]] <- export_plot(input, x, format = "pdf")
    output[["png"]] <- export_plot(input, x, format = "png")
  })
}

#' Export Plot Modal
#'
#' @param ns A [namespace][shiny::NS()] function.
#' @keywords internal
#' @noRd
download_plot <- function(ns) {
  modalDialog(
    title = "Save plot - Preview",
    size = "l",
    easyClose = FALSE,
    fade = FALSE,
    imageOutput(outputId = ns("preview")),
    layout_column_wrap(
      width = 1/3,
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
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton(
        outputId = ns("pdf"),
        label = "PDF",
        icon = icon("download")
      ),
      downloadButton(
        outputId = ns("png"),
        label = "PNG",
        icon = icon("download")
      )
    )
  )
}

#' Download Plot
#'
#' Save and Download a graphic.
#' @param input Inputs selected by the user.
#' @param x A reactive recorded plot to be saved (see [grDevices::recordPlot()]).
#' @param format A [`character`] string specifying the file extension.
#' @keywords internal
#' @noRd
export_plot <- function(input, x, format) {
  downloadHandler(
    filename = function() { make_file_name(input$name, format) },
    content = function(file) {
      device <- switch (
        format,
        pdf = function(x, ...) grDevices::pdf(x, ...),
        png = function(x, ...) grDevices::png(x, ..., units = "in", res = 300),
        stop("Unknown graphics device.", call. = FALSE)
      )

      device(file, width = input$width, height = input$height)
      grDevices::replayPlot(x())
      grDevices::dev.off()
    }
  )
}

# Widgets ======================================================================
select_cex <- function(inputId, default = c(1, 6)) {
  sliderInput(
    inputId = inputId,
    label = "Symbol size",
    min = 0.2,
    max = 9,
    value = default,
    step = 0.2
  )
}

select_pch <- function(inputId, default = c(16, 17, 15, 3, 7, 8)) {
  x <- c(square = 0, circle = 1, `triangle up` = 2, plus = 3, cross = 4,
         diamond = 5, `triangle down` = 6, `square cross` = 7, star = 8,
         `diamond plus` = 9, `circle plus` = 10, `triangles up and down` = 11,
         `square plus` = 12, `circle cross` = 13, `square triangle` = 14,
         `filled square` = 15, `filled circle` = 16, `filled triangle` = 17,
         `filled diamond` = 18, `solid circle` = 19, bullet = 20)

  selectizeInput(
    inputId = inputId,
    label = "Symbol",
    choices = x,
    selected = default,
    multiple = TRUE,
    options = list(plugins = "clear_button")
  )
}

select_lty <- function(inputId, default = "solid") {
  x <- c(solid = 1, dashed = 2, dotted = 3,
         dotdash = 4, longdash = 5, twodash = 6)

  selectizeInput(
    inputId = inputId,
    label = "Line type",
    choices = x,
    selected = default,
    multiple = TRUE,
    options = list(plugins = "clear_button")
  )
}

select_color <- function(inputId, type = NULL, default = "discreterainbow") {
  x <- khroma::info()
  x <- tapply(X = x$palette, INDEX = x$type, FUN = function(x) as.list(x))
  if (!is.null(type)) x <- x[type]

  selectInput(
    inputId = inputId,
    label = "Color palette",
    choices = x,
    selected = default,
    multiple = FALSE
  )
}
