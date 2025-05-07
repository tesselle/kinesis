# UI ===========================================================================
#' Scatter Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [scatter_server()]
#' @family plot modules
#' @keywords internal
#' @export
scatter_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Scatter Plot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Variables"),
        ## Input: select axes
        selectize_ui(id = ns("axis1"), label = tr_("Component X")),
        selectize_ui(id = ns("axis2"), label = tr_("Component Y")),
        ## Input: aesthetics mapping
        selectize_ui(id = ns("extra_quali"), label = tr_("Extra qualitative variable")),
        selectize_ui(id = ns("extra_quanti"), label = tr_("Extra quantitative variable")),
        ## Input: linear regression
        checkboxInput(
          inputId = ns("regression"),
          label = tr_("Linear regression"),
          value = FALSE
        ),
        ## Input: add ellipses
        radioButtons(
          inputId = ns("wrap"),
          label = tr_("Wrap:"),
          choiceNames = c(tr_("None"), tr_("Tolerance ellipse"),
                          tr_("Confidence ellipse"), tr_("Convex hull")),
          choiceValues = c("none", "tol", "conf", "hull"),
        ),
        checkboxGroupInput(
          inputId = ns("level"),
          label = tr_("Ellipse level:"),
          selected = "0.95",
          choiceNames = c("68%", "95%", "99%"),
          choiceValues = c("0.68", "0.95", "0.99")
        )
      ), # sidebar
      helpText(
        tr_("Click and drag to select an area, then double-click to zoom in."),
        tr_("Double-click again to reset the zoom.")
      ),
      output_plot(
        id = ns("plot"),
        tools = list(
          select_color(id = ns("col"), type = "qualitative"),
          select_pch(inputId = ns("pch"), default = NULL),
          select_cex(inputId = ns("cex")),
          checkboxInput(inputId = ns("ratio"), label = tr_("Fixed aspect ratio"), value = FALSE),
          checkboxInput(inputId = ns("grid"), label = tr_("Grid"), value = TRUE)
        ),
        title = tr_("Scatter Plot"),
        dblclick = ns("plot_dblclick"),
        brush = brushOpts(
          id = ns("plot_brush"),
          resetOnNew = TRUE
        ),
        height = "100%"
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Scatter Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @seealso [scatter_ui()]
#' @family plot modules
#' @keywords internal
#' @export
scatter_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select columns -----
    quanti <- reactive({
      req(x())
      i <- which(arkhe::detect(x = x(), f = is.numeric, margin = 2))
      colnames(x())[i]
    })
    quali <- reactive({
      req(x())
      i <- which(arkhe::detect(x = x(), f = is.numeric, margin = 2, negate = TRUE))
      colnames(x())[i]
    })
    axis1 <- vector_select_server("axis1", x = quanti)
    axis2 <- vector_select_server("axis2", x = quanti, exclude = axis1)
    extra_quali <- vector_select_server("extra_quali", x = quali)
    extra_quanti <- vector_select_server("extra_quanti", x = quanti)

    ## Interactive zoom -----
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_plot <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_plot$x <- brush_xlim(input$plot_brush)
      range_plot$y <- brush_ylim(input$plot_brush)
    }) |>
      bindEvent(input$plot_dblclick)

    ## Graphical parameters -----
    param <- reactiveValues(col = "black", pch = 16, cex = 1)
    observe({
      param$pal <- get_color("col")()
      pch <- get_value(as.integer(input$pch))
      if (isTruthy(extra_quali())) {
        extra_quali <- x()[[extra_quali()]]
        param$col <- khroma::palette_color_discrete(param$pal)(extra_quali)
        param$pch <- khroma::palette_shape(pch)(extra_quali)
      } else {
        param$col <- "black"
        param$border <- "black"
        param$pch <- pch[[1L]] %||% 16
      }
    })
    observe({
      cex <- get_value(range(as.integer(input$cex)), 1)
      if (isTruthy(extra_quanti())) {
        extra_quanti <- x()[[extra_quanti()]]
        param$cex <- khroma::palette_size_sequential(range = cex)(extra_quanti)
      } else {
        param$cex <- min(cex)
      }
    })

    ## Regression -----
    model <- reactive({
      req(x(), axis2(), axis1())
      if (!isTRUE(input$regression)) return(NULL)

      n <- nrow(x())
      group <- if (isTruthy(extra_quali())) x()[[extra_quali()]] else rep("", n)
      by(
        data = x(),
        INDICES = group,
        FUN = function(x) {
          vars <- as.formula(sprintf("%s~%s", axis2(), axis1()))
          fit <- stats::lm(vars, data = x)
          pred <- stats::predict(fit, interval = "confidence", level = as.numeric(input$level))
          list(model = fit, predict = pred, response = x[[axis1()]])
        },
        simplify = FALSE
      )
    })

    ## Ellipses -----
    wrap <- reactive({
      req(x())
      level <- as.numeric(input$level)
      group <- if (isTruthy(extra_quali())) x()[[extra_quali()]] else NULL
      switch(
        input$wrap,
        tol = function(x, y, ...) dimensio::viz_tolerance(x, y, group = group, level = level, ...),
        conf = function(x, y, ...) dimensio::viz_confidence(x, y, group = group, level = level, ...),
        hull = function(x, y, ...) dimensio::viz_hull(x, y, group = group, ...),
        function(...) invisible()
      )
    })

    ## Build plot -----
    plot_scatter <- reactive({
      ## Select data
      req(x(), axis1(), axis2())

      ## Build plot
      function() {
        graphics::plot(
          x = x()[[axis1()]],
          y = x()[[axis2()]],
          type = "p",
          xlim = range_plot$x,
          ylim = range_plot$y,
          xlab = axis1(),
          ylab = axis2(),
          panel.first = if (isTRUE(input$grid)) graphics::grid() else NULL,
          col = param$col,
          pch = param$pch,
          cex = param$cex,
          asp = ifelse(isTRUE(input$ratio), 1, NA),
          las = 1
        )

        ## Add regression
        if (length(model()) > 0) {
          for (fit in model()) {
            i <- order(fit$response)
            lines(x = fit$response[i], y = fit$predict[i, 1], lwd = 2)
          }
        }

        ## Add ellipses
        wrap()(x = x()[[axis1()]], y = x()[[axis2()]], color = param$pal)

        ## Add legend
        # TODO
      }
    })

    ## Render plot -----
    render_plot("plot", x = plot_scatter)
  })
}
