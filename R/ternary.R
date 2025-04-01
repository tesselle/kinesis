# UI ===========================================================================
#' Ternary Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [ternary_server()]
#' @family plot modules
#' @keywords internal
#' @export
ternary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      accordion(
        accordion_panel(
          title = tr_("Aesthetic mappings"),
          ## Input: select axes
          selectize_ui(id = ns("axis1"), label = tr_("Component X")),
          selectize_ui(id = ns("axis2"), label = tr_("Component Y")),
          selectize_ui(id = ns("axis3"), label = tr_("Component Z")),
          ## Input: aesthetics mapping
          selectize_ui(id = ns("symbol_color"), label = tr_("Colors")),
          selectize_ui(id = ns("symbol_shape"), label = tr_("Symbol shape")),
          selectize_ui(id = ns("symbol_size"), label = tr_("Symbol size")),
        ),
        accordion_panel(
          title = tr_("Layers"),
          ## Input: add points
          checkboxInput(
            inputId = ns("points"),
            label = tr_("Show points"),
            value = TRUE
          ),
          ## Input: add density
          checkboxInput(
            inputId = ns("density"),
            label = tr_("Density contour"),
            value = FALSE
          ),
          radioButtons(
            inputId = ns("tile"),
            label = tr_("Heatmap"),
            choiceNames = c(tr_("None"), tr_("Bin"), tr_("Density")),
            choiceValues = c("none", "bin", "dens"),
            selected = "none"
          ),
          sliderInput(
            inputId = ns("bin"),
            label = tr_("Number of bins"),
            min = 5, max = 20,
            value = 10, step = 1
          )
        ),
        accordion_panel(
          title = tr_("Transform"),
          checkboxInput(
            inputId = ns("center"),
            label = tr_("Center"),
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("scale"),
            label = tr_("Scale"),
            value = FALSE
          )
        ),
        accordion_panel(
          title = tr_("Envelopes"),
          ## Input: select group
          selectize_ui(id = ns("group"), label = tr_("Group by")),
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
            label = tr_("Ellipse level"),
            selected = "0.95",
            choiceNames = c("68%", "95%", "99%"),
            choiceValues = c("0.68", "0.95", "0.99")
          )
        ),
        accordion_panel(
          title = tr_("Annotations"),
          ## Input: add a grid
          checkboxInput(
            inputId = ns("grid"),
            label = tr_("Grid"),
            value = TRUE
          ),
          ## Input: add labels
          checkboxInput(
            inputId = ns("labels"),
            label = tr_("Labels"),
            value = FALSE
          )
          ## Input: add a legend
          # TODO
          # checkboxInput(
          #   inputId = ns("legend"),
          #   label = tr_("Legend"),
          #   value = TRUE
          # )
        )
      )
    ), # sidebar
    helpText(
      tr_("Visualize your data in the ternary space."),
      tr_("Click and drag to select an area, then double-click to zoom in."),
      tr_("Double-click again to reset the zoom.")
    ),
    output_plot(
      id = ns("ternplot"),
      tools = list(
        select_color(inputId = ns("col")),
        select_pch(inputId = ns("pch"), default = NULL),
        select_cex(inputId = ns("cex"))
      ),
      title = tr_("Ternary plot"),
      dblclick = ns("ternplot_dblclick"),
      brush = brushOpts(
        id = ns("ternplot_brush"),
        resetOnNew = TRUE
      ),
      height = "100%"
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' Ternary Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `matrix`-like object.
#' @seealso [ternary_ui()]
#' @family plot modules
#' @keywords internal
#' @export
ternary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select columns -----
    data_raw <- reactive({
      as.data.frame(x())
    })
    quanti <- reactive({
      req(data_raw())
      i <- which(arkhe::detect(x = data_raw(), f = is.numeric, margin = 2))
      colnames(data_raw())[i]
    })
    quali <- reactive({
      req(data_raw())
      i <- which(arkhe::detect(x = data_raw(), f = Negate(is.numeric), margin = 2))
      colnames(data_raw())[i]
    })
    axis1 <- vector_select_server("axis1", x = quanti)
    axis2 <- vector_select_server("axis2", x = quanti, exclude = axis1)
    axis12 <- reactive({ c(axis1(), axis2()) })
    axis3 <- vector_select_server("axis3", x = quanti, exclude = axis12)
    symbol_color <- column_select_server("symbol_color", x = data_raw)
    symbol_shape <- vector_select_server("symbol_shape", x = quali)
    symbol_size <- vector_select_server("symbol_size", x = quanti)
    symbol_group <- vector_select_server("group", x = quali)

    ## Interactive zoom -----
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_ternplot <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_ternplot$x <- brush_xlim(input$ternplot_brush)
      range_ternplot$y <- brush_ylim(input$ternplot_brush)
    }) |>
      bindEvent(input$ternplot_dblclick)

    ## Get ternary data -----
    data_tern <- reactive({
      req(data_raw(), axis1(), axis2(), axis3())
      tern <- data_raw()[, c(axis1(), axis2(), axis3())]
      tern[rowSums(tern, na.rm = TRUE) != 0, , drop = FALSE]
    })

    ## Build plot -----
    plot_ternary <- reactive({
      ## Select data
      req(data_tern())
      tern <- data_tern()
      n <- nrow(tern)

      ## Compute center and scale
      no_scale <- !input$center && !input$scale

      ## Graphical parameters
      col <- rep("black", n)
      border <- rep("black", n)
      pch <- get_value(as.integer(input$pch))
      cex <- get_value(as.integer(input$cex), 1)

      ## Grouping variable
      if (isTruthy(symbol_group())) {
        symbol_group <- data_raw()[[symbol_group()]]
      } else {
        symbol_group <- rep("", n)
      }

      ## Symbol colors
      if (isTruthy(symbol_color())) {
        symbol_color <- data_raw()[[symbol_color()]]
        col <- get_color(input$col)
        if (is.double(symbol_color)) {
          col <- khroma::palette_color_continuous(colors = col)(symbol_color)
        } else {
          col <- khroma::palette_color_discrete(colors = col)(symbol_color)
        }
        if (identical(symbol_group, symbol_color)) border <- col
      }

      ## Symbol shapes
      if (isTruthy(symbol_shape())) {
        symbol_shape <- data_raw()[[symbol_shape()]]
        pch <- khroma::palette_shape(symbols = pch)(symbol_shape)
      } else {
        pch <- pch[[1L]] %||% 16
      }

      ## Symbol sizes
      if (isTruthy(symbol_size())) {
        symbol_size <- data_raw()[[symbol_size()]]
        cex <- khroma::palette_size_sequential(range = range(cex))(symbol_size)
      } else {
        cex <- min(cex)
      }

      ## Window
      range_coord <- list(x = NULL, y = NULL, z = NULL)
      if (isTruthy(range_ternplot$x) && isTruthy(range_ternplot$y)) {
        x_pts <- c(range_ternplot$x, mean(range_ternplot$x))
        y_pts <- c(range_ternplot$y, sqrt(3) * diff(range_ternplot$y) / 2)
        range_coord <- isopleuros::coordinates_cartesian(x = x_pts, y = y_pts)
      }

      ## Heatmap
      bin <- as.numeric(input$bin)
      fun_tile <- switch(
        input$tile,
        bin = isopleuros::tile_bin(tern),
        dens = isopleuros::tile_density(tern),
        NULL
      )

      ## Envelope
      level <- as.numeric(input$level)
      fun_wrap <- switch(
        input$wrap,
        tol = function(x, ...) isopleuros::ternary_tolerance(x, level = level, ...),
        conf = function(x, ...) isopleuros::ternary_confidence(x, level = level, ...),
        hull = function(x, ...) isopleuros::ternary_hull(x, ...),
        function(...) invisible()
      )

      ## Build plot
      function() {
        graphics::par(mar = c(1, 1, 1, 1))
        z <- isopleuros::ternary_plot(
          x = tern,
          type = "n",
          xlim = range_coord$x,
          ylim = range_coord$y,
          zlim = range_coord$z,
          xlab = axis1(),
          ylab = axis2(),
          zlab = axis3(),
          center = input$center,
          scale = input$scale
        )

        ## Add grid
        if (input$grid) {
          isopleuros::ternary_grid(center = z$center, scale = z$scale)
        }

        if (no_scale) {
          ## Heatmap
          if (isTruthy(fun_tile)) {
            isopleuros::ternary_image(
              f = fun_tile,
              n = bin,
              palette = khroma::palette_color_continuous(get_color(input$col))
            )
          }

          ## Density contours
          if (input$density) {
            isopleuros::ternary_density(tern)
          }

          ## Envelope
          for (i in split(seq_len(n), f = symbol_group)) {
            z <- tern[i, , drop = FALSE]
            if (nrow(z) < 3) next
            fun_wrap(z, lty = 1, border = border[i])
          }
        }

        ## Add points
        if (input$points) {
          isopleuros::ternary_points(tern, col = col, pch = pch, cex = cex,
                                     center = z$center, scale = z$scale)
        }

        ## Add labels
        if (input$labels) {
          isopleuros::ternary_labels(tern, center = z$center, scale = z$scale,
                                     labels = rownames(tern), col = col)
        }

        ## Add legend
        # TODO
      }
    })

    ## Render plot -----
    render_plot("ternplot", x = plot_ternary)
  })
}
