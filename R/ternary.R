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
      width = "20%",
      accordion(
        accordion_panel(
          "Aesthetic mappings",
          ## Input: select axes
          selectInput(
            inputId = ns("axis1"),
            label = "Component X",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          ),
          selectInput(
            inputId = ns("axis2"),
            label = "Component Y",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
          ),
          selectInput(
            inputId = ns("axis3"),
            label = "Component Z",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
          ),
          ## Input: color mapping
          selectizeInput(
            inputId = ns("symbol_color"),
            label = "Colors",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(plugins = "clear_button")
          ),
          ## Input: symbol mapping
          selectizeInput(
            inputId = ns("symbol_shape"),
            label = "Symbol shapes",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(plugins = "clear_button")
          ),
          ## Input: symbol size mapping
          selectizeInput(
            inputId = ns("symbol_size"),
            label = "Symbol sizes",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(plugins = "clear_button")
          )
        ),
        accordion_panel(
          "Layers",
          ## Input: add points
          checkboxInput(
            inputId = ns("points"),
            label = "Show points",
            value = TRUE
          ),
          ## Input: add density
          checkboxInput(
            inputId = ns("density"),
            label = "Density contour",
            value = FALSE
          )
        ),
        accordion_panel(
          "Transform",
          checkboxInput(
            inputId = ns("center"),
            label = "Center",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("scale"),
            label = "Scale",
            value = FALSE
          )
        ),
        accordion_panel(
          "Envelopes",
          ## Input: select group
          selectizeInput(
            inputId = ns("group"),
            label = "Group by",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(plugins = "clear_button")
          ),
          ## Input: add ellipses
          radioButtons(
            inputId = ns("wrap"),
            label = "Wrap:",
            choices = c(
              "None" = "none",
              "Tolerance ellipse" = "tol",
              "Confidence ellipse" = "conf",
              "Convex hull" = "hull"
            )
          ),
          checkboxGroupInput(
            inputId = ns("level"),
            label = "Ellipse level",
            selected = "0.95",
            choiceNames = c("68%", "95%", "99%"),
            choiceValues = c("0.68", "0.95", "0.99")
          )
        ),
        accordion_panel(
          "Annotations",
          ## Input: add a grid
          checkboxInput(
            inputId = ns("grid"),
            label = "Grid",
            value = TRUE
          ),
          ## Input: add a legend
          # TODO
          # checkboxInput(
          #   inputId = ns("legend"),
          #   label = "Legend",
          #   value = TRUE
          # )
        )
      )
    ), # sidebar
    output_plot(
      id = ns("ternplot"),
      tools = list(
        select_color(inputId = ns("col")),
        select_pch(inputId = ns("pch")),
        select_cex(inputId = ns("cex"))
      ),
      height = "100%",
      click = ns("click"),
      title = "Ternary plot"
    )
    # tableOutput(outputId = ns("info"))
  ) # layout_sidebar
}

# Server =======================================================================
#' Ternary Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @seealso [ternary_ui()]
#' @family plot modules
#' @keywords internal
#' @export
ternary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Subset data -----
    is_quanti <- reactive({
      req(x())
      arkhe::detect(x(), margin = 2, f = is.numeric)
    })
    data_quali <- reactive({
      if (sum(!is_quanti()) < 1) return(NULL)
      x()[, !is_quanti(), drop = FALSE]
    })
    data_quanti <- reactive({
      if (sum(is_quanti()) < 3) return(NULL)
      x()[, is_quanti(), drop = FALSE]
    })

    ## Update UI -----
    bindEvent(
      observe({
        choices <- colnames(data_quanti())
        updateSelectInput(session, inputId = "axis1", choices = choices)
      }),
      data_quanti()
    )
    bindEvent(
      observe({
        choices <- setdiff(colnames(data_quanti()), input$axis1)
        selected2 <- if (input$axis2 %in% choices) input$axis2 else NULL
        selected3 <- if (input$axis3 %in% choices) input$axis3 else NULL
        updateSelectInput(session, inputId = "axis2",
                          choices = choices, selected = selected2)
        updateSelectInput(session, inputId = "axis3",
                          choices = choices, selected = selected3)
      }),
      input$axis1
    )
    bindEvent(
      observe({
        choices <- setdiff(colnames(data_quanti()), c(input$axis1, input$axis2))
        selected <- if (input$axis3 %in% choices) input$axis3 else NULL
        updateSelectInput(session, inputId = "axis3",
                          choices = choices, selected = selected)
      }),
      input$axis2
    )
    bindEvent(
      observe({
        choices <- c(none = "", colnames(x()))
        updateSelectInput(session, inputId = "symbol_color", choices = choices)
      }),
      x()
    )
    bindEvent(
      observe({
        choices <- c(none = "", colnames(data_quali()))
        updateSelectInput(session, inputId = "symbol_shape", choices = choices)
        updateSelectInput(session, inputId = "group", choices = choices)
      }),
      data_quali()
    )
    bindEvent(
      observe({
        choices <- c(none = "", colnames(data_quanti()))
        updateSelectInput(session, inputId = "symbol_size", choices = choices)
      }),
      data_quanti()
    )

    ## Get ternary data -----
    data_tern <- reactive({
      tern <- data_quanti()[, c(input$axis1, input$axis2, input$axis3)]
      tern[rowSums(tern, na.rm = TRUE) != 0, , drop = FALSE]
    })

    data_info <- reactive({
      req(data_tern())
      cart <- isopleuros::coordinates_ternary(
        x = data_tern(),
        center = input$center,
        scale = input$scale
      )
      cart <- cart[c("x", "y")]
      names(cart) <- c(".x", ".y") # Safety

      z <- data.frame(data_tern(), cart)
      z <- nearPoints(z, input$click, xvar = ".x", yvar = ".y")
      z[, -ncol(z) + c(1, 0)]
    })

    ## Build plot -----
    plot_ternary <- reactive({
      ## Select data
      tern <- data_tern()
      n <- nrow(tern)

      ## Compute center and scale
      no_scale <- !input$center && !input$scale

      ## Graphical parameters
      col <- rep("black", n)
      border <- rep("black", n)
      pch <- as.numeric(input$pch) %||% 16
      cex <- as.numeric(input$cex) %||% 1

      if (is_set(input$group)) {
        grp <- data_quali()[, input$group, drop = TRUE]
      } else {
        grp <- rep("", n)
      }
      if (is_set(input$symbol_color)) {
        symbol_color <- x()[, input$symbol_color, drop = TRUE]
        col <- get_color(input$col, n = length(unique(symbol_color)))
        if (is.double(symbol_color)) {
          col <- khroma::palette_color_continuous(colors = col)(symbol_color)
        } else {
          col <- khroma::palette_color_discrete(colors = col)(symbol_color)
        }
        if (all(grp == symbol_color)) border <- col
      }
      if (is_set(input$symbol_shape)) {
        symbol_shape <- data_quali()[, input$symbol_shape, drop = TRUE]
        pch <- khroma::palette_shape(symbols = pch)(symbol_shape)
      } else {
        pch <- pch[[1L]]
      }
      if (is_set(input$symbol_size)) {
        symbol_size <- data_quanti()[, input$symbol_size, drop = TRUE]
        cex <- khroma::palette_size_range(range = range(cex))(symbol_size)
      } else {
        cex <- min(cex)
      }

      ## Build plot
      graphics::par(mar = c(1, 1, 1, 1))
      z <- isopleuros::ternary_plot(
        x = tern,
        type = "n",
        xlab = input$axis1,
        ylab = input$axis2,
        zlab = input$axis3,
        center = input$center,
        scale = input$scale
      )

      ## Add grid
      if (input$grid) {
        isopleuros::ternary_grid(center = z$center, scale = z$scale)
      }

      if (no_scale) {
        ## Density contours
        if (input$density) {
          isopleuros::ternary_density(tern)
        }

        ## Envelope
        level <- as.numeric(input$level)
        fun_wrap <- switch(
          input$wrap,
          tol = function(x, ...) isopleuros::ternary_tolerance(x, level = level, ...),
          conf = function(x, ...) isopleuros::ternary_confidence(x, level = level, ...),
          hull = function(x, ...) isopleuros::ternary_hull(x, ...),
          function(...) invisible()
        )
        for (i in split(seq_len(n), f = grp)) {
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

      ## Add legend
      # TODO

      grDevices::recordPlot()
    })

    ## Render plot -----
    render_plot("ternplot", x = plot_ternary)

    ## Render table -----
    output$info <- render_table(data_info)
  })
}
