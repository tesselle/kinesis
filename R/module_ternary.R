# UI ===========================================================================
#' Ternary Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_ternary_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_ternary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h5("Ternary plot"),
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
      checkboxInput(
        inputId = ns("center"),
        label = "Center",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("scale"),
        label = "Scale",
        value = FALSE
      ),
      hr(),
      ## Input: select group
      selectizeInput(
        inputId = ns("group"),
        label = "Group",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
      hr(),
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
      sliderInput(
        inputId = ns("level"),
        label = "Ellipse level",
        min = 0.1, max = 1,
        value = 0.95, step = 0.05
      ),
      ## Input: add a grid
      checkboxInput(
        inputId = ns("grid"),
        label = "Grid",
        value = TRUE
      ),
      ## Input: add a legend
      checkboxInput(
        inputId = ns("legend"),
        label = "Legend",
        value = TRUE
      )
    ), # sidebarPanel
    mainPanel(
      fluidRow(
        div(
          class = "col-lg-6 col-md-1",
          output_plot(
            id = ns("ternplot"),
            tools = list(
              select_color(
                inputId = ns("col"),
                type = "qualitative"
              ),
              select_pch(inputId = ns("pch")),
              select_cex(inputId = ns("cex"))
            ),
            height = "auto",
            click = ns("click"),
            title = "Ternary plot"
          )
        ),
        div(
          class = "col-lg-6 col-md-1",
          tableOutput(outputId = ns("info"))
        )
      )
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
        updateSelectInput(session, inputId = "axis2", choices = choices)
        updateSelectInput(session, inputId = "axis3", choices = choices)
      }),
      input$axis1
    )
    bindEvent(
      observe({
        choices <- setdiff(colnames(data_quanti()), c(input$axis1, input$axis2))
        updateSelectInput(session, inputId = "axis3", choices = choices)
      }),
      input$axis2
    )
    bindEvent(
      observe({
        choices <- c(none = "", colnames(data_quali()))
        updateSelectInput(session, inputId = "group", choices = choices)
      }),
      data_quali()
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
      grp <- rep("", n)
      col <- rep("black", n)
      pch <- rep(as.numeric(input$pch), n)
      cex <- as.numeric(input$cex)

      if (is_set(input$group)) {
        grp <- data_quali()[, input$group]
        grp <- factor(grp, levels = unique(grp), exclude = NULL)
        col <- get_color(input$col, n = nlevels(grp))[grp]
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
        fun_wrap <- switch(
          input$wrap,
          tol = function(x, ...) isopleuros::ternary_tolerance(x, level = input$level, ...),
          conf = function(x, ...) isopleuros::ternary_confidence(x, level = input$level, ...),
          hull = function(x, ...) isopleuros::ternary_hull(x, ...),
          function(...) invisible()
        )
        for (i in split(seq_len(n), f = grp)) {
          z <- tern[i, , drop = FALSE]
          if (nrow(z) < 3) next
          fun_wrap(z, lty = 1, border = col[i])
        }
      }

      ## Add points
      if (input$points) {
        isopleuros::ternary_points(tern, col = col, pch = pch, cex = cex,
                                   center = z$center, scale = z$scale)
      }

      ## Add legend
      if (input$legend && nlevels(grp) > 1) {
        graphics::legend(
          x = "topright",
          legend = levels(grp),
          pch = unique(pch),
          col = unique(col),
          bty = "n"
        )
      }
      grDevices::recordPlot()
    })

    ## Render plot -----
    render_plot("ternplot", x = plot_ternary)

    ## Render table -----
    output$info <- render_table(data_info)
  })
}
