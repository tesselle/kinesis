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
      ## Input: select 4th variable
      # selectInput(
      #   inputId = ns("highlight"),
      #   label = "Highlight",
      #   choices = NULL,
      #   selected = NULL,
      #   multiple = FALSE,
      # ),
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
            dblclick = ns("dblclick"),
            brush = brushOpts(id = ns("brush"), resetOnNew = TRUE),
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
    ranges <- reactiveValues(x = NULL, y = NULL, z = NULL)

    ## Observe -----------------------------------------------------------------
    observeEvent(data_quanti(), {
      choices <- colnames(data_quanti())
      updateSelectInput(session, inputId = "axis1", choices = choices)
    })
    observeEvent(input$axis1, {
      choices <- setdiff(colnames(data_quanti()), input$axis1)
      updateSelectInput(session, inputId = "axis2", choices = choices)
      updateSelectInput(session, inputId = "axis3", choices = choices)
    })
    observeEvent(input$axis2, {
      choices <- setdiff(colnames(data_quanti()), c(input$axis1, input$axis2))
      updateSelectInput(session, inputId = "axis3", choices = choices)
    })
    observeEvent(data_quali(), {
      choices <- c(none = "", colnames(data_quali()))
      updateSelectInput(session, inputId = "group", choices = choices)
    })
    observeEvent(input$dblclick, {
      brush <- input$brush
      if (!is.null(brush)) {
        lim <- isopleuros::coordinates_cartesian(
          x = c(brush$xmin, brush$xmax),
          y = c(brush$ymin, brush$ymax)
        )
        ranges$x <- lim$x
        ranges$y <- lim$y
        ranges$z <- lim$z
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
        ranges$z <- NULL
      }
    })

    ## Reactive ----------------------------------------------------------------
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
    data_tern <- reactive({
      data_quanti()[, c(input$axis1, input$axis2, input$axis3)]
    })
    data_info <- reactive({
      req(data_tern())
      cart <- isopleuros::coordinates_ternary(data_tern())
      tern <- isopleuros::coordinates_cartesian(cart)
      names(cart) <- c(".x", ".y") # Safety
      names(tern) <- colnames(data_tern())
      z <- data.frame(tern, cart)
      z <- nearPoints(z, input$click, xvar = ".x", yvar = ".y")
      z[, -ncol(z) + c(1, 0)]
    })

    ## Build plot ---
    plot_ternary <- reactive({
      ## Select data
      tern <- data_tern()
      n <- nrow(tern)

      ## Graphical parameters
      grp <- rep("", n)
      col <- rep("black", n)
      pch <- rep(as.numeric(input$pch), n)
      cex <- as.numeric(input$cex)
      if (is_set(input$group)) {
        grp <- as.factor(data_quali()[, input$group])
        col <- get_color(input$col, n = nlevels(grp))[grp]
      }

      ## Build plot
      graphics::par(mar = c(1, 1, 1, 1))
      isopleuros::ternary_plot(
        x = NULL,
        xlab = input$axis1,
        ylab = input$axis2,
        zlab = input$axis3,
        xlim = ranges$x,
        ylim = ranges$y,
        zlim = ranges$z
      )
      if (input$grid) {
        isopleuros::ternary_grid()
      }
      if (input$density) {
        isopleuros::ternary_density(tern)
      }
      if (input$points) {
        isopleuros::ternary_points(tern, col = col, pch = pch, cex = cex)
      }
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
