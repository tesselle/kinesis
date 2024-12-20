# UI ===========================================================================
#' Mutlivariate Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return A tab that can be passed to [shiny::tabsetPanel()].
#' @seealso [multivariate_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      open = FALSE,
      h5("Factor maps"),
      ## Input: display options
      selectInput(
        inputId = ns("axis1"),
        label = "Horizontal axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectInput(
        inputId = ns("axis2"),
        label = "Vertical axis",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      checkboxInput(
        inputId = ns("lab_row"),
        label = "Label individuals",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("lab_col"),
        label = "Label variables",
        value = TRUE
      ),
      hr(),
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
      ),
      hr(),
      radioButtons(
        inputId = ns("extra_quanti"),
        label = "Quality assessment:",
        choices = c(
          "None" = "",
          "Contribution" = "contribution",
          "Cos2" = "cos2"
        )
      )
    ), # sidebar
    navset_card_pill(
      multivariate_results(id),
      multivariate_individuals(id),
      multivariate_variables(id),
      multivariate_screeplot(id)
    ),
    border = FALSE
  ) # layout_sidebar
}

multivariate_results <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Results",
    helpText("Click and drag to select an area, then double-click to zoom in.",
             "Double-click again to reset the zoom."),
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      output_plot(
        id = ns("plot_ind"),
        tools = list(
          select_color(inputId = ns("col_ind")),
          select_pch(inputId = ns("pch"), default = NULL),
          select_cex(inputId = ns("cex"), default = c(1, 6))
        ),
        title = "Individuals factor map",
        dblclick = ns("plot_ind_dblclick"),
        brush = brushOpts(
          id = ns("plot_ind_brush"),
          resetOnNew = TRUE
        ),
        height = "100%"
      ),
      output_plot(
        id = ns("plot_var"),
        tools = list(
          select_color(inputId = ns("col_var"), default = "YlOrBr"),
          select_lty(inputId = ns("lty"), default = NULL),
          select_cex(inputId = ns("lwd"), default = c(1, 1))
        ),
        title = "Variables factor map",
        dblclick = ns("plot_var_dblclick"),
        brush = brushOpts(
          id = ns("plot_var_brush"),
          resetOnNew = TRUE
        ),
        height = "100%"
      )
    ) # layout_columns
  )
}

multivariate_individuals <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Individuals",
    gt::gt_output(outputId = ns("info_ind"))
  )
}

multivariate_variables <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Variables",
    output_plot(id = ns("contrib_var"), title = "Contributions"),
    gt::gt_output(outputId = ns("info_var"))
  )
}

multivariate_screeplot <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Screeplot",
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      output_plot(id = ns("screeplot"), title = "Screeplot"),
      gt::gt_output(outputId = ns("variance"))
    )
  )
}

# Server =======================================================================
#' Multivariate Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`dimensio::MultivariateAnalysis-class`] object.
#' @seealso [multivariate_ui]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    observeEvent(axes(), {
      freezeReactiveValue(input, "axis1")
      updateSelectInput(inputId = "axis1", choices = axes())
    })
    observeEvent(axis1(), {
      choices <- axes()[-axis1()]
      freezeReactiveValue(input, "axis2")
      updateSelectInput(inputId = "axis2", choices = choices)
    })

    axes <- reactive({
      choices <- seq_len(nrow(eigen()))
      names(choices) <- unique(rownames(eigen()))
      choices
    })
    axis1 <- reactive({
      req(input$axis1)
      as.numeric(input$axis1)
    })
    axis2 <- reactive({
      req(input$axis2)
      as.numeric(input$axis2)
    })

    ## Eigenvalues -----
    eigen <- reactive({
      req(x())
      dimensio::get_eigenvalues(x())
    })

    plot_eigen <- reactive({
      req(x())
      function() {
        dimensio::screeplot(
          x = x(),
          cumulative = TRUE,
          labels = FALSE,
          limit = sum(eigen()[, 3] <= 99)
        )
      }
    })

    ## Interactive zoom -----
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_ind <- reactiveValues(x = NULL, y = NULL)
    range_var <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_ind$x <- brush_xlim(input$plot_ind_brush)
      range_ind$y <- brush_ylim(input$plot_ind_brush)
    }) |>
      bindEvent(input$plot_ind_dblclick)

    observe({
      range_var$x <- brush_xlim(input$plot_var_brush)
      range_var$y <- brush_ylim(input$plot_var_brush)
    }) |>
      bindEvent(input$plot_var_dblclick)

    ## Individuals -----
    plot_ind <- reactive({
      req(x())

      ## Envelope
      level <- as.numeric(input$level)
      fun_wrap <- switch(
        input$wrap,
        tol = function(x, ...) dimensio::viz_tolerance(x, level = level, ...),
        conf = function(x, ...) dimensio::viz_confidence(x, level = level, ...),
        hull = function(x, ...) dimensio::viz_hull(x, ...),
        function(...) invisible()
      )

      function() {
        dimensio::viz_rows(
          x = x(),
          axes = c(axis1(), axis2()),
          active = TRUE,
          sup = TRUE,
          labels = input$lab_row,
          extra_quanti = get_value(input$extra_quanti),
          color = khroma::color(input$col_ind),
          symbol = get_value(as.integer(input$pch)),
          size = input$cex,
          xlim = range_ind$x,
          ylim = range_ind$y,
          panel.first = graphics::grid()
        )
        fun_wrap(
          x = x(),
          margin = 1,
          axes = c(axis1(), axis2()),
          color = khroma::color(input$col_ind)
        )
      }
    })

    ## Variables -----
    plot_var <- reactive({
      req(x())
      function() {
        dimensio::viz_variables(
          x = x(),
          axes = c(axis1(), axis2()),
          active = TRUE, sup = TRUE,
          labels = input$lab_col,
          extra_quanti = get_value(input$extra_quanti),
          color = khroma::color(input$col_var),
          symbol = get_value(as.integer(input$lty)),
          size = input$lwd,
          xlim = range_var$x,
          ylim = range_var$y,
          panel.first = graphics::grid()
        )
      }
    })

    contrib_var <- reactive({
      req(x())
      function() {
        graphics::par(mfrow = c(1, 2))
        dimensio::viz_contributions(x = x(), margin = 2, axes = axis1())
        dimensio::viz_contributions(x = x(), margin = 2, axes = axis2())
      }
    })

    ## Render plots -----
    render_plot("plot_ind", x = plot_ind)
    render_plot("plot_var", x = plot_var)
    render_plot("contrib_var", x = contrib_var)
    render_plot("screeplot", x = plot_eigen)

    ## Render tables -----
    output$variance <- gt::render_gt({
      eigen() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(columns = 2, decimals = 3) |>
        gt::fmt_percent(columns = c(3, 4), decimals = 2, scale_values = FALSE) |>
        gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
    })
    output$info_ind <- gt::render_gt({
      req(x())
      info <- dimensio::summary(x(), margin = 1)@results
      info |>
        as.data.frame() |>
        multivariate_summary()
    })
    output$info_var <- gt::render_gt({
      req(x())
      info <- dimensio::summary(x(), margin = 2)@results
      info |>
        as.data.frame() |>
        multivariate_summary()
    })
  })
}

multivariate_summary <- function(x) {
  gt::gt(x, rownames_to_stub = TRUE) |>
    gt::fmt_number(decimals = 3) |>
    gt::tab_spanner(
      label = "Coordinates",
      columns = gt::ends_with("coord"),
      id = "coord"
    ) |>
    gt::tab_spanner(
      label = "Contribution",
      columns = gt::ends_with("contrib"),
      id = "contrib"
    ) |>
    gt::tab_spanner(
      label = "Squared cosinus",
      columns = gt::ends_with("cos2"),
      id = "cos2"
    ) |>
    gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
}
