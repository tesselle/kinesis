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
        value = FALSE
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
      sliderInput(
        inputId = ns("level"),
        label = "Ellipse level",
        min = 0.1, max = 0.99,
        value = 0.95, step = 0.01
      ),
      hr(),
      selectizeInput(
        inputId = ns("highlight"),
        label = "Highlight",
        choices = c("none" = "", "contribution", "cos2"),
        selected = "observation",
        multiple = FALSE,
        options = list(plugins = "clear_button")
      )
    ), # sidebar
    navset_card_pill(
      multivariate_results(id),
      multivariate_individuals(id),
      multivariate_variables(id),
      multivariate_screeplot(id)
    ) # navset_tab
  ) # layout_sidebar
}

multivariate_results <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Results",
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8)),
      output_plot(
        id = ns("plot_ind"),
        tools = list(
          select_color(
            inputId = ns("col"),
            type = "qualitative"
          ),
          select_pch(inputId = ns("pch")),
          select_cex(inputId = ns("cex"))
        ),
        title = "Individuals factor map",
        height = "100%"
      ),
      output_plot(
        id = ns("plot_var"),
        title = "Variables factor map",
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
      col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8)),
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
      dimensio::screeplot(
        x = x(),
        cumulative = TRUE,
        labels = FALSE,
        limit = sum(eigen()[, 3] <= 99)
      )
      grDevices::recordPlot()
    })

    ## Individuals -----
    plot_ind <- reactive({
      req(x())

      dimensio::viz_rows(
        x = x(),
        axes = c(axis1(), axis2()),
        active = TRUE,
        sup = TRUE,
        labels = input$lab_row,
        highlight = get_value(input$highlight),
        col = if (is_set(input$highlight)) get_color(input$col) else "black",
        pch = as.numeric(input$pch),
        cex = as.numeric(input$cex),
        panel.first = graphics::grid()
      )

      ## Envelope
      fun_wrap <- switch(
        input$wrap,
        tol = function(x, ...) dimensio::viz_tolerance(x, level = input$level, ...),
        conf = function(x, ...) dimensio::viz_confidence(x, level = input$level, ...),
        hull = function(x, ...) dimensio::viz_hull(x, ...),
        function(...) invisible()
      )

      fun_wrap(x = x(), margin = 1, axes = c(axis1(), axis2()))
      grDevices::recordPlot()
    })

    ## Variables -----
    plot_var <- reactive({
      req(x())

      dimensio::viz_variables(
        x = x(),
        axes = c(axis1(), axis2()),
        active = TRUE, sup = TRUE,
        labels = input$lab_col,
        panel.first = graphics::grid()
      )
      grDevices::recordPlot()
    })

    contrib_var <- reactive({
      req(x())

      graphics::par(mfrow = c(1, 2))
      dimensio::viz_contributions(x = x(), margin = 2, axes = axis1())
      dimensio::viz_contributions(x = x(), margin = 2, axes = axis2())
      grDevices::recordPlot()
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
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
    })
    output$info_var <- gt::render_gt({
      req(x())
      info <- dimensio::summary(x(), margin = 2)@results
      info |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
    })
  })
}
