# UI ===========================================================================
#' Co-Occurrence UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [occurrence_server()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Co-Occurrence"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        h5(tr_("Co-Occurrence")),
        radioButtons(
          inputId = ns("method"),
          label = tr_("Method"),
          choiceNames = c(tr_("Absolute frequency"),
                          tr_("Relative frequency"),
                          tr_("Binomial assessment")),
          choiceValues = c("absolute", "relative", "binomial")
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      layout_columns(
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
        output_plot(
          id = ns("plot"),
          tools = list(
            select_color(id = ns("col"), type = c("sequential", "diverging"))
          )
        ),
        card(
          gt::gt_output(outputId = ns("table"))
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Co-Occurrence Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive [`data.frame`].
#' @seealso [occurrence_ui()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get count data -----
    counts <- reactive({
      req(x())
      arkhe::keep_columns(x(), f = is.numeric)
    })

    ## Check data -----
    old <- reactive({ counts() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), counts, old, title = tr_("Co-Occurrence"))

    ## Compute index -----
    compute_occur <- ExtendedTask$new(
      function(x, method) {
        promises::future_promise({
          tabula::occurrence(x, method = tolower(method))
        })
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_occur$invoke(counts(), input$method)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_occur$result(), title = tr_("Co-Occurrence"))
    })

    ## Plot -----
    map <- reactive({
      req(results())
      col <- get_color("col")()
      function() tabula::plot_heatmap(results(), color = col)
    })

    ## Render table -----
    output$table <- gt::render_gt({
      req(results())
      results() |>
        as.matrix() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 2) |>
        gt::sub_missing()
    })

    ## Render plot -----
    render_plot("plot", x = map)

    ## Download -----
    output$download <- export_table(results, "occurrence")
  })
}
