# UI ===========================================================================
#' CA Seriation UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [seriate_server()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Seriation"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Permutation"),
        ## Input: checkbox if permute rows
        checkboxInput(
          inputId = ns("margin_row"),
          label = tr_("Permute rows"),
          value = TRUE
        ),
        ## Input: checkbox if permute columns
        checkboxInput(
          inputId = ns("margin_col"),
          label = tr_("Permute columns"),
          value = TRUE
        ),
        ## Input: select CA axes
        numericInput(
          inputId = ns("axes"),
          label = tr_("CA dimension"),
          value = 1,
          min = 1,
          max = 10,
          step = 1
        ),
        ## Output: download
        downloadButton(
          outputId = ns("export_table"),
          label = tr_("Export matrix")
        )
      ), # sidebar
      ## Output: plot reordered matrix
      navset_card_pill(
        bertin_ui(
          id = ns("plot"),
          title = tr_("Rearranged matrix")
        ),
        nav_panel(
          title = tr_("Significance"),
          layout_sidebar(
            sidebar = sidebar(
              helpText(
                HTML(sprintf(
                  tr_("Test the significance of the seriation solutions according to %s."),
                  cite_article(author = "Porcic", year = "2013", doi = "10.1016/j.jas.2013.07.013")
                ))
              ),
              sliderInput(
                inputId = ns("replicates"),
                label = tr_("Bootstrap replicates"),
                min = 0, max = 1000, value = 0, step = 100
              ),
              bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
            ), # sidebar
            layout_columns(
              col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
              tags$ul(
                tags$li(textOutput(outputId = ns("signif_obs"))),
                tags$li(textOutput(outputId = ns("signif_coef")))
              ),
              output_plot(id = ns("hist"), title = "Histogram of randomized total number of modes")
            )
          )
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' CA Seriation Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param order A reactive \R object (coercible by [kairos::as_seriation()]).
#' @return A reactive [`kairos::AveragePermutationOrder-class`] object.
#' @seealso [seriate_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_server  <- function(id, x, order) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(order))

  moduleServer(id, function(input, output, session) {
    ## Seriate -----
    data_seriate <- reactive({
      validate(need(order(), tr_("Compute the seriation order first.")))

      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)

      kairos::as_seriation(order(), margin = margin, axes = input$axes)
    })

    ## Permute -----
    data_permute <- reactive({
      req(x())
      req(data_seriate())
      kairos::permute(x(), data_seriate())
    })

    ## Test seriation -----
    compute_test <- ExtendedTask$new(
      function(x, axes, n) {
        promises::future_promise(
          expr = { kairos::assess(x, axes = axes, n = n) },
          seed = TRUE
        )
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_test$invoke(data_seriate(), axes = input$axes, n = input$replicates)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_test$result(), title = tr_("Significance of Seriation"))
    })

    ## Plot -----
    hist_random <- reactive({
      req(length(results()$random) > 0)
      function() {
        h <- hist(results()$random, plot = FALSE)
        p05 <- quantile(results()$random, probs = 0.05)
        plot(h, xlab = NULL, main = NULL, las = 1)
        abline(v = p05, col = "red", lwd = 2)
        axis(side = 3, at = p05, label = "5th percentile", col.axis = "red", lwd = 0)
      }
    })

    ## Render plot -----
    bertin_server("plot", x = data_permute)
    render_plot("hist", x = hist_random)

    ## Render values -----
    output$signif_coef <- renderText({
      coef <- if (isTruthy(results())) results()$coef else NA_real_
      sprintf(tr_("Seriation coefficient: %.3f;"), coef)
    })
    output$signif_obs <- renderText({
      observed <- if (isTruthy(results())) results()$observed else NA_integer_
      expected <- if (isTruthy(results())) results()$expected else NA_integer_
      sprintf(
        tr_("Observed total number of modes: %d (expected: %d);"),
        observed, expected
      )
    })

    ## Download -----
    output$export_table <- export_table(data_permute, name = "permuted")

    data_seriate
  })
}
