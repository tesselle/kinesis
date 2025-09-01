# UI ===========================================================================
#' Linear Model UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [lm_server()]
#' @family modeling modules
#' @keywords internal
#' @export
lm_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Linear Model"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Linear Model"),
        ## Input: select axes
        selectizeInput(
          inputId = ns("response"),
          label = tr_("Dependent variable"),
          choices = NULL,
          multiple = FALSE
        ),
        checkboxGroupInput(
          inputId = ns("explanatory"),
          label = tr_("Independent variables"),
          choices = NULL,
          inline = TRUE
        )
      ), # sidebar
      navset_card_pill(
        nav_panel(
          title = tr_("Summary"),
          verbatimTextOutput(outputId = ns("summary"))
        ),
        nav_panel(
          title = tr_("Diagnostic"),
          layout_columns(
            col_widths = breakpoints(xs = 12, sm = c(6, 6), md = c(4, 4, 4)),
            output_plot(id = ns("plot_hist"), title = tr_("Residuals histogram")),
            output_plot(id = ns("plot_qq"), title = tr_("Residual Q-Q plot")),
            output_plot(id = ns("plot_fitted"), title = tr_("Residuals-Fitted")),
            output_plot(id = ns("plot_scale"), title = tr_("Scale-Location")),
            output_plot(id = ns("plot_cook"), title = tr_("Cook's distance")),
            output_plot(id = ns("plot_lev"), title = tr_("Residuals-Leverage"))
          )
        ),
        nav_panel(
          title = tr_("Prediction"),
          layout_sidebar(
            sidebar = sidebar(
              ## Input: prediction
              radioButtons(
                inputId = ns("interval"),
                label = tr_("Interval"),
                choiceNames = c(tr_("Confidence interval")), # tr_("Prediction interval")
                choiceValues = c("confidence") # "prediction"
              ),
              radioButtons(
                inputId = ns("level"),
                label = tr_("Level"),
                selected = "0.95",
                choiceNames = c("68%", "95%", "99%"),
                choiceValues = c("0.68", "0.95", "0.99")
              )
            ),
            gt::gt_output(outputId = ns("prediction"))
          )
        )
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Linear Model Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`data.frame`].
#' @return A reactive [`lm`] object.
#' @seealso [lm_ui()]
#' @family modeling modules
#' @keywords internal
#' @export
lm_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    quanti <- reactive({
      req(x())
      i <- which(arkhe::detect(x = x(), f = is.numeric, margin = 2))
      x()[i]
    })

    observe({
      choices <- c("", colnames(quanti()))
      names(choices) <- c(tr_("Choose"), rep("", ncol(quanti())))

      freezeReactiveValue(input, "response")
      updateSelectizeInput(
        inputId = "response",
        choices = choices,
        server = TRUE
      )
    }) |>
      bindEvent(quanti())

    observe({
      choices <- setdiff(colnames(quanti()), input$response)
      selected <- intersect(choices, input$explanatory)

      freezeReactiveValue(input, "explanatory")
      updateCheckboxGroupInput(
        inputId = "explanatory",
        choices = choices,
        selected = if (length(selected) > 0) selected else NULL
      )
    }) |>
      bindEvent(input$response)

    ## Linear regression -----
    model <- reactive({
      req(x())
      req(input$response)
      req(input$explanatory)
      vars <- stats::as.formula(
        paste0(input$response, " ~ ", paste0(input$explanatory, collapse = " + "))
      )
      stats::lm(vars, data = x(), na.action = stats::na.omit, y = TRUE)
    })
    prediction <- reactive({
      data.frame(
        y = model()$y,
        stats::predict(
          object = model(),
          se.fit = FALSE,
          interval = input$interval,
          level = as.numeric(input$level)
        )
      )
    })

    ## Diagnostic tests -----
    # TODO?

    ## Diagnostic plots -----
    plot_hist <- reactive({
      function() {
        graphics::hist(stats::residuals(model()), main = NULL, xlab = "Residuals")
      }
    })
    plot_fitted <- reactive({
      function() {
        plot(model(), which = 1, caption = "", sub.caption = "")
      }
    })
    plot_qq <- reactive({
      function() {
        plot(model(), which = 2, caption = "", sub.caption = "")
      }
    })
    plot_scale <- reactive({
      function() {
        plot(model(), which = 3, caption = "", sub.caption = "")
      }
    })
    plot_cook <- reactive({
      function() {
        plot(model(), which = 4, caption = "", sub.caption = "")
      }
    })
    plot_lev <- reactive({
      function() {
        plot(model(), which = 5, caption = "", sub.caption = "")
      }
    })

    ## Render plot -----
    render_plot("plot_hist", plot_hist)
    render_plot("plot_fitted", plot_fitted)
    render_plot("plot_qq", plot_qq)
    render_plot("plot_scale", plot_scale)
    render_plot("plot_cook", plot_cook)
    render_plot("plot_lev", plot_lev)

    ## Render table -----
    output$prediction <- gt::render_gt({
      lvl <- as.numeric(input$level)
      int <- switch(
        input$interval,
        confidence = tr_("Confidence interval"),
        prediction = tr_("Prediction interval")
      )
      gt::gt(prediction(), rownames_to_stub = TRUE) |>
        gt::tab_spanner(
          label = sprintf("%s (%1.0f%%)", int, lvl * 100),
          columns = c("lwr", "upr")
        ) |>
        gt::cols_label(
          y = tr_("Response"),
          fit = tr_("Fitted"),
          lwr = tr_("Lower bound"),
          upr = tr_("Upper bound")
        )
    })

    ## Render prints -----
    output$summary <- renderPrint({ summary(model()) })

    model
  })
}
