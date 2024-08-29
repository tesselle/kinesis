# UI ===========================================================================
#' Prepare Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [prepare_server()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Prepare"),
      select_ui(ns("select")),
      accordion(
        open = FALSE,
        multiple = FALSE,
        clean_ui(ns("clean")),
        accordion_panel(
          "Filter rows",
          helpText("Remove data points that fall outside a specification."),
          filter_ui(ns("filter"))
        )
      )
    ), # sidebar
    ## Output: value box
    layout_columns(
      col_widths = breakpoints(
        xs = c(12, 12),
        md = c(6, 6),
        lg = c(3, 3)
      ),
      fill = FALSE,
      value_box(
        title = "Sparcity",
        value = textOutput(outputId = ns("value_sparcity"))
      ),
      value_box(
        title = "Missing values",
        value = textOutput(outputId = ns("value_missing"))
      ),
      card(
        helpText("If everything looks good with your data, click on 'confirm' and proceed to the next tab."),
        actionButton(
          inputId = ns("go"),
          label = "Confirm"
        )
      )
    ),
    navset_tab(
      nav_panel(
        title = "Data",
        ## Output: display data
        gt::gt_output(outputId = ns("table"))
      ),
      nav_panel(
        title = "Missing values",
        ## Output: display data
        output_plot(id = ns("missing"))
      )
    ),
    border_radius = FALSE,
    fillable = TRUE,
  ) # layout_sidebar
}

# Server =======================================================================
#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {

    ## Select data -----
    data_select <- select_server("select", x)

    ## Clean data -----
    data_clean <- clean_server("clean", data_select)

    ## Filter rows -----
    filter <- filter_server("filter", data_clean)
    data_filter <- reactive({ data_clean()[filter(), , drop = FALSE] })

    ## Send notification -----
    bindEvent(
      observe({
        if (anyNA(data_filter())) {
          ## Save the ID for removal later
          notif_missing <- showNotification(ui = "Missing values detected!",
                                            duration = NULL, id = "missing",
                                            type = "warning")
        } else {
          removeNotification("missing")
        }
      }),
      data_filter()
    )

    ## Render plot -----
    plot_missing <- reactive({
      req(data_filter())
      tabula::plot_heatmap(
        object = is.na(data_filter()),
        col = if (anyNA(data_filter())) c("#DDDDDD", "#BB5566") else "#DDDDDD",
        fixed_ratio = FALSE
      )
      grDevices::recordPlot()
    })
    render_plot("missing", x = plot_missing)

    ## Render table -----
    output$table <- gt::render_gt({
      req(data_filter())
      data_filter() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::sub_missing() |>
        gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
    })

    ## Render description -----
    output$value_sparcity <- renderText({
      req(data_filter())
      spa <- arkhe::sparsity(data_filter())
      paste0(round(spa * 100, 2), "%")
    })
    output$value_missing <- renderText({
      req(data_filter())
      sum(is.na(data_filter()))
    })

    results <- bindEvent(reactive({ data_filter() }), input$go)

    results
  })
}

# Modules ======================================================================
## Select ----------------------------------------------------------------------
select_ui <- function(id) {
  checkboxGroupInput(
    inputId = NS(id, "select"),
    label = "Select columns:",
    choices = NULL,
    selected = NULL,
    width = "100%"
  )
}
select_server <- clean_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    observe({
      freezeReactiveValue(input, "select")
      updateCheckboxGroupInput(
        inputId = "select",
        choices = colnames(x()),
        selected = colnames(x()),
        inline = TRUE
      )
    })

    ## Select columns
    reactive({
      assert_csv(x())
      j <- match(input$select, colnames(x()))
      if (anyNA(j) || length(j) == 0) x() else x()[, j, drop = FALSE]
    })
  })
}

## Clean -----------------------------------------------------------------------
clean_ui <- function(id) {
  ns <- NS(id)

  list(
    accordion_panel(
      "Clean values",
      ## Input: remove whitespace
      checkboxInput(
        inputId = ns("remove_whitespace"),
        label = "Remove leading/trailing whitespace",
        value = TRUE
      )
    ),
    accordion_panel(
      "Remove data",
      helpText("Remove any non informative data."),
      ## Input: remove zero
      checkboxInput(
        inputId = ns("remove_zero_row"),
        label = "Remove rows with zero",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("remove_zero_column"),
        label = "Remove columns with zero",
        value = FALSE
      ),
      ## Input: remove constant
      checkboxInput(
        inputId = ns("remove_constant_column"),
        label = "Remove constant columns",
        value = FALSE
      ),
      ## Input: remove all?
      checkboxInput(
        inputId = ns("all"),
        label = "Remove only if all values meet the condition",
        value = TRUE,
        width = "100%"
      )
    ),
    accordion_panel(
      "Missing values",
      ## Input: zero as missing
      checkboxInput(
        inputId = ns("zero_as_NA"),
        label = "Zero as missing value",
        value = FALSE
      ),
      ## Input: remove missing
      radioButtons(
        inputId = ns("remove"),
        label = "Remove missing values:",
        choices = c(
          "Keep as is" = "none",
          "Replace missing values with zeros" = "zero",
          "Remove rows with missing values" = "row",
          "Remove columns with missing values" = "col"
        )
      )
    )
  )
}
clean_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {

    reactive({
      out <- x()

      ## Clean whitespace
      if (isTRUE(input$remove_whitespace)) {
        out <- arkhe::clean_whitespace(out, squish = TRUE)
      }

      ## Remove rows
      ## If only zeros
      if (isTRUE(input$remove_zero_row)) {
        out <- arkhe::remove_zero(out, margin = 1, all = input$all,
                                  verbose = verbose)
      }

      ## Remove columns
      ## If only zeros
      if (isTRUE(input$remove_zero_column)) {
        out <- arkhe::remove_zero(out, margin = 2, all = input$all,
                                  verbose = verbose)
      }
      ## If constant
      if (isTRUE(input$remove_constant_column)) {
        out <- arkhe::remove_constant(out, verbose = verbose)
      }

      ## Remove zeros
      if (isTRUE(input$zero_as_NA)) {
        out <- arkhe::replace_zero(out, value = NA)
      }

      ## Remove missing values
      choice <- input$remove %||% ""
      fun <- switch(
        choice,
        zero = function(x) {
          arkhe::replace_NA(x, value = 0)
        },
        row = function(x) {
          arkhe::remove_NA(x, margin = 1, all = FALSE, verbose = verbose)
        },
        col = function(x) {
          arkhe::remove_NA(x, margin = 2, all = FALSE, verbose = verbose)
        },
        function(x) { x }
      )
      out <- fun(out)

      out
    })

  })
}

## Filter ----------------------------------------------------------------------
filter_ui <- function(id) {
  uiOutput(NS(id, "controls"))
}
filter_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    vars <- reactive({ names(x()) })

    output$controls <- renderUI({
      lapply(
        X = vars(),
        FUN = function(var) {
          filter_build(x()[[var]], session$ns(var), var)
        }
      )
    })

    # /!\ Disable suspend for output$controls /!\
    outputOptions(output, "controls", suspendWhenHidden = FALSE)

    reactive({
      each_var <- lapply(
        X = vars(),
        FUN = function(var, input) {
          filter_var(x()[[var]], input[[var]])
        },
        input = input
      )
      Reduce(f = `&`, x = each_var)
    })
  })
}
filter_var <- function(x, val) {
  if (is.null(val)) return(TRUE)
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.character(x)) {
    x %in% val
  } else {
    ## No control, so don't filter
    TRUE
  }
}
filter_build <- function(x, id, var, num = FALSE, char = TRUE) {
  if (is.numeric(x) && isTRUE(num)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(inputId = id, label = var,
                min = rng[1], max = rng[2], value = rng)
  } else if (is.character(x) && isTRUE(char)) {
    levs <- unique(x)
    selectizeInput(inputId = id, label = var, choices = levs, selected = levs,
                   multiple = TRUE, options = list(plugins = "remove_button"))
  } else {
    ## Not supported
    NULL
  }
}
