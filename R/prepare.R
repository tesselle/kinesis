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
      title = "Data",
      import_ui(ns("import")),
      select_ui(ns("select")),
      clean_ui(ns("clean")),
      # filter_ui(ns("filter"))
    ), # sidebar
    ## Output: value box
    box_ui(ns("box")),
    navset_card_pill(
      placement = "above",
      nav_panel(
        title = "Data",
        gt::gt_output(outputId = ns("table"))
      ),
      nav_panel(
        title = "Missing values",
        missing_ui(ns("missing"))
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
#' @param choose A predicate [`function`] used to select columns.
#' @param select A predicate [`function`] used to select columns.
#' @return A reactive `data.frame`.
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, choose = function(...) TRUE,
                           select = function(...) TRUE) {
  moduleServer(id, function(input, output, session) {
    ## Prepare data -----
    data_clean <- import_server("import") |>
      select_server("select", x = _, find_col = choose, use_col = select) |>
      clean_server("clean", x = _) |>
      missing_server("missing", x = _)
    # filter_server("filter", x = _)

    ## Render description -----
    box_server("box", x = data_clean)

    ## Render table -----
    output$table <- gt::render_gt({
      data_clean() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::sub_missing() |>
        gt::opt_interactive(
          use_compact_mode = TRUE,
          use_page_size_select = TRUE
        )
    })

    data_clean
  })
}

# Modules ======================================================================
## Value box -------------------------------------------------------------------
box_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = breakpoints(
      xs = c(12, 12, 12, 12),
      md = c(6, 6, 6, 6),
      lg = c(3, 3, 3, 3)
    ),
    fill = FALSE,
    value_box(
      title = "Dimensions",
      value = textOutput(outputId = ns("value_dimensions"))
    ),
    value_box(
      title = "Sparsity",
      value = textOutput(outputId = ns("value_sparsity"))
    ),
    value_box(
      title = "Missing values",
      value = textOutput(outputId = ns("value_missing"))
    ),
    card(
      helpText("Export your data for futur use."),
      downloadButton(
        outputId = ns("download"),
        label = "Download"
      )
    )
  )
}
box_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    output$value_dimensions <- renderText({
      req(x())
      paste0(dim(x()), collapse = " x ")
    })
    output$value_sparsity <- renderText({
      req(x())
      paste0(round(arkhe::sparsity(x()) * 100, 2), "%")
    })
    output$value_missing <- renderText({
      req(x())
      sum(is.na(x()))
    })

    ## Download -----
    output$download <- export_table(x, "data")
  })
}

## Select ----------------------------------------------------------------------
select_ui <- function(id, label = "Select columns:") {
  ns <- NS(id)

  checkboxGroupInput(
    inputId = ns("checked"),
    label = label,
    choices = NULL,
    selected = NULL,
    inline = TRUE,
    width = "100%"
  )
}
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param find_col A predicate [`function`] for column detection
#'  (see [arkhe::detect()]).
#' @param use_col A predicate [`function`] for column selection
#'  (see [arkhe::detect()]).
#' @param preserve A [`logical`] scalar: should existing selection be preserved
#'  on update?
#' @noRd
select_server <- function(id, x, find_col = NULL, use_col = NULL,
                          preserve = TRUE, min_row = 1, min_col = 1) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    ## Update UI
    observe({
      choices <- colnames(x())
      selected <- NULL
      col <- rep(TRUE, length(choices))
      if (length(choices) > 0 && is.function(find_col)) {
        col <- arkhe::detect(x = x(), f = find_col, margin = 2)
        choices <- choices[which(col)]
      }
      if (length(choices) > 0 && is.function(use_col)) {
        ok <- arkhe::detect(x = x(), f = use_col, margin = 2)
        selected <- choices[which(col & ok)]
      }
      if (isTRUE(preserve)) {
        ## Try to keep previous selection, if any
        keep <- intersect(choices, input$checked)
        if (length(keep) > 0) selected <- keep
      }
      freezeReactiveValue(input, "checked")
      updateCheckboxGroupInput(
        inputId = "checked",
        choices = choices,
        selected = selected
      )
    }) |>
      bindEvent(x())

    ## Bookmark
    onRestored(function(state) {
      updateCheckboxGroupInput(session, "checked", selected = state$input$checked)
    })

    ## Select variables
    reactive({
      req(input$checked)
      out <- arkhe::get_columns(x(), names = input$checked)
      validate_dim(out, i = min_row, j = min_col)
      out
    }) |>
      debounce(500)
  })
}

## Clean -----------------------------------------------------------------------
clean_ui <- function(id) {
  ns <- NS(id)

  list(
    tags$div(
      checkboxInput(
        inputId = ns("rownames"),
        label = "First column as row names"
      )
    ),
    tags$div(
      "Clean values:",
      ## Input: remove whitespace
      checkboxInput(
        inputId = ns("remove_whitespace"),
        label = "Remove leading/trailing whitespace",
        value = TRUE
      )
    ),
    tags$div(
      "Remove any non informative data:",
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
    )
  )
}
clean_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    reactive({
      out <- x()

      if (isTruthy(x())) {
        ## Assign row names
        if (isTRUE(input$rownames)) {
          out <- notify(
            arkhe::assign_rownames(out, column = 1, remove = TRUE),
            title = "Rownames"
          )
          if (!is.null(out)) x <- out
        }

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
      }

      validate_dim(out)

      out
    })
  })
}

## Missing ---------------------------------------------------------------------
missing_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    width = 1/2,
    list(
      ## Input: empty as missing
      checkboxInput(
        inputId = ns("empty_as_NA"),
        label = "Empty string as missing value",
        value = FALSE
      ),
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
    ),
    output_plot(ns("heatmap"))
  )
}
missing_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    data_replace <- reactive({
      out <- x()

      ## Replace empty strings
      if (isTRUE(input$empty_as_NA)) {
        out <- arkhe::replace_empty(out, value = NA)
      }

      ## Replace zeros
      if (isTRUE(input$zero_as_NA)) {
        out <- arkhe::replace_zero(out, value = NA)
      }

      out
    })

    data_missing <- reactive({
      out <- data_replace()

      ## Remove missing values
      choice <- get_value(input$remove, default = "")
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

      validate_dim(out)

      out
    })

    ## Render plot
    plot_missing <- reactive({
      req(data_missing())
      function() {
        col <- if (anyNA(data_missing())) c("#DDDDDD", "#BB5566") else "#DDDDDD"
        tabula::plot_heatmap(object = is.na(data_missing()), col = col,
                             fixed_ratio = FALSE)
      }
    })
    render_plot("heatmap", x = plot_missing)

    data_missing
  })
}

## Filter ----------------------------------------------------------------------
filter_ui <- function(id) {
  list(
    helpText("Remove data points that fall outside a specification."),
    uiOutput(NS(id, "controls"))
  )
}
filter_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get variable names
    vars <- reactive({ names(x()) })

    ## Build UI
    output$controls <- renderUI({
      lapply(
        X = vars(),
        FUN = function(var) filter_build(x()[[var]], session$ns(var), var)
      )
    })

    filter <- reactive({
      each_var <- lapply(
        X = vars(),
        FUN = function(var, input) filter_var(x()[[var]], input[[var]]),
        input = input
      )
      Reduce(f = `&`, x = each_var)
    })

    reactive({ x()[filter(), , drop = FALSE] })
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
