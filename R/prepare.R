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

  nav_panel(
    title = tr_("Data"),
    value = "data",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Data"),
        import_ui(ns("import")),
        select_ui(ns("select")),
        clean_ui(ns("clean"))
      ), # sidebar
      ## Output: value box
      box_ui(ns("box")),
      navset_card_pill(
        placement = "above",
        nav_panel(
          title = tr_("Data"),
          checkboxInput(inputId = ns("head"), label = tr_("Overview"), value = TRUE),
          tableOutput(outputId = ns("table"))
        ),
        nav_panel(
          title = tr_("Missing values"),
          missing_ui(ns("missing"))
        )
      ),
      border_radius = FALSE,
      fillable = TRUE,
    ) # layout_sidebar
  ) # nav_panel
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

    ## Render description -----
    box_server("box", x = data_clean)

    ## Render table -----
    output$table <- renderTable(
      expr = {
        if (isTRUE(input$head)) utils::head(data_clean()) else data_clean()
      },
      rownames = TRUE,
      colnames = TRUE,
      na = "-"
    )

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
      title = tr_("Dimensions"),
      value = textOutput(outputId = ns("value_dimensions"))
    ),
    value_box(
      title = tr_("Sparsity"),
      value = textOutput(outputId = ns("value_sparsity"))
    ),
    value_box(
      title = tr_("Missing values"),
      value = textOutput(outputId = ns("value_missing"))
    ),
    card(
      helpText(tr_("Export your data for futur use.")),
      downloadButton(
        outputId = ns("download"),
        label = tr_("Download")
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
select_ui <- function(id, label = tr_("Select columns:")) {
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
        label = tr_("First column as row names")
      )
    ),
    tags$div(
      tr_("Clean values:"),
      ## Input: remove whitespace
      checkboxInput(
        inputId = ns("remove_whitespace"),
        label = tr_("Remove leading/trailing whitespace"),
        value = TRUE
      )
    ),
    tags$div(
      tr_("Remove any non informative data:"),
      ## Input: remove zero
      checkboxInput(
        inputId = ns("remove_zero_row"),
        label = tr_("Remove rows with zero"),
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("remove_zero_column"),
        label = tr_("Remove columns with zero"),
        value = FALSE
      ),
      ## Input: remove constant
      checkboxInput(
        inputId = ns("remove_constant_column"),
        label = tr_("Remove constant columns"),
        value = FALSE
      ),
      ## Input: remove all?
      checkboxInput(
        inputId = ns("all"),
        label = tr_("Remove only if all values meet the condition"),
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

      ## Assign row names
      if (isTruthy(out) && isTRUE(input$rownames)) {
        out <- notify(
          arkhe::assign_rownames(out, column = 1, remove = TRUE),
          title = tr_("Rownames")
        )
      }

      ## Clean whitespace
      if (isTruthy(out) && isTRUE(input$remove_whitespace)) {
        out <- arkhe::clean_whitespace(out, squish = TRUE)
      }

      ## Remove rows
      ## If only zeros
      if (isTruthy(out) && isTRUE(input$remove_zero_row)) {
        out <- arkhe::remove_zero(out, margin = 1, all = input$all,
                                  verbose = verbose)
      }

      ## Remove columns
      ## If only zeros
      if (isTruthy(out) && isTRUE(input$remove_zero_column)) {
        out <- arkhe::remove_zero(out, margin = 2, all = input$all,
                                  verbose = verbose)
      }
      ## If constant
      if (isTruthy(out) && isTRUE(input$remove_constant_column)) {
        out <- arkhe::remove_constant(out, verbose = verbose)
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
        label = tr_("Empty string as missing value"),
        value = FALSE
      ),
      ## Input: zero as missing
      checkboxInput(
        inputId = ns("zero_as_NA"),
        label = tr_("Zero as missing value"),
        value = FALSE
      ),
      ## Input: remove missing
      radioButtons(
        inputId = ns("remove"),
        label = tr_("Remove missing values:"),
        choiceNames = c(
          tr_("Keep as is"),
          tr_("Replace missing values with zeros"),
          tr_("Remove rows with missing values"),
          tr_("Remove columns with missing values")
        ),
        choiceValues = c("none", "zero", "row", "col")
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
      choice <- input$remove %|||% ""
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
        tabula::plot_heatmap(object = is.na(data_missing()), color = col,
                             fixed_ratio = FALSE)
      }
    })
    render_plot("heatmap", x = plot_missing)

    data_missing
  })
}
