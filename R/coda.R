# UI ===========================================================================
#' Compositional Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      helpText("Coerce your data to compositions and define (reference) groups."),
      checkboxgroup_ui(ns("parts"), label = "Compositional parts:"),
      helpText(
        "You can use a qualitative variable to assign each sample to a group.",
        "Missing values will be interpreted as unassigned samples."
      ),
      selectize_ui(id = ns("groups"), label = "Groups"),
      helpText(
        "If your data contain several observations for the same sample (e.g. repeated measurements),",
        "you can use one or more categorical variable to split the data into subsets and compute the compositional mean for each."
      ),
      selectize_ui(id = ns("condense"), label = "Condense", multiple = TRUE),
    ), # sidebar
    card(
      card_header("Data"),
      card_body(
        ## Output: display data
        gt::gt_output(outputId = ns("table"))
      )
    ),
    border_radius = FALSE,
    fillable = TRUE
  ) # layout_sidebar
}

# Server =======================================================================
#' Compositional Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @return A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select columns
    col_parts <- column_checkbox_server("parts", x = x, find_col = is.numeric)
    col_groups <- column_select_server("groups", x = x, find_col = Negate(is.numeric))
    col_condense <- column_select_server("condense", x = x)

    ## Update UI -----
    observe({
      choices <- colnames(x())
      quali <- which(arkhe::detect(x = x(), f = Negate(is.numeric), margin = 2))

      freezeReactiveValue(input, "groups")
      updateSelectizeInput(inputId = "groups", choices = c(None = "", choices[quali]))
      freezeReactiveValue(input, "condense")
      updateSelectizeInput(inputId = "condense", choices = c(None = "", choices))
    })

    ## Bookmark -----
    onRestored(function(state) {
      updateSelectizeInput(session, inputId = "groups",
                           selected = state$input$groups)
      updateSelectizeInput(session, inputId = "condense",
                           selected = state$input$condense)
    })

    ## Coerce to compositions -----
    coda <- reactive({
      req(col_parts())

      parts <- get_value(col_parts())
      notify(
        nexus::as_composition(from = x(), parts = parts, autodetect = FALSE,
                              verbose = verbose),
        title = "Compositional Data"
      )
    }) |>
      debounce(500)

    ## Zeros -----
    # TODO

    grouped <- reactive({
      req(coda())

      out <- coda()
      if (isTruthy(col_groups())) {
        out <- nexus::group(out, by = x()[[col_groups()]], verbose = verbose)
      }
      if (isTruthy(col_condense())) {
        out <- nexus::condense(out, by = x()[col_condense()], verbose = verbose)
      }

      validate_dim(out, j = 2)
      validate_na(out)
      validate_zero(out)

      out
    }) |>
      debounce(500)

    ## Render tables -----
    output$table <- gt::render_gt({
      req(grouped())
      if (nexus::is_grouped(grouped())) {
        gt <- grouped() |>
          as.data.frame() |>
          gt::gt(rownames_to_stub = TRUE, groupname_col = ".group")
      } else {
        gt <- grouped() |>
          as.data.frame() |>
          gt::gt(rownames_to_stub = TRUE)
      }
      gt |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        # gt::tab_style_body(
        #   fn = function(x) is.na(x),
        #   style = gt::cell_text(color = "red3")
        # ) |>
        # gt::tab_style_body(
        #   fn = function(x) x == 0,
        #   style = gt::cell_text(color = "orange")
        # ) |>
        gt::opt_interactive(
          use_compact_mode = TRUE,
          use_page_size_select = TRUE
        )
    })

    grouped
  })
}

# Modules ======================================================================
## Imputation ------------------------------------------------------------------
coda_zero_ui <- function(id) {
  ns <- NS(id)

  list(
    helpText(
      "If your data contains zeros, these can be considered as values below the detection limit",
      "(thus interpreted as small unknown values).",
      "In this case, you can define the detection limit for each compositional part below.",
      "If all limits are specified, zeros will be replaced by a fraction of these limits.",
      "See", cite_article("Martin-Fernandez et al.", "2003", "10.1023/A:1023866030544", T), "for computational details."
    ),
    numericInput(
      inputId = ns("delta"),
      label = "Fraction",
      value = 2 / 3,
      min = 0,
      max = 1
    ),
    uiOutput(outputId = ns("values")),
    actionButton(inputId = ns("go"), "Replace zero")
  )
}
coda_zero_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    data <- reactiveValues(values = NULL)

    ## Build UI
    ids <- reactive({
      if (is.null(colnames(x()))) return(NULL)
      data$values <- x()
      paste0("limit_", colnames(x()))
    })

    ui <- reactive({
      req(ids())

      ui <- lapply(
        X = ids(),
        FUN = function(i) {
          numericInput(
            inputId = session$ns(i),
            label = paste(sub("limit_", "", i), "(%)", sep = " "),
            value = 0, min = 0, max = 100
          )
        }
      )

      do.call(layout_column_wrap, args = c(ui, width = 1/4))
    })
    output$values <- renderUI({ ui() })
    outputOptions(output, "values", suspendWhenHidden = FALSE)

    ## Compute
    observe({
      req(ids())
      limits <- lapply(X = ids(), FUN = function(i, x) x[[i]], x = input)
      if (all(lengths(limits) != 0) || all(limits > 0)) {
        limits <- unlist(limits) / 100
        data$values <- nexus::replace_zero(
          x = x(),
          value = limits,
          delta = input$delta
        )
      }
    }) |>
      bindEvent(input$go)

    ## Bookmark
    onRestored(function(state) {
      req(ui())
      for (i in ids()) {
        updateNumericInput(session, session$ns(i), value = state$input[[i]])
      }
    })

    reactive({ data$values })
  })
}
