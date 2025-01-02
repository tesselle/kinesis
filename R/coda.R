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
      h5("Definition"),
      checkboxGroupInput(
        inputId = ns("parts"),
        label = "Compositional parts:",
        choices = NULL,
        selected = NULL,
        inline = TRUE,
        width = "100%"
      ),
      hr(),
      helpText(
        "You can use a qualitative variable to assign each sample to a group.",
        "Missing values will be interpreted as unassigned samples."
      ),
      selectizeInput(
        inputId = ns("groups"),
        label = "Groups",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
      hr(),
      helpText(
        "If your data contain several observations for the same sample (e.g. repeated measurements),",
        "you can use one or more categorical variable to split the data into subsets and compute the compositional mean for each."
      ),
      selectizeInput(
        inputId = ns("condense"),
        label = "Condense",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      hr(),
      uiOutput(outputId = ns("description"))
    ), # sidebar
    navset_card_pill(
      placement = "above",
      nav_panel(
        title = "Data",
        ## Output: display data
        gt::gt_output(outputId = ns("table"))
      ),
      nav_panel(
        title = "Detection limits",
        coda_zero_ui(ns("zero"))
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
    ## Update UI -----
    observe({
      req(x())
      index_numeric <- arkhe::detect(x = x(), f = is.numeric, margin = 2)
      choices <- colnames(x())
      choices_quanti <- choices[which(index_numeric)]
      choices_quali <- choices[which(!index_numeric)]

      freezeReactiveValue(input, "parts")
      updateCheckboxGroupInput(inputId = "parts", choices = choices_quanti)
      freezeReactiveValue(input, "groups")
      updateSelectizeInput(inputId = "groups", choices = c(None = "", choices_quali))
      freezeReactiveValue(input, "condense")
      updateSelectizeInput(inputId = "condense", choices = c(None = "", choices))
    })

    ## Bookmark -----
    onRestored(function(state) {
      updateCheckboxGroupInput(session, inputId = "parts",
                               selected = state$input$parts)
      updateSelectizeInput(session, inputId = "groups",
                           selected = state$input$groups)
      updateSelectizeInput(session, inputId = "condense",
                           selected = state$input$condense)
    })

    ## Coerce to compositions -----
    coda <- reactive({
      req(x(), input$parts)
      validate_dim(x())
      validate_na(x())
      validate(need(length(input$parts) > 1, "Select at least two columns."))

      notify(
        {
          nexus::as_composition(
            from = x(),
            parts = input$parts,
            verbose = verbose
          )
        },
        title = "Compositional Data"
      )
    }) |>
      debounce(1000)

    grouped <- reactive({
      req(x(), coda())

      z <- coda()
      if (isTruthy(input$groups)) {
        z <- nexus::group(z, by = x()[[input$groups]], verbose = verbose)
      }
      if (isTruthy(input$condense)) {
        z <- nexus::condense(z, by = x()[input$condense])
      }

      z
    }) |>
      debounce(1000)

    ## Zeros -----
    no_zero <- coda_zero_server("zero", x = grouped)

    ## Validate -----
    valid <- reactive({
      validate(need(!anyNA(no_zero()), "Compositional data must not contain missing values."))
      validate(need(!any(no_zero() == 0), "Compositional data must not contain zeros."))
      no_zero()
    })

    ## Render description -----
    output$description <- renderUI({
      req(no_zero())
      descr <- utils::capture.output(nexus::describe(no_zero()))
      markdown(descr)
    })

    ## Render tables -----
    output$table <- gt::render_gt({
      req(no_zero())
      if (nexus::is_grouped(no_zero())) {
        gt <- no_zero() |>
          as.data.frame() |>
          gt::gt(groupname_col = ".group", rownames_to_stub = TRUE)
      } else {
        gt <- no_zero() |>
          as.data.frame() |>
          gt::gt(rownames_to_stub = TRUE)
      }
      gt |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        gt::tab_style_body(
          fn = function(x) is.na(x),
          style = gt::cell_text(color = "red3")
        ) |>
        gt::tab_style_body(
          fn = function(x) x == 0,
          style = gt::cell_text(color = "orange")
        )
    })

    valid
  })
}

# Modules ======================================================================
## Imputation ------------------------------------------------------------------
coda_zero_ui <- function(id) {
  ns <- NS(id)

  list(
    helpText(
      "If your data contains zeros, these can be considered as values below the detection limit (thus interpreted as small unknown values).",
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
    uiOutput(outputId = ns("values"))
  )
}
coda_zero_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    id <- reactive({
      req(x())
      validate_dim(x())
      paste0("limit_", colnames(x()))
    })

    ui <- reactive({
      req(id())

      n <- length(id())
      lab <- paste(sub("limit_", "", id()), "(%)", sep = " ")

      ui <- vector(mode = "list", length = n)
      for (j in seq_len(n)) {
        ui[[j]] <- numericInput(
          inputId = session$ns(id()[[j]]),
          label = lab[[j]],
          value = 0, min = 0, max = 100
        )
      }

      do.call(layout_column_wrap, args = c(ui, width = 1/4))
    })
    output$values <- renderUI({ ui() })

    ## Bookmark
    onRestored(function(state) {
      req(ui())
      values <- lapply(X = id(), FUN = function(i, x) x[[i]], x = state$input)
      updateNumericInput(session, "select", value = values)
    })

    reactive({
      req(id())

      limits <- lapply(X = id(), FUN = function(i, x) x[[i]], x = input)
      if (any(lengths(limits) == 0) || any(limits <= 0)) return(x())
      limits <- unlist(limits) / 100

      nexus::replace_zero(x = x(), value = limits, delta = input$delta)
    })
  })
}
