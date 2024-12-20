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
        options = list(plugins = "clear_button")
      ),
      hr(),
      uiOutput(outputId = ns("description"))
    ), # sidebar
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        open = FALSE,
        h5("Detection limits"),
        ## Output: set detection limits
        helpText(
          "If your data contains zeros, these can be considered as values below the detection limit (thus interpreted as small unknown values).",
          "In this case, you can define the detection limit for each compositional part below.",
          "If all limits are specified, zeros will be replaced by a fraction of these limits.",
          "See", cite_article("Martin-Fernandez et al.", "2003", "10.1023/A:1023866030544", T), "for computational details."
        ),
        numericInput(inputId = ns("delta"), label = "Fraction",
                     value = 2 / 3, min = 0, max = 1),
        uiOutput(outputId = ns("limits"))
      ),
      ## Output: display table,
      gt::gt_output(outputId = ns("table")),
      border = FALSE
    ), # layout_sidebar
    border_radius = FALSE,
    fillable = TRUE,
    class = "p-0"
  ) # layout_sidebar
}

# Server =======================================================================
#' Compositional Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    observe({
      req(x())
      index_numeric <- arkhe::detect(x = x(), f = is.numeric, margin = 2)
      index_double <- arkhe::detect(x = x(), f = is.double, margin = 2)
      choices <- colnames(x())[!index_double]

      freezeReactiveValue(input, "parts")
      updateCheckboxGroupInput(
        inputId = "parts",
        choices = colnames(x())[index_numeric],
        selected = colnames(x())[index_double],
        inline = TRUE
      )
      freezeReactiveValue(input, "groups")
      updateSelectizeInput(
        inputId = "groups",
        choices = c("", choices)
      )
      freezeReactiveValue(input, "condense")
      updateSelectizeInput(
        inputId = "condense",
        choices = c("", choices)
      )
    })

    ## Coerce to compositions -----
    coda <- reactive({
      req(x())
      validate_dim(x())
      validate_na(x())

      z <- run_with_notification(
        {
          nexus::as_composition(
            from = x(),
            parts = get_value(input$parts),
            groups = get_value(input$groups),
            verbose = get_option("verbose", default = FALSE)
          )
        },
        title = "Composition"
      )

      if (isTruthy(input$condense)) {
        by <- x()[, input$condense]
        z <- nexus::condense(z, by = by)
      }

      z
    })

    ## Render filters -----
    output$limits <- renderUI({
      req(coda())

      n <- ncol(coda())
      if (n == 0) return(NULL)

      parts <- colnames(coda())
      ids <- paste0("limit_", parts)
      lab <- paste(parts, "(%)", sep = " ")
      ui <- vector(mode = "list", length = n)
      for (j in seq_len(n)) {
        ui[[j]] <- numericInput(inputId = session$ns(ids[j]), label = lab[j],
                                value = 0, min = 0, max = 100)
      }

      ui
    })

    ## Impute zeros -----
    clean <- reactive({
      req(coda())

      parts <- colnames(coda())
      ids <- paste0("limit_", parts)
      limits <- lapply(X = ids, FUN = function(i, x) x[[i]], x = input)

      if (any(lengths(limits) == 0) || all(limits == 0)) return(coda())
      limits <- unlist(limits) / 100
      nexus::replace_zero(coda(), value = limits, delta = input$delta)
    })

    ## Validate -----
    valid <- reactive({
      validate(need(!anyNA(clean()), "Compositional data must not contain missing values."))
      validate(need(!any(clean() == 0), "Compositional data must not contain zeros."))
      clean()
    })

    ## Render description -----
    output$description <- renderUI({
      req(clean())
      descr <- utils::capture.output(nexus::describe(clean()))
      markdown(descr)
    })

    ## Render tables -----
    output$table <- gt::render_gt({
      req(clean())
      if (nexus::is_grouped(clean())) {
        gt <- clean() |>
          as.data.frame() |>
          gt::gt(groupname_col = ".group", rownames_to_stub = TRUE)
      } else {
        gt <- clean() |>
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
