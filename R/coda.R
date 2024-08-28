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
      helpText("Empty strings will be interpreted as unassigned samples."),
      selectizeInput(
        inputId = ns("groups"),
        label = "Groups",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
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
          "Define the detection limit for each compositionnal part below.",
          "Zeros (i.e. non-detected data) will be replaced by a fraction of this limit",
          cite_article("Martin-Fernandez et al.", "2003", "10.1023/A:1023866030544", FALSE, after = ".")
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
    ## Select variables -----
    observeEvent(x(), {
      index_numeric <- arkhe::detect(x = x(), f = is.numeric, margin = 2)
      choices <- colnames(x())[!index_numeric]

      freezeReactiveValue(input, "groups")
      updateSelectizeInput(
        session,
        inputId = "groups",
        choices = c("", choices),
        selected = grep("^group[s]{0,1}$", choices, ignore.case = TRUE, value = TRUE)
      )
    })

    ## Coerce to compositions -----
    coda <- reactive({
      req(x())

      run_with_notification(
        {
          nexus::as_composition(
            from = x(),
            groups = get_value(input$groups),
            verbose = get_option("verbose")
          )
        },
        what = "Composition"
      )
    })

    ## Render filters -----
    output$limits <- renderUI({
      req(coda())

      n <- ncol(coda())
      if (n == 0) return(NULL)

      parts <- colnames(coda())
      ids <- paste("limit", parts, sep = "_")
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
      ids <- paste("limit", parts, sep = "_")
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
      if (nexus::any_assigned(clean())) {
        tbl <- data.frame(group = nexus::get_groups(clean()), clean())
        gt <- gt::gt(tbl, groupname_col="group", rownames_to_stub = TRUE)
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
