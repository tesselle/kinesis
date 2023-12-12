# UI ===========================================================================
#' Prepare Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_prepare_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_prepare_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h5("1. Select columns"),
      checkboxGroupInput(
        inputId = ns("select"),
        label = NULL,
        choices = NULL,
        selected = NULL,
        width = "100%"
      ),
      hr(),
      h5("2. Remove data"),
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
      ),
      hr(),
      h5("3. Filter rows"),
      helpText("Remove data points that fall outside a specification."),
      uiOutput(outputId = ns("filter"))
    ), # sidebarPanel
    mainPanel(
      ## Output: display data
      DT::dataTableOutput(outputId = ns("table"))
    ) # column
  ) # sidebarLayout
}

# Server =======================================================================
#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [module_prepare_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_prepare_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI
    observe({
      freezeReactiveValue(input, "select")
      updateCheckboxGroupInput(
        inputId = "select",
        label = "Columns to remove:",
        choices = colnames(x()),
        selected = NULL,
        inline = TRUE
      )
    })

    ## Select data
    data_select <- reactive({
      assert_csv(x())
      j <- !(colnames(x()) %in% input$select)
      x()[, j, drop = FALSE]
    })

    ## Remove data
    data_clean <- reactive({
      out <- data_select()

      ## Remove rows
      ## If only zeros
      if (isTRUE(input$remove_zero_row)) {
        out <- arkhe::remove_zero(out, margin = 1, all = input$all)
      }

      ## Remove columns
      ## If only zeros
      if (isTRUE(input$remove_zero_column)) {
        out <- arkhe::remove_zero(out, margin = 2, all = input$all)
      }

      ## If constant
      if (isTRUE(input$remove_constant_column)) {
        out <- arkhe::remove_constant(out)
      }

      out
    })

    ## Filter rows
    data_filter <- reactive({
      each_var <- lapply(
        X = colnames(data_clean()),
        FUN = function(j, x, val) {
          ok <- filter_var(x = x[[j]], val = val[[j]])
          ok %||% TRUE
        },
        x = data_clean(),
        val = input
      )
      keep <- Reduce(f = `&`, x = each_var)
      if (all(!keep)) return(data_clean())
      data_clean()[keep, , drop = FALSE]
    })

    ## Render filters
    output$filter <- renderUI({
      req(data_clean())
      quali <- arkhe::discard(x = data_clean(), f = is.numeric,
                              margin = 2, verbose = get_option("verbose"))
      n <- ncol(quali)
      if (n == 0) return(NULL)

      parts <- colnames(quali)
      ui <- vector(mode = "list", length = n)
      for (j in seq_len(n)) {
        x <- quali[, j, drop = TRUE]
        ui[[j]] <- make_filter(session, x = x, var = parts[j])
      }

      ui
    })

    ## Render table
    output$table <-  DT::renderDataTable({ data_filter() })

    data_filter
  })
}

make_filter <- function(session, x, var) {
  ns <- session$ns
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(inputId = ns(var), label = var, width = "100%",
                min = rng[1], max = rng[2], value = rng)
  } else if (is.character(x)) {
    levs <- unique(x)
    selectInput(inputId = ns(var), label = var, width = "100%",
                choices = levs, selected = levs, multiple = TRUE)
  } else {
    ## Not supported
    NULL
  }
}
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.character(x)) {
    x %in% val
  } else {
    ## No control, so don't filter
    TRUE
  }
}
