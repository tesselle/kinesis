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

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = ns("samples"),
        label = bslib::tooltip(
          span("Sample names", icon("info-circle")),
          "Duplicated sample names will be interpreted as repeated measurements."
        ),
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
      selectizeInput(
        inputId = ns("groups"),
        label = bslib::tooltip(
          span("Groups", icon("info-circle")),
          "Empty strings will be interpreted as unassigned samples."
        ),
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
      hr(),
      ## Output: display description
      h5("Description"),
      uiOutput(outputId = ns("description"))
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Detection limits",
          ## Output: set detection limits
          helpText(
            "Define the detection limit for each part below.",
            "Zeros (i.e. non-detected data) will be replaced by a fraction of this limit",
            cite_article("Martin-Fernandez et al.", "2003", "10.1023/A:1023866030544", FALSE, after = ".")
          ),
          numericInput(inputId = ns("delta"), label = "Fraction",
                       value = 2 / 3, min = 0, max = 1),
          uiOutput(outputId = ns("limits"))
        ),
        tabPanel(
          title = "Compositions",
          ## Output: display table
          DT::dataTableOutput(outputId = ns("table"))
        )
      )
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Compositional Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [data_server()]).
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

      freezeReactiveValue(input, "samples")
      freezeReactiveValue(input, "groups")
      updateSelectizeInput(
        session,
        inputId = "samples",
        choices = c("", choices),
        selected = grep("^sample[s]{0,1}$", choices, ignore.case = TRUE, value = TRUE)
      )
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
            samples = get_value(input$samples),
            groups = get_value(input$groups),
            auto = FALSE,
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
        ui[[j]] <- tags$div(
          style = "display: inline-block;",
          numericInput(inputId = session$ns(ids[j]), label = lab[j],
                       value = 0, min = 0, max = 100)
        )
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

    ## Render metadata -----
    output$description <- renderUI({
      req(clean())
      descr <- utils::capture.output(nexus::describe(clean()))
      markdown(descr)
    })

    ## Render tables -----
    output$table <- DT::renderDataTable({
      req(clean())
      feat <- nexus::as_features(clean())
      dt <- DT::datatable(feat, rownames = TRUE)
      num <- arkhe::detect(x = feat, f = is.numeric, margin = 2, na.rm = TRUE)
      dt <- DT::formatPercentage(dt, columns = which(num), digits = 2)
      dt
    })

    clean
  })
}
