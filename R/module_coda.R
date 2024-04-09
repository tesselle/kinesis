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
        inputId = ns("codes"),
        label = bslib::tooltip(
          span("Unique identifiers", icon("info-circle")),
          "Define the unique identifier (eg. laboratory codes) of each observation."
        ),
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(plugins = "clear_button")
      ),
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

      freezeReactiveValue(input, "codes")
      freezeReactiveValue(input, "samples")
      freezeReactiveValue(input, "groups")
      updateSelectizeInput(
        session,
        inputId = "codes",
        choices = c("", choices),
        selected = grep("^code[s]{0,1}$", choices, ignore.case = TRUE, value = TRUE)
      )
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
            auto = FALSE,
            verbose = get_option("verbose")
          )
        },
        what = "Composition"
      )
    })

    ## Add metadata -----
    meta <- reactive({
      req(coda())
      run_with_notification(
        {
          out <- coda()
          if (input$codes != "") {
            nexus::set_identifiers(out) <- x()[, input$codes, drop = TRUE]
          }
          if (input$samples != "") {
            nexus::set_samples(out) <- x()[, input$samples, drop = TRUE]
          }
          if (input$groups != "") {
            nexus::set_groups(out) <- x()[, input$groups, drop = TRUE]
          }
          return(out)
        },
        what = "Metadata"
      )
    })

    ## Render filters -----
    output$limits <- renderUI({
      req(meta())

      n <- ncol(coda())
      if (n == 0) return(NULL)

      parts <- colnames(meta())
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
      req(meta())

      parts <- colnames(meta())
      ids <- paste("limit", parts, sep = "_")
      limits <- lapply(X = ids, FUN = function(i, x) x[[i]], x = input)

      if (any(lengths(limits) == 0) || all(limits == 0)) return(meta())
      limits <- unlist(limits) / 100
      nexus::replace_zero(meta(), value = limits, delta = input$delta)
    })

    ## Render metadata -----
    list_group <- reactive({
      grp <- nexus::get_groups(clean())
      grp <- grp[!is.na(grp)]
      tapply(
        X = grp, INDEX = grp,
        FUN = function(x, p) {
          n <- length(x)
          col <- if (n < p) "bg-danger" else if (n == p) "bg-warning text-dark" else "bg-success"

          tags$li(
            class="list-group-item",
            tags$span(class = "fw-bold", unique(x)),
            tags$span(class = sprintf("badge %s rounded-pill", col), n)
          )
        },
        simplify = FALSE,
        p = ncol(clean())
      )
    })

    output$description <- renderUI({
      req(clean())
      descr <- utils::capture.output(nexus::describe(clean()))
      markdown(descr)
    })

    ## Render tables -----
    output$table <- DT::renderDataTable({
      req(clean())
      dt <- DT::datatable(nexus::as_features(clean()), rownames = FALSE)
      dt <- DT::formatPercentage(dt, columns = seq_len(ncol(clean())) + 3, digits = 1)
      dt
    })

    clean
  })
}
