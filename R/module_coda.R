# UI ===========================================================================
#' Compositional Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_coda_server()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      wellPanel(
        selectizeInput(
          inputId = ns("codes"),
          label = "Unique identifiers",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
        ),
        helpText("Duplicated sample names will be interpreted as repeated measurements."),
        selectizeInput(
          inputId = ns("samples"),
          label = "Sample names",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
        ),
        helpText("Empty strings can be used to specify that a sample does not belong to any group."),
        selectizeInput(
          inputId = ns("groups"),
          label = "Reference groups",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
        )
      ), # wellPanel
      ## Output: display description
      uiOutput(outputId = ns("description"))
    ), # column
    column(
      width = 8,
      ## Output: display table
      DT::dataTableOutput(outputId = ns("table"))
    ) # column
  ) # fluidRow
}

# Server =======================================================================
#' Compositional Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_import_server()]).
#' @return A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [module_coda_ui()]
#' @family coda modules
#' @keywords internal
#' @export
module_coda_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select variables
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

    coda <- reactive({
      req(x())

      tryCatch({
        nexus::as_composition(from = x(), auto = FALSE, verbose = get_option("verbose"))
      }, warning = function(w) {
        showNotification(ui = w, type = "warning")
        return(NULL)
      }, error = function(e) {
        showNotification(ui = e, type = "error")
        return(NULL)
      }, silent = TRUE)
    })

    meta <- reactive({
      req(coda())
      out <- coda()
      tryCatch({
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
      }, warning = function(w) {
        showNotification(ui = w, type = "warning")
        return(NULL)
      }, error = function(e) {
        if (!inherits(e, "shiny.silent.error")) { # Ignore silent error
          showNotification(ui = e, type = "error")
        }
        return(NULL)
      }, silent = FALSE)
    })

    clean <- reactive({
      # TODO: detection limit
      meta()
    })

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
      assigned <- nexus::is_assigned(clean())
      n_col <- ncol(clean())
      n_obs <- nrow(clean())
      n_spl <- length(unique(nexus::get_samples(clean())))
      n_rep <- sum(nexus::is_replicated(clean()))
      n_grp <- length(unique(nexus::get_groups(clean())[assigned]))
      n_out <- sum(!assigned)

      msg_parts <- sprintf("%d parts: %s", n_col, paste0(colnames(clean()), collapse = ", "))
      msg_obs <- sprintf("%d %s:", n_obs, ngettext(n_obs, "observation", "observations"))
      msg_spl <- sprintf("%d unique %s", n_spl, ngettext(n_spl, "sample", "samples"))
      msg_rep <- sprintf("%d replicated %s", n_rep, ngettext(n_rep, "observation", "observations"))
      msg_grp <- sprintf("%d %s", n_grp, ngettext(n_grp, "group", "groups"))
      msg_out <- sprintf("%d unassigned %s", n_out, ngettext(n_out, "sample", "samples"))

      tags$ul(
        class = "list-group list-group-flush",
        tags$li(class="list-group-item", msg_parts),
        tags$li(class="list-group-item", msg_spl),
        tags$li(class="list-group-item", msg_rep),
        tags$li(
          class="list-group-item",
          msg_grp,
          tags$ul(class = "list-group list-group-flush", list_group())
        ),
        tags$li(class="list-group-item", msg_out)
      )
    })

    ## Render table
    output$table <- DT::renderDataTable({
      req(clean())
      dt <- DT::datatable(nexus::as_features(clean()), rownames = FALSE)
      dt <- DT::formatPercentage(dt, columns = seq_len(ncol(clean())) + 3,
                                 digits = 1)
      dt
    })

    clean
  })
}
