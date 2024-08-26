# UI ===========================================================================
#' Import Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [import_server()]
#' @family generic modules
#' @keywords internal
#' @export
import_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5("Import"),
      ## Input: select a file
      fileInput(
        inputId = ns("file"),
        label = "Choose a CSV file:",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      tags$p(
        helpText("Select the location of, and the CSV file you want to upload."),
        helpText("Please check the default settings below and adjust them to your data."),
        helpText("This application only supports data encoded in UFT-8."),
      ),
      tags$p(
        helpText("It assumes that you keep your data tidy: each variable must be saved in its own column and each observation (sample) must be saved in its own row.")
      ),
      ## Input: checkbox if file has header
      checkboxInput(
        inputId = ns("header"),
        label = "Header",
        value = TRUE
      ),
      ## Input: checkbox if file has row names
      checkboxInput(
        inputId = ns("rownames"),
        label = "Row names",
        value = FALSE
      ),
      ## Input: select decimal
      radioButtons(
        inputId = ns("dec"),
        label = "Decimal",
        choices = c(Dot = ".", Comma = ","),
        selected = "."
      ),
      ## Input: select separator
      radioButtons(
        inputId = ns("sep"),
        label = "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),
      ## Input: select quotes
      radioButtons(
        inputId = ns("quote"),
        label = "Quote",
        choices = c(None = "", "Double quote" = '"', "Single quote" = "'"),
        selected = '"'
      ),
      ## Input: lines of the data to skip
      numericInput(
        inputId = ns("skip"),
        label = "Lines of the data file to skip:",
        value = 0,
        min = 0,
        step = 1
      ),
      ## Input: missing string
      textInput(
        inputId = ns("na.strings"),
        label = "String to be interpreted as missing value:",
        value = ""
      ),
      ## Input: comment
      textInput(
        inputId = ns("comment"),
        label = "Character to be interpreted as comment:",
        value = "#"
      )
    ), # sidebar
    prepare_ui(ns("prepare")),
    border_radius = FALSE,
    fillable = TRUE,
    class = "p-0"
  ) # layout_sidebar
}

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
      h5("Prepare"),
      accordion(
        open = FALSE,
        multiple = FALSE,
        accordion_panel(
          "Select columns",
          checkboxGroupInput(
            inputId = ns("select"),
            label = "Keep:",
            choices = NULL,
            selected = NULL,
            width = "100%"
          )
        ),
        accordion_panel(
          "Clean values",
          ## Input: remove whitespace
          checkboxInput(
            inputId = ns("remove_whitespace"),
            label = "Remove leading/trailing whitespace",
            value = TRUE
          )
        ),
        accordion_panel(
          "Remove data",
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
          )
        ),
        accordion_panel(
          "Missing values",
          ## Input: zero as missing
          checkboxInput(
            inputId = ns("zero"),
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
        accordion_panel(
          "Filter rows",
          helpText("Remove data points that fall outside a specification."),
          uiOutput(outputId = ns("filter"))
        )
      )
    ), # sidebar
    ## Output: value box
    layout_columns(
      col_widths = breakpoints(
        xs = c(12, 12),
        md = c(6, 6),
        lg = c(3, 3)
      ),
      fill = FALSE,
      value_box(
        title = "Sparcity",
        value = textOutput(outputId = ns("value_sparcity"))
      ),
      value_box(
        title = "Missing values",
        value = textOutput(outputId = ns("value_missing"))
      )
    ),
    navset_tab(
      nav_panel(
        title = "Data",
        ## Output: display data
        gt::gt_output(outputId = ns("table"))
      ),
      nav_panel(
        title = "Missing values",
        ## Output: display data
        output_plot(id = ns("missing"))
      )
    ),
    border_radius = FALSE,
    fillable = TRUE,
  ) # layout_sidebar
}

# Server =======================================================================
#' Import Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return A reactive `data.frame`.
#' @seealso [import_ui()]
#' @family generic modules
#' @keywords internal
#' @export
import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Get file path -----
    file <- reactive({
      query <- parseQueryString(session$clientData$url_search)

      if (!is.null(query[['data']])) {
        url(as.character(query[['data']]))
      } else {
        input$file$datapath
      }
    })

    ## Read data file -----
    data <- reactive({
      assert_csv(file())

      id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      read_table(
        path = file(),
        header = input$header,
        sep = input$sep,
        dec = input$dec,
        quote = input$quote,
        rownames = if (input$rownames) 1 else NULL,
        na.strings = input$na.strings,
        skip = if (!is.na(input$skip)) input$skip else 0,
        comment.char = input$comment
      )
    })

    ## Send notification -----
    bindEvent(
      observe({
        if (is.data.frame(data()) && all(dim(data()) > 0)) {
          msg <- "Data successfully imported!"
          showNotification(ui = msg, type = "message")
        }
      }),
      data()
    )

    ## Prepare data -----
    clean <- prepare_server("prepare", data)

    clean
  })
}

#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Build UI -----
    observe({
      freezeReactiveValue(input, "select")
      updateCheckboxGroupInput(
        inputId = "select",
        choices = colnames(x()),
        selected = colnames(x()),
        inline = TRUE
      )
    })

    ## Select data -----
    data_select <- reactive({
      assert_csv(x())
      j <- match(input$select, colnames(x()))
      x()[, j, drop = FALSE]
    })

    ## Remove data -----
    data_clean <- reactive({
      out <- data_select()

      ## Remove whitespace
      if (isTRUE(input$remove_whitespace)) {
        out <- arkhe::clean_whitespace(out, squish = TRUE)
      }

      ## Remove rows
      ## If only zeros
      if (isTRUE(input$remove_zero_row)) {
        out <- arkhe::remove_zero(out, margin = 1, all = input$all,
                                  verbose = get_option("verbose"))
      }

      ## Remove columns
      ## If only zeros
      if (isTRUE(input$remove_zero_column)) {
        out <- arkhe::remove_zero(out, margin = 2, all = input$all,
                                  verbose = get_option("verbose"))
      }

      ## If constant
      if (isTRUE(input$remove_constant_column)) {
        out <- arkhe::remove_constant(out, verbose = get_option("verbose"))
      }

      out
    })

    ## Missing values -----
    data_missing <- reactive({
      out <- data_clean()

      if (input$zero) out <- arkhe::replace_zero(out, value = NA)

      fun <- switch(
        input$remove,
        none = function(x) { x },
        zero = function(x) {
          arkhe::replace_NA(x, value = 0)
        },
        row = function(x) {
          arkhe::remove_NA(x, margin = 1, all = FALSE,
                           verbose = get_option("verbose"))
        },
        col = function(x) {
          arkhe::remove_NA(x, margin = 2, all = FALSE,
                           verbose = get_option("verbose"))
        }
      )

      fun(out)
    })

    ## Render filters -----
    output$filter <- renderUI({
      req(data_missing())
      n <- ncol(data_missing())
      if (n == 0) return(NULL)

      parts <- colnames(data_missing())
      ui <- vector(mode = "list", length = n)
      for (j in seq_len(n)) {
        x <- data_missing()[, j, drop = TRUE]
        ui[[j]] <- make_filter(session, x = x, var = parts[j])
      }

      ui
    })

    ## Filter rows -----
    data_filter <- reactive({
      out <- data_missing()

      each_var <- lapply(
        X = colnames(out),
        FUN = function(j, x, val) {
          ok <- filter_var(x = x[[j]], val = val[[paste0("filter_", j)]])
          ok %||% TRUE
        },
        x = out,
        val = input
      )
      keep <- Reduce(f = `&`, x = each_var)

      if (all(!keep)) return(out)
      out[keep, , drop = FALSE]
    })

    ## Send notification -----
    bindEvent(
      observe({
        if (anyNA(data_missing())) {
          ## Save the ID for removal later
          notif_missing <- showNotification(ui = "Missing values detected!",
                                            duration = NULL, id = "missing",
                                            type = "warning")
        } else {
          removeNotification("missing")
        }
      }),
      data_missing()
    )

    ## Render plot -----
    plot_missing <- reactive({
      req(data_filter())
      tabula::plot_heatmap(
        object = is.na(data_filter()),
        col = if (anyNA(data_filter())) c("#DDDDDD", "#BB5566") else "#DDDDDD",
        fixed_ratio = FALSE
      )
      grDevices::recordPlot()
    })
    render_plot("missing", x = plot_missing)

    ## Render table -----
    output$table <- gt::render_gt({
      data_filter() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::sub_missing() |>
        gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE)
    })

    ## Render description -----
    output$value_sparcity <- renderText({
      req(data_filter())
      spa <- arkhe::sparsity(data_filter())
      paste0(round(spa * 100, 2), "%")
    })
    output$value_missing <- renderText({
      req(data_filter())
      sum(is.na(data_filter()))
    })

    data_filter
  })
}

# Helpers ======================================================================
read_table <- function(path, header = TRUE, sep = ",", dec = ".", quote = "\"'",
                       rownames = NULL, na.strings = "NA", skip = 0,
                       comment.char = "#") {
  tryCatch(
    {
      utils::read.table(file = path, header = header, sep = sep, dec = dec,
                        quote = quote, row.names = rownames,
                        na.strings = na.strings, skip = skip,
                        comment.char = comment.char)
    },
    error = function(e) {
      showModal(modalDialog(
        conditionMessage(e),
        title = "Data import failed!",
        easyClose = TRUE
      ))
      return(NULL)
    }
  )
}
make_filter <- function(session, x, var) {
  ns <- session$ns
  if (is.numeric(x)) {
    return(NULL)
    rng <- range(x, na.rm = TRUE)
    sliderInput(inputId = ns(var), label = var, width = "100%",
                min = rng[1], max = rng[2], value = rng)
  } else if (is.character(x)) {
    levs <- unique(x)
    selectizeInput(inputId = ns(paste0("filter_", var)),
                   label = var, width = "100%",
                   choices = levs, selected = levs, multiple = TRUE,
                   options = list(plugins = "remove_button"))
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
