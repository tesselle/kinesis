# UI ===========================================================================
#' Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [data_server()]
#' @family generic modules
#' @keywords internal
#' @export
data_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Import",
      kinesis::import_ui(ns("import"))
    ), # tabPanel
    tabPanel(
      title = "Prepare",
      kinesis::prepare_ui(ns("prepare"))
    ), # tabPanel
    tabPanel(
      title = "Missing values",
      kinesis::missing_ui(ns("missing"))
    ) # tabPanel
  ) # tabsetPanel
}

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

  sidebarLayout(
    sidebarPanel(
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
      tags$hr(),
      ## Input: lines of the data to skip
      numericInput(
        inputId = ns("skip"),
        label = "Lines of the data file to skip:",
        value = 0,
        min = 0,
        step = 1
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
    ), # sidebarPanel
    mainPanel(
      ## Output: display data
      DT::dataTableOutput(outputId = ns("table"))
    ) # mainPanel
  ) # sidebarLayout
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

#' Missing Values UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [missing_server()]
#' @family generic modules
#' @keywords internal
#' @export
missing_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
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
    ), # sidebarPanel
    mainPanel(
      ## Output: summary
      fluidRow(
        div(
          class = "col-lg-6 col-md-1",
          tableOutput(outputId = ns("summary_column"))
        ),
        div(
          class = "col-lg-6 col-md-1",
          tableOutput(outputId = ns("summary_row"))
        )
      )
    ) # mainPanel
  ) # sidebarLayout
}

# Server =======================================================================
#' Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return A reactive `data.frame`.
#' @seealso [data_ui()]
#' @family generic modules
#' @keywords internal
#' @export
data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    import_server("import") |>
      prepare_server("prepare", x = _) |>
      missing_server("missing", x = _)
  })
}

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
    ## Get file path
    file <- reactive({
      query <- parseQueryString(session$clientData$url_search)

      if (!is.null(query[['data']])) {
        url(as.character(query[['data']]))
      } else {
        input$file$datapath
      }
    })

    ## Read data file
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

    ## Send notification
    bindEvent(
      observe({
        if (is.data.frame(data()) && all(dim(data()) > 0)) {
          msg <- "Data successfully imported!"
          showNotification(ui = msg, type = "message")
        }
      }),
      data()
    )

    ## Render table
    output$table <- DT::renderDataTable({ data() })

    data
  })
}

#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, x) {
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
      quali <- arkhe::discard(
        x = data_clean(),
        f = is.numeric,
        margin = 2,
        verbose = get_option("verbose")
      )
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

#' Missing Values Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [missing_ui()]
#' @family generic modules
#' @keywords internal
#' @export
missing_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## A notification ID
    id <- NULL

    ## Clean data -----
    clean <- reactive({
      out <- x()

      if (input$zero) {
        out <- arkhe::replace_zero(out, value = NA)
      }

      fun <- switch(
        input$remove,
        none = function(x) { x },
        zero = function(x) arkhe::replace_NA(x, value = 0),
        row = function(x) arkhe::remove_NA(x, margin = 1, all = FALSE),
        col = function(x) arkhe::remove_NA(x, margin = 2, all = FALSE)
      )

      fun(out)
    })

    ## Send notification -----
    bindEvent(
      observe({
        if (anyNA(clean())) {
          ## If there's currently a notification, don't add another
          if (!is.null(id)) return()
          ## Save the ID for removal later
          id <<- showNotification(ui = "Missing values detected!",
                                  duration = NULL, id = "missing",
                                  type = "warning")
        } else if (!is.null(id)) {
          removeNotification(id)
          id <<- NULL
        }
      }),
      clean()
    )

    ## Count missing values -----
    na_row <- reactive({
      req(clean())
      count_missing(clean(), margin = 1)
    })
    na_col <- reactive({
      req(clean())
      count_missing(clean(), margin = 2)
    })

    ## Render tables -----
    output$summary_row <- render_table(na_row)
    output$summary_column <- render_table(na_col)

    clean
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
    rng <- range(x, na.rm = TRUE)
    sliderInput(inputId = ns(var), label = var, width = "100%",
                min = rng[1], max = rng[2], value = rng)
  } else if (is.character(x)) {
    levs <- unique(x)
    selectizeInput(inputId = ns(var), label = var, width = "100%",
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
count_missing <- function(x, margin = 1) {
  total <- as.integer(arkhe::count(x, f = is.na, margin = margin))
  has_na <- total > 0
  prop_na <- round(total[has_na] * 100 / ncol(x))
  if (margin == 1) {
    id <- rownames(x)[has_na]
  } else {
    id <- colnames(x)[has_na]
  }
  tbl_na <- data.frame(id, total[has_na], sprintf("%g%%", prop_na))
  mar <- if (margin == 1) "Row" else "Column"
  colnames(tbl_na) <- c(mar, "Count", "Proportion")
  tbl_na
}
