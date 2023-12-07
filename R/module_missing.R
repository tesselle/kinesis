# UI ===========================================================================
#' Missing Values UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_missing_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_missing_ui <- function(id) {
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
#' Missing Values Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by
#'  [module_import_server()]).
#' @return A reactive `data.frame`.
#' @seealso [module_missing_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_missing_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    keep_row <- reactive({
      apply(X = x(), MARGIN = 1, FUN = function(x) !any(is.na(x)))
    })

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

    output$summary_row <- renderTable({
      req(clean())
      count_missing(clean(), margin = 1)
    }, striped = TRUE, width = "100%")

    output$summary_column <- renderTable({
      req(clean())
      count_missing(clean(), margin = 2)
    }, striped = TRUE, width = "100%")

    clean
  })
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
