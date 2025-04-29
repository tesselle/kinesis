# UI ===========================================================================
#' Time Intervals UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [time_interval_server()]
#' @family chronology modules
#' @keywords internal
#' @export
time_interval_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Intervals"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Time Intervals"),
        selectize_ui(
          id = ns("lower"),
          label = tr_("Lower temporal boundary")
        ),
        selectize_ui(
          id = ns("upper"),
          label = tr_("Upper temporal boundary")
        ),
        selectizeInput(
          inputId = ns("calendar"),
          label = tr_("Calendar"),
          choices = c("CE", "BCE", "BP"),
          selected = "CE"
        ),
        selectize_ui(
          id = ns("groups"),
          label = tr_("Groups")
        )
      ), # sidebar
      output_plot(
        id = ns("plot"),
        tools = list(
          select_color(ns("color"), type = "qualitative")
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Aoristic Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @return A reactive `list`.
#' @seealso [time_interval_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
time_interval_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    col_lower <- column_select_server("lower", x = x, find_col = is.numeric)
    col_upper <- column_select_server("upper", x = x, find_col = is.numeric)
    col_groups <- column_select_server("groups", x = x, find_col = Negate(is.numeric))

    lower <- reactive({
      req(col_lower())
      x()[[col_lower()]]
    })
    upper <- reactive({
      req(col_upper())
      x()[[col_upper()]]
    })
    groups <- reactive({
      if (isTruthy(col_groups())) x()[[col_groups()]] else NULL
    })
    calendar <- reactive({
      req(input$calendar)
      aion::calendar(input$calendar)
    })

    ## Time Intervals -----
    results <- reactive({
      req(x(), lower(), upper(), calendar())
      notify(
        {
          aion::intervals(start = lower(), end = upper(),
                          calendar = calendar(), names = rownames(x()))
        },
        title = tr_("Aoristic Analysis")
      )
    })

    ## Plot -----
    plot <- reactive({
      req(results())
      grp <- NULL
      col <- "black"
      if (length(groups()) > 0) {
        grp <- groups()
        pal <- get_color("color")()
        col <- khroma::palette_color_discrete(pal)(grp)
      }
      function() {
        aion::plot(results(), calendar = aion::CE(), groups = grp, col = col)
        if (length(groups()) > 0) {
          graphics::legend(x = "topleft", legend = unique(grp),
                           fill = unique(col))
        }
      }
    })

    ## Render plots -----
    render_plot("plot", x = plot)

    reactive({ list(results = results(), groups = groups()) })
  })
}
