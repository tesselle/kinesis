# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param package A [`character`] vector of package names to be cited.
#' @seealso [home_server()]
#' @family page modules
#' @keywords internal
#' @export
home_ui <- function(id, package) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Welcome!",
      help_tesselle(),
      help_license(),
      tags$img(
        src = "static/tesselle.png",
        alt = "Logo of the tesselle project.",
        style = "width: 75%; margin: auto;"
      )
    ), # sidebar
    navset_card_pill(
      placement = "above",
      nav_panel(
        title = "Overview",
        h3(get_option("title")),
        markdown(get_option("description")),
        help_warranty(),
        tags$p(
          class = "logo",
          tags$a(href = "https://www.archeosciences-bordeaux.fr", rel = "external",
                 tags$img(src = "static/logo-archeosciences.svg")),
          tags$a(href = "https://www.huma-num.fr", rel = "external",
                 tags$img(src = "static/logo-humanum.svg"))
        )
      ),
      nav_panel(
        title = "How to cite",
        help_cite(package)
      ),
      nav_panel(
        title = "Bookmark",
        tags$p("You can save the state of the application and get a URL which will restore the application with that state.",
               "You can then copy the URL and save it for later, or share it with others so they can visit the application in the bookmarked state."),
        tags$p("This is not intended for long-term storage. There is no guarantee as to how long your bookmark will last."),
        if (get_option("bookmark")) {
          tags$div(class = "d-grid d-md-block", bookmarkButton())
        } else {
          tags$p("Bookmarking is currently disabled.")
        }
      )
    ) # navset_card_pill
  ) # layout_sidebar
}

#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [footer_server()]
#' @family page modules
#' @keywords internal
#' @export
footer_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tags$footer(
    style = "border-top: 1px; margin-top: 1em; width: 100%; text-align: center;",
    tags$p(
      actionLink(inputId = ns("session"), label = "Session info"),
      HTML(" &middot; "),
      tags$a(href = "https://codeberg.org/tesselle/kinesis",
             target = "_blank", rel = "external", "Source code"),
      HTML(" &middot; "),
      tags$a(href = "https://codeberg.org/tesselle/kinesis/issues",
             target = "_blank", rel = "external", "Report a bug or request")
    )
  )
}

# Server =======================================================================
#' Home Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [home_ui()]
#' @family page modules
#' @keywords internal
#' @export
home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Bookmark -----
    onBookmark(function(state) {
      saved_time <- Sys.time()

      msg <- sprintf("Last saved at %s.", saved_time)
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      message(msg)

      # state is a mutable reference object,
      # we can add arbitrary values to it.
      state$values$time <- saved_time
    })

    onRestore(function(state) {
      msg <- sprintf("Restoring from state bookmarked at %s.", state$values$time)
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      message(msg)
    })

    ## Render -----
    output$session <- renderPrint({ utils::sessionInfo() })
  })
}

#' Footer Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [footer_ui()]
#' @family page modules
#' @keywords internal
#' @export
footer_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$session, {
      showModal(
        modalDialog(
          title = "Session Info",
          info_session(),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}

#' Collect Information About the Current R Session
#'
#' @param ... Currently not used.
#' @return Text marked as HTML.
#' @keywords internal
#' @noRd
info_session <- function(...) {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}
