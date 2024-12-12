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

  nav_panel(
    title = "Home",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        h5("Workflow"),
        markdown(get_option("workflow")),
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
          title = "About",
          help_tesselle(),
          help_license()
        ),
        nav_panel(
          title = "Metadata",
          textInput(inputId = ns("user"), label = "User name"),
          textInput(inputId = ns("project"), label = "Project ID")
        )
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

#' Header UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [header_server()]
#' @family page modules
#' @keywords internal
#' @export
header_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  list(
    uiOutput(outputId = ns("alert_config"))
  )
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
      tags$a(href = "https://github.com/tesselle/kinesis",
             target = "_blank", rel = "external", "Source code"),
      HTML(" &middot; "),
      tags$a(href = "https://github.com/tesselle/kinesis/issues",
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
    ## User data -----
    session$userData$user_name <- reactive({ input$user })
    session$userData$project_name <- reactive({ input$project })

    ## Render -----
    output$session <- renderPrint({ utils::sessionInfo() })
  })
}

#' Header Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [header_ui()]
#' @family page modules
#' @keywords internal
#' @export
header_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Display alert -----
    output$alert_config <- renderUI({
      if (get_option("production")) return(NULL)
      div(
        class = "alert alert-warning",
        role = "alert",
        "This application is under development, so you shouldn't use it for anything serious!"
      )
    })
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
