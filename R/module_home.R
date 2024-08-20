# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [home_server()]
#' @family page modules
#' @keywords internal
#' @export
home_ui <- function(id, name = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = "Home",
    layout_sidebar(
      sidebar = sidebar(
        width = "20%",
        h5("Workflow"),
      ), # sidebar
      navset_card_pill(
        placement = "above",
        nav_panel(
          title = "How to use",
          h3("Overview"),
          tags$p(
            "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY."
          ),
          h3("How to cite"),
          tags$p(
            "If you use this application in your research, you must report
              and cite it properly to ensure transparency of your results.
              Moreover, authors and maintainers of this project are more likely to
              continue their work if they see that it's being used and valued
              by the research community."
          ),
          tags$p("To cite in your publications, please use:"),
          cite_package(name),
          tags$p(
            class = "logo",
            tags$a(href = "https://www.archeosciences-bordeaux.fr", rel = "external",
                   tags$img(src = "static/logo-archeosciences.svg")),
            tags$a(href = "https://www.huma-num.fr", rel = "external",
                   tags$img(src = "static/logo-humanum.svg"))
          )
        ),
        nav_panel(
          title = "About",
          h3("What is", tags$i("tesselle", .noWS = "after"), "?"),
          tags$img(src="static/tesselle.png", alt="tesselle logo",
                   style="float:right;width:150px;margin:0 10px;"),
          tags$p(
            "This app is a part of the", tags$strong("tesselle"), "project,",
            "a collection of packages for research and teaching in archaeology.
              The", tags$strong("tesselle"), "packages focus on quantitative
              analysis methods developed for archaeology. They can be used to
              explore and analyze common data types in archaeology: count data,
              compositional data and chronological data."
          ),
          tags$p(
            "For more information and relevant links see:",
            tags$a("tesselle.org", href = url_tesselle(),
                   target = "_blank", rel = "external", .noWS = "after"), "."
          ),
          # h3("Who is", tags$i("tesselle"), "for?"),
          h3("License"),
          tags$p(
            "This app is distributed as a free and open source",
            tags$a("R package", href = url_tesselle("kinesis"),
                   target = "_blank", rel = "external", .noWS = "after"), "."
          ),
          tags$p(
            "You can redistribute it and/or modify it under the terms of the
              GNU General Public License as published by the Free Software
              Foundation, either version 3 of the License, or (at your option)
              any later version."
          )
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
