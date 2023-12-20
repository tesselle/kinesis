# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param package A [`character`] string specifying the app name.
#' @seealso [module_home_server()]
#' @family UI modules
#' @keywords internal
#' @export
module_home_ui <- function(id, name = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Home",
    sidebarLayout(
      sidebarPanel(
        h5("Workflow"),
      ), # sidebarPanel
      mainPanel(
        tabsetPanel(
          tabPanel(
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
          tabPanel(
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
              tags$a("R package", href = url_tesselle("janus"),
                     target = "_blank", rel = "external", .noWS = "after"), "."
            ),
            tags$p(
              "You can redistribute it and/or modify it under the terms of the
              GNU General Public License as published by the Free Software
              Foundation, either version 3 of the License, or (at your option)
              any later version."
            )
          ),
          tabPanel(
            title = "Save",
            h4("Bookmarking"),
            tags$p(textOutput(outputId = ns("bookmarking"))),
            bookmarkButton(),
            tags$p(textOutput(outputId = ns("last_saved")))
          )
        )
      ) # mainPanel
    ) # sidebarLayout
  ) # tabPanel
}

# Server =======================================================================
#' Home Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @seealso [module_home_ui()]
#' @family server modules
#' @keywords internal
#' @export
module_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Render -----
    output$session <- renderPrint({ utils::sessionInfo() })

    ## Bookmark -----
    saved <- reactiveVal()

    onBookmark(function(state) {
      saved(Sys.time())

      msg <- sprintf("Last saved at %s.", saved())
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      cat(msg, "\n")

      # state is a mutable reference object,
      # we can add arbitrary values to it.
      state$values$time <- saved()
    })

    onRestore(function(state) {
      saved(state$values$time)

      msg <- sprintf("Restoring from state bookmarked at %s.", state$values$time)
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      cat(msg, sep = "\n")
    })

    output$bookmarking <- renderText({
      book <- janus::get_option("bookmark") != "disable"
      sprintf("Bookmarking is %s.", ifelse(book, "enabled", "disabled"))
    })
    output$last_saved <- renderText({
      req(saved())
      paste("Last saved at", saved())
    })
  })
}
