
# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @family UI modules
#' @keywords internal
#' @export
module_about_ui <- function(id, cite = "janus") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "About",
    icon = icon("info-circle"),
    fluidRow(
      column(
        width = 8,
        align = "center",
        offset = 2,
        wellPanel(
          img(src = "logo.png", width = "120px", alt = "janus"),
          h4(
            paste("janus", utils::packageVersion("janus"), sep = " ")
          ),
          tags$br(),
          tags$p(
            icon("github"), "Source code:", tags$br(),
            tags$a(href = "https://github.com/tesselle/janus", rel = "external",
                   title = "GitHub", "https://github.com/tesselle/janus")
          ),
          tags$p(
            "This program is free software: you can redistribute it and/or
              modify it under the terms of the GNU General Public License as
              published by the Free Software Foundation, either version 3 of
              the License, or (at your option) any later version."
          ),
          tags$p(
            "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
              GNU General Public License for more details."
          ),
          tags$br(),
          tags$p("To cite in publications use:"),
          markdown(format(utils::citation(cite), style = "text"))
        ) # wellPanel
      ) # column
    ) # fluidRow
  ) # tabPanel
}
