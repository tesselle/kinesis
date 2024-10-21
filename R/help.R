# HELP TEXT

help_warranty <- function(...) {
  tags$p(
    "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY."
  )
}

help_cite <- function(package) {
  list(
    tags$p(
      "If you use this application in your research, you must report",
      "and cite it properly to ensure transparency of your results.",
      "Moreover, authors and maintainers of this project are more likely",
      "to continue their work if they see that it's being used and valued",
      "by the research community."
    ),
    tags$p("To cite in your publications, please use:"),
    cite_package(package)
  )
}

help_license <- function(...) {
  list(
    h3("License"),
    tags$p(
      "This app is distributed as a free and open source",
      tags$a("R package", href = url_tesselle("kinesis"),
             target = "_blank", rel = "external", .noWS = "after"), "."
    ),
    tags$p(
      "You can redistribute it and/or modify it under the terms",
      "of the GNU General Public License as published",
      "by the Free Software Foundation, either version 3 of the License,",
      "or (at your option) any later version."
    )
  )
}

help_tesselle <- function(...) {
  list(
    h3("What is", tags$em("tesselle", .noWS = "after"), "?"),
    tags$p(
      "This app is a part of the", tags$strong("tesselle"), "project,",
      "a collection of packages for research and teaching in archaeology.",
      "The", tags$strong("tesselle"), "packages focus on quantitative",
      "analysis methods developed for archaeology. They can be used to",
      "explore and analyze common data types in archaeology: count data,",
      "compositional data and chronological data."
    ),
    tags$p(
      "For more information and relevant links see:",
      tags$a("tesselle.org", href = url_tesselle(),
             target = "_blank", rel = "external", .noWS = "after"), "."
    )
    # h3("Who is", tags$em("tesselle"), "for?")
  )
}

help_workflow <- function(...) {
  def <- .help_workflow_panel()
  tab <- switch (
    get_option("name"),
    source = c("Data", "Composition", "Transform", "Statistics", "Plot", "Analysis"),
    ternary = c("Data", "Plot"),
    "Data"
  )

  txt <- def[tab]
  # dl <- mapply(
  #   dt = names(txt),
  #   dd = txt,
  #   FUN = function(dt, dd) { list(tags$dt(dt), tags$dd(dd)) }
  # )
  # tags$dl(ol)
  ol <- mapply(
    dt = names(txt),
    dd = txt,
    FUN = function(dt, dd) { list(tags$li(tags$strong(dt, .noWS = "after"), ".", dd)) }
  )
  tags$ol(ol)
}

.help_workflow_panel <- function(...) {
  list(
    Data = "Import your data and perform basic data cleansing and preparation steps.",
    Composition = "Coerce your data to compositions and define (reference) groups.",
    Transform = "Compute log-ratio transformations of compositional data.",
    Statistics = "Data summary and descriptive statistics.",
    Plot = "Visualize your data.",
    Analysis = "Perform multivariate data analysis."
  )
}
