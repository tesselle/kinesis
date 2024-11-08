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
