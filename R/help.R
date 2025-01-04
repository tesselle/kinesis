# HELP TEXT

#' Build an URL
#'
#' @param package A [`character`] string giving the name of a package.
#' @return A [`character`] string (URL).
#' @keywords internal
#' @noRd
url_tesselle <- function(package = NULL) {
  if (is.null(package)) return("https://www.tesselle.org/")
  sprintf("https://packages.tesselle.org/%s/", package)
}

#' Citing \R Packages
#'
#' @param x A [`character`] vector giving the name of one or more package.
#' @return Citations properly formated in HTML.
#' @keywords internal
#' @noRd
cite_package <- function(x = NULL) {
  x <- c("kinesis", x)
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x)[[1]], style = "text")
      txt <- paste0(vapply(X = bib, FUN = markdown, FUN.VALUE = character(1)))
      HTML(txt)
    }
  )
}

#' Citing a Publication
#'
#' @param author A [`character`] string giving the name of the author(s).
#' @param author An [`integer`] or a [`character`] string giving the publication
#'  year.
#' @param doi A [`character`] string giving the DOI. If not `NULL`, it will be
#'  used to create a link.
#' @param text A [`logical`] scalar. If `FALSE`, the citation will be printed
#'  in parentheses.
#' @param before,after A [`character`] string to be inserted before and after
#'  the citation, resp.
#' @param html A [`logical`] scalar. If `TRUE` (the default), the text is marked
#'  as HTML.
#' @return An author-date citation in HTML.
#' @keywords internal
#' @noRd
cite_article <- function(author, year, doi = NULL, text = TRUE,
                         before = "", after = "", html = TRUE) {
  right <- paste0(")", after)
  if (is.null(doi)) {
    link <- tags$span(year, .noWS = "outside")
  } else {
    url <- sprintf("https://doi.org/%s", doi)
    link <- tags$a(year, href = url, target = "_blank", .noWS = "outside")
  }

  if (text) {
    cite <- tags$span(before, author, "(", link, right)
  } else {
    cite <- tags$span(
      paste0("(", author, ", "), link, right,
      .noWS = c("after-begin", "before-end")
    )
  }
  if (!html) cite <- as.character(cite)
  cite
}

info_article <- function(...) {
  cite_article(..., before = icon("info-circle"), after = ".")
}

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
  withTags(
    p(
      "This app is distributed as a free and open source",
      a("R package", href = url_tesselle("kinesis"),
        target = "_blank", rel = "external", .noWS = "after"), "."
    )
  )
}

help_tesselle <- function(...) {
  withTags(
    list(
      p(
        "This app is a part of the", strong("tesselle"), "project,",
        "a collection of packages for research and teaching in archaeology.",
        "The", strong("tesselle"), "packages focus on quantitative",
        "analysis methods developed for archaeology. They can be used to",
        "explore and analyze common data types in archaeology: count data,",
        "compositional data and chronological data."
      ),
      p(
        "For more information and relevant links see:",
        a("tesselle.org", href = url_tesselle(), target = "_blank",
          rel = "external", .noWS = "after"), "."
      )
    )
  )
}
