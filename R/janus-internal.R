# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

is_set <- function(x) {
  !is.null(x) && x != ""
}

assert_csv <- function(x) {
  validate(need(x, message = "Import a CSV file first."))
}

url_tesselle <- function(package = NULL, campaign = TRUE) {
  mtm <- if (campaign) "?mtm_campaign=shiny" else ""
  if (is.null(package)) {
    sprintf("https://www.tesselle.org/%s", mtm)
  } else {
    sprintf("https://packages.tesselle.org/%s/%s", package, mtm)
  }
}

cite_package <- function(x = NULL, which = 1) {
  x <- c("janus", x)
  lapply(
    X = x,
    FUN = function(x, which) {
      bib <- format(utils::citation(x), style = "text")[which]
      txt <- paste0(vapply(X = bib, FUN = markdown, FUN.VALUE = character(1)))
      HTML(txt)
    },
    which = which
  )
}

cite_article <- function(author, year, doi, text = FALSE) {
  url <- sprintf("https://doi.org/%s", doi)
  link <- tags$a(year, href = url, target = "_blank", .noWS = "outside")

  if (text) {
    tags$span(author, "(", link, ")")
  } else {
    tags$span(
      paste0("(", author, ", "), link, ")",
      .noWS = c("after-begin", "before-end")
    )
  }
}

info_session <- function() {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}

#' Table Output
#'
#' A wrapper around [shiny::renderTable()].
#' @param x A reactive [`data.frame`].
#' @param ... Further arguments to be passed to [shiny::renderTable()].
#' @importFrom shiny renderTable
#' @keywords internal
#' @noRd
render_table <- function(x, ..., striped = TRUE, hover = FALSE, bordered = FALSE,
                         width = "100%", rownames = TRUE, digits = 3) {
  stopifnot(is.reactive(x))

  renderTable(
    expr = x(),
    striped = striped,
    hover = hover,
    bordered = bordered,
    width = width,
    rownames = rownames,
    colnames = TRUE,
    digits = digits,
    ...
  )
}
