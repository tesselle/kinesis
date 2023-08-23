# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

cite_markdown <- function(x) {
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x), style = "text")
      markdown(bib)
    }
  )
}
