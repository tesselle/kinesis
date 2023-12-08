# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

is_set <- function(x) {
  !is.null(x) && x != ""
}

url_tesselle <- function(package = NULL, campaign = TRUE) {
  mtm <- if (campaign) "?mtm_campaign=shiny" else ""
  if (is.null(package)) {
    sprintf("https://www.tesselle.org/%s", mtm)
  } else {
    sprintf("https://packages.tesselle.org/%s/%s", package, mtm)
  }
}

cite_markdown <- function(x = NULL) {
  x <- c("janus", x)
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x), style = "text")
      markdown(bib)
    }
  )
}

info_markdown <- function() {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}
