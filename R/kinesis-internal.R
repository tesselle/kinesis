# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

is_set <- function(x) {
  !is.null(x) && length(x) != 0 && !all(x == "")
}

get_value <- function(x) {
  if (!is_set(x)) return(NULL)
  x
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
  x <- c("kinesis", x)
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

info_session <- function() {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}

run_with_notification <- function(expr, what = NULL) {
  warn <- err <- NULL
  res <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        if (!inherits(e, "shiny.silent.error")) { # Ignore silent error
          err <<- conditionMessage(e)
        }
        return(NULL)
      }
    ),
    warning = function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  notif <- function(text, what = NULL, how = "default") {
    # text <- paste0(text, collapse = "\n")
    if (!is.null(what)) text <- sprintf("**%s**\n%s", what, text)
    showNotification(ui = markdown(text, hardbreaks = TRUE), type = how)
  }

  if (!is.null(err)) notif(text = err, what = what, how = "error")
  if (!is.null(warn)) notif(text = warn, what = what, how = "warning")

  res
}
