.onLoad <- function(libname, pkgname) {
  op <- options()
  op.janus <- list(
    janus.config = "production",
    janus.verbose = FALSE
  )
  toset <- !(names(op.janus) %in% names(op))
  if(any(toset)) options(op.janus[toset])

  invisible()
}
