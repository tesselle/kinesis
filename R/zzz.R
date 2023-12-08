.onLoad <- function(libname, pkgname) {
  path <- system.file("static", package = "janus")
  shiny::addResourcePath(prefix = "static", directoryPath = path)

  invisible()
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("static")

  invisible()
}
