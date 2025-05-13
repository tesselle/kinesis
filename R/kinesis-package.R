#' @details
#'  \tabular{ll}{
#'   **Package:** \tab kinesis \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.1.0 \cr
#'   **License:** \tab GPL-3 \cr
#'   **Zenodo:** \tab \doi{10.5281/zenodo.14645671} \cr
#'  }
#'
#' @section Package options:
#'  \pkg{kinesis} uses the following [options()] to configure behavior:
#'  * `kinesis.workers`: an [`integer`] specifying the number of \R sessions
#'  to be used for asynchronous (parallel) computing. Defaults to 1.
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order)
#'
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'   Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#'  }
#'
#'  **Package maintainer**
#'
#'  Nicolas Frerebeau\cr
#'  \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#'  Archéosciences Bordeaux (UMR 6034)\cr
#'  Maison de l'Archéologie\cr
#'  Université Bordeaux Montaigne\cr
#'  F-33607 Pessac cedex\cr
#'  France
#' @name kinesis-package
#' @aliases kinesis-package kinesis
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import bslib
#' @importFrom future plan multisession
NULL

# Suppress R CMD check note "All declared Imports should be used."
unused <- function() {
  folio::boves
  datasets::iris
}
