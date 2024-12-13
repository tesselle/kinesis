#' @details
#'  \tabular{ll}{
#'   **Package:** \tab kinesis \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.0.0.9000 \cr
#'   **License:** \tab GPL-3 \cr
#'  }
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
NULL

#' @import nexus
#' @importFrom arkhe %||% assign_rownames clean_whitespace detect
#'  discard get_columns keep_columns remove_constant remove_NA remove_zero
#'  replace_NA replace_zero seek_rows seek_columns sparsity
#' @importFrom dimensio ca export get_eigenvalues pca screeplot summary
#'  viz_confidence viz_hull viz_rows viz_tolerance viz_variables
#'  viz_contributions
#' @importFrom isopleuros coordinates_ternary ternary_confidence ternary_density
#' ternary_grid ternary_hull ternary_points ternary_plot ternary_tolerance
#' @importFrom kairos as_seriation assess permute
#' @importFrom khroma color info palette_color_discrete palette_color_continuous
#'  palette_shape
#' @importFrom tabula plot_heatmap plot_ford
NULL
