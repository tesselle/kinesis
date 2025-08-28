# UI ===========================================================================
#' Diversity UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [diversity_server()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Indices"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Count data"),
        variables_ui(ns("count"), label = tr_("Types"))
      ), # sidebar
      navset_card_pill(
        bertin_ui(ns("plot"), title = tr_("Plot")),
        diversity_alpha_ui(ns("alpha")),
        diversity_beta_ui(ns("beta")),
        occurrence_ui(ns("occurrence"))
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Alpha Diversity Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @return A reactive [`data.frame`] (see [tabula::diversity()]).
#' @seealso [diversity_ui()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get count data -----
    counts <- variables_server("count", x = x, detect = is.numeric)

    ## Diversity -----
    bertin_server("plot", x = counts)
    alpha <- diversity_alpha_server("alpha", x = counts)
    diversity_beta_server("beta", x = counts, y = alpha, z = x)
    occ <- occurrence_server("occurrence", x = counts)

    counts
  })
}

