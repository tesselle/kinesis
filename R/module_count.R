# Server =======================================================================
#' Count Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [data_server()]).
#' @return A reactive [`data.frame`].
#' @family count data modules
#' @keywords internal
#' @export
count_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    count <- reactive({
      req(x())
      out <- arkhe::keep_cols(x(), f = is.numeric, verbose = get_option("verbose"))
      if (any(dim(out) == 0)) return(NULL)
      out
    })

    count
  })
}
