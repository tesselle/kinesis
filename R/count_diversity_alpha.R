# UI ===========================================================================
#' Alpha Diversity UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [diversity_alpha_server()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_alpha_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = HTML(tr_("&#945; Diversity")),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        h5(tr_("Diversity Measures")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      card(
        gt::gt_output(outputId = ns("measures"))
      )
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
#' @seealso [diversity_alpha_ui()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_alpha_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get count data -----
    counts <- reactive({
      req(x())
      arkhe::keep_columns(x(), f = is.numeric, verbose = verbose)
    })

    alpha <- reactive({
      req(counts())

      ## Compute index -----
      notify(
        {
          index <- t(apply(
            X = counts(),
            MARGIN = 1,
            FUN = function(x) {
              c(
                Size = sum(x),
                Observed = tabula::observed(x),
                ## Heterogeneity
                Shannon = tabula::index_shannon(x, evenness = FALSE, unbiased = FALSE),
                Brillouin = tabula::index_brillouin(x, evenness = FALSE),
                ## Dominance
                Simpson = tabula::index_simpson(x, evenness = FALSE, unbiased = FALSE),
                Berger = tabula::index_berger(x),
                ## Richness
                Menhinick = tabula::index_menhinick(x),
                Margalef = tabula::index_margalef(x),
                Chao1 = tabula::index_chao1(x, unbiased = FALSE),
                ACE = tabula::index_ace(x),
                Squares = tabula::index_squares(x)
              )
            }
          ))
          rownames(index) <- rownames(counts())
          as.data.frame(index)
        },
        title = "Alpha diversity"
      )
    })

    ## Render table -----
    output$measures <- gt::render_gt({
      alpha() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::tab_spanner(
          label = tr_("Heterogeneity"),
          columns = c(3, 4) + 1,
          id = "heterogeneity"
        ) |>
        gt::tab_spanner(
          label = tr_("Dominance"),
          columns = c(5, 6) + 1,
          id = "dominance"
        ) |>
        gt::tab_spanner(
          label = tr_("Richness"),
          columns = c(7, 8, 9, 10, 11) + 1,
          id = "richness"
        ) |>
        gt::tab_header(title = tr_("Diversity Measures")) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing()
    })

    ## Download -----
    output$download <- export_table(alpha, "alpha")

    alpha
  })
}
