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

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      h5(tr_("Diversity Measures")),
      downloadButton(
        outputId = ns("download"),
        label = tr_("Download results")
      )
    ), # sidebar
    navset_card_pill(
      placement = "above",
      nav_panel(
        title = tr_("Results"),
        gt::gt_output(outputId = ns("measures"))
      ),
      nav_panel(
        title = tr_("Definitions"),
        tags$dl(
          tags$dt("Heterogeneity index"),
          tags$dd("The higher the heterogeneity value, the more diverse the individuals are in the dataset."),
          tags$dl(
            tags$dt(cite_article("Shannon", 1948, "10.1002/j.1538-7305.1948.tb01338.x")),
            tags$dd("The Shannon index assumes that individuals are randomly sampled from an infinite population and that all types are represented in the sample.",
                    "It combine both richness and evenness to provide an overall measure of diversity in a given sample."),
            tags$dt(cite_article("Brillouin", 1956)),
            tags$dd("The Brillouin index describes a known collection: it does not assume random sampling in an infinite population.")
          ),

          tags$dt("Dominance index"),
          tags$dd("Dominance is a measure of whether a community is dominated by certain types (an increase in the value means a decrease in diversity)."),
          tags$dl(
            tags$dt(cite_article("Simpson", 1949, "10.1038/163688a0")),
            tags$dd("The Simpson dominance provides an indication of the probability that two randomly chosen individuals belong to the same type."),
            tags$dt(cite_article("Berger-Parker", 1970, "10.1126/science.168.3937.1345")),
            tags$dd("The Berger-Parker index expresses the proportional importance of the most abundant type. This metric is highly biased by sample size and richness.")
          ),

          tags$dt("Richness index"),
          tags$dd("Richness quantifies how many different types the dataset of interest contains, it does not take into account the abundances of the types."),
          tags$dl(
            tags$dt(cite_article("Menhinick", 1964, "10.2307/1934933")),
            tags$dd("The Menhinick index normalizes the species richness by the community size."),
            tags$dt(cite_article("Margalef", 1958)),
            tags$dd(""),
            tags$dt("Chao 1", cite_article("Chao", 1984, text = FALSE)),
            tags$dd("An estimate of total species richness."),
            tags$dt("ACE", cite_article("Chao & Lee", 1992, doi = "10.1080/01621459.1992.10475194", text = FALSE)),
            tags$dd("Abundance-based Coverage Estimator."),
            tags$dt("Squares Estimator", cite_article("Alroy", 2018, doi = "10.1111/ele.13152", text = FALSE)),
            tags$dd("")
          )
        )
      )
    )
  ) # layout_sidebar
}

# Server =======================================================================
#' Alpha Diversity Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @seealso [diversity_alpha_ui()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_alpha_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    alpha <- reactive({
      req(x())

      notify(
        {
          index <- t(apply(
            X = x(),
            MARGIN = 1,
            FUN = function(x) {
              c(
                Size = sum(x),
                Observed = tabula::observed(x),
                ## Heterogeneity
                Shanon = tabula::index_shannon(x, evenness = FALSE, unbiased = FALSE),
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
          rownames(index) <- rownames(x())
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
