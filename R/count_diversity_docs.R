# UI ===========================================================================
#' Diversity Definitions UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @family count data modules
#' @keywords internal
#' @export
diversity_docs_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Definitions"),
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      card(
        card_title(tr_("Alpha Diversity")),
        tags$dl(
          tags$dt("Heterogeneity index"),
          tags$dd("The higher the heterogeneity value, the more diverse the individuals are in the dataset."),
          tags$dl(
            tags$dt(cite_article("Shannon", 1948, "10.1002/j.1538-7305.1948.tb01338.x")),
            tags$dd(
              "The Shannon index assumes that individuals are randomly sampled from an infinite population and that all types are represented in the sample.",
              "It combine both richness and evenness to provide an overall measure of diversity in a given sample."
            ),
            tags$dt(cite_article("Brillouin", 1956)),
            tags$dd("The Brillouin index describes a known collection: it does not assume random sampling in an infinite population.")
          ),

          tags$dt("Dominance index"),
          tags$dd("Dominance is a measure of whether a community is dominated by certain types (an increase in the value means a decrease in diversity)."),
          tags$dl(
            tags$dt(cite_article("Simpson", 1949, "10.1038/163688a0")),
            tags$dd("The Simpson dominance provides an indication of the probability that two randomly chosen individuals belong to the same type."),
            tags$dt(cite_article("Berger-Parker", 1970, "10.1126/science.168.3937.1345")),
            tags$dd(
              "The Berger-Parker index expresses the proportional importance of the most abundant type.
            This metric is highly biased by sample size and richness."
            )
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
      ),
      card(
        card_title(tr_("Beta Diversity")),
        card_body()
      )
    ) # layout_columns
  ) # nav_panel
}
