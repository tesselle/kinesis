# UI ===========================================================================
#' Compositional Data Summary UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [coda_summary_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      downloadButton(
        outputId = ns("download"),
        label = "Download tables"
      ),
      h5("Univariate statistics"),
      selectInput(
        inputId = ns("hist_select"),
        label = "Select a part",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      output_plot(
        id = ns("hist"),
        title = "Histogram",
        note = info_article(author = "Filzmoser et al.", year = "2009",
                            doi = "10.1016/j.scitotenv.2009.08.008")
      )
    ),
    h5("Multivariate statistics"),
    navset_tab(
      nav_panel(
        title = "Location",
        gt::gt_output(outputId = ns("mean")),
        gt::gt_output(outputId = ns("quantile"))
      ),
      nav_panel(
        title = "Covariance",
        gt::gt_output(outputId = ns("covariance"))
      ),
      nav_panel(
        title = "PIP",
        gt::gt_output(outputId = ns("pip"))
      ),
      nav_panel(
        title = "Variation matrix",
        layout_columns(
          col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8)),
          output_plot(id = ns("heatmap")),
          output_plot(id = ns("dendrogram"))
        ),
        gt::gt_output(outputId = ns("variation"))
      )
    ) # navset_card_underline
  ) # layout_sidebar
}

# Server =======================================================================
#' Compositional Data Summary Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_summary_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Location -----
    data_loc <- reactive({
      req(x())
      if (nexus::is_grouped(x())) {
        nexus::aggregate(x(), FUN = nexus::mean, na.rm = FALSE)
      } else {
        m <- nexus::mean(x(), na.rm = FALSE)
        matrix(m, nrow = 1, dimnames = list("center", names(m)))
      }
    })

    ## Spread -----
    # TODO

    ## Percentiles -----
    data_quant <- reactive({
      req(x())
      nexus::quantile(x(), probs = seq(0, 1, 0.25))
    })

    ## Histogram -----
    bindEvent(
      observe({
        choices <- colnames(x())
        freezeReactiveValue(input, "hist_select")
        updateSelectInput(inputId = "hist_select", choices = choices)
      }),
      x()
    )
    plot_hist <- reactive({
      req(x())
      function() nexus::hist(x(), select = input$hist_select)
    })

    ## CLR covariance -----
    data_cov <- reactive({
      req(x())
      nexus::covariance(x(), center = TRUE)
    })

    ## PIP -----
    data_pip <- reactive({
      req(x())
      nexus::pip(x())
    })

    ## Variation matrix -----
    data_var <- reactive({
      req(x())
      nexus::variation(x())
    })

    ## Clustering -----
    plot_clust <- reactive({
      d <- stats::as.dist(data_var())
      h <- stats::hclust(d, method = "ward.D2")

      function() {
        plot(h, hang = -1, main = "", sub = "",
             xlab = "", ylab = "Total variation", las = 1)
      }
    })

    ## Heatmap -----
    plot_heatmap <- reactive({
      req(data_var())
      function() tabula::plot_heatmap(data_var(), fixed_ratio = TRUE)
    })

    Aitchison1986 <- info_article(
      author = "Aitchison", year = "1986", html = FALSE
    )
    Egozcue2023 <- info_article(
      author = "Egozcue & Pawlowsky-Glahn", year = "2023",
      doi = "10.57645/20.8080.02.7", html = FALSE
    )

    ## Render table -----
    output$mean <- gt::render_gt({
      req(x())
      data_loc() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = nexus::is_grouped(x())) |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        gt::tab_header(title = "Compositional Mean")
    })
    output$quantile <-  gt::render_gt({
      data_quant() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        gt::tab_header(title = "Percentile Table")
    })
    output$covariance <- gt::render_gt({
      covar <- data_cov()
      covar[lower.tri(covar, diag = FALSE)] <- NA

      covar |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_header(title = "Centered Log-Ratio Covariance") |>
        gt::tab_source_note(source_note = gt::html(Aitchison1986))
    })
    output$pip <- gt::render_gt({
      prop <- data_pip()
      prop[lower.tri(prop, diag = TRUE)] <- NA

      prop |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_style_body(
          fn = function(x) x >= 0.75,
          style = gt::cell_fill(color = "#FFAABB")
        ) |>
        gt::tab_header(title = "Proportionality Index of Parts") |>
        gt::tab_source_note(source_note = gt::html(Egozcue2023))
    })
    output$variation <- gt::render_gt({
      varia <- data_var()
      varia[lower.tri(varia, diag = TRUE)] <- NA

      varia |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_header(title = "Variation Matrix") |>
        gt::tab_source_note(source_note = gt::html(Aitchison1986))
    })

    ## Render plot -----
    render_plot("heatmap", x = plot_heatmap)
    render_plot("dendrogram", x = plot_clust)
    render_plot("hist", x = plot_hist)

    ## Download -----
    output$download <- export_multiple(
      location = data_loc,
      quantiles = data_quant,
      covariance = data_cov,
      variation = data_var,
      name = "coda_summary"
    )
  })
}
