# UI ===========================================================================
#' Compositional Data Hierarchical Clustering UI
#'
#' @seealso [coda_hclust_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hclust_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("HCLUST"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Hierarchical Clustering"),
        selectInput(
          inputId = ns("dist"),
          label = tr_("Distance method"),
          choices = c(Aitchison = "euclidean")
        ),
        selectInput(
          inputId = ns("clust"),
          label = tr_("Clustering linkage"),
          choices = c("ward.D", "ward.D2", "single", "complete",
                      "average", "mcquitty", "median", "centroid"),
          selected = "ward.D2"
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        numericInput(
          inputId = ns("cut"),
          label = tr_("Desired number of clusters"),
          value = 1, min = 1, max = NA, step = 1
        ),
        downloadButton(
          outputId = ns("download_dist"),
          label = tr_("Download distances")
        ),
        downloadButton(
          outputId = ns("download_clust"),
          label = tr_("Download clusters")
        )
      ), # sidebar
      output_plot(
        id = ns("plot_dendro"),
        tools = list(
          select_color(id = ns("col_dendro"), type = "qualitative")
        ),
        title = tr_("Dendrogram")
      ),
      border_radius = FALSE,
      fillable = TRUE
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Compositional Data Hierarchical Clustering Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return A reactive [`hclust`] object.
#' @seealso [coda_hclust_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hclust_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("HCLUST"))

    ## Compute cluster -----
    compute_hclust <- ExtendedTask$new(
      function(x, method, clust) {
        promises::future_promise({
          clr <- nexus::transform_clr(x)
          d <- nexus::dist(clr, method = method)
          h <- stats::hclust(d, method = clust)
          h$dist <- d
          if (nexus::is_grouped(x)) h$groups <- nexus::group_names(x)
          h
        })
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_hclust$invoke(x(), input$dist, input$clust)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_hclust$result(), title = tr_("Hierarchical Clustering"))
    })
    distances <- reactive({
      results()$dist
    })
    groups <- reactive({
      req(input$cut)
      stats::cutree(results(), k = input$cut)
    })

    ## Dendrogram -----
    plot_dendro <- reactive({
      req(results(), input$cut)
      col <- get_color("col_dendro")()
      function() {
        xlab <- sprintf(tr_("Aitchison distance, %s linkage"), results()$method)
        plot(results(), hang = -1, main = NULL, sub = "",
             xlab = xlab, ylab = "Height", las = 1)

        if (input$cut > 1) {
          stats::rect.hclust(results(), k = input$cut)
        }

        i <- results()$order
        g <- results()$groups
        if (!is.null(g)) {
          col <- khroma::palette_color_discrete(col)(g)
          graphics::points(
            x = seq_along(i),
            y = rep(0, length(i)),
            col = col[i],
            pch = 16
          )

          arg <- list(x = "topright", pch = 16, bty = "n")
          leg <- stats::aggregate(
            data.frame(col = col),
            by = list(legend = g),
            FUN = unique
          )
          leg <- as.list(leg)
          leg <- utils::modifyList(leg, arg)
          do.call(graphics::legend, args = leg)
        }
      }
    })

    ## Render plot -----
    render_plot("plot_dendro", x = plot_dendro)

    ## Download -----
    output$download_dist <- export_table(distances, name = "distances")
    output$download_clust <- export_table(groups, name = "clusters")
  })
}
