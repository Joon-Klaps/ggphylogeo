#' Launch Interactive Phylogeography Viewer
#'
#' Creates a Shiny application with an interactive plotly visualization of
#' continuous phylogeographic data. Hover over nodes to see labels, dates,
#' and coordinates.
#'
#' @param treedata A treedata object with phylogeographic annotations from
#'   treeio::read.beast()
#' @param lon name of longitude column (default: "location2")
#' @param lat name of latitude column (default: "location1")
#' @param most_recent_sample Date, numeric year, or "YYYY-MM-DD" string for
#'   calibrating dates
#' @param stream logical; whether to use stream plotting style (default TRUE)
#' @param height_branches column for branch ages (default: "height_mean")
#' @param height_hpd column for HPD ages (default: "height_median")
#' @param level HPD level (default: "0.80")
#' @param ... Additional arguments passed to the app
#' @return A Shiny app object (run with shiny::runApp or returns when run
#'   interactively)
#' @export
#' @examples
#' \dontrun{
#' library(treeio)
#' tree <- read.beast("path/to/beast.tree")
#' run_phylogeo_app(tree, most_recent_sample = "2019-06-15")
#' }
run_phylogeo_app <- function(
  treedata,
  lon = "location2",
  lat = "location1",
  most_recent_sample = NULL,
  stream = TRUE,
  height_branches = "height_mean",
  height_hpd = "height_median",
  level = "0.80",
  ...
) {
  # Check required packages
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with: install.packages('shiny')",
         call. = FALSE)
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required. Install it with: install.packages('plotly')",
         call. = FALSE)
  }

  check_treedata(treedata)

  # Build phylogeographic data
  full_pgeo <- build_phylogeo(
    treedata,
    lon = lon,
    lat = lat,
    height_branches = height_branches,
    height_hpd = height_hpd,
    level = level,
    most_recent_sample = most_recent_sample,
  )

  # Time range for the video
  time_range <- range(full_pgeo$nodes$age, na.rm = TRUE)

  # Get tip labels for highlighting selection
  tip_labels <- unique(full_pgeo$nodes$label[full_pgeo$nodes$istip])

  # Define UI
  window_max <- as.numeric(diff(time_range))
  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive Phylogeography Viewer"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Display Options"),

        shiny::checkboxInput(
          "show_hpd",
          "Show HPD polygons",
          value = TRUE
        ),

        shiny::checkboxInput(
          "show_branches",
          "Show branches",
          value = TRUE
        ),

        shiny::checkboxInput(
          "show_nodes",
          "Show nodes",
          value = TRUE
        ),

        shiny::checkboxInput(
          "show_tips_only",
          "Show tips only (hide internal nodes)",
          value = FALSE
        ),

        shiny::hr(),
        shiny::h4("Highlight Tips"),

        shiny::selectizeInput(
          "highlight_tips",
          "Select tips to highlight:",
          choices = tip_labels,
          multiple = TRUE,
          options = list(placeholder = "Type to search...")
        ),


        # Color picker with fallback if colourpicker not installed
        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::colourInput(
            "highlight_color",
            "Highlight color:",
            value = "red"
          )
        } else {
          shiny::selectInput(
            "highlight_color",
            "Highlight color:",
            choices = c("red", "blue", "green", "orange", "purple", "cyan", "magenta"),
            selected = "red"
          )
        },

        shiny::hr(),
        shiny::h4("Node Sizes"),

        shiny::sliderInput(
          "tip_size",
          "Tip size:",
          min = 0.5, max = 10, value = 3, step = 0.5
        ),

        shiny::sliderInput(
          "node_size",
          "Internal node size:",
          min = 0.5, max = 5, value = 2.5, step = 0.5
        ),

        shiny::hr(),
        shiny::h4("Appearance"),

      ),

      shiny::mainPanel(
        width = 9,
        plotly::plotlyOutput("phylogeo_plot", height = "700px"),

        shiny::hr(),
        shiny::h4("Selected Node Info"),
        shiny::verbatimTextOutput("click_info")
      )
    )
  )

  # Define server
  server <- function(input, output, session) {

    output$phylogeo_plot <- plotly::renderPlotly({
      # Show all nodes/branches (no time filtering)
      nodes_df <- full_pgeo$nodes
      branches_df <- full_pgeo$branches

      # Optionally hide internal nodes if requested
      if (isTRUE(input$show_tips_only)) {
        nodes_df <- nodes_df[nodes_df$istip, , drop = FALSE]
      }

      active_nodes <- nodes_df
      active_branches <- branches_df

      p <- suppressWarnings({
        p0 <- ggplot2::ggplot() +
          # Background world map
          ggplot2::borders("world", colour = "gray90", fill = "gray98")

        # Branches layer
        if (input$show_branches) {
          p0 <- p0 + geom_phylo_branches(
              data = active_branches,
              curvature = 0.2,
              ncp = 50,
              linewidth = 0.8
          )
        }

        # HPD polygons
        if (input$show_hpd) {
          p0 <- p0 + geom_phylo_hpd(data = full_pgeo$hpd)
        }

        # Nodes layer with tooltip text
        if (input$show_nodes) {
          nodes <- active_nodes
          nodes$tooltip <- paste0(
            "Label: ", nodes$label, "\n",
            "Date: ", as.character(nodes$age), "\n",
            "Lon: ", round(nodes$lon, 4), ", Lat: ", round(nodes$lat, 4)
          )

          # Separate tips and internal nodes
          tips_df <- nodes[nodes$istip, , drop = FALSE]
          internal_df <- nodes[!nodes$istip, , drop = FALSE]

          # Tips highlighting
          if (!is.null(input$highlight_tips) && length(input$highlight_tips) > 0) {
            tips_df$highlighted <- tips_df$label %in% input$highlight_tips
          } else {
            tips_df$highlighted <- FALSE
          }

          # Internal nodes
          if (nrow(internal_df) > 0) {
            p0 <- p0 + ggplot2::geom_point(
              data = internal_df,
              ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip),
              shape = 21,
              size = input$node_size,
              stroke = input$node_size * 0.3,
              colour = "black",
              inherit.aes = FALSE
            )
          }

          # Tip nodes
          if (nrow(tips_df) > 0) {
            normal_tips <- tips_df[!tips_df$highlighted, , drop = FALSE]
            highlighted_tips <- tips_df[tips_df$highlighted, , drop = FALSE]

            if (nrow(normal_tips) > 0) {
              p0 <- p0 + ggplot2::geom_point(
                data = normal_tips,
                ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip),
                shape = 21,
                size = input$tip_size,
                stroke = input$tip_size * 0.3,
                colour = "black",
                inherit.aes = FALSE
              )
            }

            if (nrow(highlighted_tips) > 0) {
              p0 <- p0 + ggplot2::geom_point(
                data = highlighted_tips,
                ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip),
                shape = 21,
                size = input$tip_size * 1.3,
                stroke = input$tip_size * 0.3,
                colour = input$highlight_color,
                inherit.aes = FALSE
              )
            }
          }
        }

        # Aesthetics and coordinate system
        p0 <- p0 +
          ggplot2::scale_fill_viridis_c(guide = "none") +
          ggplot2::scale_colour_viridis_c(name = "Date") +
          ggplot2::scale_linewidth(range = c(0.2, 1.5), guide = "none") +
          ggplot2::scale_alpha(range = c(0.05, 1), guide = "none") +
          ggplot2::coord_quickmap() +
          ggplot2::theme_void() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#f0f5f7", color = NA))

        p0
      })

      # Convert to plotly with hover text
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(showlegend = FALSE)
    })

    # Placeholder for click info (can be extended)
    output$click_info <- shiny::renderPrint({
      "Click on a point to see details (not implemented)."
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}


#' Convert ggphylogeo plot to interactive plotly
#'
#' Takes a ggplot2 object created by autoplot.treedata and converts it to
#' an interactive plotly visualization.
#'
#' @param p A ggplot2 object from autoplot.treedata
#' @param tooltip Which aesthetics to include in tooltip (default: all)
#' @return A plotly object
#' @export
#' @examples
#' \dontrun{
#' library(treeio)
#' tree <- read.beast("path/to/beast.tree")
#' p <- autoplot(tree, most_recent_sample = 2019)
#' ggplotly_phylogeo(p)
#' }
ggplotly_phylogeo <- function(p, tooltip = "all") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required. Install it with: install.packages('plotly')",
         call. = FALSE)
  }

  plotly::ggplotly(p, tooltip = tooltip)
}
