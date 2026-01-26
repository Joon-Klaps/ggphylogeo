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
#' @param height_branches column for branch heights (default: "height_mean")
#' @param height_hpd column for HPD heights (default: "height_median")
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
  pgeo <- build_phylogeo(
    treedata,
    lon = lon,
    lat = lat,
    height_branches = height_branches,
    height_hpd = height_hpd,
    level = level,
    most_recent_sample = most_recent_sample
  )

  # Get tip labels for highlighting selection
  tip_labels <- unique(pgeo$nodes$label[pgeo$nodes$istip])

  # Define UI
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
          min = 2, max = 15, value = 6, step = 1
        ),

        shiny::sliderInput(
          "node_size",
          "Internal node size:",
          min = 2, max = 15, value = 4, step = 1
        ),

        shiny::hr(),
        shiny::h4("Map Options"),

        shiny::numericInput(
          "map_pad",
          "Map padding (degrees):",
          value = 1, min = 0, max = 10, step = 0.5
        )
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

    # Reactive: filtered node data
    nodes_filtered <- shiny::reactive({
      nodes <- pgeo$nodes

      if (input$show_tips_only) {
        nodes <- nodes[nodes$istip, , drop = FALSE]
      }

      # Add highlight column
      if (!is.null(input$highlight_tips) && length(input$highlight_tips) > 0) {
        nodes$highlighted <- nodes$label %in% input$highlight_tips
      } else {
        nodes$highlighted <- FALSE
      }

      nodes
    })

    # Reactive: map boundaries
    map_bbox <- shiny::reactive({
      get_data_bbox(pgeo$branches, pgeo$hpd, pad = input$map_pad)
    })

    # Main plot
    output$phylogeo_plot <- plotly::renderPlotly({
      nodes <- nodes_filtered()
      bbox <- map_bbox()

      # Format endheight for tooltip
      if (inherits(nodes$endheight, "Date")) {
        nodes$date_label <- format(nodes$endheight, "%Y-%m-%d")
      } else {
        nodes$date_label <- round(nodes$endheight, 2)
      }

      # Create hover text
      nodes$hover_text <- sprintf(
        "<b>%s</b><br>Date: %s<br>Lat: %.3f<br>Lon: %.3f<br>Type: %s",
        nodes$label,
        nodes$date_label,
        nodes$lat,
        nodes$lon,
        ifelse(nodes$istip, "Tip", "Internal node")
      )

      # Start building plotly figure
      fig <- plotly::plot_ly()

      # Add world map background
      map_data <- tryCatch(
        ggplot2::map_data("world"),
        error = function(e) NULL
      )

      if (!is.null(map_data)) {
        # Group polygons
        groups <- unique(map_data$group)
        for (grp in groups[seq_len(min(200, length(groups)))]) {
          grp_data <- map_data[map_data$group == grp, ]
          fig <- plotly::add_polygons(
            fig,
            x = grp_data$long,
            y = grp_data$lat,
            fillcolor = "rgba(240, 240, 240, 0.5)",
            line = list(color = "gray70", width = 0.5),
            hoverinfo = "skip",
            showlegend = FALSE
          )
        }
      }

      # Add HPD polygons
      if (input$show_hpd && !is.null(pgeo$hpd) && nrow(pgeo$hpd) > 0) {
        hpd_groups <- unique(pgeo$hpd$group)
        for (grp in hpd_groups) {
          hpd_data <- pgeo$hpd[pgeo$hpd$group == grp, ]
          fig <- plotly::add_polygons(
            fig,
            x = hpd_data$lon,
            y = hpd_data$lat,
            fillcolor = "rgba(100, 149, 237, 0.2)",
            line = list(color = "cornflowerblue", width = 0.3),
            hoverinfo = "skip",
            showlegend = FALSE
          )
        }
      }

      # Add branches as lines
      if (input$show_branches && !is.null(pgeo$branches) && nrow(pgeo$branches) > 0) {
        branches <- pgeo$branches
        moving <- (branches$lon != branches$lonend) |
                  (branches$lat != branches$latend)
        moving <- moving & !is.na(moving)
        branches <- branches[moving, , drop = FALSE]

        if (nrow(branches) > 0) {
          for (i in seq_len(nrow(branches))) {
            fig <- plotly::add_trace(
              fig,
              type = "scatter",
              mode = "lines",
              x = c(branches$lon[i], branches$lonend[i]),
              y = c(branches$lat[i], branches$latend[i]),
              line = list(color = "gray40", width = 0.8),
              hoverinfo = "skip",
              showlegend = FALSE
            )
          }
        }
      }

      # Add nodes
      if (input$show_nodes && nrow(nodes) > 0) {
        # Separate by type and highlight status
        internal <- nodes[!nodes$istip, , drop = FALSE]
        tips_normal <- nodes[nodes$istip & !nodes$highlighted, , drop = FALSE]
        tips_highlight <- nodes[nodes$istip & nodes$highlighted, , drop = FALSE]

        # Internal nodes
        if (nrow(internal) > 0) {
          fig <- plotly::add_trace(
            fig,
            data = internal,
            type = "scatter",
            mode = "markers",
            x = ~lon,
            y = ~lat,
            text = ~hover_text,
            hoverinfo = "text",
            marker = list(
              size = input$node_size,
              color = as.numeric(internal$endheight),
              colorscale = "Viridis",
              showscale = TRUE,
              colorbar = list(title = "Date"),
              line = list(width = 0)
            ),
            name = "Internal nodes",
            showlegend = TRUE
          )
        }

        # Normal tips
        if (nrow(tips_normal) > 0) {
          fig <- plotly::add_trace(
            fig,
            data = tips_normal,
            type = "scatter",
            mode = "markers",
            x = ~lon,
            y = ~lat,
            text = ~hover_text,
            hoverinfo = "text",
            marker = list(
              size = input$tip_size,
              color = as.numeric(tips_normal$endheight),
              colorscale = "Viridis",
              showscale = FALSE,
              line = list(color = "black", width = 1)
            ),
            name = "Tips",
            showlegend = TRUE
          )
        }

        # Highlighted tips
        if (nrow(tips_highlight) > 0) {
          fig <- plotly::add_trace(
            fig,
            data = tips_highlight,
            type = "scatter",
            mode = "markers",
            x = ~lon,
            y = ~lat,
            text = ~hover_text,
            hoverinfo = "text",
            marker = list(
              size = input$tip_size * 1.5,
              color = as.numeric(tips_highlight$endheight),
              colorscale = "Viridis",
              showscale = FALSE,
              line = list(color = input$highlight_color, width = 3)
            ),
            name = "Highlighted",
            showlegend = TRUE
          )
        }
      }

      # Layout
      fig <- plotly::layout(
        fig,
        xaxis = list(
          title = "Longitude",
          range = bbox$xlim,
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          title = "Latitude",
          range = bbox$ylim
        ),
        legend = list(x = 0.02, y = 0.98),
        hovermode = "closest"
      )

      fig
    })

    # Click info output
    output$click_info <- shiny::renderPrint({
      click_data <- plotly::event_data("plotly_click")
      if (is.null(click_data)) {
        cat("Click on a node to see details")
      } else {
        nodes <- nodes_filtered()
        # Find closest node
        dists <- sqrt((nodes$lon - click_data$x)^2 + (nodes$lat - click_data$y)^2)
        idx <- which.min(dists)
        if (length(idx) > 0) {
          node <- nodes[idx, ]
          cat(sprintf("Label: %s\n", node$label))
          if (inherits(node$endheight, "Date")) {
            cat(sprintf("Date: %s\n", format(node$endheight, "%Y-%m-%d")))
          } else {
            cat(sprintf("Height: %.4f\n", node$endheight))
          }
          cat(sprintf("Latitude: %.4f\n", node$lat))
          cat(sprintf("Longitude: %.4f\n", node$lon))
          cat(sprintf("Type: %s\n", ifelse(node$istip, "Tip (leaf)", "Internal node")))
        }
      }
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
