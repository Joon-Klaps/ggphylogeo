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
  req_pkgs <- c("shiny", "plotly", "maps", "ggplot2")
  for (pkg in req_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required. Install with install.packages('", pkg, "')"), call. = FALSE)
    }
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

  # 1. Get unique coordinates of our nodes
  node_locs <- unique(full_pgeo$nodes[, c("lon", "lat")])

  # 2. Identify which countries these nodes fall into
  # map.where returns "Country" or "Country:Region". We split to get just "Country".
  # We use the 'world' database from the maps package.
  countries_raw <- maps::map.where(database = "world", x = node_locs$lon, y = node_locs$lat)

  # Clean up country names (remove NAs and sub-regions like ":Alaska")
  present_countries <- unique(sapply(strsplit(countries_raw, ":"), `[`, 1))
  present_countries <- present_countries[!is.na(present_countries)]

  # Load world cities for the background layer
  data(world.cities, package = "maps")

  # Get tip labels for highlighting selection
  tip_labels <- unique(full_pgeo$nodes$label[full_pgeo$nodes$istip])

  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive Phylogeography Viewer"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Display Options"),

        shiny::checkboxInput("show_hpd", "Show HPD polygons", value = TRUE),
        shiny::checkboxInput("show_branches", "Show branches", value = TRUE),
        shiny::checkboxInput("show_nodes", "Show nodes", value = TRUE),
        shiny::checkboxInput("show_tips_only", "Show tips only", value = FALSE),

        shiny::hr(),
        shiny::h4("Context Map"),
        shiny::checkboxInput("show_bg_cities", "Show Major Cities", value = TRUE),
        shiny::numericInput("min_pop", "Min Population:", value = 1000000, min = 10000, step = 10000),

        shiny::hr(),
        shiny::h4("Highlight Tips"),
        shiny::selectizeInput(
          "highlight_tips", "Select tips:", choices = tip_labels, multiple = TRUE
        ),

        if (requireNamespace("colourpicker", quietly = TRUE)) {
          colourpicker::colourInput("highlight_color", "Highlight color:", value = "red")
        } else {
          shiny::selectInput("highlight_color", "Highlight color:", choices = c("red", "blue"), selected = "red")
        },

        shiny::hr(),
        shiny::h4("Node Sizes"),
        shiny::sliderInput("tip_size", "Tip size:", min = 0.5, max = 10, value = 3, step = 0.5),
        shiny::sliderInput("node_size", "Internal node size:", min = 0.5, max = 5, value = 2.5, step = 0.5),

        shiny::hr(),
        shiny::h4("Click History"),
        shiny::div(
            style = "height: 200px; overflow-y: scroll; background-color: #f9f9f9; border: 1px solid #ddd; padding: 10px;",
            shiny::uiOutput("click_history_list")
        ),
        shiny::actionButton("clear_history", "Clear History", class = "btn-xs")
      ),

      shiny::mainPanel(
        width = 9,
        plotly::plotlyOutput("phylogeo_plot", height = "700px")
      )
    )
  )

  # Define server
  server <- function(input, output, session) {

    vals <- shiny::reactiveValues(history = list())

    # We only want cities in the countries where we have data
    background_cities <- shiny::reactive({
      req(input$min_pop)

      # Filter global list:
      # 1. Must be in our identified countries
      # 2. Must meet population threshold
      cities <- world.cities[
        world.cities$country.etc %in% present_countries &
        world.cities$pop > input$min_pop,
      ]

      # Create a tooltip for the city dots
      cities$tooltip <- paste0(cities$name, "\nPop: ", format(cities$pop, big.mark=","))
      return(cities)
    })

    output$phylogeo_plot <- plotly::renderPlotly({
      # Show all nodes/branches (no time filtering)
      nodes_df <- full_pgeo$nodes
      branches_df <- full_pgeo$branches

      # Optionally hide internal nodes if requested
      if (isTRUE(input$show_tips_only)) {
        nodes_df <- nodes_df[nodes_df$istip, , drop = FALSE]
      }

      p <- suppressWarnings({
        p0 <- ggplot2::ggplot() +
          ggplot2::borders("world", colour = "gray90", fill = "gray98")

        # Add Background Cities Layer
        # This goes BEFORE the tree branches so it stays in the background
        if (isTRUE(input$show_bg_cities)) {
          bg_data <- background_cities()
          if (nrow(bg_data) > 0) {
            p0 <- p0 + ggplot2::geom_point(
              data = bg_data,
              ggplot2::aes(x = long, y = lat, text = tooltip),
              color = "grey70",
              size = 2,
              alpha = 0.6,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_text(
              data = bg_data,
              ggplot2::aes(x = long, y = lat, label = name),
              color = "grey50",
              size = 5,
              vjust = -1,
              inherit.aes = FALSE
            )
          }
        }

        if (isTRUE(input$show_branches)) {
          p0 <- p0 + geom_phylo_branches(data = branches_df, linewidth = 0.8)
        }

        if (isTRUE(input$show_hpd)) {
          p0 <- p0 + geom_phylo_hpd(data = full_pgeo$hpd)
        }

        if (isTRUE(input$show_nodes)) {
          nodes <- nodes_df
          nodes$key_id <- nodes$label

          nodes$tooltip <- paste0(
            "ID: ", nodes$label, "\n",
            "Date: ", as.character(nodes$age), "\n",
            "Lat/Lon: ", round(nodes$lat, 2), "/", round(nodes$lon, 2)
          )

          tips_df <- nodes[nodes$istip, , drop = FALSE]
          internal_df <- nodes[!nodes$istip, , drop = FALSE]

          # Internal nodes
          if (nrow(internal_df) > 0) {
            p0 <- p0 + ggplot2::geom_point(
              data = internal_df,
              ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip, key = key_id),
              shape = 21, size = input$node_size, stroke = input$node_size * 0.3, colour = "black",
              inherit.aes = FALSE
            )
          }

          # Tip nodes
          if (!is.null(input$highlight_tips) && length(input$highlight_tips) > 0) {
             tips_df$highlighted <- tips_df$label %in% input$highlight_tips
          } else {
             tips_df$highlighted <- FALSE
          }

          normal_tips <- tips_df[!tips_df$highlighted, ]
          high_tips <- tips_df[tips_df$highlighted, ]

          if (nrow(normal_tips) > 0) {
            p0 <- p0 + ggplot2::geom_point(
              data = normal_tips,
              ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip, key = key_id),
              shape = 21, size = input$tip_size, stroke = input$tip_size * 0.3, colour = "black",
              inherit.aes = FALSE
            )
          }
          if (nrow(high_tips) > 0) {
            p0 <- p0 + ggplot2::geom_point(
              data = high_tips,
              ggplot2::aes(x = lon, y = lat, fill = as.numeric(age), text = tooltip, key = key_id),
              shape = 21, size = input$tip_size * 1.3, stroke = input$tip_size * 0.3, colour = input$highlight_color,
              inherit.aes = FALSE
            )
          }
        }

        p0 <- p0 +
          ggplot2::scale_fill_viridis_c(guide = "none", direction = -1) +
          ggplot2::scale_colour_viridis_c(direction = -1) +
          ggplot2::theme_void() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#f0f5f7", color = NA))

        p0
      })

      plotly::ggplotly(p, tooltip = "text", source = "phylogeo_map") %>%
        plotly::layout(showlegend = FALSE)
    })

    # Observer for Click Events (Same as before)
    shiny::observeEvent(plotly::event_data("plotly_click", source = "phylogeo_map"), {
      click_data <- plotly::event_data("plotly_click", source = "phylogeo_map")

      if (!is.null(click_data$key)) {
        selected_node <- full_pgeo$nodes[full_pgeo$nodes$label == click_data$key, ]
        if (nrow(selected_node) > 0) {
          entry <- list(
            id = selected_node$label,
            time = Sys.time(),
            age = selected_node$age,
            loc = paste0(round(selected_node$lat, 2), ", ", round(selected_node$lon, 2)),
            type = ifelse(selected_node$istip, "Tip", "Internal Node")
          )
          vals$history <- append(list(entry), vals$history)
        }
      }
    })

    shiny::observeEvent(input$clear_history, {
        vals$history <- list()
    })

    output$click_history_list <- shiny::renderUI({
      if (length(vals$history) == 0) {
        return(shiny::tags$em("Click on nodes to see history here..."))
      }
      history_items <- lapply(vals$history, function(item) {
        shiny::div(
          style = "border-bottom: 1px solid #eee; padding-bottom: 5px; margin-bottom: 5px;",
          shiny::tags$strong(item$id),
          shiny::tags$span(style = "float: right; color: #888; font-size: 0.8em;", format(item$time, "%H:%M:%S")),
          shiny::br(),
          shiny::tags$small(paste("Type:", item$type, "| Age:", item$age)),
          shiny::br(),
          shiny::tags$small(paste("Loc:", item$loc))
        )
      })
      shiny::tagList(history_items)
    })
  }

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
