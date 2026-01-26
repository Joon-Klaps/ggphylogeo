#' Autoplot continuous phylogeography
#'
#' @param object treedata object with phylogeographic annotations
#' @param debug logical debug messages
#' @param show_map logical whether to overlay map (default: TRUE)
#' @param map_fill fill color for map polygons (default: "gray95")
#' @param map_colour outline color for map polygons (default: "gray50")
#' @param lat name of latitude column in treedata (default: "location1")
#' @param lon name of longitude column in treedata (default: "location2")
#' @param height_branches column name used for branch heights (default: "height_mean")
#' @param height_hpd column name used for HPD polygon heights (default: "height_median")
#' @param level character or numeric HPD level to use (default: "0.80")
#' @param digits integer number of decimal digits for coordinate rounding (default: 2)
#' @param map_alpha transparency for map polygons (default: 0.5)
#' @param map_pad numeric degrees to pad the map extent (default: 1)
#' @param map_name character name of map database (default: "world")
#' @param ... additional arguments passed to specific geoms
#' @export
autoplot.treedata <- function(object,
                              debug = getOption("ggphylogeo.debug", FALSE),
                              show_map = TRUE,
                              lat="location1",
                              lon="location2",
                              map_fill = "gray95",
                              map_colour = "gray50",
                              height_branches = "height_mean",
                              height_hpd = "height_median",
                              level = "0.80",
                              digits = 2,
                              map_alpha = 0.5,
                              map_pad = 1,
                              map_name = "world",
                              ...) {

  pgeo <- build_phylogeo( object,
    lat = lat,
    lon = lon,
    height_branches = height_branches,
    height_hpd = height_hpd,
    level = level,
    digits = digits,
    debug = debug
  )

  if (debug) {
    message(sprintf("autoplot.treedata: segments=%d rows; hpd=%d rows",
                    nrow(pgeo$branches), nrow(pgeo$hpd)))
  }

  # 2. Calculate Viewport (Bounding Box)
  bbox <- get_data_bbox(pgeo$branches, pgeo$hpd, pad = map_pad)
  if (debug) {
    message(sprintf("autoplot.treedata: Viewport x[%.2f, %.2f] y[%.2f, %.2f]",
                    bbox$xlim[1], bbox$xlim[2], bbox$ylim[1], bbox$ylim[2]))
  }

  # 3. Initialize plot
  p <- ggplot2::ggplot()

  # 4. Add map layer (Background)
  if (isTRUE(show_map)) {
    # Load the full map data
    # We put this in a tryCatch to be safe against missing map packages
    map_data <- tryCatch(
      ggplot2::map_data(map_name),
      error = function(e) {
        warning(paste("Could not load map data:", e$message))
        NULL
      }
    )

    if (!is.null(map_data)) {
      p <- p + ggplot2::geom_polygon(
        data = map_data,
        ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
        fill = map_fill,
        colour = map_colour,
        alpha = map_alpha,
        inherit.aes = FALSE
      )
    }
  }

  # 5. Add HPD polygons
  if (!is.null(pgeo$hpd) && nrow(pgeo$hpd) > 0) {
    p <- p + geom_phylo_hpd(data = pgeo$hpd, alpha = 0.2,  linewidth = 0.4)
  }

  # 6. Add phylo branches
  if (!is.null(pgeo$branches) && nrow(pgeo$branches) > 0) {
    p <- p + geom_phylo_branches(data = pgeo$branches)
  }

  if (!is.null(pgeo$nodes) && nrow(pgeo$nodes) > 0) {
    p <- p + geom_phylo_nodes(data = pgeo$nodes, size = 2)
  }

  # 7. Apply Coordinates and Theme
  # coord_quickmap preserves aspect ratio and sets the limits (cropping)
  p <- p +
    ggplot2::coord_quickmap(xlim = bbox$xlim, ylim = bbox$ylim) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  return(p)
}
