#' Autoplot continuous phylogeography
#'
#' Creates a ggplot2 visualization of continuous phylogeographic data from
#' BEAST analyses. Supports various customization options including map
#' backgrounds, tip highlighting, and direction legends.
#'
#' @param object treedata object with phylogeographic annotations
#' @param debug logical; emit debug messages
#' @param show_map logical; overlay map background
#' @param lat name of latitude column
#' @param lon name of longitude column
#' @param map_fill fill color for map polygons
#' @param map_colour outline color for map polygons
#' @param height_branches column used for branch heights
#' @param height_hpd column used for HPD heights
#' @param level HPD level (e.g., "0.80" for 80% HPD)
#' @param digits decimal rounding for coordinates
#' @param map_alpha transparency for map polygons (0-1)
#' @param map_pad padding (degrees) for map extent
#' @param map_name map database name (e.g., "world", "usa", "state")
#' @param most_recent_sample date/year used to calibrate endheight to real
#'   dates. Can be Date, POSIXt, numeric year (e.g., 2019), or character
#'   "YYYY-MM-DD"
#' @param curvature curvature of branch curves (default 0.3)
#' @param arrow arrow specification for branches (see grid::arrow)
#' @param show_direction_legend logical; show mini legend explaining branch
#'   direction (default FALSE)
#' @param highlight_tips character vector of tip labels to highlight
#' @param highlight_color color for highlighted tips (default "red")
#' @param tip_size size of tip nodes (default 2.5)
#' @param node_size size of internal nodes (default 2)
#' @param tip_stroke stroke width for tip nodes (default 0.5)
#' @param date_format format string for date labels (default "\%Y")
#' @param ... passed to geoms
#' @return A ggplot2 object
#' @export
autoplot.treedata <- function(
  object,
  debug = getOption("ggphylogeo.debug", FALSE),
  show_map = TRUE,
  lat = "location1",
  lon = "location2",
  map_fill = "gray95",
  map_colour = "gray50",
  height_branches = "height_mean",
  height_hpd = "height_median",
  level = "0.80",
  digits = 2,

  map_alpha = 0.5,
  map_pad = 1,
  map_name = "world",
  most_recent_sample = NULL,
  curvature = 0.3,
  arrow = NULL,
  show_direction_legend = FALSE,
  highlight_tips = NULL,
  highlight_color = "red",
  tip_size = 2.5,
  node_size = 2,
  tip_stroke = 0.5,
  date_format = "%Y",
  ...
) {

  ## ---- Build phylogeographic data ------------------------------------------
  pgeo <- build_phylogeo(
    object,
    lat = lat,
    lon = lon,
    height_branches = height_branches,
    height_hpd = height_hpd,
    level = level,
    digits = digits,
    most_recent_sample = most_recent_sample,
    debug = debug
  )

  if (debug) {
    message(sprintf(
      "autoplot.treedata: branches=%d rows; hpd=%d rows; nodes=%d rows",
      nrow(pgeo$branches), nrow(pgeo$hpd), nrow(pgeo$nodes)
    ))
  }

  ## ---- Compute plotting extent ---------------------------------------------
  bbox <- get_data_bbox(pgeo$branches, pgeo$hpd, pad = map_pad)

  if (debug) {
    message(sprintf(
      "Viewport x[%.2f, %.2f], y[%.2f, %.2f]",
      bbox$xlim[1], bbox$xlim[2],
      bbox$ylim[1], bbox$ylim[2]
    ))
  }

  ## ---- Initialize plot -----------------------------------------------------
  p <- ggplot2::ggplot()

  ## ---- Map background ------------------------------------------------------
  if (isTRUE(show_map)) {
    map_data <- tryCatch(
      ggplot2::map_data(map_name),
      error = function(e) {
        warning("Could not load map data: ", e$message)
        NULL
      }
    )

    if (!is.null(map_data)) {
      p <- p + ggplot2::geom_polygon(
        data = map_data,
        ggplot2::aes(long, lat, group = group),
        fill = map_fill,
        colour = map_colour,
        alpha = map_alpha,
        inherit.aes = FALSE
      )
    }
  }

  ## ---- Phylogeographic layers ----------------------------------------------
  # HPD polygons (uncertainty regions)
  if (!is.null(pgeo$hpd) && nrow(pgeo$hpd) > 0) {
    p <- p + geom_phylo_hpd(
      data = pgeo$hpd,
      alpha = 0.2,
      linewidth = 0.4
    )
  }

  # Branch curves
  if (!is.null(pgeo$branches) && nrow(pgeo$branches) > 0) {
    branches <- pgeo$branches
    moving <- (branches$lon != branches$lonend) |
              (branches$lat != branches$latend)
    moving <- moving & !is.na(moving)

    if (any(moving)) {
      p <- p + geom_phylo_branches(
        data = branches[moving, , drop = FALSE],
        curvature = curvature,
        arrow = arrow,
        ...
      )
    }
  }

  ## ---- Node layers (tips vs internal nodes) --------------------------------
  if (!is.null(pgeo$nodes) && nrow(pgeo$nodes) > 0) {
    nodes <- pgeo$nodes

    # Separate tips and internal nodes
    tips_data <- nodes[nodes$istip, , drop = FALSE]
    internal_data <- nodes[!nodes$istip, , drop = FALSE]

    # Add highlighting column for tips
    if (!is.null(highlight_tips) && length(highlight_tips) > 0) {
      tips_data$highlighted <- tips_data$label %in% highlight_tips
    } else {
      tips_data$highlighted <- FALSE
    }

    # Internal nodes: no stroke, smaller size
    if (nrow(internal_data) > 0) {
      p <- p + ggplot2::geom_point(
        data = internal_data,
        ggplot2::aes(x = lon, y = lat, fill = endheight),
        shape = 21,
        size = node_size,
        stroke = 0,
        inherit.aes = FALSE
      )
    }

    # Tips: with stroke, larger size
    if (nrow(tips_data) > 0) {
      # Non-highlighted tips
      normal_tips <- tips_data[!tips_data$highlighted, , drop = FALSE]
      highlighted_tips <- tips_data[tips_data$highlighted, , drop = FALSE]

      if (nrow(normal_tips) > 0) {
        p <- p + ggplot2::geom_point(
          data = normal_tips,
          ggplot2::aes(x = lon, y = lat, fill = endheight),
          shape = 21,
          size = tip_size,
          stroke = tip_stroke,
          colour = "black",
          inherit.aes = FALSE
        )
      }

      # Highlighted tips (in specified color)
      if (nrow(highlighted_tips) > 0) {
        p <- p + ggplot2::geom_point(
          data = highlighted_tips,
          ggplot2::aes(x = lon, y = lat, fill = endheight),
          shape = 21,
          size = tip_size * 1.3,
          stroke = tip_stroke * 2,
          colour = highlight_color,
          inherit.aes = FALSE
        )
      }
    }
  }

  ## ---- Direction legend ----------------------------------------------------
  if (isTRUE(show_direction_legend)) {
    p <- p + add_direction_legend(bbox, curvature)
  }

  ## ---- Coordinates ---------------------------------------------------------
  p <- p + ggplot2::coord_quickmap(
    xlim = bbox$xlim,
    ylim = bbox$ylim
  )

  ## ---- Fill scale ----------------------------------------------------------
  # Determine if endheight is Date type for proper formatting
  is_date_scale <- FALSE
  if (!is.null(pgeo$nodes) && inherits(pgeo$nodes$endheight, "Date")) {
    is_date_scale <- TRUE
  } else if (!is.null(pgeo$hpd) && inherits(pgeo$hpd$endheight, "Date")) {
    is_date_scale <- TRUE
  } else if (!is.null(pgeo$branches) && inherits(pgeo$branches$endheight, "Date")) {
    is_date_scale <- TRUE
  }

  if (is_date_scale) {
    if (debug) message("Adding date scale for fill aesthetic")
    p <- p + ggplot2::scale_fill_viridis_c(
      name = "Date",
      labels = function(x) format(as.Date(x, origin = "1970-01-01"), date_format)
    )
  } else {
    # Numeric scale - show as years if values suggest year-like data
    p <- p + ggplot2::scale_fill_viridis_c(name = "Time")
  }

  ## ---- Theme ---------------------------------------------------------------
  p +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
}
