# Utility functions for ggphylogeo

#' Check object is treedata
#' @noRd
check_treedata <- function(x) {
  if (!inherits(x, "treedata")) {
    stop("Input must be a treedata object (from treeio::read.beast)", call. = FALSE)
  }
}

#' Calculate data bounding box with padding
#'
#' @param segs data.frame with branch segments (must have lon, lonend, lat, latend)
#' @param hpd data.frame with HPD polygons (must have lon, lat)
#' @param pad numeric padding in degrees to add around the data extent
#' @return A list with xlim and ylim vectors
#' @noRd
get_data_bbox <- function(segs, hpd, pad = 1) {
  # Collect all coordinates
  lons <- c()
  lats <- c()

  if (!is.null(segs) && nrow(segs) > 0) {
    lons <- c(lons, segs$lon, segs$lonend)
    lats <- c(lats, segs$lat, segs$latend)
  }

  if (!is.null(hpd) && nrow(hpd) > 0) {
    lons <- c(lons, hpd$lon)
    lats <- c(lats, hpd$lat)
  }

  lons <- lons[!is.na(lons)]
  lats <- lats[!is.na(lats)]

  # Default to World View if no data
  if (length(lons) == 0 || length(lats) == 0) {
    return(list(xlim = c(-180, 180), ylim = c(-90, 90)))
  }

  # Calculate limits with padding
  lonlim <- c(min(lons) - pad, max(lons) + pad)
  latlim <- c(min(lats) - pad, max(lats) + pad)

  # Clamp to valid geographic range
  lonlim[1] <- max(lonlim[1], -180)
  lonlim[2] <- min(lonlim[2], 180)
  latlim[1] <- max(latlim[1], -90)
  latlim[2] <- min(latlim[2], 90)

  list(xlim = lonlim, ylim = latlim)
}

#' Add dispersion legend annotation
#'
#' Builds a mini-legend showing the dispersion convention for curved branches.
#' The legend displays two curved arrows demonstrating left-to-right and
#' right-to-left dispersions.
#'
#' @param bbox list with xlim and ylim from `get_data_bbox()`
#' @param curvature curvature value used in the main plot
#' @param text_size text size for the legend labels (default: 2.5)
#' @return A list of ggplot2 annotation layers that can be added to a ggplot object
#' @examples
#' bbox <- list(xlim = c(-10, 10), ylim = c(-5, 5))
#' p <- ggplot2::ggplot() + ggplot2::xlim(bbox$xlim) + ggplot2::ylim(bbox$ylim)
#' p + add_dispersion_legend(bbox)
#' @export
add_dispersion_legend <- function(bbox, curvature = -0.3, text_size = 2.5) {
  # Position legend in top-left corner
  x_range <- diff(bbox$xlim)
  y_range <- diff(bbox$ylim)

  # Legend box position (top-left)
  legend_x <- bbox$xlim[1] + x_range * 0.15
  legend_y <- bbox$ylim[2] - y_range * 0.12
  legend_width <- x_range * 0.12
  legend_height <- y_range * 0.08

  # Create legend data
  legend_df <- data.frame(
    x = c(legend_x - legend_width/2, legend_x + legend_width/2),
    y = c(legend_y - legend_height/8, legend_y - legend_height/4),
    xend = c(legend_x + legend_width/2, legend_x - legend_width/2),
    yend = c(legend_y - legend_height/8, legend_y - legend_height/4),
    dispersion = c("Earlier \u2192 Later", "Later \u2192 Earlier")
  )

  # Return annotation layers
  list(
    # Background box
    ggplot2::annotate(
      "rect",
      xmin = legend_x - legend_width * 0.7,
      xmax = legend_x + legend_width * 0.7,
      ymin = legend_y - legend_height * 0.8,
      ymax = legend_y + legend_height * 0.8,
      fill = "white",
      colour = "gray50",
      alpha = 0.9
    ),
    # Top curve (left to right = Earlier to Later)
    ggplot2::geom_curve(
      data = legend_df[1, ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = curvature,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed", ends = "first"),
      colour = "gray30",
      linewidth = 0.5,
      inherit.aes = FALSE
    ),
    # Bottom curve (right to left)
    ggplot2::geom_curve(
      data = legend_df[2, ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = curvature,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed", ends = "first"),
      colour = "gray30",
      linewidth = 0.5,
      inherit.aes = FALSE
    ),
    # Label
    ggplot2::annotate(
      "text",
      x = legend_x,
      y = legend_y + legend_height * 0.6,
      label = "Dispersion",
      size = text_size,
      fontface = "bold"
    )
  )
}

#' Match numeric phy node ids to rows in metadata
#' @noRd
match_nodes <- function(phy, data) {
  node_ids <- suppressWarnings(as.numeric(data$node))
  key <- match(seq_len(max(phy$edge)), node_ids)

  if (any(is.na(key))) {
    stop("Some phylo nodes have no matching metadata", call. = FALSE)
  }
  key
}

#' Calibrate numeric ages (e.g. years before most recent sample) to Date
#'
#' If `most_recent_sample` is NULL this returns `endh` unchanged. If provided,
#' it must be a Date, POSIXt, or numeric year (e.g. 2019). Numeric endh values
#' are treated as years and converted to dates by subtracting years*365.25
#' days from the `most_recent_sample`.
#'
#' @param endh numeric vector or Date vector
#' @param most_recent_sample Date, POSIXt, or numeric year (single value)
#' @param debug logical
#' @return Date vector when `most_recent_sample` provided, otherwise returns `endh` as-is
#' @keywords internal
#' @noRd
calibrate_age <- function(endh, most_recent_sample = NULL, debug = FALSE) {
  if (is.null(most_recent_sample)) return(endh)

  # Accept character dates in ISO format
  if (is.character(most_recent_sample)) {
    try_date <- as.Date(most_recent_sample)
    if (is.na(try_date)) {
      stop("most_recent_sample character string could not be parsed as Date; use 'YYYY-MM-DD' or supply a Date object", call. = FALSE)
    }
    mrs_date <- try_date
  } else if (inherits(most_recent_sample, "POSIXt")) {
    mrs_date <- as.Date(most_recent_sample)
  } else if (is.numeric(most_recent_sample) && length(most_recent_sample) == 1 && most_recent_sample > 1000) {
    mrs_date <- as.Date(sprintf("%04d-01-01", as.integer(most_recent_sample)))
  } else if (inherits(most_recent_sample, "Date")) {
    mrs_date <- most_recent_sample
  } else {
    stop("most_recent_sample must be a Date, POSIXt, numeric year, or character 'YYYY-MM-DD'", call. = FALSE)
  }

  endh_vec <- as.numeric(endh)
  out_dates <- rep(as.Date(NA), length(endh_vec))
  non_na <- !is.na(endh_vec)
  out_dates[non_na] <- mrs_date - round(as.numeric(endh_vec[non_na]) * 365.25)

  if (debug) message(sprintf("calibrate_age: converted %d/%d values to Date", sum(non_na), length(endh_vec)))

  out_dates
}

#' Extract numeric values for all nodes from treedata
#' @noRd
get_node_vals <- function(treedata, var_name) {
  d <- treedata@data
  t <- treedata@phylo
  key <- match_nodes(t, d)

  if (!var_name %in% names(d)) {
    return(rep(NA_real_, max(t$edge)))
  }

  raw_vals <- d[[var_name]][key]

  # Safely extract first numeric element, handling lists, NULLs, and empty vectors
  vapply(raw_vals, function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) NA_real_ else as.numeric(x[1])
  }, numeric(1))
}

#' Theme for ggphylogeo plots
#'
#' A ggplot2 theme tailored for phylogeographic plots.
#'
#' @return A ggplot2 theme object
#' @examples
#' ggplot2::ggplot() + theme_phylogeo()
#' @export
theme_phylogeo <- function() {
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "lightblue1"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    )
}

#' guides for ggphylogeo plots
#'
#' A ggplot2 guide tailored for phylogeographic plots.
#'
#' @return A ggplot2 guide object
#' @examples
#' ggplot2::ggplot() + guides_phylogeo()
#' @export
guides_phylogeo <- function() {
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      theme = ggplot2::theme(
        legend.key.width  = ggplot2::unit(0.5, "lines"),
        legend.key.height = ggplot2::unit(10, "lines"),
        legend.key = ggplot2::element_rect(fill = NA, colour = "black")
      )
    ),
    # Throws an error
    # size = ggplot2::element_blank(),
    # alpha = ggplot2::element_blank()
  )
}