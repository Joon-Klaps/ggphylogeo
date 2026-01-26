#' Build phylogeographic branch segments
#'
#' Constructs a data frame of branch segments for phylogeographic visualization.
#' Each row represents a branch from parent to child node with geographic
#' coordinates and temporal information.
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param lon name of longitude column (default: "location1")
#' @param lat name of latitude column (default: "location2")
#' @param height column name used for branch heights (default: "height_mean")
#' @param digits number of decimal places for rounding coordinates (default: 2)
#' @param most_recent_sample Date or numeric year (e.g. 2019) used to calibrate
#'   endheight to actual dates. If NULL (default) no calibration is performed.
#' @param debug logical debug messages
#' @return A data.frame with columns: startnode, endnode, lon, lat, lonend,
#'   latend, startheight, endheight, istip, label
#' @export
build_branches <- function(
  treedata,
  lon = "location1",
  lat = "location2",
  height = "height_mean",
  digits = 2,
  most_recent_sample = NULL,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  if (debug) {
    message(sprintf("build_branches: lon=%s lat=%s height=%s", lon, lat, height))
  }

  phy <- treedata@phylo
  edges <- phy$edge
  n_tips <- length(phy$tip.label)
  n_total <- max(edges)

  # Extract values for all nodes
  lons <- get_node_vals(treedata, lon)
  lats <- get_node_vals(treedata, lat)
  heights <- get_node_vals(treedata, height)

  # Calibrate heights if most_recent_sample provided
  if (!is.null(most_recent_sample)) {
    heights <- calibrate_endheight(heights, most_recent_sample, debug = debug)
  }

  # Build labels for all nodes
  labels <- character(n_total)
  labels[seq_len(n_tips)] <- phy$tip.label
  labels[(n_tips + 1):n_total] <- paste0("Node ", (n_tips + 1):n_total)

  parent <- edges[, 1]
  child <- edges[, 2]

  branch <- data.frame(
    startnode   = parent,
    endnode     = child,
    lon         = round(lons[parent], digits),
    lat         = round(lats[parent], digits),
    lonend      = round(lons[child], digits),
    latend      = round(lats[child], digits),
    startheight = heights[parent],
    endheight   = heights[child],
    istip       = child <= n_tips,
    label       = labels[child],
    stringsAsFactors = FALSE
  )

  # Remove invalid segments (missing coordinates)
  valid <- !is.na(branch$lon) & !is.na(branch$lonend)
  branch <- branch[valid, ]

  if (debug) {
    message(sprintf("build_branches: created %d segments (%d to tips, %d internal)",
                    nrow(branch), sum(branch$istip), sum(!branch$istip)))
  }

  # Order by endheight (descending) for proper layering
  branch[order(branch$endheight, decreasing = TRUE), ]
}
