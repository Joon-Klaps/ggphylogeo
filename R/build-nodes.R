#' Build node table for continuous phylogeography
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param lon name of longitude column prefix (default: "location1")
#' @param lat name of latitude column prefix (default: "location2")
#' @param height column name used for node ages (default: "height_median")
#' @param digits numeric digits for rounding coordinates (default: 2)
#' @param most_recent_sample Date or numeric year (e.g. 2019) used to calibrate
#'   ages to actual dates. If NULL (default) no calibration is performed.
#' @param debug logical debug messages
#' @return A data.frame with columns: node, lon, lat, age, istip, label
#' @export
build_nodes <- function(
  treedata,
  lon = "location1",
  lat = "location2",
  height = "height_median",
  digits = 2,
  most_recent_sample = NULL,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  phy <- treedata@phylo
  n_tips <- length(phy$tip.label)
  n_total <- max(phy$edge)
  edges <- phy$edge

  # Extract coordinate and height values for all nodes
  lons <- get_node_vals(treedata, lon)
  lats <- get_node_vals(treedata, lat)
  ages <- get_node_vals(treedata, height)

  # Calibrate ages to dates if most_recent_sample provided
  if (!is.null(most_recent_sample)) {
    ages <- calibrate_age(ages, most_recent_sample, debug = debug)
  }

  # Build labels: tip labels for tips, "Node N" for internal nodes
  labels <- character(n_total)
  labels[seq_len(n_tips)] <- phy$tip.label
  labels[(n_tips + 1):n_total] <- paste0("Node ", (n_tips + 1):n_total)

  # Identify valid nodes (those with coordinates)
  node_ids <- seq_along(lons)
  valid <- !is.na(lons) & !is.na(lats)

  if (debug) {
    message(sprintf("build_nodes: found %d valid nodes (%d tips, %d internal)",
                    sum(valid), sum(valid & node_ids <= n_tips),
                    sum(valid & node_ids > n_tips)))
  }

  data.frame(
    node = node_ids[valid],
    lon = round(lons[valid], digits),
    lat = round(lats[valid], digits),
    age = ages[valid],
    istip = node_ids[valid] <= n_tips,
    label = labels[valid],
    stringsAsFactors = FALSE
  )
}
