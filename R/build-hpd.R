#' Build HPD polygons for continuous phylogeography
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param level character HPD level to use in column names (e.g. "0.80")
#' @param lon name of longitude column prefix (default: "location1")
#' @param lat name of latitude column prefix (default: "location2")
#' @param height column name used for HPD polygon ages (default: "height_median")
#' @param debug logical debug messages
#' @param most_recent_sample Date or numeric year (e.g. 2019) used to calibrate
#'   ages to actual dates. If NULL (default) no calibration is performed.
#' @return A data.frame with columns: node, polygon, lon, lat, ageParent, age,
#'   group; or NULL if no valid HPD polygons found.
#' @export
build_hpd <- function(
  treedata,
  level = "0.80",
  lon = "location1",
  lat = "location2",
  height = "height_median",
  most_recent_sample = NULL,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  if (debug) {
    message(sprintf("build_hpd: level=%s lon=%s lat=%s height=%s",
                    level, lon, lat, height))
  }

  d <- treedata@data
  t <- treedata@phylo

  # Get row index for each node ID (1..Nnodes)
  key <- match_nodes(t, d)

  # Identify HPD columns
  lon_pattern <- paste0("^", lon, "_", level, "HPD_")
  lat_pattern <- paste0("^", lat, "_", level, "HPD_")

  loncols <- grep(lon_pattern, names(d), value = TRUE)
  latcols <- grep(lat_pattern, names(d), value = TRUE)

  # Natural sort to ensure _1, _2, _10 match up
  loncols <- loncols[order(nchar(loncols), loncols)]
  latcols <- latcols[order(nchar(latcols), latcols)]

  if (length(loncols) == 0 || length(latcols) == 0) {
    if (debug) message("build_hpd: no HPD columns found")
    return(NULL)
  }

  if (length(loncols) != length(latcols)) {
    warning("Mismatch in number of longitude/latitude HPD columns; HPDs may be incorrect.")
  }

  n_poly <- min(length(loncols), length(latcols))

  # Get height for each node
  node_ages <- get_node_vals(treedata, height)

  if (!is.null(most_recent_sample)) {
    node_ages <- calibrate_age(node_ages, most_recent_sample, debug = debug)
  }

  # Pre-allocate list for better performance
  polys_list <- vector("list", length(key) * n_poly)
  poly_idx <- 1L

  # Iterate over all nodes
  for (i in seq_along(key)) {
    row_idx <- key[i]

    # Skip if invalid row or height
    if (is.na(row_idx)) next

    age_val <- node_ages[i]
    if (is.na(age_val)) next

    # Process each polygon for this node
    for (j in seq_len(n_poly)) {
      lons_vec <- d[[loncols[j]]][[row_idx]]
      lats_vec <- d[[latcols[j]]][[row_idx]]

      # Skip invalid polygons
      if (is.null(lons_vec) || is.null(lats_vec) ||
          length(lons_vec) < 3 || length(lats_vec) < 3 ||
          any(is.na(lons_vec)) || any(is.na(lats_vec))) {
        next
      }

      # Ensure equal lengths
      if (length(lons_vec) != length(lats_vec)) {
        warning(sprintf("Node %d polygon %d: lon/lat length mismatch (%d vs %d)",
                        i, j, length(lons_vec), length(lats_vec)))
        next
      }

      # Get the number of points in this polygon
      n_pts <- length(lons_vec)

      # Create polygon data frame with explicit lengths
      polys_list[[poly_idx]] <- data.frame(
        node = rep(i, n_pts),
        polygon = rep(j, n_pts),
        lon = as.numeric(lons_vec),
        lat = as.numeric(lats_vec),
        age = rep(age_val, n_pts),
        group = rep(paste(i, j, sep = "-"), n_pts),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      poly_idx <- poly_idx + 1L
    }
  }

  # Remove NULL entries and combine
  polys_list <- polys_list[!sapply(polys_list, is.null)]

  if (length(polys_list) == 0) {
    if (debug) message("build_hpd: no polygons generated")
    return(NULL)
  }

  polys <- do.call(rbind, polys_list)

  if (debug) {
    message(sprintf("build_hpd: created %d polygon points across %d polygons",
                    nrow(polys), length(unique(polys$group))))
  }

  # Order by age for consistent layering (oldest/highest first)
  polys[order(polys$age, decreasing = TRUE), , drop = FALSE]
}