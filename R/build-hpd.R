#' Build HPD polygons for continuous phylogeography
#'
#' Extracts highest posterior density (HPD) uncertainty regions from a BEAST
#' continuous phylogeographic analysis. Each node may have multiple HPD polygons
#' representing the spatial uncertainty of its location estimate.
#'
#' @param treedata treedata object with phylogeographic annotations from
#'   `treeio::read.beast()`
#' @param level character HPD level to use (e.g. "0.80" for 80% HPD)
#' @param lon name of longitude coordinate column prefix (default: "location1")
#' @param lat name of latitude coordinate column prefix (default: "location2")
#' @param height column name for node ages (default: "height_median")
#' @param most_recent_sample Date, numeric year, or "YYYY-MM-DD" string for
#'   calibrating ages to real dates. If NULL, ages remain as numeric heights.
#' @param debug logical; emit debug messages
#' @return A data.frame with columns: node, polygon, lon, lat, age, group;
#'   or NULL if no valid HPD polygons found.
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
  phy <- treedata@phylo
  key <- match_nodes(phy, d)

 # Find HPD columns and sort naturally
  hpd_cols <- find_hpd_columns(names(d), lon, lat, level)
  if (is.null(hpd_cols)) {
    if (debug) message("build_hpd: no HPD columns found")
    return(NULL)
  }

  # Get calibrated node ages
  node_ages <- get_node_vals(treedata, height)
  if (!is.null(most_recent_sample)) {
    node_ages <- calibrate_age(node_ages, most_recent_sample, debug = debug)
  }

  # Extract polygons for all nodes
  polys <- extract_hpd_polygons(d, key, node_ages, hpd_cols)

  if (is.null(polys) || nrow(polys) == 0) {
    if (debug) message("build_hpd: no polygons generated")
    return(NULL)
  }

  if (debug) {
    message(sprintf("build_hpd: created %d polygon points across %d polygons",
                    nrow(polys), length(unique(polys$group))))
  }

  # Order by age (oldest first) for proper layering
  polys[order(polys$age, decreasing = TRUE), , drop = FALSE]
}

#' Find and validate HPD column names
#' @noRd
find_hpd_columns <- function(colnames, lon, lat, level) {
  lon_pattern <- paste0("^", lon, "_", level, "HPD_")
  lat_pattern <- paste0("^", lat, "_", level, "HPD_")

  loncols <- grep(lon_pattern, colnames, value = TRUE)
  latcols <- grep(lat_pattern, colnames, value = TRUE)

  # Natural sort (handles _1, _2, _10 correctly)
  loncols <- loncols[order(nchar(loncols), loncols)]
  latcols <- latcols[order(nchar(latcols), latcols)]

  if (length(loncols) == 0 || length(latcols) == 0) return(NULL)

  if (length(loncols) != length(latcols)) {
    warning("Mismatch in lon/lat HPD column counts; using minimum.")
  }

  n <- min(length(loncols), length(latcols))
  list(lon = loncols[seq_len(n)], lat = latcols[seq_len(n)])
}

#' Extract HPD polygon coordinates for all nodes
#' @noRd
extract_hpd_polygons <- function(data, key, node_ages, hpd_cols) {
  n_poly <- length(hpd_cols$lon)
  results <- vector("list", length(key) * n_poly)
  idx <- 1L

  for (i in seq_along(key)) {
    row_idx <- key[i]
    age_val <- node_ages[i]

    # Skip invalid nodes
    if (is.na(row_idx) || is.na(age_val)) next

    for (j in seq_len(n_poly)) {
      poly_df <- create_polygon_df(
        data, row_idx, i, j, age_val,
        hpd_cols$lon[j], hpd_cols$lat[j]
      )

      if (!is.null(poly_df)) {
        results[[idx]] <- poly_df
        idx <- idx + 1L
      }
    }
  }

  # Combine non-NULL results
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(NULL)

  do.call(rbind, results)
}

#' Create a single polygon data frame
#' @noRd
create_polygon_df <- function(data, row_idx, node_id, poly_id, age, lon_col, lat_col) {
  lons <- data[[lon_col]][[row_idx]]
  lats <- data[[lat_col]][[row_idx]]

  # Validate polygon data
  if (is.null(lons) || is.null(lats) ||
      length(lons) < 3 || length(lats) < 3 ||
      length(lons) != length(lats) ||
      any(is.na(lons)) || any(is.na(lats))) {
    return(NULL)
  }

  n_pts <- length(lons)
  data.frame(
    node = rep(node_id, n_pts),
    polygon = rep(poly_id, n_pts),
    lon = as.numeric(lons),
    lat = as.numeric(lats),
    age = rep(age, n_pts),
    group = rep(paste(node_id, poly_id, sep = "-"), n_pts),
    stringsAsFactors = FALSE
  )
}