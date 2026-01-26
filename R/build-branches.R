#' Build phylogeographic branch segments
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param lon name of longitude column (default: "location1")
#' @param lat name of latitude column (default: "location2")
#' @param height column name used for branch heights (default: "height_mean")
#' @param digits number of decimal places for rounding coordinates (default: 2)
#' @param debug logical debug messages
#' @export
build_branches <- function(
  treedata,
  lon = "location1",
  lat = "location2",
  height = "height_mean",
  digits = 2,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  d <- treedata@data
  t <- treedata@phylo
  key <- match_nodes(t, d)
  edges <- t$edge

  # Helper for safe extraction: returns NA if value is NULL or empty
  safe_get <- function(col_name, node_idx) {
    # Get the row index from our key
    row_idx <- key[node_idx]

    # If node isn't in data or column doesn't exist
    if (is.na(row_idx) || !col_name %in% names(d)) return(NA)

    val <- d[[col_name]][[row_idx]]

    # Handle NULLs or empty vectors which cause the "0 rows" error
    if (is.null(val) || length(val) == 0) return(NA)

    return(as.numeric(val))
  }

  if (debug) message(sprintf("build_branches: lon=%s lat=%s height=%s", lon, lat, height))

  branch_list <- lapply(seq_len(nrow(edges)), function(j) {
    n1 <- edges[j, 1] # Parent node
    n2 <- edges[j, 2] # Child node

    # Extract values safely
    lon_val    <- safe_get(lon, n1)
    lat_val    <- safe_get(lat, n1)
    lonend_val <- safe_get(lon, n2)
    latend_val <- safe_get(lat, n2)
    h_start  <- safe_get(height, n1)
    h_end    <- safe_get(height, n2)

    data.frame(
      startnode   = n1,
      endnode     = n2,
      lon           = round(lon_val, digits),
      lat           = round(lat_val, digits),
      lonend        = round(lonend_val, digits),
      latend        = round(latend_val, digits),
      startheight = h_start,
      endheight   = h_end,
      stringsAsFactors = FALSE
    )
  })

  branch <- do.call(rbind, branch_list)

  # Remove segments where we have no spatial data (e.g. root with no location)
  # This prevents plotting 'ghost' lines from (NA, NA)
  branch <- branch[!is.na(branch$lon) & !is.na(branch$lonend), ]

  branch$istip <- branch$endnode <= length(t$tip.label)

  if (debug) message(sprintf("build_branches: created %d segments", nrow(branch)))

  return(branch[order(-branch$endheight), ])
}