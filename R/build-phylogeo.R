#' Build combined phylogeographic data
#'
#' This wraps `build_branches()` and `build_hpd()` and assembles a
#' nodes data.frame so the combined object can be passed as `data` to the
#' geoms (e.g. `geom_phylo_branches(data = phylo_phylogeo)`).
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param lon name of longitude column (default: "location1")
#' @param lat name of latitude column (default: "location2")
#' @param height_branches column name for branch heights (default: "height_mean")
#' @param height_hpd column name for HPD heights (default: "height_median")
#' @param level HPD level (default: "0.80")
#' @param digits numeric digits for rounding coordinates (default: 2)
#' @param debug logical debug messages
#' @export
build_phylogeo <- function(
  treedata,
  lon = "location1",
  lat = "location2",
  height_branches = "height_mean",
  height_hpd = "height_median",
  level = "0.80",
  digits = 2,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  if (debug) message("build_phylogeo: building branches and HPD polygons")

  branches <- build_branches(treedata, lon = lon, lat = lat, height = height_branches, digits = digits, debug = debug)
  hpd <- build_hpd(treedata, level = level, lon = lon, lat = lat, height = height_hpd, debug = debug)

  # Build nodes table from branch segments (prefer end-coordinates when present)
  t <- treedata@phylo
  ntips <- length(t$tip.label)

  if (nrow(branches) == 0) {
    nodes <- data.frame(node = integer(0), lon = double(0), lat = double(0), endheight = double(0), istip = logical(0))
  } else {
    node_ids <- sort(unique(c(branches$startnode, branches$endnode)))

    nodes_list <- lapply(node_ids, function(n) {
      row_end <- branches[branches$endnode == n, , drop = FALSE]

      if (nrow(row_end) > 0) {
        lonval <- row_end$lonend[1]
        latval <- row_end$latend[1]
        h <- row_end$endheight[1]
      } else {
        row_start <- branches[branches$startnode == n, , drop = FALSE]
        lonval <- row_start$lon[1]
        latval <- row_start$lat[1]
        h <- row_start$startheight[1]
      }

      data.frame(
        node = n,
        lon = round(as.numeric(lonval), digits),
        lat = round(as.numeric(latval), digits),
        endheight = as.numeric(h),
        istip = n <= ntips,
        stringsAsFactors = FALSE
      )
    })

    nodes <- do.call(rbind, nodes_list)
  }

  if (debug) message(sprintf("build_phylogeo: branches=%d rows; hpd=%d rows; nodes=%d rows",
                           nrow(branches), ifelse(is.null(hpd), 0, nrow(hpd)), nrow(nodes)))

  structure(
    list(branches = branches, hpd = hpd, nodes = nodes),
    class = "phylo_phylogeo"
  )
}
