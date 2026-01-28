#' Build combined phylogeographic data
#'
#' This wraps `build_branches()` and `build_hpd()` and assembles a
#' nodes data.frame so the combined object can be passed as `data` to the
#' geoms (e.g. `geom_phylo_branches(data = phylo_phylogeo)`).
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param lon name of longitude column (default: "location1")
#' @param lat name of latitude column (default: "location2")
#' @param height_branches column name for branch ages (default: "height_mean")
#' @param height_hpd column name for HPD ages (default: "height_median")
#' @param level HPD level (default: "0.80")
#' @param digits numeric digits for rounding coordinates (default: 2)
#' @param most_recent_sample Date or numeric year (e.g. 2019) used to calibrate age to actual dates. If NULL (default) no calibration is performed.
#' @param debug logical debug messages
#' @export
build_phylogeo <- function(
  treedata,
  lat = "location1",
  lon = "location2",
  height_branches = "height_mean",
  height_hpd = "height_median",
  level = "0.80",
  digits = 2,
  most_recent_sample = NULL,
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)

  if (debug) message("build_phylogeo: building branches and HPD polygons")

  ## ---- Build raw components (with calibration) ---------------------------
  branches <- build_branches(
    treedata,
    lon = lon,
    lat = lat,
    height = height_branches,
    digits = digits,
    most_recent_sample = most_recent_sample,
    debug = debug
  )

  hpd <- build_hpd(
    treedata,
    level = level,
    lon = lon,
    lat = lat,
    height = height_hpd,
    most_recent_sample = most_recent_sample,
    debug = debug
  )

  if (is.null(hpd) && debug) {
    message("build_phylogeo: no HPD polygons built")
  }

  nodes <- build_nodes(
      treedata,
      lon = lon,
      lat = lat,
      height = height_hpd,
      digits = digits,
      most_recent_sample = most_recent_sample,
      debug = debug
  )

  viewbox <- get_data_bbox(branches, hpd)

  ## ---- Final sanity check --------------------------------------------------
  if (debug) {
    message(sprintf(
      "build_phylogeo: branches=%d rows; hpd=%d rows; nodes=%d rows",
      nrow(branches),
      ifelse(is.null(hpd), 0, nrow(hpd)),
      nrow(nodes)
    ))
  }

  structure(
    list(
      branches = branches,
      hpd = hpd,
      nodes = nodes,
      viewbox = viewbox
    ),
    class = "phylo_phylogeo"
  )
}
