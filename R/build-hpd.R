#' Build HPD polygons for continuous phylogeography
#'
#' @param treedata treedata object with phylogeographic annotations
#' @param level character HPD level to use in column names (e.g. "0.80")
#' @param lon name of longitude column prefix (default: "location1")
#' @param lat name of latitude column prefix (default: "location2")
#' @param height column name used for HPD polygon heights (default: "height_median")
#' @param debug logical debug messages
#' @export
build_hpd <- function(
  treedata,
  level = "0.80",
  lon = "location1",
  lat = "location2",
  height = "height_median",
  debug = getOption("ggphylogeo.debug", FALSE)
) {
  check_treedata(treedata)
  if (debug) message(sprintf("build_hpd: level=%s lon=%s lat=%s height=%s", level, lon, lat, height))

  d <- treedata@data
  t <- treedata@phylo
  key <- match_nodes(t, d)

  loncols <- grep(paste0(lon, "_", level, "HPD_"), names(d))
  latcols <- grep(paste0(lat, "_", level, "HPD_"), names(d))

  if (length(loncols) != length(latcols)) {
    stop("Mismatched HPD lon/y columns", call. = FALSE)
  }
  if (debug) message(sprintf("build_hpd: found %d loncols and %d latcols", length(loncols), length(latcols)))

  polys <- list()

  for (node in seq_along(key)) {
    nodeno <- key[node]
    if (is.na(nodeno)) next

    for (i in seq_along(loncols)) {
      lons <- d[[loncols[i]]][[nodeno]]
      lats <- d[[latcols[i]]][[nodeno]]

      if (all(is.na(lons))) next

      polys[[length(polys) + 1]] <- data.frame(
        node = node,
        polygon = i,
        lon = lons,
        lat = lats,
        endheight = d[[height]][[nodeno]]
      )
    }
  }

  if (length(polys) == 0) {
    if (debug) message("build_hpd: no polygons found; returning empty data.frame")
    return(data.frame(node = integer(0), polygon = integer(0), lon = double(0), lat = double(0), endheight = double(0), group = character(0)))
  }

  polys <- do.call(rbind, polys)

  # ensure endheight is numeric (some columns may be stored as lists)
  polys$endheight <- as.numeric(unlist(polys$endheight))
  if (debug) {
    message(sprintf("build_hpd: created %d polygon rows; endheight summary: %s",
                    nrow(polys), paste(utils::capture.output(print(summary(polys$endheight))), collapse = "; ")))
  }

  polys$group <- paste(polys$node, polys$polygon, sep = "-")

  # ordering by endheight (descending)
  # ensure safe ordering if all NAs
  if (all(is.na(polys$endheight))) {
    if (debug) message("build_hpd: all endheight values are NA; returning without reordering")
    return(polys)
  }
  polys[order(-polys$endheight), ]
}

