#' Build HPD polygons for continuous phylogeography
#'
#' @export
build_phylo_hpd <- function(
  treedata,
  level = 0.8,
  x_prefix = "location1",
  y_prefix = "location2",
  height = "height_median"
) {
  check_treedata(treedata)

  d <- treedata@data
  t <- treedata@phylo
  key <- match_nodes(t, d)

  xcols <- grep(paste0(x_prefix, "_", level, "HPD_"), names(d))
  ycols <- grep(paste0(y_prefix, "_", level, "HPD_"), names(d))

  if (length(xcols) != length(ycols)) {
    stop("Mismatched HPD x/y columns", call. = FALSE)
  }

  polys <- list()

  for (node in seq_along(key)) {
    nodeno <- key[node]

    for (i in seq_along(xcols)) {
      xs <- d[[xcols[i]]][[nodeno]]
      ys <- d[[ycols[i]]][[nodeno]]

      if (all(is.na(xs))) next

      polys[[length(polys) + 1]] <- data.frame(
        node = node,
        polygon = i,
        x = xs,
        y = ys,
        endheight = d[[height]][[nodeno]]
      )
    }
  }

  polys <- do.call(rbind, polys)
  polys$group <- paste(polys$node, polys$polygon, sep = "-")
  polys[order(-polys$endheight), ]
}

