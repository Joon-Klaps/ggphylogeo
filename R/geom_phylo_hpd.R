#' Phylogeographic HPD polygons
#'
#' @export
geom_phylo_hpd <- function(mapping = NULL, data = NULL, ...) {
  ggplot2::geom_polygon(
    mapping = mapping %||% ggplot2::aes(
      x = x, y = y, group = group, colour = endheight
    ),
    data = data,
    fill = NA,
    ...
  )
}

