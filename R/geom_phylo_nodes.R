#' Phylogeographic nodes
#'
#' Plot all tree nodes as points filled by their age value (same color
#' scale as HPD polygons).
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing nodes.
#' @param shape Point shape (default: 21).
#' @param ... Other arguments passed on to ggplot2::geom_point.
#' @export
geom_phylo_nodes <- function(mapping = NULL, data = NULL, shape = 21, ...) {
  # Accept combined `phylo_phylogeo` objects and extract the nodes component
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$nodes
  }

  ggplot2::geom_point(
    mapping = mapping %||% ggplot2::aes(x = lon, y = lat, fill = age),
    data = data,
    shape = shape,
    ...
  )
}
