#' Phylogeographic HPD polygons
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing HPD polygons.
#' @param ... Other arguments passed on to ggplot2::geom_polygon.
#' @export
geom_phylo_hpd <- function(mapping = NULL, data = NULL, ...) {
  # Accept combined `phylo_phylogeo` objects and extract the hpd component
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$hpd
  }

  ggplot2::geom_polygon(
    mapping = mapping %||% ggplot2::aes(
      x = lon, y = lat, group = group, fill = endheight, alpha = alpha
    ),
    data = data,
    ...
  )
}
