#' Phylogeographic tree segments
#'
#' @export
geom_phylo_segments <- function(mapping = NULL, data = NULL, ...) {
  ggplot2::geom_segment(
    mapping = mapping %||% ggplot2::aes(
      x = x, y = y, xend = xend, yend = yend, colour = endheight
    ),
    data = data,
    ...
  )
}

