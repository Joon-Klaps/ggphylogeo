#' Phylogeographic tree branches
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing branches.
#' @param ... Other arguments passed on to ggplot2::geom_curve.
#' @export
geom_phylo_branches <- function(mapping = NULL, data = NULL, ...) {
  # Accept combined `phylo_phylogeo` objects and extract the branches component
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$branches
  }

  ggplot2::geom_curve(
    mapping = mapping %||% ggplot2::aes(
      x = lon, y = lat, xend = lonend, yend = latend,
    ),
    data = data,
    ...
  )
}

