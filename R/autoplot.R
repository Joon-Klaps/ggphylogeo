#' Autoplot continuous phylogeography
#'
#' @export
autoplot.treedata <- function(object, ...) {
  segs <- build_phylo_segments(object)
  hpd  <- build_phylo_hpd(object)

  ggplot2::ggplot() +
    geom_phylo_hpd(data = hpd, linewidth = 0.4) +
    geom_phylo_segments(data = segs) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::theme_bw()
}

