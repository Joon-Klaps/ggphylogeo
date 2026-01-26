# Internal package setup
#'
#' @importFrom utils capture.output
#' @importFrom treeio read.beast
#' @importFrom ape as.phylo
#' @importFrom viridisLite viridis
NULL

# Register global variables used in NSE / ggplot aesthetics to avoid R CMD check notes
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  ".data", "lon", "lat", "lonend", "latend", "group", "endheight", "alpha"
))
