check_treedata <- function(x) {
  if (!inherits(x, "treedata")) {
    stop("Input must be a treedata object (from treeio::read.beast)", call. = FALSE)
  }
}
