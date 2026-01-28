#' ggphylogeo: ggplot2 tools for continuous phylogeography
#'
#' Visualisation tools for BEAST-style continuous phylogeographic analyses.
#'
#' @keywords internal
#' @importFrom treeio %>%
"_PACKAGE"

# Fix "no visible binding" notes for aes variables used in ggplot2
utils::globalVariables(c("age", "ageParent", "lon", "lat", "long", "group",
                         "x", "y", "progress"))
