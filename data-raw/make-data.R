# Script to create package data from tree files
# Run this script with: source("data-raw/make-data.R")

library(treeio)

# Load WNV tree
wnv_tree <- treeio::read.beast(
 system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
)

# Load YFV tree
yfv_tree <- treeio::read.beast(
 system.file("extdata", "YFV_RRW_cauchy.MCC.tree", package = "ggphylogeo")
)

# Load HIV-1 tree
hiv1_tree <- treeio::read.beast(
 system.file("extdata", "HIV1.MCC.tree", package = "ggphylogeo")
)

# Load H3N2 tree
h3n2_tree <- treeio::read.beast(
 system.file("extdata", "H3N2t.MCC.tree", package = "ggphylogeo")
)


# Save as .rda files in data/
usethis::use_data(wnv_tree, overwrite = TRUE)
usethis::use_data(yfv_tree, overwrite = TRUE)
usethis::use_data(hiv1_tree, overwrite = TRUE)
usethis::use_data(h3n2_tree, overwrite = TRUE)
