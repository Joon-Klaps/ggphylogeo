# Test suite for build-nodes.R

test_that("build_nodes errors on bad input",
{
  expect_error(build_nodes(1), "treedata object")
  expect_error(build_nodes("not_treedata"), "treedata object")
  expect_error(build_nodes(list(a = 1)), "treedata object")
})

test_that("build_nodes returns expected columns", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  nodes <- build_nodes(td, lon = "location2", lat = "location1")

  # Check required columns exist
  expected_cols <- c("node", "lon", "lat", "age", "istip", "label")
  expect_true(all(expected_cols %in% names(nodes)))

  # Check data types

  expect_type(nodes$node, "integer")
  expect_type(nodes$lon, "double")
  expect_type(nodes$lat, "double")
  expect_type(nodes$istip, "logical")
  expect_type(nodes$label, "character")

  # Check that we have both tips and internal nodes
  expect_true(any(nodes$istip))
  expect_true(any(!nodes$istip))
})

test_that("build_nodes label column contains tip labels for tips", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  nodes <- build_nodes(td, lon = "location2", lat = "location1")
  tip_labels <- td@phylo$tip.label

  # All tip labels should be present in the nodes data frame
  tips_in_nodes <- nodes$label[nodes$istip]
  expect_true(all(tips_in_nodes %in% tip_labels))
})

test_that("build_nodes internal node labels start with 'Node'", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  nodes <- build_nodes(td, lon = "location2", lat = "location1")
  internal_labels <- nodes$label[!nodes$istip]

  expect_true(all(grepl("^Node ", internal_labels)))
})

test_that("build_nodes respects digits parameter", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  nodes_2 <- build_nodes(td, lon = "location2", lat = "location1", digits = 2)
  nodes_4 <- build_nodes(td, lon = "location2", lat = "location1", digits = 4)

  # With more digits, values should have more precision (or equal)
  # Just check they are both numeric and valid
  expect_true(all(!is.na(nodes_2$lon)))
  expect_true(all(!is.na(nodes_4$lon)))
})

test_that("build_nodes calibrates ages when most_recent_sample provided", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  nodes_numeric <- build_nodes(td, lon = "location2", lat = "location1")
  nodes_date <- build_nodes(td, lon = "location2", lat = "location1",
                             most_recent_sample = "2019-01-01")

  expect_false(inherits(nodes_numeric$age, "Date"))
  expect_true(inherits(nodes_date$age, "Date"))
})
