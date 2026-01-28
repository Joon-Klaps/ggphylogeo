# Test suite for build-branches.R

test_that("build_branches errors on bad input", {
  expect_error(build_branches(1), "treedata object")
  expect_error(build_branches("not_treedata"), "treedata object")
  expect_error(build_branches(list(a = 1)), "treedata object")
})

test_that("build_branches returns expected columns", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches <- build_branches(td, lon = "location2", lat = "location1")

  # Check required columns exist
  expected_cols <- c("startnode", "endnode", "lon", "lat", "lonend", "latend",
                     "ageParent", "age", "istip", "label")
  expect_true(all(expected_cols %in% names(branches)))

  # Check data types
  expect_type(branches$startnode, "integer")
  expect_type(branches$endnode, "integer")
  expect_type(branches$lon, "double")
  expect_type(branches$lat, "double")
  expect_type(branches$lonend, "double")
  expect_type(branches$latend, "double")
  expect_type(branches$istip, "logical")
  expect_type(branches$label, "character")
})

test_that("build_branches has both tips and internal branches", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches <- build_branches(td, lon = "location2", lat = "location1")

  expect_true(any(branches$istip))
  expect_true(any(!branches$istip))
})

test_that("build_branches label matches tip labels for tip branches", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches <- build_branches(td, lon = "location2", lat = "location1")
  tip_labels <- td@phylo$tip.label

  # Labels for tip branches should be in tip.label
  tips_in_branches <- branches$label[branches$istip]
  expect_true(all(tips_in_branches %in% tip_labels))
})

test_that("build_branches respects digits parameter", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches_2 <- build_branches(td, lon = "location2", lat = "location1", digits = 2)
  branches_4 <- build_branches(td, lon = "location2", lat = "location1", digits = 4)

  # Both should work without error
  expect_true(all(!is.na(branches_2$lon)))
  expect_true(all(!is.na(branches_4$lon)))
})

test_that("build_branches calibrates ages when most_recent_sample provided", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches_numeric <- build_branches(td, lon = "location2", lat = "location1")
  branches_date <- build_branches(td, lon = "location2", lat = "location1",
                                   most_recent_sample = "2019-01-01")

  expect_false(inherits(branches_numeric$age, "Date"))
  expect_true(inherits(branches_date$age, "Date"))
})

test_that("build_branches orders by age descending", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  branches <- build_branches(td, lon = "location2", lat = "location1")

  # Numeric ages should be in descending order
  ages <- as.numeric(branches$age)
  expect_true(all(diff(ages) <= 0) || all(is.na(ages)))
})
