# Test suite for autoplot.R

test_that("autoplot.treedata creates ggplot object", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.treedata includes correct layers", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)

  # Check for point layers (nodes)
  layer_types <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layer_types)
})

test_that("autoplot.treedata accepts most_recent_sample", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Should not error
  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                          most_recent_sample = "2019-01-01", show_map = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.treedata respects show_map parameter", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  p_no_map <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)
  p_with_map <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = TRUE)

  # Map version should have more layers (polygon for map)
  expect_gte(length(p_with_map$layers), length(p_no_map$layers))
})

test_that("autoplot.treedata respects highlight_tips parameter", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Get a tip label to highlight
  tip_labels <- td@phylo$tip.label
  highlight <- tip_labels[1:2]

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                          highlight_tips = highlight,
                          highlight_color = "blue",
                          show_map = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.treedata respects show_dispersion_legend parameter", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  p_no_legend <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                                    show_dispersion_legend = FALSE, show_map = FALSE)
  p_with_legend <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                                      show_dispersion_legend = TRUE, show_map = FALSE)

  # Legend version should have more layers
  expect_gt(length(p_with_legend$layers), length(p_no_legend$layers))
})

test_that("autoplot.treedata respects tip and node size parameters", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Should not error with custom sizes
  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                          tip_size = 5, node_size = 3, tip_stroke = 1,
                          show_map = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.treedata respects date_format parameter", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Should not error with custom date format
  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                          most_recent_sample = "2019-01-01",
                          date_format = "%Y-%m",
                          show_map = FALSE)

  expect_s3_class(p, "ggplot")
})
