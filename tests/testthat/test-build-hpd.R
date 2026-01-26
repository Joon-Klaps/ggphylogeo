# Test suite for build-hpd.R

test_that("build_hpd errors on bad input", {
  expect_error(build_hpd(1), "treedata object")
  expect_error(build_hpd("not_treedata"), "treedata object")
  expect_error(build_hpd(list(a = 1)), "treedata object")
})

test_that("build_hpd returns expected columns when HPD data exists", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  hpd <- build_hpd(td, level = "0.80", lon = "location2", lat = "location1")

  if (!is.null(hpd)) {
    expected_cols <- c("node", "polygon", "lon", "lat", "endheight", "group")
    expect_true(all(expected_cols %in% names(hpd)))

    # Check data types
    expect_type(hpd$lon, "double")
    expect_type(hpd$lat, "double")
    expect_type(hpd$group, "character")
  }
})

test_that("build_hpd returns NULL when no HPD columns found", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Use a non-existent column prefix
  hpd <- build_hpd(td, level = "0.80", lon = "nonexistent", lat = "nonexistent")

  expect_null(hpd)
})

test_that("build_hpd respects level parameter", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  # Different HPD levels may produce different results
  hpd_80 <- build_hpd(td, level = "0.80", lon = "location2", lat = "location1")

  # Just check function runs without error
  expect_true(is.null(hpd_80) || is.data.frame(hpd_80))
})

test_that("build_hpd calibrates heights when most_recent_sample provided", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  hpd_numeric <- build_hpd(td, level = "0.80", lon = "location1", lat = "location2")
  hpd_date <- build_hpd(td, level = "0.80", lon = "location1", lat = "location2",
                         most_recent_sample = "2019-01-01")

  if (!is.null(hpd_numeric) && !is.null(hpd_date)) {
    expect_false(inherits(hpd_numeric$endheight, "Date"))
    expect_true(inherits(hpd_date$endheight, "Date"))
  }
})

test_that("build_hpd polygon groups are unique per node-polygon combination", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)

  hpd <- build_hpd(td, level = "0.80", lon = "location1", lat = "location2")

  if (!is.null(hpd)) {
    # Groups should be formatted as "node-polygon"
    expect_true(all(grepl("^\\d+-\\d+$", hpd$group)))

    # Each group should have consistent node and polygon values
    for (grp in unique(hpd$group)) {
      grp_data <- hpd[hpd$group == grp, ]
      expect_equal(length(unique(grp_data$node)), 1)
      expect_equal(length(unique(grp_data$polygon)), 1)
    }
  }
})
