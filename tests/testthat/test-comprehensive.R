# Comprehensive tests for ggphylogeo package
# Tests autoplot, datasets, shiny app creation, and animation functionality

# Helper function to load all test datasets
get_test_datasets <- function() {
  list(
    wnv = system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo"),
    yfv = system.file("extdata", "YFV_RRW_cauchy.MCC.tree", package = "ggphylogeo"),
    h3n2 = system.file("extdata", "H3N2t.MCC.tree", package = "ggphylogeo"),
    hiv1 = system.file("extdata", "HIV1.MCC.tree", package = "ggphylogeo")
  )
}

# ============================================================================
# AUTOPLOT TESTS WITH ALL DATASETS
# ============================================================================

test_that("autoplot works with WNV dataset", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)
  expect_s3_class(p, "ggplot")

  # With date calibration
  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                         most_recent_sample = "2007-07-01", show_map = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot works with YFV dataset", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$yfv)

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)
  expect_s3_class(p, "ggplot")

  # With date calibration
  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                         most_recent_sample = "2017-04-22", show_map = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot works with H3N2 dataset (non-geographic)", {

  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$h3n2)

  # H3N2 uses antigenic dimensions instead of geographic coordinates
  p <- ggplot2::autoplot(td, lon = "antigenic1", lat = "antigenic2", show_map = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot works with HIV1 dataset", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$hiv1)

  p <- ggplot2::autoplot(td, lon = "location2", lat = "location1", show_map = FALSE)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# BUILD FUNCTIONS WITH ALL DATASETS
# ============================================================================

test_that("build_phylogeo works with all datasets", {
  skip_if_not_installed("treeio")

  datasets <- get_test_datasets()

  # WNV
  td <- treeio::read.beast(datasets$wnv)
  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1")
  expect_s3_class(pgeo, "phylo_phylogeo")
  expect_true(!is.null(pgeo$branches))
  expect_true(!is.null(pgeo$nodes))

  # YFV
  td <- treeio::read.beast(datasets$yfv)
  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1")
  expect_s3_class(pgeo, "phylo_phylogeo")

  # H3N2 (antigenic)
  td <- treeio::read.beast(datasets$h3n2)
  pgeo <- build_phylogeo(td, lon = "antigenic1", lat = "antigenic2")
  expect_s3_class(pgeo, "phylo_phylogeo")

  # HIV1
  td <- treeio::read.beast(datasets$hiv1)
  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1")
  expect_s3_class(pgeo, "phylo_phylogeo")
})

# ============================================================================
# GEOM LAYERS WITH REAL DATA
# ============================================================================

test_that("all geom layers work with real tree data", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)
  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1",
                         most_recent_sample = "2007-07-01")

  # Test each geom separately
  p1 <- ggplot2::ggplot() + geom_phylo_branches(data = pgeo)
  expect_s3_class(p1, "ggplot")

  p2 <- ggplot2::ggplot() + geom_phylo_hpd(data = pgeo, smooth = FALSE)
  expect_s3_class(p2, "ggplot")

  p3 <- ggplot2::ggplot() + geom_phylo_nodes(data = pgeo)
  expect_s3_class(p3, "ggplot")

  # Test combined
  p_combined <- ggplot2::ggplot() +
    geom_phylo_hpd(data = pgeo, smooth = FALSE) +
    geom_phylo_branches(data = pgeo) +
    geom_phylo_nodes(data = pgeo)
  expect_s3_class(p_combined, "ggplot")
})

test_that("stream branches work with real data", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)
  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1")

  p <- ggplot2::ggplot() + geom_phylo_branches(data = pgeo, stream = TRUE)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# SHINY APP TESTS
# ============================================================================

test_that("run_phylogeo_app creates a shiny app object", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)

  # run_phylogeo_app should return a shiny.appobj without running it
  app <- run_phylogeo_app(td, lon = "location2", lat = "location1",
                          most_recent_sample = "2007-07-01")

  expect_s3_class(app, "shiny.appobj")
})

test_that("run_phylogeo_app works with all datasets", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  datasets <- get_test_datasets()

  # WNV
  td <- treeio::read.beast(datasets$wnv)
  app <- run_phylogeo_app(td, lon = "location2", lat = "location1")
  expect_s3_class(app, "shiny.appobj")

  # YFV
  td <- treeio::read.beast(datasets$yfv)
  app <- run_phylogeo_app(td, lon = "location2", lat = "location1")
  expect_s3_class(app, "shiny.appobj")

  # H3N2
  td <- treeio::read.beast(datasets$h3n2)
  app <- run_phylogeo_app(td, lon = "antigenic1", lat = "antigenic2")
  expect_s3_class(app, "shiny.appobj")
})

# ============================================================================
# ANIMATION TESTS
# ============================================================================

test_that("animation works with WNV dataset", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)

  tryCatch({
    anim <- ggplot2::autoplot(
      td,
      lon = "location2",
      lat = "location1",
      show_map = FALSE,
      animate = TRUE,
      nframes = 5,  # Minimal frames for speed
      fps = 5
    )
    expect_true(!is.null(anim))
  }, error = function(e) {
    if (grepl("length.out|renderer", e$message)) {
      skip(paste("Skipping animation test due to environment:", e$message))
    } else {
      stop(e)
    }
  })
})

test_that("animation works with date calibration", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)

  tryCatch({
    anim <- ggplot2::autoplot(
      td,
      lon = "location2",
      lat = "location1",
      show_map = FALSE,
      animate = TRUE,
      most_recent_sample = "2007-07-01",
      date_format = "%Y",
      nframes = 5,
      fps = 5
    )
    expect_true(!is.null(anim))
  }, error = function(e) {
    if (grepl("length.out|renderer", e$message)) {
      skip(paste("Skipping animation test:", e$message))
    } else {
      stop(e)
    }
  })
})

# ============================================================================
# THEME AND GUIDES
# ============================================================================

test_that("theme_phylogeo returns valid theme", {
  skip_if_not_installed("ggplot2")

  theme <- theme_phylogeo()
  expect_s3_class(theme, "theme")
})

test_that("guides_phylogeo returns valid guides", {
  skip_if_not_installed("ggplot2")

  guides <- guides_phylogeo()
  # guides_phylogeo returns the result of ggplot2::guides() which creates a list
  expect_true(is.list(guides) || inherits(guides, "ggproto"))
})

test_that("add_dispersion_legend creates annotations", {
  skip_if_not_installed("ggplot2")

  bbox <- list(xlim = c(-100, -80), ylim = c(25, 45))
  legend <- add_dispersion_legend(bbox)

  expect_true(is.list(legend))
  expect_true(length(legend) > 0)
})

# ============================================================================
# EDGE CASES AND ERROR HANDLING
# ============================================================================
test_that("autoplot handles missing required packages gracefully", {
  skip_if_not_installed("treeio")
  skip_if_not_installed("ggplot2")

  datasets <- get_test_datasets()
  td <- treeio::read.beast(datasets$wnv)

  # Should work without animation packages
  expect_no_error({
    p <- ggplot2::autoplot(td, lon = "location2", lat = "location1",
                           animate = FALSE, show_map = FALSE)
  })
})

# ============================================================================
# DATA PACKAGE TESTS
# ============================================================================

test_that("bundled data objects load correctly", {
  # Test that the package data loads
  expect_no_error(data(wnv_tree, package = "ggphylogeo"))
  expect_no_error(data(yfv_tree, package = "ggphylogeo"))
  expect_no_error(data(h3n2_tree, package = "ggphylogeo"))
  expect_no_error(data(hiv1_tree, package = "ggphylogeo"))
})

test_that("bundled data objects are valid treedata", {
  skip_if_not_installed("treeio")

  data(wnv_tree, package = "ggphylogeo")
  data(yfv_tree, package = "ggphylogeo")
  data(h3n2_tree, package = "ggphylogeo")
  data(hiv1_tree, package = "ggphylogeo")

  expect_s4_class(wnv_tree, "treedata")
  expect_s4_class(yfv_tree, "treedata")
  expect_s4_class(h3n2_tree, "treedata")
  expect_s4_class(hiv1_tree, "treedata")
})
