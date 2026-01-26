test_that("build_phylogeo returns a phylo_phylogeo object and geoms accept it", {
  skip_if_not_installed("ggplot2")

  # Fake branches/hpd/nodes so we don't need a full treedata object here
  branches <- data.frame(
    startnode = c(1, 2), endnode = c(2, 3),
    x = c(0, 1), y = c(0, 1), xend = c(1, 2), yend = c(1, 2),
    startheight = c(0.5, 0.6), endheight = c(0.6, 0.7), stringsAsFactors = FALSE
  )

  hpd <- data.frame(x = c(0.5, 1.5), y = c(0.5, 1.5), group = c("a", "b"), endheight = c(0.6, 0.7), stringsAsFactors = FALSE)

  nodes <- data.frame(node = 1:3, x = c(0,1,2), y = c(0,1,2), endheight = c(0.5,0.6,0.7), istip = c(FALSE, TRUE, TRUE), stringsAsFactors = FALSE)

  phylo_phylogeo <- structure(list(branches = branches, hpd = hpd, nodes = nodes), class = "phylo_phylogeo")

  p <- ggplot2::ggplot() +
    geom_phylo_branches(data = phylo_phylogeo) +
    geom_phylo_hpd(data = phylo_phylogeo) +
    geom_phylo_nodes(data = phylo_phylogeo)

  layers <- p$layers

  expect_true(any(sapply(layers, function(l) inherits(l$geom, "GeomSegment"))))
  expect_true(any(sapply(layers, function(l) inherits(l$geom, "GeomPolygon"))))
  expect_true(any(sapply(layers, function(l) inherits(l$geom, "GeomPoint"))))
})


test_that("endheight is converted to Date when most_recent_sample provided", {
  skip_if_not_installed("treeio")

  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)
  mrs <- as.Date("2019-01-01")

  segs <- build_branches(td, lon = "location2", lat = "location1", most_recent_sample = mrs)
  expect_true(inherits(segs$endheight, "Date") || inherits(segs$endheight, "Date"))

  polys <- build_hpd(td, level = "0.80", lon = "location1", lat = "location2", most_recent_sample = mrs)
  expect_true(inherits(polys$endheight, "Date"))

  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1", most_recent_sample = mrs)
  expect_true(inherits(pgeo$nodes$endheight, "Date"))
})


test_that("most_recent_sample accepts character 'YYYY-MM-DD'", {
  skip_if_not_installed("treeio")
  tree_file <- system.file("extdata", "WNV_cauchy.MCC.tree", package = "ggphylogeo")
  td <- treeio::read.beast(tree_file)
  mrs_str <- "2017-04-22"

  segs <- build_branches(td, lon = "location2", lat = "location1", most_recent_sample = mrs_str)
  expect_true(inherits(segs$endheight, "Date"))

  polys <- build_hpd(td, level = "0.80", lon = "location1", lat = "location2", most_recent_sample = mrs_str)
  expect_true(inherits(polys$endheight, "Date"))

  pgeo <- build_phylogeo(td, lon = "location2", lat = "location1", most_recent_sample = mrs_str)
  expect_true(inherits(pgeo$nodes$endheight, "Date"))
})