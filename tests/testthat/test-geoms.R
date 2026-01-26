# Test suite for geom functions

test_that("geom_phylo_branches extracts branches from phylo_phylogeo", {
  skip_if_not_installed("ggplot2")

  branches <- data.frame(
    lon = c(0, 1), lat = c(0, 1),
    lonend = c(1, 2), latend = c(1, 2),
    startheight = c(0.5, 0.6),
    endheight = c(0.6, 0.7),
    stringsAsFactors = FALSE
  )

  pgeo <- structure(list(branches = branches), class = "phylo_phylogeo")

  p <- ggplot2::ggplot() + geom_phylo_branches(data = pgeo)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_phylo_branches works with data.frame directly", {
  skip_if_not_installed("ggplot2")

  branches <- data.frame(
    lon = c(0, 1), lat = c(0, 1),
    lonend = c(1, 2), latend = c(1, 2),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() + geom_phylo_branches(data = branches)

  expect_s3_class(p, "ggplot")
})

test_that("geom_phylo_hpd extracts hpd from phylo_phylogeo", {
  skip_if_not_installed("ggplot2")

  hpd <- data.frame(
    lon = c(0.5, 1.5, 1.0),
    lat = c(0.5, 0.5, 1.0),
    group = c("a", "a", "a"),
    endheight = c(0.6, 0.6, 0.6),
    alpha = c(0.2, 0.2, 0.2),
    stringsAsFactors = FALSE
  )

  pgeo <- structure(list(hpd = hpd), class = "phylo_phylogeo")

  p <- ggplot2::ggplot() + geom_phylo_hpd(data = pgeo)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_phylo_nodes extracts nodes from phylo_phylogeo", {
  skip_if_not_installed("ggplot2")

  nodes <- data.frame(
    node = 1:3,
    lon = c(0, 1, 2),
    lat = c(0, 1, 2),
    endheight = c(0.5, 0.6, 0.7),
    istip = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  pgeo <- structure(list(nodes = nodes), class = "phylo_phylogeo")

  p <- ggplot2::ggplot() + geom_phylo_nodes(data = pgeo)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_phylo_nodes works with data.frame directly", {
  skip_if_not_installed("ggplot2")

  nodes <- data.frame(
    lon = c(0, 1, 2),
    lat = c(0, 1, 2),
    endheight = c(0.5, 0.6, 0.7),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() + geom_phylo_nodes(data = nodes)

  expect_s3_class(p, "ggplot")
})

test_that("geom functions accept custom mappings", {
  skip_if_not_installed("ggplot2")

  branches <- data.frame(
    x = c(0, 1), y = c(0, 1),
    xend = c(1, 2), yend = c(1, 2),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    geom_phylo_branches(
      data = branches,
      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
    )

  expect_s3_class(p, "ggplot")
})
