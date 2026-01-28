# Test suite for utils.R

test_that("check_treedata errors on non-treedata objects", {
  expect_error(check_treedata(1), "treedata object")
  expect_error(check_treedata("string"), "treedata object")
  expect_error(check_treedata(list()), "treedata object")
  expect_error(check_treedata(data.frame()), "treedata object")
})

test_that("get_data_bbox returns valid structure", {
  # Empty inputs -> world view
  bbox <- get_data_bbox(NULL, NULL)
  expect_type(bbox, "list")
  expect_named(bbox, c("xlim", "ylim"))
  expect_equal(bbox$xlim, c(-180, 180))
  expect_equal(bbox$ylim, c(-90, 90))
})

test_that("get_data_bbox computes correct bounds from data", {
  segs <- data.frame(
    lon = c(10, 20),
    lonend = c(15, 25),
    lat = c(40, 50),
    latend = c(45, 55)
  )

  bbox <- get_data_bbox(segs, NULL, pad = 0)

  expect_equal(bbox$xlim, c(10, 25))
  expect_equal(bbox$ylim, c(40, 55))
})

test_that("get_data_bbox applies padding correctly", {
  segs <- data.frame(
    lon = c(10, 20),
    lonend = c(15, 25),
    lat = c(40, 50),
    latend = c(45, 55)
  )

  bbox <- get_data_bbox(segs, NULL, pad = 5)

  expect_equal(bbox$xlim, c(5, 30))
  expect_equal(bbox$ylim, c(35, 60))
})

test_that("get_data_bbox clamps to valid geographic range", {
  # Data near edges
  segs <- data.frame(
    lon = c(-175, 175),
    lonend = c(-170, 180),
    lat = c(-85, 85),
    latend = c(-80, 90)
  )

  bbox <- get_data_bbox(segs, NULL, pad = 10)

  expect_lte(bbox$xlim[2], 180)
  expect_gte(bbox$xlim[1], -180)
  expect_lte(bbox$ylim[2], 90)
  expect_gte(bbox$ylim[1], -90)
})

test_that("get_data_bbox combines segment and hpd data", {
  segs <- data.frame(
    lon = c(10),
    lonend = c(15),
    lat = c(40),
    latend = c(45)
  )

  hpd <- data.frame(
    lon = c(5, 30),
    lat = c(35, 60)
  )

  bbox <- get_data_bbox(segs, hpd, pad = 0)

  expect_equal(bbox$xlim, c(5, 30))
  expect_equal(bbox$ylim, c(35, 60))
})

test_that("calibrate_age returns unchanged input when mrs is NULL", {
  ages <- c(1, 2, 3)
  result <- calibrate_age(ages, NULL)
  expect_equal(result, ages)
})

test_that("calibrate_age converts to Date with Date input", {
  ages <- c(0, 1, 10)  # years before mrs
  mrs <- as.Date("2020-01-01")

  result <- calibrate_age(ages, mrs)

  expect_s3_class(result, "Date")
  expect_equal(result[1], mrs)  # 0 years before = same date
})

test_that("calibrate_age accepts character date string", {
  ages <- c(0, 1, 10)
  result <- calibrate_age(ages, "2020-01-01")

  expect_s3_class(result, "Date")
})

test_that("calibrate_age accepts numeric year", {
  ages <- c(0, 1, 10)
  result <- calibrate_age(ages, 2020)

  expect_s3_class(result, "Date")
})

test_that("calibrate_age errors on invalid mrs", {
  ages <- c(0, 1, 10)

  expect_error(calibrate_age(ages, "invalid"))
  expect_error(calibrate_age(ages, 500))  # Not a valid year
})

test_that("calibrate_age handles NA values", {
  ages <- c(0, NA, 10)
  result <- calibrate_age(ages, "2020-01-01")

  expect_s3_class(result, "Date")
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

test_that("add_dispersion_legend returns list of ggplot2 layers", {
  skip_if_not_installed("ggplot2")

  bbox <- list(xlim = c(-100, -80), ylim = c(25, 45))
  legend <- add_dispersion_legend(bbox, curvature = 0.25)

  expect_type(legend, "list")
  expect_gt(length(legend), 0)
})
