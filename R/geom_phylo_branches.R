#' Phylogeographic tree branches
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing branches.
#' @param ... Other arguments passed on to ggplot2::geom_curve.
#' @export
geom_phylo_branches <- function(mapping = NULL, data = NULL, stream = FALSE, include_time = FALSE, ...) {
  # Extract branches component from combined phylo_phylogeo objects
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$branches
  }

  if (!stream) {
    geom_curve_path(data = data, mapping = mapping, include_time = include_time, ...)
  } else {
    geom_stream_path(data = data, mapping = mapping, include_time = include_time, ...)
  }
}

#' Generate discretized curve points from branch data
discretize_curves <- function(data, curvature = 0.25, ncp = 30,
                              include_progress = FALSE,
                              include_time = FALSE) {

  do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]

    # Bezier control point
    midx <- (row$lon + row$lonend) / 2
    midy <- (row$lat + row$latend) / 2
    dx <- row$lonend - row$lon
    dy <- row$latend - row$lat
    cx <- midx - curvature * dy
    cy <- midy + curvature * dx

    t <- seq(0, 1, length.out = ncp)

    x <- (1 - t)^2 * row$lon +
         2 * (1 - t) * t * cx +
         t^2 * row$lonend

    y <- (1 - t)^2 * row$lat +
         2 * (1 - t) * t * cy +
         t^2 * row$latend

    out <- data.frame(
      x = x,
      y = y,
      group = i,
      progress = t
    )

    if (include_time) {
      out$time <- row$startheight + t * (row$endheight - row$startheight)
    }

    if (!include_progress) {
      out$progress <- NULL
    }

    out
  }))
}

#' Simple curve geom (plotly-compatible alternative to geom_curve)
geom_curve_path <- function(data, mapping = NULL, curvature = 0.25, ncp = 30, include_time = FALSE, ...) {
  curve_data <- discretize_curves(data, curvature, ncp, include_progress = FALSE, include_time = include_time)

  ggplot2::geom_path(
    data = curve_data,
    mapping = mapping %||% ggplot2::aes(x = x, y = y, group = group),
    colour = "black",
    lineend = "round",
    ...
  )
}

#' Stream curve geom with directional effects
geom_stream_path <- function(data, mapping = NULL, curvature = 0.25, ncp = 30,
                              global_alpha = 0.6, include_time = FALSE, ...) {
  curve_data <- discretize_curves(data, curvature, ncp, include_progress = TRUE, include_time = include_time)

  ggplot2::geom_path(
    data = curve_data,
    mapping = mapping %||% ggplot2::aes(
      x = x,
      y = y,
      group = group,
      alpha = progress,
      linewidth = progress
    ),
    ...
  )
}