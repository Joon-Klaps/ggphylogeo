#' Phylogeographic tree branches
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing branches.
#' @param stream logical; use stream plotting (default FALSE)
#' @param ... Other arguments passed on to ggplot2::geom_curve.
#' @export
geom_phylo_branches <- function(mapping = NULL, data = NULL, stream = FALSE, ...) {
  # Extract branches component from combined phylo_phylogeo objects
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$branches
  }

  if (!stream) {
    geom_curve_path(data = data, mapping = mapping, ...)
  } else {
    geom_stream_path(data = data, mapping = mapping, ...)
  }
}

#' Generate discretized curve points from branch data
#'
#' @description
#' Calculates points along a quadratic Bezier curve between two geographic coordinates.
#' Used for creating curved branch paths that can be animated.
#'
#' @param data data.frame with columns lon, lat, lonend, latend, ageParent, age
#' @param curvature numeric curvature amount (default 0.25)
#' @param ncp integer number of control points/segments per curve (default 30)
#' @param include_progress logical; if TRUE, adds 'progress' (0-1) column
#' @return A data.frame with interpolated x, y coordinates, age, and grouping info
#' @noRd
discretize_curves <- function(data, curvature = 0.25, ncp = 100,
                              include_progress = FALSE) {

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
      progress = t,
      # age interpolated along curve for gganimate transition_reveal
      age = row$ageParent + t * (row$age - row$ageParent)
    )

    if (!include_progress) {
      out$progress <- NULL
    }

    out
  }))
}

#' Simple curve geom (plotly-compatible alternative to geom_curve)
#' @noRd
geom_curve_path <- function(data, mapping = NULL, curvature = -0.25, ncp = 100, ...) {
  curve_data <- discretize_curves(data, curvature, ncp, include_progress = FALSE)

  ggplot2::geom_path(
    data = curve_data,
    mapping = mapping %||% ggplot2::aes(x = x, y = y, group = group),
    colour = "black",
    lineend = "round",
    ...
  )
}

#' Stream curve geom with dispersional effects
#' @noRd
geom_stream_path <- function(data, mapping = NULL, curvature = -0.25, ncp = 100, ...) {
  curve_data <- discretize_curves(data, curvature, ncp, include_progress = TRUE)

  list(
    ggplot2::geom_path(
      data = curve_data,
      mapping = mapping %||% ggplot2::aes(
        x = x,
        y = y,
        group = group,
        alpha = progress*3,
        linewidth = progress,
      ),
      lineend = "round",
      show.legend = FALSE,
      ...
    ),
    # Hide the progress-based alpha/linewidth from legends
    ggplot2::scale_alpha_continuous(guide = "none"),
    ggplot2::scale_linewidth_continuous(guide = "none")
  )
}