#' Phylogeographic HPD polygons
#'
#' Plots highest posterior density (HPD) uncertainty regions as filled polygons.
#' By default, polygons are smoothed using kernel smoothing for visual appeal.
#' Polygons are filled by age to match the temporal color scale of branches.
#'
#' @param mapping Aesthetic mappings created with `ggplot2::aes()`. If NULL,
#'   uses default mapping: `aes(x = lon, y = lat, group = group, fill = age)`.
#' @param data A data.frame with HPD polygon data or a `phylo_phylogeo` object.
#'   Must contain columns: lon, lat, group, age.
#' @param alpha Numeric transparency (0-1). Default 0.5.
#' @param smooth Logical; apply kernel smoothing to polygons. Default TRUE.
#'   Set FALSE for faster rendering or when smoothing causes issues.
#' @param smoothness Numeric smoothing intensity (default 1). Higher values
#'   produce smoother polygons but may distort small features.
#' @param ... Additional arguments passed to the underlying geom.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' \dontrun{
#' data(wnv_tree)
#' pgeo <- build_phylogeo(wnv_tree)
#' ggplot() + geom_phylo_hpd(data = pgeo$hpd, alpha = 0.3)
#' }
geom_phylo_hpd <- function(mapping = NULL,
                           data = NULL,
                           alpha = 0.5,
                           smooth = TRUE,
                           smoothness = 1,
                           ...) {
  # Extract HPD component from phylo_phylogeo objects
  if (inherits(data, "phylo_phylogeo")) {
    data <- data$hpd
  }

  if (is.null(data)) return(NULL)

  default_aes <- ggplot2::aes(x = lon, y = lat, group = group, fill = age)

  if (!smooth) {
    return(ggplot2::geom_polygon(
      mapping = mapping %||% default_aes,
      data = data,
      alpha = alpha,
      ...
    ))
  }

  # Convert to sf and smooth
  if (!inherits(data, "sf")) {
    data <- hpd_to_sf(data)
  }
  data <- smooth_hpd(data, smoothness = smoothness)

  ggplot2::geom_sf(
    mapping = mapping %||% ggplot2::aes(fill = age),
    data = data,
    alpha = alpha,
    colour = NA,
    ...
  )
}

#' Convert HPD data frame to sf object
#' @noRd
hpd_to_sf <- function(hpd_df, crs = 4326) {
  # Filter invalid coordinates
  hpd_df <- hpd_df[
    is.finite(hpd_df$lon) & is.finite(hpd_df$lat) &
    abs(hpd_df$lon) <= 180 & abs(hpd_df$lat) <= 90,
  ]

  groups <- unique(hpd_df$group)
  polygon_list <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    grp_data <- hpd_df[hpd_df$group == groups[i], ]
    poly <- try_create_polygon(grp_data)

    if (!is.null(poly)) {
      polygon_list[[i]] <- list(
        group = groups[i],
        age = mean(grp_data$age, na.rm = TRUE),
        geometry = poly
      )
    }
  }

  # Remove NULL entries
  polygon_list <- polygon_list[!vapply(polygon_list, is.null, logical(1))]

  if (length(polygon_list) == 0) {
    warning("No valid polygons created. Check your input data.")
    return(sf::st_sf(
      group = character(0),
      age = numeric(0),
      geometry = sf::st_sfc(crs = crs)
    ))
  }

  sf::st_sf(
    group = vapply(polygon_list, `[[`, character(1), "group"),
    age = vapply(polygon_list, `[[`, numeric(1), "age"),
    geometry = sf::st_sfc(
      lapply(polygon_list, `[[`, "geometry"),
      crs = crs
    )
  )
}

#' Try to create a valid polygon from group data
#' @noRd
try_create_polygon <- function(grp_data) {
  if (nrow(grp_data) < 3) return(NULL)

  coords <- unique(cbind(grp_data$lon, grp_data$lat))
  if (nrow(coords) < 3) return(NULL)

  # Close the polygon
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords <- rbind(coords, coords[1, ])
  }

  tryCatch({
    poly <- sf::st_polygon(list(coords))
    if (!sf::st_is_valid(poly)) {
      poly <- sf::st_make_valid(poly)
    }

    # Validate result
    if (sf::st_is_valid(poly) && !sf::st_is_empty(poly) &&
        !anyNA(sf::st_coordinates(poly))) {
      return(poly)
    }
    NULL
  }, error = function(e) NULL)
}

#' Apply smoothing to HPD sf object
#' @noRd
smooth_hpd <- function(sf_obj, smoothness = 2) {
  if (!requireNamespace("smoothr", quietly = TRUE)) {
    return(sf_obj)
  }

  # Remove invalid geometries
  sf_obj <- sf_obj[!is.na(sf::st_dimension(sf_obj$geometry)), ]

  geoms <- sf::st_geometry(sf_obj)
  smoothed <- lapply(geoms, function(g) {
    smooth_polygon(g, sf::st_crs(sf_obj), smoothness)
  })

  sf::st_geometry(sf_obj) <- sf::st_sfc(smoothed, crs = sf::st_crs(sf_obj))
  sf_obj[!is.na(sf::st_dimension(sf_obj$geometry)), ]
}

#' Smooth a single polygon
#' @noRd
smooth_polygon <- function(geom, crs, smoothness) {
  # Check if smoothable (enough vertices, non-zero extent)
  coords <- sf::st_coordinates(geom)
  n_unique <- nrow(unique(coords[, 1:2, drop = FALSE]))

  if (n_unique < 6 || diff(range(coords[, 1])) == 0 ||
      diff(range(coords[, 2])) == 0) {
    return(geom)
  }

  tryCatch({
    result <- smoothr::smooth(
      sf::st_sfc(geom, crs = crs),
      method = "ksmooth",
      smoothness = smoothness
    )[[1]]

    # Return original if smoothing introduced NAs
    if (anyNA(sf::st_coordinates(result))) geom else result
  }, error = function(e) geom)
}