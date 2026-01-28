#' Phylogeographic HPD polygons
#'
#' @param mapping Aesthetic mappings, created with ggplot2::aes().
#' @param data A data frame or `phylo_phylogeo` object containing HPD polygons.
#' @param alpha numeric transparency level (0-1), default 0.5
#' @param smooth logical; whether to smooth HPD polygons (default TRUE)
#' @param smoothness numeric smoothing parameter (passed to smoothr)
#' @param ... Other arguments passed to ggplot2::geom_sf.
#' @export
geom_phylo_hpd <- function(mapping = NULL,
                           data = NULL,
                           alpha = 0.5,
                           smooth = TRUE,
                           smoothness = 1,
                           ...) {
  # Accept combined `phylo_phylogeo` objects and extract the hpd component
  if (!is.null(data) && inherits(data, "phylo_phylogeo")) {
    data <- data$hpd
  }

  if (!smooth){
    ggplot2::geom_polygon(
      mapping = mapping %||% ggplot2::aes(
        x = lon, y = lat, group = group, fill = age
      ),
      data = data,
      alpha = alpha,
      ...
    )
  } else {
    if (!inherits(data, "sf")) {
      data <- .hpd_df_to_sf(data)
    }
    data <- .smooth_hpd_sf(data, smoothness = smoothness)
    ggplot2::geom_sf(
      mapping = mapping %||% ggplot2::aes(fill = age),
      data = data,
      alpha = alpha,
      colour = NA,
      ...
    )
  }

}

.hpd_df_to_sf <- function(hpd_df, crs = 4326) {

  # Filter invalid coordinates
  hpd_df <- hpd_df %>%
    dplyr::filter(
      is.finite(lon),
      is.finite(lat),
      abs(lon) <= 180,
      abs(lat) <= 90
    )

  # Create a list to store valid polygons
  polygon_list <- list()

  groups <- unique(hpd_df$group)

  for (grp in groups) {
    grp_data <- hpd_df %>% dplyr::filter(group == grp)

    # Check if we have enough data
    if (nrow(grp_data) < 3) next

    coords <- cbind(grp_data$lon, grp_data$lat)
    unique_coords <- unique(coords)

    # Need at least 3 unique points
    if (nrow(unique_coords) < 3) next

    # Close the polygon
    if (!all(unique_coords[1, ] == unique_coords[nrow(unique_coords), ])) {
      unique_coords <- rbind(unique_coords, unique_coords[1, ])
    }

    # Create polygon
    tryCatch({
      poly <- sf::st_polygon(list(unique_coords))

      if (!sf::st_is_valid(poly)) {
        poly <- sf::st_make_valid(poly)
      }

      # Only add if valid and no NAs in coordinates
      coords_check <- sf::st_coordinates(poly)
      if (sf::st_is_valid(poly) &&
          !sf::st_is_empty(poly) &&
          !anyNA(coords_check)) {  # KEY CHECK HERE
        polygon_list[[length(polygon_list) + 1]] <- list(
          group = grp,
          age = mean(grp_data$age, na.rm = TRUE),
          geometry = poly
        )
      }
    }, error = function(e) {
      # Silently skip problematic polygons
    })
  }

  # Convert to sf object
  if (length(polygon_list) == 0) {
    warning("No valid polygons created. Check your input data.")
    return(sf::st_sf(
      group = character(0),
      age = numeric(0),
      geometry = sf::st_sfc(crs = crs)
    ))
  }

  sf_obj <- data.frame(
    group = sapply(polygon_list, function(x) x$group),
    age = sapply(polygon_list, function(x) x$age)
  ) %>%
    sf::st_sf(
      geometry = sf::st_sfc(
        lapply(polygon_list, function(x) x$geometry),
        crs = crs
      )
    )

  # FINAL SIMPLE FILTER - remove any rows with NA in geometry coordinates
  sf_obj <- sf_obj[!is.na(sf::st_dimension(sf_obj$geometry)), ]

  sf_obj
}

.smooth_hpd_sf <- function(sf_obj, smoothness = 2) {
  if (!requireNamespace("smoothr", quietly = TRUE)) {
    return(sf_obj)
  }

  # Filter out any problematic geometries BEFORE smoothing
  sf_obj <- sf_obj[!is.na(sf::st_dimension(sf_obj$geometry)), ]

  geoms <- sf::st_geometry(sf_obj)

  smoothed <- lapply(seq_along(geoms), function(i) {
    g <- geoms[[i]]

    if (!.is_smoothable_polygon(g)) {
      return(g)
    }

    tryCatch({
      result <- smoothr::smooth(
        sf::st_sfc(g, crs = sf::st_crs(sf_obj)),
        method = "ksmooth",
        smoothness = smoothness
      )[[1]]

      # Check if smoothing introduced NAs
      coords_check <- sf::st_coordinates(result)
      if (anyNA(coords_check)) {
        return(g)  # Return original if smoothing created NAs
      }

      return(result)
    }, error = function(e) {
      return(g)
    })
  })

  sf::st_geometry(sf_obj) <- sf::st_sfc(smoothed, crs = sf::st_crs(sf_obj))

  # Final filter after smoothing
  sf_obj <- sf_obj[!is.na(sf::st_dimension(sf_obj$geometry)), ]

  sf_obj
}

.is_smoothable_polygon <- function(geom, min_vertices = 6) {
  coords <- sf::st_coordinates(geom)
  n_unique <- nrow(unique(coords[, 1:2, drop = FALSE]))

  n_unique >= min_vertices &&
    diff(range(coords[, 1])) > 0 &&
    diff(range(coords[, 2])) > 0
}