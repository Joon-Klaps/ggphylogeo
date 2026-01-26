#' Calculate data bounding box with padding
#' @noRd
get_data_bbox <- function(segs, hpd, pad = 1) {
  # Collect all coordinates
  lons <- c()
  lats <- c()

  if (!is.null(segs) && nrow(segs) > 0) {
    lons <- c(lons, segs$lon, segs$lonend)
    lats <- c(lats, segs$lat, segs$latend)
  }

  if (!is.null(hpd) && nrow(hpd) > 0) {
    lons <- c(lons, hpd$lon)
    lats <- c(lats, hpd$lat)
  }

  lons <- lons[!is.na(lons)]
  lats <- lats[!is.na(lats)]

  # Default to World View if no data
  if (length(lons) == 0 || length(lats) == 0) {
    return(list(xlim = c(-180, 180), ylim = c(-90, 90)))
  }

  # Calculate limits with padding
  # We use absolute degree padding here as requested (pad=1 means 1 degree buffer)
  lonlim <- c(min(lons) - pad, max(lons) + pad)
  latlim <- c(min(lats) - pad, max(lats) + pad)

  # Constrain to valid lat/long
  lonlim[1] <- max(lonlim[1], -180)
  lonlim[2] <- min(lonlim[2], 180)
  latlim[1] <- max(latlim[1], -90)
  latlim[2] <- min(latlim[2], 90)

  list(xlim = lonlim, ylim = latlim)
}