#' Autoplot continuous phylogeography with optional animation
#'
#' Creates a ggplot2 visualization of continuous phylogeographic data from
#' BEAST analyses. Supports various customization options including map
#' backgrounds, tip highlighting, dispersion legends, and animated accumulation
#' over time.
#'
#' @param object treedata object with phylogeographic annotations
#' @param debug logical; emit debug messages
#' @param show_map logical; overlay map background
#' @param lat name of latitude column
#' @param lon name of longitude column
#' @param map_fill fill color for map polygons
#' @param map_colour outline color for map polygons
#' @param height_branches column used for branch ages
#' @param height_hpd column used for HPD ages
#' @param level HPD level (e.g., "0.80" for 80% HPD)
#' @param digits decimal rounding for coordinates
#' @param map_alpha transparency for map polygons (0-1)
#' @param map_pad padding (degrees) for map extent
#' @param map_name map database name (e.g., "world", "usa", "state")
#' @param most_recent_sample date/year used to calibrate age to real
#'   dates. Can be Date, POSIXt, numeric year (e.g., 2019), or character
#'   "YYYY-MM-DD"
#' @param curvature curvature of branch curves (default 0.3)
#' @param arrow arrow specification for branches (see grid::arrow)
#' @param show_dispersion_legend logical; show mini legend explaining branch
#'   dispersion (default FALSE)
#' @param highlight_tips character vector of tip labels to highlight
#' @param highlight_color color for highlighted tips (default "red")
#' @param tip_size size of tip nodes (default 3.5)
#' @param node_size size of internal nodes (default 3)
#' @param tip_stroke stroke width for tip nodes (default 1)
#' @param date_format format string for date labels (default "%Y")
#' @param stream logical; use stream plotting (default FALSE)
#' @param animate logical; create animated plot accumulating over time (default FALSE)
#' @param nframes number of frames for animation (default 100)
#' @param fps frames per second for animation (default 10)
#' @param shadow_alpha alpha transparency for accumulated past data (default 0.3)
#' @param shadow_size size multiplier for accumulated past data (default 0.8)
#' @param shadow_colour colour for accumulated past data (default NULL, uses same as current)
#' @param end_pause number of frames to pause at end of animation (default 20)
#' @param renderer gganimate renderer function (default gifski_renderer())
#' @param ... passed to geoms
#' @return A ggplot2 object (if animate=FALSE) or gganim object (if animate=TRUE)
#' @export
autoplot.treedata <- function(
  object,
  debug = getOption("ggphylogeo.debug", FALSE),
  show_map = TRUE,
  lat = "location1",
  lon = "location2",
  map_fill = "white",
  map_colour = "gray50",
  height_branches = "height_mean",
  height_hpd = "height_median",
  level = "0.80",
  digits = 2,

  map_pad = 1,
  map_name = "world",
  most_recent_sample = NULL,
  curvature = -0.25,
  arrow = NULL,
  smooth = TRUE,
  show_dispersion_legend = TRUE,
  highlight_tips = NULL,
  highlight_color = "red",
  tip_size = 3.5,
  node_size = 3,
  tip_stroke = 1,
  date_format = "%Y",
  stream = FALSE,

  # Animation parameters
  animate = FALSE,
  nframes = 100,
  fps = 10,
  renderer = NULL,
  ...
) {

  if (animate && smooth){
    warning("Parameter 'smooth' is ignored when 'animate=TRUE'; animation uses transition_reveal, incompatible with smoothing.")
    smooth <- FALSE
  }

  ## ---- Build phylogeographic data ------------------------------------------
  pgeo <- build_phylogeo(
    object,
    lat = lat,
    lon = lon,
    height_branches = height_branches,
    height_hpd = height_hpd,
    level = level,
    digits = digits,
    most_recent_sample = most_recent_sample,
    debug = debug
  )

  bbox <- pgeo$viewbox

  if (debug) {
    message(sprintf(
      "autoplot.treedata: branches=%d rows; hpd=%d rows; nodes=%d rows",
      nrow(pgeo$branches), nrow(pgeo$hpd), nrow(pgeo$nodes)
    ))
  }

  ## ---- Validate data for animation ----------------------------------------
  if (isTRUE(animate)) {
    # Check for NA values in age across all data
    na_counts <- list(
      nodes = if (!is.null(pgeo$nodes)) sum(is.na(pgeo$nodes$age)) else 0,
      branches = if (!is.null(pgeo$branches)) sum(is.na(pgeo$branches$age)) else 0,
      hpd = if (!is.null(pgeo$hpd)) sum(is.na(pgeo$hpd$age)) else 0
    )

    if (debug) {
      message("Checking data validity for animation:")
      message(sprintf("  NA values in nodes$age: %d", na_counts$nodes))
      message(sprintf("  NA values in branches$age: %d", na_counts$branches))
      message(sprintf("  NA values in hpd$age: %d", na_counts$hpd))
    }

    # Remove rows with NA age values
    if (!is.null(pgeo$nodes) && na_counts$nodes > 0) {
      if (debug) message("  Removing nodes with NA age")
      pgeo$nodes <- pgeo$nodes[!is.na(pgeo$nodes$age), , drop = FALSE]
    }

    if (!is.null(pgeo$branches) && na_counts$branches > 0) {
      if (debug) message("  Removing branches with NA age")
      pgeo$branches <- pgeo$branches[!is.na(pgeo$branches$age), , drop = FALSE]
    }

    if (!is.null(pgeo$hpd) && na_counts$hpd > 0) {
      if (debug) message("  Removing HPD regions with NA age")
      pgeo$hpd <- pgeo$hpd[!is.na(pgeo$hpd$age), , drop = FALSE]
    }

    # Check if we have any data left
    if (is.null(pgeo$nodes) || nrow(pgeo$nodes) == 0) {
      stop("No valid nodes with non-NA age values for animation")
    }

    # Check time range
    time_range <- range(pgeo$nodes$age, na.rm = TRUE)
    if (debug) {
      message(sprintf("  Time range: [%s, %s]",
                     format(time_range[1]), format(time_range[2])))
    }

    if (time_range[1] == time_range[2]) {
      warning("All age values are the same - animation will show single time point")
    }
  }

  ## ---- Compute plotting extent ---------------------------------------------
  bbox <- get_data_bbox(pgeo$branches, pgeo$hpd, pad = map_pad)

  if (debug) {
    message(sprintf(
      "Viewport x[%.2f, %.2f], y[%.2f, %.2f]",
      bbox$xlim[1], bbox$xlim[2],
      bbox$ylim[1], bbox$ylim[2]
    ))
  }

  ## ---- Initialize plot -----------------------------------------------------
  p <- ggplot2::ggplot()

  ## ---- Map background ------------------------------------------------------
  if (isTRUE(show_map)) {
    map_data <- tryCatch(
      ggplot2::map_data(map_name),
      error = function(e) {
        warning("Could not load map data: ", e$message)
        NULL
      }
    )

    if (!is.null(map_data)) {
      p <- p + ggplot2::geom_polygon(
        data = map_data,
        ggplot2::aes(long, lat, group = group),
        fill = "white",
        colour = "gray50",
        inherit.aes = FALSE
      )
    }
  }

  ## ---- Phylogeographic layers ----------------------------------------------
  # HPD polygons (uncertainty regions)
  if (!is.null(pgeo$hpd) && nrow(pgeo$hpd) > 0) {
    p <- p + geom_phylo_hpd(
      data = pgeo$hpd,
      alpha = 0.2,
      linewidth = 0.4,
      smooth = smooth
    )
  }

  # Branch curves
  if (!is.null(pgeo$branches) && nrow(pgeo$branches) > 0) {
    branches <- pgeo$branches
    moving <- (branches$lon != branches$lonend) |
              (branches$lat != branches$latend)
    moving <- moving & !is.na(moving)

    if (any(moving)) {
      p <- p + geom_phylo_branches(
        data = branches[moving, , drop = FALSE],
        stream = stream,
        curvature = curvature,
        arrow = arrow,
        ...
      )
    }
  }

  ## ---- Node layers (tips vs internal nodes) --------------------------------
  if (!is.null(pgeo$nodes) && nrow(pgeo$nodes) > 0 && animate == FALSE) {
    nodes <- pgeo$nodes

    # Separate tips and internal nodes
    tips_data <- nodes[nodes$istip, , drop = FALSE]
    internal_data <- nodes[!nodes$istip, , drop = FALSE]

    # Add highlighting column for tips
    if (!is.null(highlight_tips) && length(highlight_tips) > 0) {
      tips_data$highlighted <- tips_data$label %in% highlight_tips
    } else {
      tips_data$highlighted <- FALSE
    }

    # Internal nodes: no stroke, smaller size
    if (nrow(internal_data) > 0) {
      p <- p + ggplot2::geom_point(
        data = internal_data,
        ggplot2::aes(x = lon, y = lat, fill = age),
        shape = 21,
        size = node_size,
        stroke = tip_stroke/2,
        inherit.aes = FALSE
      )
    }

    # Tips: with stroke, larger size
    if (nrow(tips_data) > 0) {
      # Non-highlighted tips
      normal_tips <- tips_data[!tips_data$highlighted, , drop = FALSE]
      highlighted_tips <- tips_data[tips_data$highlighted, , drop = FALSE]

      if (nrow(normal_tips) > 0) {
        p <- p + ggplot2::geom_point(
          data = normal_tips,
          ggplot2::aes(x = lon, y = lat, fill = age),
          shape = 21,
          size = tip_size,
          stroke = tip_stroke,
          colour = "black",
          inherit.aes = FALSE
        )
      }

      # Highlighted tips (in specified color)
      if (nrow(highlighted_tips) > 0) {
        p <- p + ggplot2::geom_point(
          data = highlighted_tips,
          ggplot2::aes(x = lon, y = lat, fill = age),
          shape = 21,
          size = tip_size * 1.3,
          stroke = tip_stroke * 2,
          colour = highlight_color,
          inherit.aes = FALSE
        )
      }
    }
  }

  ## ---- Dispersion legend ----------------------------------------------------
  if (isTRUE(show_dispersion_legend)) {
    p <- p + add_dispersion_legend(bbox, curvature)
  }

  ## ---- Coordinates ---------------------------------------------------------
  p <- p + ggplot2::coord_sf(
    xlim = bbox$xlim,
    ylim = bbox$ylim
  )

  ## ---- Fill scale ----------------------------------------------------------
  # Determine if age is Date type for proper formatting
  is_date_scale <- FALSE
  if (!is.null(pgeo$nodes) && inherits(pgeo$nodes$age, "Date")) {
    is_date_scale <- TRUE
  } else if (!is.null(pgeo$hpd) && inherits(pgeo$hpd$age, "Date")) {
    is_date_scale <- TRUE
  } else if (!is.null(pgeo$branches) && inherits(pgeo$branches$age, "Date")) {
    is_date_scale <- TRUE
  }

  if (is_date_scale) {
    if (debug) message("Adding date scale for fill aesthetic")
    p <- p + ggplot2::scale_fill_viridis_c(
      name = "Date",
      direction= -1,
      labels = function(x) format(as.Date(x, origin = "1970-01-01"), date_format)
    )
  } else {
    # Numeric scale - show as years if values suggest year-like data
    p <- p + ggplot2::scale_fill_viridis_c(name = "Age")
  }

  ## ---- Theme ---------------------------------------------------------------
  p <- p +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_phylogeo() +
    guides_phylogeo()

  ## ---- Animation -----------------------------------------------------------
  if (isTRUE(animate)) {
    if (!requireNamespace("gganimate", quietly = TRUE)) {
      stop("Package 'gganimate' is required for animation. Install with install.packages('gganimate')")
    }

    if (debug) message("Adding animation with transition_reveal")

    # Animation parameters shadow_alpha/size/colour are ignored as transition_reveal
    # handles the accumulation of branches and nodes automatically.

    # Add animation layers
    if (is_date_scale) {
      # For date scales, use the date directly
      p <- p +
        gganimate::transition_reveal(age) +
        ggplot2::labs(
          title = glue::glue("Date: {{format(as.Date(frame_along, origin='1970-01-01'), '{date_format}')}}")
        )
    } else {
      # For numeric scales
      p <- p +
        gganimate::transition_reveal(age) +
        ggplot2::labs(
          title = "Time: {round(frame_along, 1)}"
        )
    }

    # Set default renderer if not specified
    if (is.null(renderer)) {
      if (requireNamespace("gifski", quietly = TRUE)) {
        renderer <- gganimate::gifski_renderer()
      } else {
        warning("Package 'gifski' not found. Using default renderer. Install with install.packages('gifski') for better performance.")
        renderer <- gganimate::magick_renderer()
      }
    }

    # Render the animation
    if (debug) {
      message(sprintf("Rendering animation with %d frames at %d fps", nframes, fps))
    }

    anim <- gganimate::animate(
      p,
      nframes = nframes,
      fps = fps,
      end_pause = end_pause,
      renderer = renderer
    )

    return(anim)
  }

  # Return static plot if not animating
  return(p)
}