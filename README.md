# ggphylogeo

ggplot2 extensions for visualising continuous phylogeographic
reconstructions from BEAST.

## Installation

```r
# remotes::install_github("Joon-Klaps/ggphylogeo")
```

## ✨ Contributing & Development

Any contributions are welcome! Please fork the repository and
submit a pull request.

To load all the scripts individually during development, use:

```r
devtools::load_all()
```

For building and checking the package, use:

```r
devtools::build()
devtools::check()
```

For running tests, use:

```r
devtools::test()
```

## Quickstart

Visualize a tree file immediately with `autoplot`:

```r
library(ggphylogeo)
library(treeio)

# Load your BEAST MCC tree
tree <- read.beast("path/to/beast.tree")
# Basic plot
autoplot(tree)

# Calibrate dates and formatting
autoplot(tree,
  most_recent_sample = "2000-09-01",
  date_format = "%Y-%m",
  map_fill = "antiquewhite"
)
```

![inst/extdata/yfv.png](inst/extdata/yfv.png)

## Detailed Plotting

Three main layers are used to do so:

- `geom_phylo_branches()`: to plot the branches of the tree
- `geom_phylo_nodes()`: to plot the nodes and tips of the tree
- `geom_phylo_hpd()`: to plot the uncertainty regions (highest posterior density, HPD)

The relevant data is extracted from the tree object:
`build_phylogeo()` function prepares the data for plotting, returing:
`branches`, `nodes`, and `hpd` data frames (as well as `viewbox` to set map limits).

```r
library(ggphylogeo)
library(treeio)
library(ggplot2)

# load an example tree
data("wnv_tree")

# extract the phylogeographic components
pgeo <- build_phylogeo(wnv_tree, most_recent_sample = "2007-07-01")

ggplot() +
    geom_polygon(data = map_data("world"), aes(x=long, y=lat, group=group), fill="white", color="lightgray") +
    geom_phylo_hpd(data = pgeo$hpd) +
    geom_phylo_branches(data = pgeo$branches) +
    geom_phylo_nodes(data = pgeo$nodes) +
    coord_quickmap(xlim = pgeo$viewbox$xlim, ylim = pgeo$viewbox$ylim) +
    scale_fill_viridis_c(name="Year", direction= -1,
      labels = function(x) format(as.Date(x), "%Y"),
      breaks =  seq(as.Date("1997-01-01"), as.Date("2008-01-01"),by = "2 year")
    ) +
    theme_phylogeo() +
    guides_phylogeo()
```

![wnv_animation.gif](inst/extdata/wnv_animation.gif)

For more control, you can build the plot layer by layer using `ggphylogeo` geoms and standard `ggplot2` functions. This allows using custom maps.

<!-- Update example to show how to use a custom map from naturealearthdata -->

```r
library(ggplot2)

# Prepare the data
# Extract the components using build_phylogeo
pgeo <- build_phylogeo(tree, most_recent_sample = "2000-09-01")

# Get a custom map (e.g. USA states)
usa_map <- map_data("state")

# Build the plot
p <- ggplot() +
  # 1. Custom background map
  geom_polygon(data = usa_map, aes(long, lat, group = group),
               fill = "grey90", colour = "white") +
  # 2. Add uncertainty regions (HPD)
  geom_phylo_hpd(data = pgeo$hpd, fill = "orange", alpha = 0.3) +
  # 3. Add branches
  geom_phylo_branches(data = pgeo$branches, aes(color = age)) +
  # 4. Add nodes (tips)
  geom_phylo_nodes(data = pgeo$nodes[pgeo$nodes$istip,], size = 2) +
  # 5. Styling
  scale_color_viridis_c(name = "Time") +
  coord_quickmap() +
  theme_void()

print(p)
```

![wnv_polished.png](inst/extdata/wnv_polished.png)

Data doesn't necessarily need to be of geographic origin, for example
continuous trait reconstructions can also be visualised:

```r

data("h3n2_tree")

H3N2 <- build_phylogeo(h3n2_tree, lon="antigenic1", lat = "antigenic2")

ggplot() +
    geom_phylo_hpd(data = H3N2$hpd, alpha = 0.3, smooth=FALSE) +
    geom_phylo_branches(data = H3N2$branches) +
    geom_phylo_nodes(data = H3N2$nodes, size = 3.5) +
    scale_fill_viridis_c(name="antigenic value", direction= 1, option="plasma") +
    theme_phylogeo() +
    theme_classic() +
    guides_phylogeo() +
    labs(x="Antigenic Dimension 1", y="Antigenic Dimension 2")
```

![h3n2.png](inst/extdata/h3n2.png)

## Shiny App

```R
library(ggphylogeo)
library(treeio)

# Load tree
tree <- read.beast("path/to/beast.tree")

# Basic plot with date scale
p <- autoplot(tree,
              most_recent_sample = "2019-06-15",
              date_format = "%Y")

# With highlighting and dispersion legend
p <- autoplot(tree,
              most_recent_sample = 2019,
              highlight_tips = c("Sample_A", "Sample_B"),
              highlight_color = "red",
              show_dispersion_legend = TRUE)

# Interactive Shiny app
run_phylogeo_app(tree, most_recent_sample = "2019-06-15")

# Quick plotly conversion, not all features supported
ggplotly_phylogeo(p)
```
