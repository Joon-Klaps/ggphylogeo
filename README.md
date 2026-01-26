# ggphylogeo

ggplot2 extensions for visualising continuous phylogeographic
reconstructions from BEAST.

## Installation

```r
# remotes::install_github("Joon-Klaps/ggphylogeo")
```

## Example

```r
library(ggphylogeo)
library(treeio)

td <- read.beast("example.MCC.nexus")
autoplot(td)
```

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

# With highlighting and direction legend
p <- autoplot(tree,
              most_recent_sample = 2019,
              highlight_tips = c("Sample_A", "Sample_B"),
              highlight_color = "red",
              show_direction_legend = TRUE)

# Interactive Shiny app
run_phylogeo_app(tree, most_recent_sample = "2019-06-15")

# Quick plotly conversion
ggplotly_phylogeo(p)
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
