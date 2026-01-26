# ggphylogeo

ggplot2 extensions for visualising continuous phylogeographic
reconstructions from BEAST.

## Installation
```r
# remotes::install_github("yourname/ggphylogeo")
```

## Example
```r
library(ggphylogeo)
library(treeio)

td <- read.beast("example.nexus")
autoplot(td)
```
