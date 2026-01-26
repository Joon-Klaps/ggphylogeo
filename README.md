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
