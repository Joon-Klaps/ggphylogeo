#' Build phylogeographic branch segments
#'
#' @export
build_phylo_segments <- function(
  treedata,
  x = "location1",
  y = "location2",
  height = "height_mean",
  digits = 2
) {
  check_treedata(treedata)

  d <- treedata@data
  t <- treedata@phylo
  key <- match_nodes(t, d)

  edges <- t$edge

  segs <- lapply(seq_len(nrow(edges)), function(j) {
    n1 <- edges[j, 1]
    n2 <- edges[j, 2]

    data.frame(
      startnode = n1,
      endnode   = n2,
      x         = round(as.numeric(d[[x]][[key[n1]]]), digits),
      y         = round(as.numeric(d[[y]][[key[n1]]]), digits),
      xend      = round(as.numeric(d[[x]][[key[n2]]]), digits),
      yend      = round(as.numeric(d[[y]][[key[n2]]]), digits),
      startheight = d[[height]][[key[n1]]],
      endheight   = d[[height]][[key[n2]]]
    )
  })

  segs <- do.call(rbind, segs)
  segs$istip <- segs$endnode <= length(t$tip.label)

  segs[order(-segs$endheight), ]
}

