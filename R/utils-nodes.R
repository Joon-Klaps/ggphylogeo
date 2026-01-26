match_nodes <- function(phy, data) {
  node_ids <- suppressWarnings(as.numeric(data$node))
  key <- match(seq_len(max(phy$edge)), node_ids)

  if (any(is.na(key))) {
    stop("Some phylo nodes have no matching metadata", call. = FALSE)
  }
  key
}

