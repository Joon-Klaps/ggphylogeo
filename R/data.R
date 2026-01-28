#' West Nile Virus phylogeographic tree
#'
#' A treedata object containing the maximum clade credibility (MCC) tree
#' from a BEAST continuous phylogeographic analysis of West Nile Virus
#' sequences in North America.
#'
#' @format A treedata object with phylogeographic annotations including:
#' \describe{
#'   \item{location1}{Latitude coordinate}
#'   \item{location2}{Longitude coordinate}
#'   \item{height_mean}{Mean node height (time before present)}
#'   \item{height_median}{Median node height}
#'   \item{location1_0.80HPD}{80 percent HPD intervals for latitude}
#'   \item{location2_0.80HPD}{80 percent HPD intervals for longitude}
#' }
#' @source BEAST phylogeographic analysis
#' @examples
#' \dontrun{
#' data(wnv_tree)
#' autoplot(wnv_tree, most_recent_sample = "2007-07-01")
#' }
"wnv_tree"

#' Yellow Fever Virus phylogeographic tree
#'
#' A treedata object containing the maximum clade credibility (MCC) tree
#' from a BEAST relaxed random walk (RRW) continuous phylogeographic
#' analysis of Yellow Fever Virus sequences in South America.
#'
#' @format A treedata object with phylogeographic annotations including:
#' \describe{
#'   \item{location1}{Latitude coordinate}
#'   \item{location2}{Longitude coordinate}
#'   \item{height_mean}{Mean node height (time before present)}
#'   \item{height_median}{Median node height}
#'   \item{location1_0.80HPD}{80 percent HPD intervals for latitude}
#'   \item{location2_0.80HPD}{80 percent HPD intervals for longitude}
#' }
#' @source BEAST phylogeographic analysis with relaxed random walk model
#' @examples
#' \dontrun{
#' data(yfv_tree)
#' autoplot(yfv_tree)
#' }
"yfv_tree"


#' HIV-1 subtype G phylogeographic tree
#'
#' A treedata object containing the maximum clade credibility (MCC) tree
#' from a BEAST relaxed random walk (RRW) continuous phylogeographic
#' analysis of HIV-1.
#'
#' @format A treedata object with phylogeographic annotations including:
#' \describe{
#'   \item{location1}{Latitude coordinate}
#'   \item{location2}{Longitude coordinate}
#'   \item{height_mean}{Mean node height (time before present)}
#'   \item{height_median}{Median node height}
#'   \item{location1_0.80HPD}{80 percent HPD intervals for latitude}
#'   \item{location2_0.80HPD}{80 percent HPD intervals for longitude}
#' }
#' @source BEAST phylogeographic analysis with relaxed random walk model
#' @examples
#' \dontrun{
#' data(hiv1_tree)
#' autoplot(hiv1_tree)
#' }
"hiv1_tree"


#' Influenza A H3N2 antigenic cartography tree
#'
#' A treedata object containing the maximum clade credibility (MCC) tree
#' from a BEAST continuous trait analysis of Influenza A H3N2 sequences.
#' This dataset demonstrates using ggphylogeo with non-geographic continuous
#' traits (antigenic dimensions).
#'
#' @format A treedata object with continuous trait annotations including:
#' \describe{
#'   \item{antigenic1}{First antigenic dimension coordinate}
#'   \item{antigenic2}{Second antigenic dimension coordinate}
#'   \item{height_mean}{Mean node height (time before present)}
#'   \item{height_median}{Median node height}
#'   \item{antigenic1_0.80HPD}{80 percent HPD intervals for dimension 1}
#'   \item{antigenic2_0.80HPD}{80 percent HPD intervals for dimension 2}
#' }
#' @source BEAST continuous trait analysis
#' @examples
#' \dontrun{
#' data(h3n2_tree)
#' pgeo <- build_phylogeo(h3n2_tree, lon = "antigenic1", lat = "antigenic2")
#' }
"h3n2_tree"