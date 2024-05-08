#' @title plotTopologies
#' @name plotTopologies
#' @description This function plots topologies given a \code{tree_df}
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param nrow number of rows
#' @param ncol number of columns
#' @param title title
#' @param tip_color tip color
#' @importFrom ape read.tree
#' @importFrom ggplot2 facet_wrap ggtitle
#' @importFrom ggtree ggtree geom_tiplab
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' data("pos", package = "TreeSwitchR")
#' topologies <- TreeSwitchR::get_topologies(trees, pos)
#' tree_df <- topologies[["tree_df"]]
#' plotTopologies(tree_df = tree_df)
#' @export plotTopologies
#' @author Kristian K Ullrich

plotTopologies <- function(
    tree_df,
    nrow = NULL,
    ncol = NULL,
    title = "Tree Topologies",
    tip_color = par("fg"),
    ...) {
    unique_topologies <- ape::read.tree(
        text = names(table(tree_df[["topology_trees"]])))
    ggtree::ggtree(unique_topologies, ...) +
        ggtree::geom_tiplab(colour = tip_color, ...) +
        ggplot2::facet_wrap(~.id, scale="free", nrow = nrow, ncol = ncol, ...) +
        ggplot2::ggtitle(title)
}
