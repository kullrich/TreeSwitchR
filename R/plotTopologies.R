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
        text = names(
        base::sort(
            tree_df[["topology_n"]][!duplicated(tree_df[["topology_n"]])])
        )
    )
    unique_topologies_counts <- base::sort(
        tree_df[["topology_n_counts"]][!duplicated(tree_df[["topology_n"]])],
        decreasing = TRUE)
    gg <- ggtree::ggtree(unique_topologies, ...) +
        ggtree::geom_tiplab(colour = tip_color, ...) +
        ggplot2::facet_wrap(~.id, scale="free", nrow = nrow, ncol = ncol) +
        ggplot2::ggtitle(title)
    tree_titles <- paste0(
        gg[["data"]][[".id"]],
        " - ",
        unique_topologies_counts[as.numeric(
            gsub("Tree #", "", gg[["data"]][[".id"]]))])
    gg[["data"]][[".id"]] <- base::factor(
        tree_titles, levels = base::unique(tree_titles))
    gg
}
