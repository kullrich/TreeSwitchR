#' @title plotKaryogram
#' @name plotKaryogram
#' @description This function plots tree topologies distribution along
#' chromosomes using karyotype layout.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param nrow number of rows
#' @param ncol number of columns
#' @param title title
#' @param tip_color tip color
#' @importFrom GenomeInfoDb seqinfo
#' @importFrom ggplot2 facet_wrap ggtitle
#' @importFrom ggtree ggtree geom_tiplab
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' data("pos", package = "TreeSwitchR")
#' topologies <- TreeSwitchR::get_topologies(trees, pos)
#' tree_gr <- topologies[["tree_gr"]]
#' plotKaryogram(tree_gr = tree_gr)
#' @export plotKaryogram
#' @author Kristian K Ullrich

plotKaryogram <- function(
    tree_gr
    ) {
    ggbio::autoplot(tree_gr, layout = "karyogram") +
        ggbio::layout_karyogram(aes(x=start, y=topo_switches),
        geom = "point", ylim=c(0, 1))
}
