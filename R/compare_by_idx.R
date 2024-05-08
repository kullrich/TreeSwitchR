#' @title compare_by_idx
#' @name compare_by_idx
#' @description This function plots two trees face to face given two indices.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param idx1 index (chromosome index if chrom1 is set)
#' for first tree [mandatory]
#' @param idx2 index (chromosome index if chrom2 is set)
#' for second tree [mandatory]
#' @param chrom1 select chromosome for first tree [optional]
#' @param chrom2 select chromosome for second tree [optional]
#' @param treetype select tree to compare (trees, rooted_trees, topoloy_trees)
#' @param use.edge.length a logical indicating whether the branch lengths
#' should be used to plot the trees [FALSE]
#' @param space a positive value that specifies the distance between the two
#' trees [0]
#' @param length.line a positive value that specifies the length of the
#' horizontal line associated to each taxa [1]
#' @param gap a value specifying the distance between the tips of the phylogeny
#' and the lines [2]
#' @param type a character string specifying the type of phylogeny to be drawn
#' (phylogram, cladogram)
#' @param rotate 	a logical indicating whether the nodes of the phylogeny can
#' be rotated by clicking [FALSE]
#' @param col a character vector indicating the color to be used for the links;
#' recycled as necessary
#' @param lwd id. for the width
#' @param lty d. for the line type
#' @param show.tip.label a logical indicating whether to show the tip labels on
#' the phylogeny [TRUE]
#' @param font an integer specifying the type of font for the
#' labels: 1 (plain text),
#' 2 (bold),
#' 3 (italic, the default),
#' or 4 (bold italic)
#' @importFrom dplyr filter slice
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' TreeSwitchR::compare_by_idx(
#'     tree_df = topologies[["tree_df"]],
#'     idx1 = 1,
#'     idx2 = 2)
#' @export compare_by_idx
#' @author Kristian K Ullrich

compare_by_idx <- function(
    tree_df,
    idx1,
    idx2,
    chrom1 = NULL,
    chrom2 = NULL,
    treetype = "trees",
    use.edge.length = TRUE,
    space = 0,
    length.line = 1,
    gap = 2,
    type = "phylogram",
    rotate = FALSE,
    col = par("fg"),
    lwd = par("lwd"),
    lty = par("lty"),
    show.tip.label = TRUE,
    font = 3
) {
    xytree <- TreeSwitchR::get_by_idx(
        tree_df = tree_df,
        idx1 = idx1,
        idx2 = idx2,
        chrom1 = chrom1,
        chrom2 = chrom2,
        treetype = treetype)
    xtree <- xytree[["xtree"]]
    ytree <- xytree[["ytree"]]
    associations <- cbind(
        xtree[["tip.label"]],
        xtree[["tip.label"]])
    ape::cophyloplot(
        x = xtree,
        y = ytree,
        assoc = associations,
        use.edge.length = use.edge.length,
        space = space,
        length.line = length.line,
        gap = gap,
        type = type,
        rotate = rotate,
        col = col,
        lwd = lwd,
        lty = lty,
        show.tip.label = show.tip.label,
        font = font)
}
