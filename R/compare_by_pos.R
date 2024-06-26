#' @title compare_by_pos
#' @name compare_by_pos
#' @description This function plots two trees face to face given two positions.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param chrom1 select chromosome for first tree [mandatory]
#' @param chromStart1 select chromosome start for first tree [mandatory]
#' @param chromEnd1 select chromosome end for first tree [mandatory]
#' @param chrom2 select chromosome for second tree [mandatory]
#' @param chromStart2 select chromosome start for second tree [mandatory]
#' @param chromEnd2 select chromosome end for second tree [mandatory]
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
#' TreeSwitchR::compare_by_pos(
#'     tree_df = topologies[["tree_df"]],
#'     chrom1 = "chrom",
#'     chromStart1 = 1,
#'     chromEnd1 = 2,
#'     chrom2 = "chrom",
#'     chromStart2 = 5,
#'     chromEnd2 = 6)
#' @export compare_by_pos
#' @author Kristian K Ullrich

compare_by_pos <- function(
    tree_df,
    chrom1,
    chromStart1,
    chromEnd1,
    chrom2,
    chromStart2,
    chromEnd2,
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
    xytree <- TreeSwitchR::get_by_pos(
        tree_df = tree_df,
        chrom1 = chrom1,
        chromStart1 = chromStart1,
        chromEnd1 = chromEnd1,
        chrom2 = chrom2,
        chromStart2 = chromStart2,
        chromEnd2 = chromEnd2,
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
