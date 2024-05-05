#' @title order_tree_pos
#' @name order_tree_pos
#' @description This function orders trees by tree position (chrom, chromStart).
#' The output is a \code{list} which contains:
#' 1. ordered trees
#' 2. ordered positions
#' @param trees phylogenetic trees [mandatory]
#' @param pos [mandatory]
#' @examples
#' data("pos", package="TreeSwitchR")
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' ordered <- TreeSwitchR::order_tree_pos(trees, pos)
#' @export order_tree_pos
#' @author Kristian K Ullrich

order_tree_pos <- function(
    trees,
    pos) {
    pos_order <- order(pos[["chrom"]], as.numeric(pos[["chromStart"]]))
    ordered_trees <- trees[pos_order]
    ordered_pos <- pos[pos_order, , drop = FALSE]
    out <- list(
        ordered_trees,
        ordered_pos
    )
    names(out) <- c(
        "ordered_trees",
        "ordered_pos")
    return(out)
}
