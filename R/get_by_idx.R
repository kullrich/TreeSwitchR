#' @title get_by_idx
#' @name get_by_idx
#' @description This function returns two trees given two indices.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param idx1 index (chromosome index if chrom1 is set)
#' for first tree [mandatory]
#' @param idx2 index (chromosome index if chrom2 is set)
#' for second tree [mandatory]
#' @param chrom1 select chromosome for first tree [optional]
#' @param chrom2 select chromosome for second tree [optional]
#' @param treetype select tree to compare (trees, rooted_trees, topoloy_trees)
#' @importFrom dplyr filter slice
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' TreeSwitchR::get_by_idx(
#'     tree_df = topologies[["tree_df"]],
#'     idx1 = 1,
#'     idx2 = 2)
#' @export get_by_idx
#' @author Kristian K Ullrich

get_by_idx <- function(
    tree_df,
    idx1,
    idx2,
    chrom1 = NULL,
    chrom2 = NULL,
    treetype = "trees"
    ) {
    tree1_df <- tree_df |> dplyr::slice(idx1)
    tree2_df <- tree_df |> dplyr::slice(idx2)
    if(!is.null(chrom1)) {
        tree1_df <- tree_df |> dplyr::filter(chrom == chrom1) |>
            dplyr::slice(idx1)
    }
    if(!is.null(chrom2)) {
        tree2_df <- tree_df |> dplyr::filter(chrom == chrom2) |>
            dplyr::slice(idx2)
    }
    xtree <- ape::read.tree(text = tree1_df[[treetype]])
    ytree <- ape::read.tree(text = tree2_df[[treetype]])
    out <- list(
        xtree,
        ytree
    )
    names(out) <- c(
        "xtree",
        "ytree"
    )
    return(out)
}
