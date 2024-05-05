#' @title get_by_pos
#' @name get_by_pos
#' @description This function returns two trees given two positions.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param chrom1 select chromosome for first tree [mandatory]
#' @param chromStart1 select chromosome start for first tree [mandatory]
#' @param chromEnd1 select chromosome end for first tree [mandatory]
#' @param chrom2 select chromosome for second tree [mandatory]
#' @param chromStart2 select chromosome start for second tree [mandatory]
#' @param chromEnd2 select chromosome end for second tree [mandatory]
#' @param treetype select tree to compare (trees, rooted_trees, topoloy_trees)
#' @importFrom dplyr filter slice
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' TreeSwitchR::get_by_pos(
#'     tree_df = topologies[["tree_df"]],
#'     chrom1 = "chrom",
#'     chromStart1 = 1,
#'     chromEnd1 = 2,
#'     chrom2 = "chrom",
#'     chromStart2 = 5,
#'     chromEnd2 = 6)
#' @export get_by_pos
#' @author Kristian K Ullrich

get_by_pos <- function(
    tree_df,
    chrom1,
    chromStart1,
    chromEnd1,
    chrom2,
    chromStart2,
    chromEnd2,
    treetype = "trees"
) {
  tree1_df <- tree_df |> dplyr::filter(
    chrom == chrom1, chromStart == chromStart1, chromEnd == chromEnd1)
  tree2_df <- tree_df |> dplyr::filter(
    chrom == chrom2, chromStart == chromStart2, chromEnd == chromEnd2)
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
