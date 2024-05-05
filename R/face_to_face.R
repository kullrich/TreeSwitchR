#' @title face_to_face
#' @name face_to_face
#' @description This function plots two trees face to face.
#' @param xtree xtree
#' @param ytree ytree
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 geom_line
#' @importFrom ggtree ggtree geom_tree geom_tiplab
#' @importFrom tibble tibble
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' xytrees <- TreeSwitchR::get_by_idx(
#'     tree_df = topologies[["tree_df"]],
#'     idx1 = 1,
#'     idx2 = 2)
#' TreeSwitchR::face_to_face(
#'     xtree = xytrees[["xtree"]],
#'     ytree = xytrees[["ytree"]])
#' @export face_to_face
#' @author Kristian K Ullrich

face_to_face <- function(
    xtree,
    ytree,
    layout = "rectangular",
    link_color = par("fg"),
    tip_color = par("fg"),
    link_space = 0.2,
    ...
    ) {
    ggx <- ggtree::ggtree(xtree, layout = layout, ...)
    ggy <- ggtree::ggtree(ytree, layout = layout, ...)
    d1 <- ggx[["data"]]
    d2 <- ggy[["data"]]
    ## reverse x-axis and
    ## set offset to make the tree on the right-hand side of the first tree
    d2[["x"]] <- max(d2[["x"]]) - d2[["x"]] + max(d1[["x"]]) + 1
    ggxy <- ggx + ggtree::geom_tree(
        data = d2, layout = layout) +
        ggnewscale::new_scale_fill() +
        ggtree::geom_tiplab(data = d1, colour = tip_color, ...) +
        ggtree::geom_tiplab(data = d2, hjust = 1, colour = tip_color, ...)
    d1[["x"]] <- d1[["x"]] + link_space
    d2[["x"]] <- d2[["x"]] - link_space
    dd <- tibble::tibble(dplyr::bind_rows(d1, d2)) |>
        dplyr::filter(complete.cases(label))
    ggxy + ggplot2::geom_line(
        ggtree::aes(x, y, group = label),
        data = dd, color = link_color)
}
