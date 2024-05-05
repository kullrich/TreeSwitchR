#' @title plotTreeSummary
#' @name plotTreeSummary
#' @description This function plots tree topologies distribution along
#' chromsomes.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param chrom select chromosome to plot summary [optional]
#' @param chromPad distance between chromosomes [optional]
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point labs geom_vline
#' @importFrom plotly ggplotly
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' TreeSwitchR::plotTreeSummary(topologies[["tree_df"]])
#' @export plotTreeSummary
#' @author Kristian K Ullrich

plotTreeSummary <- function(
    tree_df,
    chrom = NULL,
    chromPad = 1) {
    if(!is.null(chrom)) {
        tree_df <- tree_df |> dplyr::filter(chrom == chrom)
    }
    chrom_split <- TreeSwitchR::get_not_consecutive(tree_df[["chrom"]])
    if(length(chrom_split) != 0) {
        x_axis <- numeric(dim(tree_df)[1])
        index_offset <- 0
        offset <- 0
        for(chr in unique(tree_df[["chrom"]])) {
            chr_df <- tree_df |>
                dplyr::filter(chrom == chr)
            x_axis[seq(from = index_offset + 1,
                to = index_offset + dim(chr_df)[1])] <-
                chr_df[["chromStart"]] + offset
            index_offset <- index_offset + dim(chr_df)[1]
            offset <- offset + max(chr_df[["chromEnd"]]) + chromPad
        }
        tree_df[["x_axis"]] <- x_axis
    } else {
        tree_df[["x_axis"]] <- tree_df[["chromStart"]]
    }
    fig_tree <- ggplot2::ggplot(data = tree_df,
        ggplot2::aes(x = x_axis, y = topology_n)) +
        ggplot2::geom_point(
            ggplot2::aes(color = as.factor(topology_n)),
            show.legend = FALSE
        ) +
        ggplot2::labs(
            title = "Tree Summary",
            x = "",
            y = "tree topology")
    for(c_split in chrom_split) {
        fig_tree <- fig_tree +
            ggplot2::geom_vline(xintercept = tree_df[["chromEnd"]][c_split] +
            chromPad, linetype="dashed", color = "red")
    }
    fig_switches <- ggplot2::ggplot(data = tree_df,
        ggplot2::aes(x = x_axis, ymax= topo_switches, ymin = 0)) +
        ggplot2::geom_linerange() +
        ggplot2::labs(
            x = "position (bp)")
    for(c_split in chrom_split) {
      fig_switches <- fig_switches +
        ggplot2::geom_vline(xintercept = tree_df[["chromEnd"]][c_split] +
                              chromPad, linetype="dashed", color = "red")
    }
    fig <- plotly::subplot(
        fig_tree,
        fig_switches,
        nrows = 2,
        heights = c(0.9, 0.1),
        margin = 0.05,
        titleY = TRUE,
        titleX = TRUE)
    out <- list(
        fig_tree,
        fig_switches,
        fig
    )
    names(out) <- c(
        "fig_tree",
        "fig_switches",
        "fig")
    return(out)
}
