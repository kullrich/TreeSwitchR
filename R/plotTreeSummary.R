#' @title plotTreeSummary
#' @name plotTreeSummary
#' @description This function plots tree topologies distribution along
#' chromosomes.
#' @param tree_df tree \code{data.frame} produced by
#' \code{get_topologies} [mandatory]
#' @param chrom select chromosome to plot summary [optional]
#' @param chromStart select chromosome start to plot [optional]
#' @param chromEnd select chromosome end to plot [optional]
#' @param chromPad distance between chromosomes [0]
#' @param colorBy color tree topologies either by rank [default] or by counts
#' @param chromSplitColor color of chrom split [red]
#' @param plotType plot type either by point [default] or lollipop
#' @param pointSize point size [0.5]
#' @param pointShape point shape [15]
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_point labs geom_vline element_blank
#' geom_linerange
#' @importFrom plotly ggplotly
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' data("pos", package = "TreeSwitchR")
#' topologies <- TreeSwitchR::get_topologies(trees, pos)
#' tree_df <- topologies[["tree_df"]]
#' p1 <- TreeSwitchR::plotTreeSummary(
#'     tree_df,
#'     colorBy = "rank")
#' p2 <- TreeSwitchR::plotTreeSummary(
#'     tree_df,
#'     colorBy = "counts")
#' p3 <- TreeSwitchR::plotTreeSummary(
#'     tree_df,
#'     colorBy = "rank",
#'     chrom = "chr1")
#' p4 <- TreeSwitchR::plotTreeSummary(
#'     tree_df,
#'     colorBy = "rank",
#'     chrom = "chr2")
#' plotly::subplot(
#'     p1[["fig"]],
#'     p2[["fig"]],
#'     p3[["fig"]],
#'     p4[["fig"]])
#'
#' @export plotTreeSummary
#' @author Kristian K Ullrich

plotTreeSummary <- function(
    tree_df,
    chrom = NULL,
    chromStart = NULL,
    chromEnd = NULL,
    chromPad = 0,
    colorBy = "rank",
    chromSplitColor = "red",
    plotType = "point",
    pointSize = 0.5,
    pointShape = 15) {
    if(!is.null(chrom)) {
        tree_df <- tree_df |> dplyr::filter(.data[["chrom"]] == .env[["chrom"]])
    }
    if(!is.null(chromStart)) {
        tree_df <- tree_df |> dplyr::filter(.data[["chromStart"]] >=
            .env[["chromStart"]])
    }
    if(!is.null(chromStart)) {
        tree_df <- tree_df |> dplyr::filter(.data[["chromEnd"]] <=
            .env[["chromEnd"]])
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
    if(colorBy == "rank") {
        if(plotType == "point") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                ggplot2::aes(color = as.factor(topology_n)),
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        } else if(plotType == "lollipop") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                ggplot2::aes(color = as.factor(topology_n)),
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::geom_segment(
                ggplot2::aes(x = x_axis, xend = x_axis,
                y = 0, yend = topology_n, color = as.factor(topology_n)),
                show.legend = FALSE) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        }
        for(c_split in chrom_split) {
            fig_tree <- fig_tree +
              ggplot2::geom_vline(xintercept = tree_df[["chromEnd"]][c_split] +
                  chromPad, linetype="dashed", color = chromSplitColor)
        }
    } else if(colorBy == "counts") {
        if(plotType == "point") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                ggplot2::aes(color = topology_n_counts),
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        } else if(plotType == "lollipop") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                ggplot2::aes(color = topology_n_counts),
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::geom_segment(
                ggplot2::aes(x = x_axis, xend = x_axis,
                y = 0, yend = topology_n, color = as.factor(topology_n)),
                show.legend = FALSE) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        }
        for(c_split in chrom_split) {
            fig_tree <- fig_tree +
            ggplot2::geom_vline(xintercept = tree_df[["chromEnd"]][c_split] +
                chromPad, linetype="dashed", color = chromSplitColor)
        }
    } else {
        if(plotType == "point") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        } else if(plotType == "loillipop") {
            fig_tree <- ggplot2::ggplot(data = tree_df,
                ggplot2::aes(x = x_axis, y = topology_n)) +
            ggplot2::geom_point(
                show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::geom_segment(
                ggplot2::aes(x = x_axis, xend = x_axis,
                y = 0, yend = topology_n),
                show.legend = FALSE) +
            ggplot2::labs(
                title = "Tree Summary",
                x = NULL,
                y = "tree topology") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
        }
        for(c_split in chrom_split) {
            fig_tree <- fig_tree +
            ggplot2::geom_vline(xintercept = tree_df[["chromEnd"]][c_split] +
                chromPad, linetype="dashed", color = chromSplitColor)
        }
    }
    if(colorBy == "rank") {
        fig_switches <- ggplot2::ggplot(data = tree_df,
            ggplot2::aes(x = x_axis, ymax = topo_switches, ymin = 0)) +
            ggplot2::geom_linerange() +
            ggplot2::geom_point(
                ggplot2::aes(
                    x = x_axis, y = 0.5, color = as.factor(topology_n)
                ), show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                x = NULL,
                y = NULL) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank()
            )
        for(c_split in chrom_split) {
            fig_switches <- fig_switches +
                ggplot2::geom_vline(
                    xintercept = tree_df[["chromEnd"]][c_split] +
                    chromPad, linetype="dashed", color = chromSplitColor)
        }
    } else if(colorBy == "counts") {
        fig_switches <- ggplot2::ggplot(data = tree_df,
            ggplot2::aes(x = x_axis, ymax = topo_switches, ymin = 0)) +
            ggplot2::geom_linerange() +
            ggplot2::geom_point(
                ggplot2::aes(
                    x = x_axis, y = 0.5, color = topology_n_counts
                ), show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                x = NULL,
                y = NULL) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank()
            )
        for(c_split in chrom_split) {
            fig_switches <- fig_switches +
                ggplot2::geom_vline(
                    xintercept = tree_df[["chromEnd"]][c_split] +
                    chromPad, linetype="dashed", color = chromSplitColor)
        }
    } else {
        fig_switches <- ggplot2::ggplot(data = tree_df,
            ggplot2::aes(x = x_axis, ymax = topo_switches, ymin = 0)) +
            ggplot2::geom_linerange() +
            ggplot2::geom_point(
                ggplot2::aes(
                    x = x_axis, y = 0.5, color = "grey"
                ), show.legend = FALSE, shape = pointShape, size = pointSize) +
            ggplot2::labs(
                x = NULL,
                y = NULL) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank()
            )
        for(c_split in chrom_split) {
            fig_switches <- fig_switches +
                ggplot2::geom_vline(
                    xintercept = tree_df[["chromEnd"]][c_split] +
                    chromPad, linetype="dashed", color = chromSplitColor)
        }
    }
    fig_switches_counts <- ggplot2::ggplot(data = tree_df,
        ggplot2::aes(x = x_axis, ymax = topo_switches, ymin = 0)) +
        ggplot2::geom_linerange(
            ggplot2::aes(color = topo_switches_pairs_counts),
            show.legend = FALSE) +
        ggplot2::labs(
            x = "position (bp)",
            y = NULL) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
        )
    for(c_split in chrom_split) {
        fig_switches_counts <- fig_switches_counts +
            ggplot2::geom_vline(
                xintercept = tree_df[["chromEnd"]][c_split] +
                chromPad, linetype="dashed", color = chromSplitColor)
    }
    fig <- plotly::subplot(
        plotly::style(fig_tree, showlegend = FALSE),
        plotly::style(fig_switches, showlegend = FALSE),
        plotly::style(fig_switches_counts, showlegend = FALSE),
        nrows = 3,
        heights = c(0.8, 0.1, 0.1),
        margin = 0,
        titleY = TRUE,
        titleX = TRUE)
    out <- list(
        fig_tree,
        fig_switches,
        fig_switches_counts,
        fig
    )
    names(out) <- c(
        "fig_tree",
        "fig_switches",
        "fig_switches_counts",
        "fig")
    return(out)
}
