#' @title get_topologies
#' @name get_topologies
#' @description This function creates tree topologies from phylogenetic trees.
#' The output is a \code{list} which contains:
#' 1. a \code{data.frame} tree_df with the columns:
#' original trees, rooted trees, topo trees, topology,
#' chrom, chromStart, chromEnd
#' 2. trees
#' 3. rooted_trees
#' 4. topo_trees
#' 5. topo_trees_table_sorted
#' 6. topos
#' 7. pos
#' @param trees phylogenetic trees [mandatory]
#' @param pos tree positions [optional]
#' @param outgroup [optional]
#' @param sort specify if trees should be sorted by tree position first [TRUE]
#' @importFrom ape read.tree write.tree root rmtree
#' @seealso \code{\link[Gviz]{GeneRegionTrack}}
#' @seealso \code{\link[Gviz]{plotTracks}}
#' @examples
#' trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
#' topologies <- TreeSwitchR::get_topologies(trees)
#' topologies[["tree_df"]]
#' @export get_topologies
#' @author Kristian K Ullrich

get_topologies <- function(
    trees,
    pos = NULL,
    outgroup = NULL,
    sort = TRUE) {
    if(is.null(pos)) {
        pos <- TreeSwitchR::create_pos(length(trees))
    } else {
        stopifnot(length(trees) == dim(pos)[1])
    }
    if(sort) {
        ordered_trees <- TreeSwitchR::order_tree_pos(trees, pos)
        trees <- ordered_trees[["ordered_trees"]]
        pos <- ordered_trees[["ordered_pos"]]
    }
    rooted_trees <- NULL
    if(!is.null(outgroup)) {
        rooted_trees <- ape::read.tree(
            text = unlist(
                lapply(
                    trees, function(x) {
                        ape::write.tree(ape::root(x, outgroup))}
                )
            )
        )
    } else {
    rooted_trees <- ape::read.tree(
        text = unlist(
            lapply(
                trees, function(x) {
                    ape::write.tree(phangorn::midpoint(x))}
                )
            )
        )
    }
    topo_trees <- rooted_trees
    for(i in seq(from = 1, to = length(topo_trees))) {
        topo_trees[[i]][["edge.length"]] <- rep(1,
            length(topo_trees[[i]][["edge.length"]]))
    }
    topo_trees_table_sorted <- sort(table(ape::write.tree(topo_trees)),
        decreasing = TRUE)
    topos <- seq(from = 1, to = length(topo_trees_table_sorted))
    names(topos) <- names(topo_trees_table_sorted)
    topos_trees <- ape::read.tree(text=names(topos))
    topos_replace <- names(topos)
    names(topos_replace) <- names(topos)
    skip<-NULL
    for(i in seq(from = 1, to = length(topos_trees)-1)){
        if(i %in% skip){next}
        for(j in seq(from = i+1, to = length(topos_trees))){
            if(all.equal(topos_trees[[i]], topos_trees[[j]])){
                #cat(i, " ", j, "\n")
                topos_replace[j] <- names(topos)[i]
                skip<-c(skip, j)
            }
        }
    }
    topo_trees <- ape::read.tree(
        text = topos_replace[ape::write.tree(topo_trees)])
    topo_trees_table_sorted <- sort(table(ape::write.tree(topo_trees)),
        decreasing = FALSE)
    topos <- seq(from = length(topo_trees_table_sorted), to = 1)
    names(topos) <- names(topo_trees_table_sorted)
    tree_df <- tibble::tibble(
        trees = ape::write.tree(trees),
        rooted_trees = ape::write.tree(rooted_trees),
        topology_trees = ape::write.tree(topo_trees),
        topology_n = topos[ape::write.tree(topo_trees)],
        chrom = pos[["chrom"]],
        chromStart = pos[["chromStart"]],
        chromEnd = pos[["chromEnd"]]
    )
    topo_switches <- numeric(dim(tree_df)[1])
    topo_switches[
        TreeSwitchR::get_not_consecutive(tree_df[["topology_n"]])] <- 1
    tree_df["topo_switches"] <- topo_switches
    out <- list(
        tree_df,
        trees,
        rooted_trees,
        topo_trees,
        topo_trees_table_sorted,
        topos,
        pos)
    names(out) <- c(
        "tree_df",
        "trees",
        "rooted_trees",
        "topo_trees",
        "topo_trees_table_sorted",
        "topos",
        "pos")
    return(out)
}
