#' @title read_treefile
#' @name read_treefile
#' @description This function reads newick trees from a file.
#' @param treefile treefile [mandatory]
#' @importFrom ape read.tree rmtree
#' @seealso \code{\link[ape]{read.tree}}
#' @examples
#' trees <- ape::rmtree(N = 100, n = 3, rooted = FALSE)
#' treefile <- tempfile()
#' ape::write.tree(trees, file = treefile)
#' trees <- TreeSwitchR::read_treefile(treefile)
#' trees
#' @export read_treefile
#' @author Kristian K Ullrich

read_treefile <- function(
    treefile) {
    trees <- ape::read.tree(file = treefile, comment.char = "#")
    return(trees)
}
