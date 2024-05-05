#' @title create_pos
#' @name create_pos
#' @description This function creates a simple position matrix of lnegth n.
#' @param n number of trees [mandatory]
#' @importFrom tibble tibble
#' @examples
#' pos <- TreeSwitchR::create_pos(n = 10)
#' pos
#' @export create_pos
#' @author Kristian K Ullrich

create_pos <- function(
    n) {
    pos <- tibble::tibble(
        chrom = rep("chrom", n),
        chromStart = seq(from = 1, to = n),
        chromEnd = seq(from = 2, to = n + 1))
    return(pos)
}
