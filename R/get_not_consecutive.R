#' @title get_not_consecutive
#' @name get_not_consecutive
#' @description This function returns the index of non consecutive positions.
#' @param x vector to find non consecutive positions [mandatory]
#' @examples
#' x <- c("a", "a", "b", "b", "c", "d")
#' TreeSwitchR::get_not_consecutive(x)
#' @export get_not_consecutive
#' @author Kristian K Ullrich

get_not_consecutive <- function(
    x) {
    i <- which(
        x[seq(from = 1, to = length(x) - 1)] !=
        x[seq(from = 2, to = length(x))])
    return(i)
}
