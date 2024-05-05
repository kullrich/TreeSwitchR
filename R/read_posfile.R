#' @title read_posfile
#' @name read_posfile
#' @description This function reads a chromosome position file.
#' Each line corresponds to one tree.
#' chrom<tab>chromStart<tab>chromEnd
#' @param posfile tree position file [mandatory]
#' @param header header (chrom<tab>chromStart<tab>chromEnd) exists [TRUE]
#' @param sort sort by chrom and chromStart [TRUE]
#' @importFrom readr read_tsv
#' @seealso \code{\link[readr]{read_tsv}}
#' @examples
#' data("pos", package="TreeSwitchR")
#' posfile <- tempfile()
#' write.table(
#'     pos,
#'     sep = "\t",
#'     quote = FALSE,
#'     col.names = TRUE,
#'     row.names = FALSE,
#'     file = posfile)
#' pos <- TreeSwitchR::read_posfile(posfile)
#' pos
#' @export read_posfile
#' @author Kristian K Ullrich

read_posfile <- function(
    posfile,
    header = TRUE,
    sort = TRUE) {
    pos <- readr::read_tsv(posfile, col_names = header)
    if(!header) {
        colnames(pos) <- c("chrom", "chromStart", "chromEnd")
    }
    return(pos)
}
