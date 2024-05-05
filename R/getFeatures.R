#' @title getFeatures
#' @name getFeatures
#' @description This function gets annotated Features of a \code{TxDB} object
#' for a given chromosome region and returns a \code{GRanges} object.
#' @param txdb \code{TxDB} object [mandatory]
#' @param chromosome chromosome [mandatory]
#' @param from region start position in bp [mandatory]
#' @param to region end position in bp [mandatory]
#' @importFrom Gviz GeneRegionTrack
#' @seealso \code{\link[Gviz]{GeneRegionTrack}}
#' @examples
#' # GTF
#' gtffile <- tempfile(pattern = "gtf.")
#' download.file(url = paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
#' "000/006/765/",
#' "GCF_000006765.1_ASM676v1/GCF_000006765.1_ASM676v1_genomic.gtf.gz"),
#' destfile = gtffile)
#' gtf_txdb <- TreeSwitchR::make_gtf_txdb(gtffile)
#' TreeSwitchR::plotFeatures(txdb = gtf_txdb,
#'     chromosome = "NC_002516.2",
#'     from = 1,
#'     to = 100000)
#' @export getFeatures
#' @author Kristian K Ullrich

getFeatures <- function(
    txdb,
    chromosome,
    from,
    to){
    options(ucscChromosomeNames = FALSE)
    geneTrack <- Gviz::GeneRegionTrack(
        txdb,
        chromosome = chromosome,
        from = from,
        to = to)
    return(geneTrack@range)
}
