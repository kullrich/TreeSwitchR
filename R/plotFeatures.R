#' @title plotFeatures
#' @name plotFeatures
#' @description This function plots annotated Features of a \code{TxDB} object
#' for a given chromosome region.
#' @param txdb \code{TxDB} object [mandatory]
#' @param chromosome chromosome [mandatory]
#' @param from region start position in bp [mandatory]
#' @param to region end position in bp [mandatory]
#' @importFrom Gviz GeneRegionTrack plotTracks
#' @seealso \code{\link[Gviz]{GeneRegionTrack}}
#' @seealso \code{\link[Gviz]{plotTracks}}
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
#' @export plotFeatures
#' @author Kristian K Ullrich

plotFeatures <- function(
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
    Gviz::plotTracks(
        geneTrack,
        chromosome = chromosome,
        from = from,
        to = to,
        showId = TRUE)
}
