#' @title make_gtf_txdb
#' @name make_gtf_txdb
#' @description This function makes a \code{TxDb} object from a GFF3/GTF file.
#' @param gtffile GFF3/GTF file [mandatory]
#' @importFrom txdbmaker makeTxDbFromGFF
#' @seealso \code{\link[txdbmaker]{makeTxDbFromGFF}}
#' @examples
#' # GTF
#' gtffile <- tempfile(pattern = "gtf.")
#' download.file(url = paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
#' "000/006/765/",
#' "GCF_000006765.1_ASM676v1/GCF_000006765.1_ASM676v1_genomic.gtf.gz"),
#' destfile = gtffile)
#' gtf_txdb <- TreeSwitchR::make_gtf_txdb(gtffile)
#' # GFF3
#' gff3file <- tempfile(pattern = "gff3.")
#' download.file(url = paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
#' "000/006/765/",
#' "GCF_000006765.1_ASM676v1/GCF_000006765.1_ASM676v1_genomic.gff.gz"),
#' destfile = gff3file)
#' gff3_txdb <- TreeSwitchR::make_gtf_txdb(gff3file)
#' @export make_gtf_txdb
#' @author Kristian K Ullrich

make_gtf_txdb <- function(
    gtffile) {
    format <- "gff3"
    if(grep(".gtf", gtffile) || grep(".GTF", gtffile)) {
        format <- "gtf"
    }
    gtf_txdb <- txdbmaker::makeTxDbFromGFF(gtffile, format = format)
    return(gtf_txdb)
}
