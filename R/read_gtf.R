#' @title read_gtf
#' @name read_gtf
#' @description This function reads a GFF3/GTF file.
#' @param gtffile GFF3/GTF file [mandatory]
#' @importFrom rtracklayer import
#' @seealso \code{\link[rtracklayer]{import}}
#' @examples
#' # GTF
#' gtffile <- tempfile(pattern = "gtf.")
#' download.file(file = paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
#' "000/006/765/",
#' "GCF_000006765.1_ASM676v1/GCF_000006765.1_ASM676v1_genomic.gtf.gz"),
#' destfile = gtffile)
#' gtf <- TreeSwitchR::read_gtf(gtffile)
#' # GFF3
#' gff3file <- tempfile(pattern = "gff3.")
#' download.file(file = paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
#' "000/006/765/",
#' "GCF_000006765.1_ASM676v1/GCF_000006765.1_ASM676v1_genomic.gff.gz"),
#' destfile = gff3file)
#' gff3_txdb <- TreeSwitchR::read_gtf(gff3file)
#' @export read_gtf
#' @author Kristian K Ullrich

read_gtf <- function(
      gtffile) {
      format <- "gff3"
      if(grep(".gtf", gtffile) || grep(".GTF", gtffile)) {
          format <- "gtf"
      }
      gtf <- rtracklayer::import(gtffile, format = format)
      return(gtf)
}
