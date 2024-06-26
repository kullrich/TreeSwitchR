% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotKaryogram.R
\name{plotKaryogram}
\alias{plotKaryogram}
\title{plotKaryogram}
\usage{
plotKaryogram(tree_gr)
}
\arguments{
\item{tree_df}{tree \code{data.frame} produced by
\code{get_topologies} [mandatory]}

\item{nrow}{number of rows}

\item{ncol}{number of columns}

\item{title}{title}

\item{tip_color}{tip color}
}
\description{
This function plots tree topologies distribution along
chromosomes using karyotype layout.
}
\examples{
trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
data("pos", package = "TreeSwitchR")
topologies <- TreeSwitchR::get_topologies(trees, pos)
tree_gr <- topologies[["tree_gr"]]
plotKaryogram(tree_gr = tree_gr)
}
\author{
Kristian K Ullrich
}
