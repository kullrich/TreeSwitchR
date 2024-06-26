% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_by_idx.R
\name{get_by_idx}
\alias{get_by_idx}
\title{get_by_idx}
\usage{
get_by_idx(
  tree_df,
  idx1,
  idx2,
  chrom1 = NULL,
  chrom2 = NULL,
  treetype = "trees"
)
}
\arguments{
\item{tree_df}{tree \code{data.frame} produced by
\code{get_topologies} [mandatory]}

\item{idx1}{index (chromosome index if chrom1 is set)
for first tree [mandatory]}

\item{idx2}{index (chromosome index if chrom2 is set)
for second tree [mandatory]}

\item{chrom1}{select chromosome for first tree [optional]}

\item{chrom2}{select chromosome for second tree [optional]}

\item{treetype}{select tree to compare (trees, rooted_trees, topoloy_trees)}
}
\description{
This function returns two trees given two indices.
}
\examples{
trees <- ape::rmtree(N = 100, n = 4, rooted = FALSE)
topologies <- TreeSwitchR::get_topologies(trees)
TreeSwitchR::get_by_idx(
    tree_df = topologies[["tree_df"]],
    idx1 = 1,
    idx2 = 2)
}
\author{
Kristian K Ullrich
}
