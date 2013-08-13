\name{computeSuperIndex}
\alias{computeSuperIndex}
\title{
computeSuperIndex: computes the median expression of gene signature
across a group of samples
}
\description{
given a gct; a character vector of features to subset the gct with
(sig); and an logical specifying whether sig refers to affymetrix
probesets or gene symbols (is.probeset), returns a numeric vector of
the median expression of the features present in the expression matrix
(should there be any, NA if not), computed sample-wise
}
\usage{
computeSuperIndex(gct, sig, is.probeset = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{sig}{
a character vector of features to subset the gct with, to be selected
among the gene symbols (row.descriptions) or the manufacturer's
feature-specific identifiers (or probesets, row names of the
expression matrix)
}
  \item{is.probeset}{
logical specifying whether the features should match the row names of
the expression matrix or the row.description vector. Defaults to FALSE
}
}
\details{
Argument gct is a list composed of the following elements:
\itemize{
  \item{row.descriptions}{character vector of length equal to the first
dimension of the data matrix, specifying the gene symbols for each
probe in the array}
  \item{data}{matrix containing the expression data. Each row corresponds
to a feature and each column to a sample. Rownames and colnames
specify, respectively, the unique manufacturer's feature-specific identifiers
and the unique sample identifiers}
}
}
\value{
A numeric vector of the same length as the number of samples in the
expression matrix. Alternatively, NA if none of the features specified
in argument 'sig' is found in the gct.
}
\references{
See more details here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}