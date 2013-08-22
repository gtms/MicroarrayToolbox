\name{computeSuperIndex}
\alias{computeSuperIndex}
\title{
computeSuperIndex: computes the median expression of gene signature
across a group of samples
}
\description{
Given a gct and a gene signature, returns the median expression of the genes in
the signature for every sample in the expression matrix.
}
\usage{
computeSuperIndex(gct, sig, is.probeset = FALSE)
}
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{sig}{
a character vector of features to subset the gct with, to be selected among the
gene symbols (row.descriptions) or the manufacturer's feature-specific
identifiers (the probesets in the rownames of the expression matrix)
}
  \item{is.probeset}{
logical specifying whether the features should match the rownames of the
expression matrix (TRUE) or the row.description vector (FALSE). Defaults to FALSE
}
}
\details{
A gct object is a list composed of the following objects:
\itemize{
  \item{row.descriptions: }{character vector of length equal to the first
dimension of the data matrix, specifying the symbols of the genes targeted by
each feature in the array}
  \item{data: }{matrix containing the expression data. Each row corresponds to a
feature and each column to a sample. Rownames and colnames specify,
respectively, the unique manufacturer's feature-specific identifiers and the
unique sample identifiers}
}
}
\value{
A numeric vector of the same length as the number of samples in the
expression matrix. Alternatively, NA if none of the features specified
in argument 'sig' is found in the gct.
}
\references{
More details about the gct format here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}