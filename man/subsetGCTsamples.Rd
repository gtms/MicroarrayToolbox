\name{subsetGCTsamples}
\alias{subsetGCTsamples}
\title{
subsetGCTsamples
}
\description{
This function subsets the samples of a gct with a logical vector.
}
\usage{
subsetGCTsamples(gct, logical.vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{logical.vec}{
a logical vector of the same length as the number of samples in the
gct
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
A gct object, see details for more information on its structure.
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
