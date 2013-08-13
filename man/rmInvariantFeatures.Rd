\name{rmInvariantFeatures}
\alias{rmInvariantFeatures}
\title{
rmInvariantFeatures
}
\description{
This function removes features with null standard deviation.
}
\usage{
rmInvariantFeatures(gct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gct}{
a gct object, see details
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
A gct object, see details for more information on its structure.
}
\references{
See more details here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}