\name{pruneFeatures}
\alias{pruneFeatures}
\title{
pruneFeatures: removes features with excessive absent calls
}
\description{
Given a gct and a value 0 <= tol < 1, returns the gct pruned from the
features with more than 'tol' percent of absent calls.
}
\usage{
pruneFeatures(gct, tol = 0.2)
}
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{tol}{
a numeric vector specifying the maximum tolerated absent calls per
feature. Between 0 and 1, defaults to .2
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