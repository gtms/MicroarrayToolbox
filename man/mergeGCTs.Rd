\name{mergeGCTs}
\alias{mergeGCTs}
\title{
mergeGCTs: merges a list of gct's retaining only the common features
}
\description{
Given a list of gct objects, returns a single gct with all the samples
pooled together with common features only.
}
\usage{
mergeGCTs(gct.list)
}
\arguments{
  \item{gct.list}{
a list of gct objects. See details for more information
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
A merged gct object, see details for more information on its structure.
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