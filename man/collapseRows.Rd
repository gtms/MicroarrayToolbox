\name{collapseRows}
\alias{collapseRows}
\title{
collapseRows: implements the MaxMean algorithm to collapse features
targeting the same gene
}
\description{
The MaxMean algorithm selects, for every group of features targeting
a particular gene, the feature for which the mean of expression across
all samples is the highest.
}
\usage{
collapseRows(gct)
}
\arguments{
  \item{gct}{
a gct object, see details
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
A gct object for which all features are collapsed, i.e., every gene
symbol in row.descriptions has only one corresponding feature. See
details for more information on the gct format.
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