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
a gct object for which all features are collapsed, i.e., every gene
symbol in row.descriptions has only one corresponding feature. See
details for more information on the gct format
}
\references{
See more details here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}
