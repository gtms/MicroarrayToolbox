\name{jetSet}
\alias{jetSet}
\title{
jetSet: collapses features targeting the same gene using the Jetset
method
}
\description{
Given a gct with an Affymetrix gene expression matrix and a character
vector identifying the chip used in the experiment (annotation.pkg,
which must evaluate to one of c ("hgu133a", "hgu133plus2",
"hgu95av2"), returns a gct with probesets collpased according to the
mapping defined by the Jetset method.
}
\usage{
jetSet(gct, annotation.pkg)
}
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{annotation.pkg}{
a character vector specifying the Affymetrix annotation package used
in the experiment, must evaluate to one of c ("hgu133a", "hgu133plus2",
"hgu95av2")
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