\name{ggplotMDS}
\alias{ggplotMDS}
\title{
ggplotMDS: draws a MDS plot from a gct
}
\description{
Given a gct and a number of graphical parameters, returns a ggplot
with a visual representation of a Multidimensional Scaling of the expression
profiles of the samples.
}
\usage{
ggplotMDS(gct, labels = NULL, colour = NULL, title = "MDS", path = NULL, width = 10, height = 7, ...)
}
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{labels}{
factor of the same length as the number of samples in the gct specifying an
alternative encoding for sample names. Defaults to NULL
}
  \item{colour}{
factor of the same length as the number of samples in the gct specifying colour
encoding of the samples. Defaults to NULL
}
  \item{title}{
character vector specifying the title of the plot
}
  \item{path}{
character vector specifying the eventual path on disk where to write the output
pdf file (the complete file path is a concatenate of this variable and the title variable)
}
  \item{width}{
graphical parameter specifying the width of the output pdf file
}
  \item{height}{
graphical parameter specifying the height of the output pdf file
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters
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