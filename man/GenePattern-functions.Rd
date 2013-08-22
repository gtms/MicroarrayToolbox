\name{GenePattern}
\alias{readGCT}
\alias{readCLS}
\alias{writeGCT}
\alias{writeCLS}
\title{
Functions from the GenePattern package
}
\description{
These functions are taken from the package GenePattern and provide functionality
to read/write gct and cls files from/to disk.
}
\usage{
readGCT(file)
writeGCT(gct, filename, check.file.extension = TRUE)
readCLS(file)
writeCLS(cls, filename, check.file.extension = TRUE)
}
\arguments{
  \item{gct}{
a gct object, see details
} 
  \item{cls}{
a cls object, see details
}
  \item{file}{
character vector specifying the path to the file to be read from disk
}
  \item{filename}{
character vector specifying the path of the file to be saved on disk
}
  \item{check.file.extension}{
logical specifying whether specific file extensions should be checked (defaults to TRUE)
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

A cls object is a list composed of the following objects:
\itemize{
  \item{labels: }{a factor of length equal to the number of samples of a
  gct, specifying the class at which each sample belongs to}
  \item{names: }{character vector specifying the levels of the class factor}
}
}
\references{
More details about the GenePattern package, the gct and cls formats here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
}
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/cls
}
}
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}