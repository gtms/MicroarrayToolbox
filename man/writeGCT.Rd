\name{writeGCT}
\alias{writeGCT}
\title{
readGCT
}
\description{
This function writes a .gct file to disk.
}
\usage{
writeGCT(gct, filename, check.file.extension = TRUE)
}
\arguments{
  \item{gct}{
a gct object, see details
} 
  \item{filename}{
character vector specifying the path to the file to be saved on disk
}
  \item{check.file.extension}{
logical specifying whether specific file extensions should be checked (defaults to TRUE)
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
\references{
See more details here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}
\author{
Broad Institute
}