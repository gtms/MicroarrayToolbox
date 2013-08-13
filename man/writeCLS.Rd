\name{writeCLS}
\alias{writeCLS}
\title{
readCLS
}
\description{
This function writes a .cls file to disk.
}
\usage{
writeCLS(cls, filename, check.file.extension = TRUE)
}
\arguments{
  \item{cls}{
a cls object, see details
} 
  \item{filename}{
character vector specifying the path to the file to be saved on disk
}
  \item{check.file.extension}{
logical specifying whether specific file extensions should be checked (defaults to TRUE)
}
}
\details{
Argument cls is a list composed of the following elements:
\itemize{
  \item{labels}{a factor of length equal to the number of samples of a
  gct, specifying the class at which each sample belongs to}
  \item{names}{character vector specifying the levels of the class factor}
}
}
\references{
See more details here:
url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/cls
}
}
\author{
Broad Institute
}