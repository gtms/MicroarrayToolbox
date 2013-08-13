\name{readCLS}
\alias{readCLS}
\title{
readCLS: reads .cls file from disk
}
\description{
Reads a .cls file from disk and creates a list from it.
}
\usage{
readCLS(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character vector specifying the path of the file to be read
}
}
\value{
a list composed of the following elements:
  \item{labels}{a factor of length equal to the number of samples of a
  gct, specifying the class at which each sample belongs to}
  \item{names}{character vector specifying the levels of the class factor}
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