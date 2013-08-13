\name{instant.pkgs}
\alias{instant.pkgs}
\title{
instant.pkgs: internal function to install packages and load the
corresponding libraries
}
\description{
Given a data frame with two variables, 'pkg' and 'repos', installs all
packages 'pkg' from the corresponding 'repos' that are not yet already
installed, then proceeds to attach them to the search path.
}
\usage{
instant.pkgs(pkg.dfr, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg.dfr}{
data frame with two variables:
\itemize{
	\item{pkg}{character vector of packages to install (if not yet
	installed on the system and to attach to the search path)}
	\item{repos}{character vector of the repository from where to
	eventually install the packages. Must be one of 'cran' or
	'bioc'(bioconductor)}
}
}
  \item{verbose}{
  logical specifying the verbosity of the output. Defaults to false
}
}
\references{
See:
\url{
http://www.geneontology.org/
}
}
\author{
Adapted from an original version of Kay Cichini.
Licence: cc by-nc-sa
}