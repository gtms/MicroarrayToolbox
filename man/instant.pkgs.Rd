\name{instant.pkgs}
\alias{instant.pkgs}
\title{
instant.pkgs: automatically installs a list of R packages from either CRAN or
Bioconductor repositories.
}
\description{
Given a data frame instructing a list of packages to retrieve from either CRAN
of Bioconductor repositories, proceeds to fetch and install them.
}
\usage{
instant.pkgs(pkg.dfr, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg.dfr}{
a data frame composed by a character vector of the packages to be installed
(pkgs) and the corresponding repositories to fetch them from (repos, must be one
of `cran' or `bioc')
}
  \item{verbose}{
whether or note to provide verbose output.  Defaults to FALSE.
}
}
\details{
Sensibly seeks to install the list of provided packages.  Will not update packages already installed.
}
\value{
This function has no side effects.
}
\references{
}
\author{
  Gil Tomas
  \email{gil.tomas@ulb.ac.be}
}
\examples{
bioc.pkgs <- c ("aCGH", "ctc", "Heatplus", "hgu133a.db", "hgu133b.db",
                "hgu133plus2.db")
cran.pkgs <- c ("plyr", "data.table", "ggplot2", "bigmemory", "diveRsity",
                "outliers")

pkg.dfr <- data.frame (pkg = c (cran.pkgs, bioc.pkgs),
                       repos = c (rep ("cran", 6), rep ("bioc", 6)),
                       stringsAsFactors = FALSE)
pkg.dfr <- pkg.dfr[order (pkg.dfr$pkg), ]

instant.pkgs (pkg.dfr, verbose = TRUE)
