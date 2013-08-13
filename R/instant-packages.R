## Adapted from https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/instant_pkgs.R
## 24Jul2013

## Script name: instant-packages.R
## Purpose: Package installation and loading
## Author: Kay Cichini
## Date: 2012-06-19
## Licence: cc by-nc-sa

## pkg.dfr: data frame composed by a character vector of the packages to be
## installed (pkgs) and the corresponding repositories to fetch them from
## (repos, must be one of `cran' or `bioc')

instant.pkgs <- function (pkg.dfr,
                          verbose = FALSE) {
    ## define inst.pkg
    inst.pkg <- function (pkg, repos = NULL) {
                                        # pkg is a character vector with the
                                        # name of the package to be installed
                                        # repos must be one of "cran" or "bioc"
        if (is.null (repos) | length (repos) != 1)
            stop ("Please provide a single repos between \"cran\" or \"bioc\"")
        if (!xor (repos == "cran", repos == "bioc"))
            stop ("Please provide a repos between \"cran\" or \"bioc\"")
        if (repos == "cran") {
            install.packages (pkg, repos = "http://cran.r-project.org")
        } else {
            source ("http://bioconductor.org/biocLite.R")
            biocLite (pkg)
        }
    }
    
    pkgs.miss <- !pkg.dfr$pkg %in% installed.packages ()[, 1]
    if (sum (pkgs.miss) > 0) {
        mapply (inst.pkg,
                pkg = pkg.dfr[pkgs.miss, "pkg"],
                repos = pkg.dfr[pkgs.miss, "repos"])
    }
    
    if (sum (pkgs.miss) == 0 & verbose) {
        message ("\n... packages were already installed.\n")
    }
    
    ## load packages not already loaded:
    attached <- search ()
    attached.pkgs <- attached[grepl ("package", attached)]
    to.attach <- with (pkg.dfr, pkg[which (!pkg %in% gsub ("package:", "", attached.pkgs))])
    
    if (length (to.attach) > 0) {
        lapply (to.attach, require, character.only = TRUE)
    }
    
    if (length (to.attach) == 0 & verbose) {
	message ("\n... packages were already loaded.\n")
    }
}

## Examples:
## bioc.pkgs <- c ("aCGH", "ctc", "Heatplus", "hgu133a.db", "hgu133b.db",
##                 "hgu133plus2.db")
## cran.pkgs <- c ("plyr", "data.table", "ggplot2", "bigmemory", "diveRsity",
##                 "outliers")
## 
## pkg.dfr <- data.frame (pkg = c (cran.pkgs, bioc.pkgs),
##                        repos = c (rep ("cran", 6), rep ("bioc", 6)),
##                        stringsAsFactors = FALSE)
## pkg.dfr <- pkg.dfr[order (pkg.dfr$pkg), ]
## 
## instant.pkgs (pkg.dfr, verbose = TRUE)
