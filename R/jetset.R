## jetSet
## 30Jul2013
##
## UPDATED on 14Aug2013:
## updated the name of the function 'instant.pkgs' to 'instantPkgs'
##
## given a gct of an Affymetrix gene expression matrix and a character
## vector identifying the chip used in the experiment (annotation.pkg, which must
## evaluate to one of c ("hgu133a", "hgu133plus2", "hgu95av2"), returns a gct with
## probesets collpased according to the mapping defined by the jetset method,
## described here:
## http://www.biomedcentral.com/1471-2105/12/474
##
## NOTE: requires prior installation of the 'jetset' package that can be downloaded
## from here:
## http://www.cbs.dtu.dk/biotools/jetset/

jetSet <- function (gct,
                    annotation.pkg) { ## character vector to be chosen
    ## among one of "hgu133a", "hgu133plus2" or "hgu95av2"
    if (!require (jetset)) stop (cat ("Please make sure that the latest version of the 'jetset' package is installed.\nYou can download it from here: http://www.cbs.dtu.dk/biotools/jetset/\n"))
    pkg.dfr <- data.frame (pkg = c ("data.table",
                               "annotate",
                               paste0 (annotation.pkg, ".db")),
                           repos = c ("cran", rep ("bioc", 2)),
                           stringsAsFactors = FALSE)
    instantPkgs (pkg.dfr)
    dfr <- jscores (annotation.pkg)
    dtb <- data.table (dfr, keep.rownames = TRUE)
    setnames (dtb, "rn", "probeset")
    dtb[, new.symb := getSYMBOL (probeset, annotation.pkg)]
    top.probesets <- dtb[, probeset[which.max (overall)], by = new.symb]
    keep <- rownames (gct$data) %in% top.probesets[, V1]
    rd <- gct$row.descriptions[keep]
    dt <- gct$data[keep, ]
    na.id <- is.na (rd)
    list (row.descriptions = rd[!na.id],
          data = dt[!na.id, ])
}
