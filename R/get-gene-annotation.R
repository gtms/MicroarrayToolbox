## getGeneAnnotation
## 24Jul2013
##
## UPDATED on 14Aug2013:
## updated the name of the function 'instant.pkgs' to 'instantPkgs'
##
## given a character vector of Affymetrix identifiers, returns a data table,
## not keyed, with the following variables:
## vendor.id: character vector of the provided Affymetrix identifiers
## symbol: character vector of the corresponding gene symbols
## gene.name: character vector of the corresponding gene names
## entrez.id: numeric vector of the corresponding NCBI entrez gene IDs
## ACCNUM: character vector of the corresponding GeneBank accesion numbers
## map: character vector of the corresponding probesets' genomic locations
##
## affy.ids: character vector of Affymetrix identifiers
## annotation.pkg: character vector defining the bioconductor annotation data
## package from which to retrieve the metadata, e.g., "hgu133plus2" 
getGeneAnnotation <- function (affy.ids,
                               annotation.pkg) {
    pkg.dfr <- data.frame (pkg = c ("annotate",
                               "data.table",
                               paste0 (annotation.pkg, ".db")),
                           repos = c ("bioc", "cran", "bioc"),
                           stringsAsFactors = FALSE)
    instantPkgs (pkg.dfr)

    ## some probesets are mapped into more than one genomic location
    map.lst <- lookUp (affy.ids, annotation.pkg, "MAP")
    ## which ones?
    idd <- sapply (map.lst, function (x) length (x) > 1)
    ## concatenates the vectors of multiple genomic locations into vector of length one
    ## different genomic locations show separated by '; ' 
    ff <- function (v) Reduce (function (x, y) paste (x, y, sep = "; "), v)
    map.lst[idd] <- sapply (map.lst[idd], ff)
    ## return data.table
    data.table (vendor.id = affy.ids,
                symbol = unlist (lookUp (affy.ids, annotation.pkg, "SYMBOL")),
                gene.name = unlist (lookUp (affy.ids, annotation.pkg, "GENENAME")),
                entrez.id = as.numeric (lookUp (affy.ids, annotation.pkg, "ENTREZID")),
                accnum = unlist (lookUp (affy.ids, annotation.pkg, "ACCNUM")),
                map = unlist (map.lst))
}
