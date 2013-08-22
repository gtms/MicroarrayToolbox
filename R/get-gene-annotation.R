getGeneAnnotation <- function (affy.ids,
                               annotation.pkg) {
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
