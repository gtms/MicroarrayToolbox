## getGO
## 29Jul2013
##
## given a character vector of gene symbols OR a character numeric vector of
## Entrez IDs, returns a data table, with the following variables:
## gene: factor with the gene symbols queried 
## go: character vector with each of the retrieved Gene Ontology identifiers
## go.evidence: factor of the retrieved Gene Ontology evidence codes
## (cf. http://www.geneontology.org/GO.evidence.shtml)
## go.ontology: factor of the retrieved Gene Ontology domains terms
## (cf. http://www.geneontology.org/GO.ontology.structure.shtml)
## term: factor of the retrieved Gene Ontology terms

getGO <- function (vec, # character vector of gene symbols OR numeric
                                        # vector of entrez gene ids
                   is.symbol = TRUE, # if FALSE, assumes vec is a
                                        # numeric vector of entrez gene ids
                   path = NULL) { # full path of the .csv file should
                                        # the user want to save the output
                                        # on disk
    pkg.dfr <- data.frame (pkg = c ("plyr",
                               "data.table",
                               "org.Hs.eg.db",
                               "GO.db"),
                           repos = c (rep ("cran", 2),
                               rep ("bioc", 2)),
                           stringsAsFactors = FALSE)
    instant.pkgs (pkg.dfr)

    ## reformats GO.db keys 
    myGO2df <- function (go) {
        dfr <- as.data.frame (Term (go$GOID))
        dfr <- cbind (go$Evidence, go$Ontology, dfr)
        colnames (dfr) <- c ("go.evidence", "go.ontology", "term")
        dfr
    }
    
    tab <- toTable (org.Hs.egSYMBOL)
    
    if (is.symbol) {
        vec <- vec[vec %in% tab$symbol]
        tab <- tab[tab$symbol %in% vec, ]
    } else {
        vec <- as.character (vec)
        vec <- vec[vec %in% tab$gene_id]
        tab <- tab[tab$gene_id%in% vec, ]
    }
    
    go.terms <- mget (tab$gene_id, org.Hs.egGO, ifnotfound = NA)
    go.terms <- setNames (go.terms, tab$symbol)
    go.terms <- go.terms[!is.na (go.terms)]
    go.lst <- llply (go.terms, function (gene) ldply (gene, myGO2df))
    go.dfr <- ldply (names (go.lst), function (gene) {
        cbind (rep (gene, nrow (go.lst[[gene]])), go.lst[[gene]])
    })
    colnames (go.dfr)[1:2] <- c ("gene", "go") 
    
    if (!is.null (path)) {
        write.csv2 (go.dfr, file = path)
    }
    data.table (go.dfr)
}
