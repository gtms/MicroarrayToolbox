getGO <- function (vec,
                   is.symbol = TRUE,
                   path = NULL) {
    ## require (org.Hs.eg.db)
    ## reformats GO.db keys
    myGO2df <- function (go) {
        dfr <- as.data.frame (Term (go$GOID))
        dfr <- cbind (go$Evidence, go$Ontology, dfr)
        colnames (dfr) <- c ("go.evidence", "go.ontology", "term")
        dfr
    }
    
    tab <- toTable (org.Hs.eg.db::org.Hs.egSYMBOL)
    
    if (is.symbol) {
        vec <- vec[vec %in% tab$symbol]
        tab <- tab[tab$symbol %in% vec, ]
    } else {
        vec <- as.character (vec)
        vec <- vec[vec %in% tab$gene_id]
        tab <- tab[tab$gene_id%in% vec, ]
    }
    
    go.terms <- mget (tab$gene_id, org.Hs.eg.db::org.Hs.egGO, ifnotfound = NA)
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
