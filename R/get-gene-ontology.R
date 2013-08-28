getGO <- function (vec,
                   is.symbol = TRUE,
                   path = NULL) {
    ## removes duplicated values
    vec <- unique (vec)
    ## defines entrez gene IDs
    if (is.symbol) {
        symb2eg.dtb <- data.table (toTable (org.Hs.egSYMBOL2EG))
        egs <- symb2eg.dtb[symbol %in% vec, gene_id]
    } else {
        egs <- as.character (vec)
    }
    ## produces data frame with GO terms associated with entrez gene IDs
    go.terms <- toTable (org.Hs.egGO[egs])
    go.terms$Term <- Term (go.terms$go_id)
    ## adds gene symbols and transforms data frame into data table
    setkey (symb2eg.dtb, "gene_id")
    go.terms <- data.table (go.terms, key = "gene_id")
    dtb <- symb2eg.dtb[go.terms]
    ## rewrites colnames
    setnames (dtb, tolower (gsub ("_", ".", names (dtb))))
    ## saves table on disk if path != NULL
    if (!is.null (path)) {
        write.csv2 (dtb, file = path)
    }
    ## returns data table
    dtb
}
