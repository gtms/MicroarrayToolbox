## subsetGCTwithSig
## 29Jul2013
##
## given a gct and a character vector of gene symbols (sig), returns the subset
## of the gct whose row.descriptions match sig

subsetGCTwithSig <- function (gct,
                              sig) { # character vector of gene symbols
    idd <- gct$row.descriptions %in% sig
    list (row.descriptions = gct$row.descriptions[idd],
          data = gct$data[idd, , drop = FALSE])
}
