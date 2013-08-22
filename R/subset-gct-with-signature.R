subsetGCTwithSig <- function (gct,
                              sig) {
    idd <- gct$row.descriptions %in% sig
    list (row.descriptions = gct$row.descriptions[idd],
          data = gct$data[idd, , drop = FALSE])
}
