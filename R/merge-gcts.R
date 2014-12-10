mergeGCTs <- function (gct.list) {
    gct.list <- lapply (gct.list, collapseRows)
    common.features <- Reduce (intersect, lapply (gct.list, function (gct) gct$row.descriptions))
    rdx.list <- lapply (gct.list, subsetGCTwithSig, sig = common.features)
    list (row.descriptions = common.features,
          data = do.call (cbind, lapply (rdx.list, function (gct) gct$data)))
}
