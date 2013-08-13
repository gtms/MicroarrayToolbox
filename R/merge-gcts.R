## mergeGCTs
## 24Jul2013
## given a list of gct objects, returns a single merged gct with
## common features
##
## gct.list: list of gct objects
mergeGCTs <- function (gct.list) {
    gct.list <- lapply (gct.list, myCollapseRows)
    common.features <- Reduce (intersect, lapply (gct.list, function (gct) gct$row.descriptions))
    rdx.list <- lapply (gct.list, subsetGCTwithSig, sig = common.features)
    list (row.descriptions = common.features,
          data = do.call (cbind, lapply (rdx.list, function (gct) gct$data)))
}
